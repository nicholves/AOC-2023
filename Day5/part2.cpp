#include <iostream>
#include <cstdint>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <limits>
#include <thread>

#include "part2.hpp"


void outputNums(std::vector<uint64_t>& nums) {
  std::cout << "Current values: " << std::endl;
  for (auto num : nums) {
    std::cout << num << std::endl;
  }
}

void outputRange(const Range& range) {
  std::cout << "From: " << range.sourceStart << " until that plus: " << range.rangeLength << " to: " << range.destinationStart << std::endl;
}

std::vector<uint64_t> progressThroughRanges(std::vector<uint64_t>& inputs, const std::vector<std::vector<Range>>& rangesets) {
  int numSets = rangesets.size();
  int finished = 0;
  const auto processorCount = std::thread::hardware_concurrency() - 2; // 2 less than the max to be friendly

  // needed data
  uint64_t* data = inputs.data();
  uint64_t seedsPerThread = inputs.size() / processorCount;

  // danger if we have more threads than seeds
  if (seedsPerThread < 1) {
      seedsPerThread = inputs.size();
  }
  
  
  uint64_t totalSeeds = inputs.size();


  for (const std::vector<Range>& rangeSet : rangesets) {
    uint64_t i = 0;
    std::vector<std::thread> threads;

    // create all the threads and give assignments
    while (i + seedsPerThread < totalSeeds) {
        threads.emplace_back(progressRange, data, rangeSet, i, i + seedsPerThread);
        i += seedsPerThread;
    }
    threads.emplace_back(progressRange, data, rangeSet, i, totalSeeds);

    // join back with all the threads
    for (auto& thread : threads) {
        thread.join();
    }

    finished++;
    std::cout << "Completed computation for: " << finished << " of: " << numSets << std::endl;
  }

  return inputs;
}

void progressRange(uint64_t* inputs, const std::vector<Range>& ranges, uint64_t start, uint64_t end) {
  for (uint64_t i = start; i < end; ++i) {
    uint64_t before = inputs[i];
    for (const Range& range : ranges ) {
        uint64_t result = evaluateRange(before, range);

        if (result != before) {
          inputs[i] = result;
          break;
        }
    }
  }
}

uint64_t evaluateRange(uint64_t input, const Range& range) {
  if ((range.sourceStart <= input) && (input <= range.sourceStart + range.rangeLength)) {
    return range.destinationStart + (input - range.sourceStart);
  }

  return input;
}

Range buildRange(const std::string& line) {
  std::stringstream nums(line);
  Range range;

  nums >> range.destinationStart;
  nums >> range.sourceStart;
  nums >> range.rangeLength;

  return range;
}

std::vector<Range> linesToRanges(const std::vector<std::string>& lines, const std::string& header) {
  int lineIndex = findLineInLines(lines, header) + 1;
  std::vector<Range> ranges;
  while (!lines[lineIndex].empty() && lines[lineIndex][0] < 'A') {
    ranges.emplace_back(buildRange(lines[lineIndex]));
    lineIndex++;
  }

  return ranges;
}

void parseSeedsLine(std::vector<uint64_t>& seeds, const std::string& line) {
  int position = sizeof("seeds: ") - 1;


  std::stringstream nums(line.substr(position));
  uint64_t num;

  while (!nums.eof()) {
    nums >> num;
    seeds.push_back(num);
  }
}

int findLineInLines(const std::vector<std::string>& lines, const std::string& line) {
  int i = 0;

  while (lines[i] != line) {
    i++;
  }

  return i;
}

void buildSeedsFromRanges(std::vector<uint64_t>& seeds, const std::vector<uint64_t>& ranges) {
  for (int i = 0; i < ranges.size(); i+=2) {
    for (uint64_t j = ranges[i]; j < ranges[i] + ranges[i+1]; ++j) {
      seeds.push_back(j);
    }
  }
}

int main(int argc, char** argv) {
  if (argc < 2) {
    std::cout << "Please provide a FilePath argument" << std::endl;
    return 1;
  }

  // open and read file
  std::ifstream file(argv[1]);

  if (!file.is_open()) {
    std::cout << "Invalid FilePath: " << argv[1] << " provided" << std::endl;
    return 1;
  }
  
  std::vector<std::string> lines;
  while (!file.eof()) {
    std::string line;
    std::getline(file, line);

    if (!line.empty())
      lines.push_back(line);
  }
  file.close();

  std::vector<uint64_t> initSeeds;
  parseSeedsLine(initSeeds, lines[0]);
  std::vector<uint64_t> seeds;

  buildSeedsFromRanges(seeds, initSeeds);

  std::cout << "Total seeds: " << seeds.size() << std::endl;




  std::vector<std::vector<Range>> ranges;


  // seed to soil
  std::vector<Range> seedToSoilRanges = linesToRanges(lines, "seed-to-soil map:");
  ranges.push_back(seedToSoilRanges);

  // seed to fertilizer
  std::vector<Range> soilToFertilRanges = linesToRanges(lines, "soil-to-fertilizer map:");
  ranges.push_back(soilToFertilRanges);

  // fertilizer to water
  std::vector<Range> fertilToWaterRanges = linesToRanges(lines, "fertilizer-to-water map:");
  ranges.push_back(fertilToWaterRanges);

  // water to light
  std::vector<Range> waterToLightRanges = linesToRanges(lines, "water-to-light map:");
  ranges.push_back(waterToLightRanges);

  // light to temperature
  std::vector<Range> lightToTempRanges = linesToRanges(lines, "light-to-temperature map:");
  ranges.push_back(lightToTempRanges);

  // temperature to humidity
  std::vector<Range> tempToHumidRanges = linesToRanges(lines, "temperature-to-humidity map:");
  ranges.push_back(tempToHumidRanges);

  // humidity to location
  std::vector<Range> humidToLocRanges = linesToRanges(lines, "humidity-to-location map:");
  ranges.push_back(humidToLocRanges);


  progressThroughRanges(seeds, ranges);

  uint64_t min = std::numeric_limits<uint64_t>::max();
  for (uint64_t location : seeds) {
    if (location < min) {
      min = location;
    }
  }

  std::cout << "The minimum location is: " << min << std::endl;

  return 0;
}
