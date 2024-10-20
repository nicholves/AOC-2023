#pragma once
#include <cstdint>
#include <string>
#include <vector>


typedef struct Range {
    uint64_t destinationStart;
    uint64_t sourceStart;
    uint64_t rangeLength;
} Range;


typedef struct SeedRange {
    uint64_t start;
    uint64_t end;
} SeedRange;

Range buildRange(const std::string& line);
std::vector<Range> linesToRanges(const std::vector<std::string>& lines, const std::string& header);

void parseSeedsLine(std::vector<uint64_t>& seeds, const std::string& line);
int findLineInLines(const std::vector<std::string>& lines, const std::string& line);

std::vector<uint64_t> progressThroughRanges(std::vector<uint64_t>& inputs, const std::vector<std::vector<Range>>& rangesets);
void progressRange(uint64_t* inputs, const std::vector<Range>& ranges, uint64_t start, uint64_t end);
uint64_t evaluateRange(uint64_t input, const Range& range);

void buildSeedsFromRanges(std::vector<uint64_t>& seeds, const std::vector<uint64_t>& ranges);