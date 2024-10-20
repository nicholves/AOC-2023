#pragma once
#include <cstdint>
#include <string>
#include <vector>


typedef struct Range {
    uint64_t destinationStart;
    uint64_t sourceStart;
    uint64_t rangeLength;
} Range;

Range buildRange(const std::string& line);
std::vector<Range> linesToRanges(const std::vector<std::string>& lines, const std::string& header);

void parseSeedsLine(std::vector<uint64_t>& seeds, const std::string& line);
int findLineInLines(const std::vector<std::string>& lines, const std::string& line);

std::vector<uint64_t> progressThroughRanges(std::vector<uint64_t>& inputs, const std::vector<std::vector<Range>>& rangesets);
void progressRange(std::vector<uint64_t>& inputs, const std::vector<Range>& ranges);
uint64_t evaluateRange(uint64_t input, const Range& range);