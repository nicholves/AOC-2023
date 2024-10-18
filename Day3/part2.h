#pragma once

int min(int x, int y);
int max(int x, int y);

int getNumLines(const char* const content, uint64_t size);
int isDigit(char x);
int getNumDigits(int x);

int distinctParts(const char* const content, int pos1, int pos2, const uint64_t size);
int isGear(const char* const content, int pos, const uint64_t size, int* gearRatio);
int getPartNumberValue(const char* const content, int pos, const uint64_t size);
void getDiscludedIndicesFromPart(const char* const content, int pos, const uint64_t size, int* indices, int* numExcluded);