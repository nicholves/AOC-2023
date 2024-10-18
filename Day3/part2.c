#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "part2.h"

int numCols, numRows;

int min(int x, int y) {
  return x <= y ? x : y;
}

int max(int x, int y) {
  return x >= y ? x : y;
}

int getNumLines(const char* const content, uint64_t size) {
  int i = 0;

  int lineCount = 1;

  while (i < size) {
    if (content[i] == '\n' && i != size - 1) {
      lineCount++;
    }

    i++;
  }

  return lineCount;
}

int isDigit(char x) {
  return x >= '0' && x <= '9';
}

int distinctParts(const char* const content, int pos1, int pos2, const uint64_t size) {
  int start = min(pos1, pos2);
  int end = max(pos1, pos2);

  if (start < 0 || end > size) {
    return 0;
  }

  if (!isDigit(content[start]) || !isDigit(content[end])) {
    return 0;
  }


  // there should be at least 1 non digit between these positions for them to be distinct
  for (int i = start + 1; i < end; ++i) {
    if (!isDigit(content[i])) 
      return 1;
  }


  return 0;
}

int isGear(const char* const content, int pos, const uint64_t size, int* gearRatio) {
  if (content[pos] != '*') {
    return 0;
  }

  int adjascentPartNumbers = 0;

  int posesToCheck[] = {pos - (numCols + 1) - 1, pos - (numCols + 1) + 0, pos - (numCols + 1) + 1,
                        pos - 1,                                          pos + 1,
                        pos + (numCols + 1) - 1, pos + (numCols + 1) + 0, pos + (numCols + 1) + 1};

  
  int excludedIndexes[2048];
  int numExcludedIndexes = 0;

  for (int i = 0; i < sizeof(posesToCheck) / sizeof(int); ++i) {
    for (int j = 0; j < sizeof(posesToCheck) / sizeof(int); ++j) {
      if (i == j) continue;

      int flag = 0;
      for (int k = 0; k < numExcludedIndexes; ++k) {
        if (posesToCheck[i] == excludedIndexes[k] || posesToCheck[j] == excludedIndexes[k]) 
          flag = 1;
      }

      if (flag)
        continue;

      if (distinctParts(content, posesToCheck[i], posesToCheck[j], size)) {
        getDiscludedIndicesFromPart(content, posesToCheck[i], size, excludedIndexes, &numExcludedIndexes);
        getDiscludedIndicesFromPart(content, posesToCheck[j], size, excludedIndexes, &numExcludedIndexes);

        adjascentPartNumbers += 2;
        int partValue1 = getPartNumberValue(content, posesToCheck[i], size);
        int partValue2 = getPartNumberValue(content, posesToCheck[j], size);

        *gearRatio = partValue1 * partValue2;
      }
    }
  }

  int isAGear = adjascentPartNumbers == 2;

  return isAGear;
}


int getPartNumberValue(const char* const content, int pos, const uint64_t size) {
  while (pos > 0 && isDigit(content[pos - 1])) {
    pos--;
  }

  return atoi(content + pos);
}

void getDiscludedIndicesFromPart(const char* const content, int pos, const uint64_t size, int* indices, int* numExcluded) {
  indices[(*numExcluded)++] = pos;

  int curPos = pos;
  while (curPos - 1 >= 0 && isDigit(content[curPos - 1])) {
    indices[(*numExcluded)++] = --curPos;
  }

  curPos = pos;
  while (curPos + 1 < size && isDigit(content[curPos + 1])) {
    indices[(*numExcluded)++] = ++curPos;
  }
}

int getNumDigits(int x) {
  int numDigits = 1;

  while (x >= 10) {
    numDigits++;
    x = x / 10;
  }

  return numDigits;
}


int main(int argc, char** argv) {
  if (argc < 2) {
    printf("No arguments provided\n");
    return 1;
  }

  // open and read file
  FILE* fptr = fopen(argv[1], "r");

  if (fptr == NULL) {
    printf("The file %s cannot be opened\n", argv[1]);
    return 1;
  }
  
  // get length of file
  fseek(fptr, 0, SEEK_END);
  const uint64_t size = ftell(fptr);
  
  fseek(fptr, 0, SEEK_SET);

  // alocate memory for file
  const char* const content = malloc(size + 1);

  fread(content, 1, size + 1, fptr);
  fclose(fptr);


  // get the no of columns per line
  int i = 0;
  while (content[i] != '\n') {
    i++;
  }

  const int colsPerLine = i;
  numCols = colsPerLine;
  numRows = getNumLines(content, size);


  int totalSum = 0;


  i = 0;
  while (i < size) {
    int gearRatio = 0;

    if (isGear(content, i, size, &gearRatio)) {
      totalSum += gearRatio;
    }

    i++;
  }

  free(content);

  printf("The result is: %d\n", totalSum);
  return 0;
}
