#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

int numCols, numRows;



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

int isSymbol(char x) {
  if (x >= '0' && x <= '9') {
    return 0;
  }

  if (x == '.') {
    return 0;
  }

  if (x == '\n') {
    return 0;
  }

  return 1;
}


// returns a one if this part number is a adjascent to a symbol or zero otherwise
int includable(const char* const content, int pos, const uint64_t size) {
  if (content[pos] < '0' || content[pos] > '9') { // this is not a digit
    return 0;
  }


  for (int i = -1; i <= 1; ++i) {
    int posToCheck1 = pos - (numCols + 1) + i;
    int posToCheck2 = pos + i;
    int posToCheck3 = pos + (numCols + 1) + i;

    if (posToCheck1 >= 0 && posToCheck1 < size) {
      if (isSymbol(content[posToCheck1])) return 1;
    }

    if (posToCheck2 >= 0 && posToCheck2 < size) {
      if (isSymbol(content[posToCheck2])) return 1;
    }

    if (posToCheck3 >= 0 && posToCheck3 < size) {
      if (isSymbol(content[posToCheck3])) return 1;
    }
  }

  // finally if the next character in the array is a number we should check it also
  if (pos + 1 < size) {
    if (content[pos + 1] >= '0' && content[pos + 1] <= '9') 
      return includable(content, pos + 1, size);
  }


  return 0;
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
    // this character is not a digit
    if (content[i] < '0' || content[i] > '9') {
      i++;
      continue;
    }

    // we have a digit
    int partNumber = atoi(content + i);

    if (includable(content, i, size)) {
      totalSum += partNumber;
    }

    i += getNumDigits(partNumber);
  }

  free(content);

  printf("The result is: %d\n", totalSum);
  return 0;
}
