#include <stdio.h>

void fprint_array(FILE *stream, int array[], int length) {
  fprintf(stream, "[");
  for (int i = 0; i < length; i++) {
    fprintf(stream, "%d", array[i]);
    if (i < length - 1) {
      fprintf(stream, ", ");
    }
  }
  fprintf(stream, "]");
}
