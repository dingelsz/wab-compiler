/* For LLVM, you need a runtime function to produce ouput.  Include
   this file with final compilation with clang. For example:

   shell % clang out.ll misc/runtime.c -o out.exe
*/

#include <stdio.h>

int _print_int(int x) {
  printf("Out: %i\n", x);
  return 0;
}
