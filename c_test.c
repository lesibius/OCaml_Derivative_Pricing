#include <stdio.h>
#include "closedform.h"

void main()
{
  double x;
  
  printf("test\n");
  x = 10;

  x = bachelier_call(0.01,0.5,99.50125,10,110);

  printf("x = %f\n",x);
  
}
