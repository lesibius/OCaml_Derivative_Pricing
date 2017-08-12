#include <stdio.h>
#include "../src/closedform.h"

void main()
{
  double call,put,forward;
  
  printf("test\n");

  forward  = bachelier_forward(0.02,98.01987,1);
  call = bachelier_call(0.02,98.01987,10,1,110);
  put = bachelier_put(0.02,98.01987,10,1,110);

  printf("forward = %f\n",forward);
  printf("call = %f\n",call);
  printf("put = %f\n",put);
  
}
