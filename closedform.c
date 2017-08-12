#include <math.h>
#include "closedform.h"


double N0(double value)
{
   return 0.5 * erfc(-value * M_SQRT1_2);
}




/*************************************************
                    BACHELIER
 *************************************************/

double bachelier_call(double rf, double t, double s, double sigma, double k)
{
  double D,F; 
  double sigma_bar;
  double d;
  double A,B; //Two parts of the pricing formula
  
  F = s * exp(t * rf);
  D = exp(-t * rf);
  sigma_bar = sigma * sqrt(t) / s;

  d = (F - k) / (F * sigma_bar);

  A = (F - k) * N0(d);
  B = (F * sigma_bar) * (M_SQRT1_2 / sqrt(M_PI)) * exp(-pow(d,2)/2);
  
  return(D*(A+B));
}


double bachelier_put()
{
  return(2);
}
