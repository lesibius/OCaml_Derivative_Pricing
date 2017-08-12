#include <math.h>
#include "closedform.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>


double N0(double value)
{
   return 0.5 * erfc(-value * M_SQRT1_2);
}




/*************************************************
                    BACHELIER
 *************************************************/

double bachelier_forward(double rf, double s, double t)
{
  return(s * exp(rf * t));
}


double bachelier_call(double rf, double s, double sigma, double t, double k)
{
  double D,F; 
  double sigma_bar;
  double d;
  double A,B; //Two parts of the pricing formula
  
  F = bachelier_forward(rf,s,t);
  D = exp(-t * rf);
  sigma_bar = sigma * sqrt(t) / s;

  d = (F - k) / (F * sigma_bar);

  A = (F - k) * N0(d);
  B = (F * sigma_bar) * (M_SQRT1_2 / sqrt(M_PI)) * exp(-pow(d,2)/2);
  
  return(D*(A+B));
}

double bachelier_put(double rf, double s, double sigma, double t, double k)
{
  double D,F; 
  double sigma_bar;
  double d;
  double A,B; //Two parts of the pricing formula
  
  F = bachelier_forward(rf,s,t);
  D = exp(-t * rf);
  sigma_bar = sigma * sqrt(t) / s;

  d = (F - k) / (F * sigma_bar);

  A = (k - F) * N0(-d);
  B = (F * sigma_bar) * (M_SQRT1_2 / sqrt(M_PI)) * exp(-pow(d,2)/2);
  
  return(D*(A+B));
}



/*
CAMLprim value caml_bachelier_call(value rf, value t, value s, value sigma, value k)
{
  value RF,T,S,SIGMA,K;
  double result;
  
  RF = Double_val(rf);
  T = Double_val(t);
  S = Double_val(s);
  SIGMA = Double_val(sigma);
  K = Double_val(k);

  result = bachelier_call(RF,T,S,SIGMA,K);

  return caml_copy_double(result);
  
}
*/
