#include <stdio.h>
#include <caml/mlvalues.h>
#include <math.h>

//From Christoph Bauer in the Caml-list mailing list

CAMLprim value erf_float(value x)
{
  return copy_double(erf(Double_val(x)));
}
