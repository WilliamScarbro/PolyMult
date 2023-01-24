#include <stdlib.h>

#include "Kernels.h"
#include "Gen.h"

#include "Util.h"
#include "LPerm.h"
#include "Phi.h"
#include "Conv.h"


void K16(int *X, int *Y){
  int p=17;
  int n=16;
  int k=2;
  int N=16;
  int d=16;
  
  // setup
  int w=Nth_root(17,generator(17),16);
  int *W0=malloc(16*sizeof(int));
  Phi_W(w,d,N,k,W0);

  // computation
  Phi(n,k,d,N,p,X,Y,W0);
}
