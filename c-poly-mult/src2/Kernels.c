#include <stdlib.h>

#include "Kernels.h"
#include "Gen.h"

#include "Util.h"
#include "LPerm.h"
#include "Phi.h"
#include "Gamma.h"
#include "Conv.h"


void K16(int *X, int *Y){
  int** Xp=&X;
  int** Yp=&Y;
  int p=17;
  int n=16;
  int k=2;
  int N=64;
  int d=16; 
  
  // setup
  int w=Nth_root(17,generator(17),16);
  int *W0=malloc(16*sizeof(int));
  Phi_W(w,d,N,k,p,W0);
  int *W1=malloc(16*sizeof(int));
  Phi_inv_W(w,d,N,k,p,W1);
  
  // computation
  Phi(n,k,d,N,p,*Xp,*Yp,W0);
  swap(Xp,Yp);
  Phi_inv(n,k,d,N,p,*Xp,*Yp,W0);

  //cleanup
  swap(Xp,Yp);
  Id(n,*Xp,*Yp);
  free(W0);
  free(W1);
}

void Gamma4(int **X, int **Y){
  int p=17;
  int n=4;
  int N=8;
  int d=4;

  //setup
  int w=Nth_root(p,generator(p),N);
  int *W0=malloc(n*sizeof(int));
  Gamma_W(w,n,d,N,p,W0);
  int *W1=malloc(n*sizeof(int));
  Gamma_inv_W(w,n,d,N,p,W1);
  
  
  //computation
  Gamma(n, d, N, p, *X, *Y, W0);
  swap(X,Y);
  Gamma_inv(n,d,N,p,*X,*Y,W1);
  
  //cleanup
  swap(X,Y);
  Id(n,*X,*Y);
  free(W0);
}
