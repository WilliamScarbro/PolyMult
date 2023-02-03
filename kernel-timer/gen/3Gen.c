#include <stdlib.h>
#include "timer.h"
#include "NTLib.h"
#include "Util.h"
#include "LPerm.h"
#include "Phi.h"
#include "Gamma.h"

void Sampled(int** X,int** Y){
  int w = Nth_root(5,generator(5),4);

  //Pre-Compute Constants
  int* W0 = malloc(2*sizeof(int));
  int* W1 = malloc(2*sizeof(int));
  Phi_W(w,0,4,2,5,W0);
  Phi_W(w,2,4,2,5,W1);

  initialize_timer();
  start_timer();

  //Computation
  LPerm(4,2,*X+0,*Y+0);
  swap(X,Y);
  Phi(2,2,0,4,5,*X+0,*Y+0,W0);
  Phi(2,2,0,4,5,*X+2,*Y+2,W0);
  swap(X,Y);
  LPerm(4,2,*X+0,*Y+0);
  swap(X,Y);
  Phi(2,2,0,4,5,*X+0,*Y+0,W0);
  Phi(2,2,2,4,5,*X+2,*Y+2,W1);

  stop_timer();
  printf("Elapsed time: %f\n",elapsed_time());

  //free Pre-Computed Constants
  free(W0);
  free(W1);
}
int main(int argc, char** argv){
  int* X = malloc(sizeof(int)*4);
  int* Y = malloc(sizeof(int)*4);

  Sampled(&X,&Y);

  free(X);
  free(Y);
}
