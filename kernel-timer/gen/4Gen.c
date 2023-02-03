#include <stdlib.h>
#include "timer.h"
#include "NTLib.h"
#include "Util.h"
#include "LPerm.h"
#include "Phi.h"
#include "Gamma.h"

void Sampled(int** X,int** Y){
  int w = Nth_root(17,generator(17),4);

  //Pre-Compute Constants
  int* W1 = malloc(4*sizeof(int));
  int* W2 = malloc(4*sizeof(int));
  int* W0 = malloc(16*sizeof(int));
  Phi_W(w,0,4,2,17,W1);
  Phi_W(w,2,4,2,17,W2);
  Phi_W(w,0,4,2,17,W0);

  initialize_timer();
  start_timer();

  //Computation
  Phi(4,2,0,4,17,*X+0,*Y+0,W0);
  swap(X,Y);
  Phi(2,2,0,4,17,*X+0,*Y+0,W1);
  Phi(2,2,2,4,17,*X+2,*Y+2,W2);

  stop_timer();
  printf("Elapsed time: %f\n",elapsed_time());

  //free Pre-Computed Constants
  free(W1);
  free(W2);
  free(W0);
}
int main(int argc, char** argv){
  int* X = malloc(sizeof(int)*4);
  int* Y = malloc(sizeof(int)*4);

  for(int i=0; i<4; i++){
    X[i]=i;
  }
  Sampled(&X,&Y);

  print_array("result",X,4);

  free(X);
  free(Y);
}
