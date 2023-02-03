
#include <stdlib.h>

#include "timer.h"
#include "NTLib.h"
#include "Util.h"
#include "LPerm.h"
#include "Phi.h"
#include "Gamma.h"



void Generated4(int** X,int** Y){
  int w = Nth_root(5,generator(5),4);

  //Pre-Compute Constants
  int* W1 = malloc(2*sizeof(int));
  int* W2 = malloc(2*sizeof(int));
  int* W0 = malloc(4*sizeof(int));
  Phi_W(w,0,4,2,5,W1);
  Phi_W(w,2,4,2,5,W2);
  Phi_W(w,0,4,2,5,W0);
  
  initialize_timer();
  start_timer();

  //Computation
  Phi(4,2,0,4,5,*X+0,*Y+0,W0);
  swap(X,Y);
  Phi(2,2,0,4,5,*X+0,*Y+0,W1);
  Phi(2,2,0,4,5,*X+2,*Y+2,W1);
  swap(X,Y);
  Phi(2,2,0,4,5,*X+0,*Y+0,W1);
  Phi(2,2,2,4,5,*X+2,*Y+2,W2);

  stop_timer();
  
  //free Pre-Computed Constants
  free(W1);
  free(W2);
  free(W0);

  printf("Elapsed time: %f\n",elapsed_time());
}

int main(int argc, char** argv){
  int* X = malloc(sizeof(int)*4);
  int* Y = malloc(sizeof(int)*4);

  Generated4(&X,&Y);

  free(X);
  free(Y);
}
