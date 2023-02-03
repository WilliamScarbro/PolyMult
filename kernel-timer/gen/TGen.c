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
  int* W0 = malloc(4*sizeof(int));
  int* W1 = malloc(4*sizeof(int));
  Phi_W(w,0,4,2,5,W0);
  Phi_W(w,2,4,2,5,W1);

  initialize_timer();
  start_timer();

  //Computation
  TPerm(16,2,2,*X,*Y);
  
  stop_timer();
  printf("Elapsed time: %f\n",elapsed_time());

  //free Pre-Computed Constants
  free(W0);
  free(W1);
}
int main(int argc, char** argv){
  int n=16;
  int* X = malloc(sizeof(int)*n);
  int* Y = malloc(sizeof(int)*n);

  for (int i=0; i<n; i++)
    X[i]=i;
  
  Sampled(&X,&Y);

  print_array("result",Y,n);
  free(X);
  free(Y);
}
