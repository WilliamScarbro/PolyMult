#include <stdlib.h>
#include "timer.h"
#include "NTLib.h"
#include "Util.h"
#include "LPerm.h"
#include "Phi.h"
#include "Gamma.h"

void Sampled(int** X,int** Y){
  int w = Nth_root(257,generator(257),64);

  //Pre-Compute Constants
  int* W0 = malloc(64*sizeof(int));
  Phi_W(w,0,64,64,257,W0);

  initialize_timer();
  start_timer();

  //Computation
  Phi(64,64,0,64,257,*X+0,*Y+0,W0);

  stop_timer();
  printf("Elapsed time: %f\n",elapsed_time());

  //free Pre-Computed Constants
  free(W0);
}
int main(int argc, char** argv){
  int* X = malloc(sizeof(int)*64);
  int* Y = malloc(sizeof(int)*64);

  Sampled(&X,&Y);

  free(X);
  free(Y);
}
