#include <stdlib.h>
#include "timer.h"
#include "NTLib.h"
#include "Util.h"
#include "LPerm.h"
#include "Phi.h"
#include "Gamma.h"

void Sampled(int** X,int** Y){
  int w = Nth_root(17,generator(17),16);

  //Pre-Compute Constants
  int* W7 = malloc(4*sizeof(int));
  int* W11 = malloc(4*sizeof(int));
  int* W9 = malloc(4*sizeof(int));
  int* W13 = malloc(4*sizeof(int));
  int* W8 = malloc(4*sizeof(int));
  int* W12 = malloc(4*sizeof(int));
  int* W10 = malloc(4*sizeof(int));
  int* W14 = malloc(4*sizeof(int));
  int* W3 = malloc(16*sizeof(int));
  int* W5 = malloc(16*sizeof(int));
  int* W4 = malloc(16*sizeof(int));
  int* W6 = malloc(16*sizeof(int));
  int* W1 = malloc(64*sizeof(int));
  int* W2 = malloc(64*sizeof(int));
  int* W0 = malloc(256*sizeof(int));
  Phi_W(w,0,16,2,17,W7);
  Phi_W(w,2,16,2,17,W11);
  Phi_W(w,4,16,2,17,W9);
  Phi_W(w,6,16,2,17,W13);
  Phi_W(w,8,16,2,17,W8);
  Phi_W(w,10,16,2,17,W12);
  Phi_W(w,12,16,2,17,W10);
  Phi_W(w,14,16,2,17,W14);
  Phi_W(w,0,16,2,17,W3);
  Phi_W(w,4,16,2,17,W5);
  Phi_W(w,8,16,2,17,W4);
  Phi_W(w,12,16,2,17,W6);
  Phi_W(w,0,16,2,17,W1);
  Phi_W(w,8,16,2,17,W2);
  Phi_W(w,0,16,2,17,W0);

  initialize_timer();
  start_timer();

  //Computation
  Phi(16,2,0,16,17,*X+0,*Y+0,W0);
  swap(X,Y);
  Phi(8,2,0,16,17,*X+0,*Y+0,W1);
  Phi(8,2,8,16,17,*X+8,*Y+8,W2);
  swap(X,Y);
  Phi(4,2,0,16,17,*X+0,*Y+0,W3);
  Phi(4,2,8,16,17,*X+4,*Y+4,W4);
  Phi(4,2,4,16,17,*X+8,*Y+8,W5);
  Phi(4,2,12,16,17,*X+12,*Y+12,W6);
  swap(X,Y);
  Phi(2,2,0,16,17,*X+0,*Y+0,W7);
  Phi(2,2,8,16,17,*X+2,*Y+2,W8);
  Phi(2,2,4,16,17,*X+4,*Y+4,W9);
  Phi(2,2,12,16,17,*X+6,*Y+6,W10);
  Phi(2,2,2,16,17,*X+8,*Y+8,W11);
  Phi(2,2,10,16,17,*X+10,*Y+10,W12);
  Phi(2,2,6,16,17,*X+12,*Y+12,W13);
  Phi(2,2,14,16,17,*X+14,*Y+14,W14);

  stop_timer();
  printf("Elapsed time: %f\n",elapsed_time());

  //free Pre-Computed Constants
  free(W7);
  free(W11);
  free(W9);
  free(W13);
  free(W8);
  free(W12);
  free(W10);
  free(W14);
  free(W3);
  free(W5);
  free(W4);
  free(W6);
  free(W1);
  free(W2);
  free(W0);
}
int main(int argc, char** argv){
  int* X = malloc(sizeof(int)*16);
  int* Y = malloc(sizeof(int)*16);

  for(int i=0; i<16; i++){
    X[i]=i;
  }
  Sampled(&X,&Y);

  print_array("result",X,16);

  free(X);
  free(Y);
}
