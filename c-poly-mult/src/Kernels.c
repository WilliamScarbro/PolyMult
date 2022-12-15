#include "Kernels.h"
#include "interface.h"

#include "LPerm.h"
#include "Phi.h"

// void initialize(interface_t*,int n, int p, int N, int k, int d, int* X, int* Y);

void Id(interface_t inter){
  for (int i=0; i<inter.n; i++){
    inter.Y[i]=inter.X[i];
  }
}
  
void swapInOut(interface_t* inter){
  int* temp=inter->X;
  inter->X=inter->Y;
  inter->Y=temp;
}
void LPerm16_4(int* X, int* Y){
  interface_t lp;
  initialize(&lp,16,17,16,4,1,X,Y);
  //printf("LPerm16_4 %d %d %d %d %d\n",lp.n,lp.p,lp.k,lp.d);
  LPerm(lp);
  destroy(&lp);
}

void Phi4(int* X, int* Y){
  interface_t inter;
  initialize(&inter,4,5,4,2,0,X,Y);
  Phi(inter);
  
  destroy(&inter);
}

void Phi4_inv(int* X, int* Y){
  interface_t inter;
  initialize(&inter,4,5,4,2,0,X,Y);
  Phi_inv(inter);
  destroy(&inter);
}

void Phi8_id(int* X, int* Y){
  interface_t inter;
  initialize(&inter,8,17,8,2,0,X,Y);
  Phi(inter);
  swapInOut(&inter);
  Phi_inv(inter);
  swapInOut(&inter);
  Id(inter);
  
}
