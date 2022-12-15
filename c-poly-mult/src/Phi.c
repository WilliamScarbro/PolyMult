#include <stdio.h>

#include "Gen.h"
#include "Phi.h"

void Phi(interface_t inter){
  printf("Phi n:%d k:%d d:%d N:%d p:%d\n",inter.n,inter.k,inter.d,inter.N,inter.p);
  int d=inter.d;
  int k=inter.k;
  int N=inter.N;
  int p=inter.p;
  int n=inter.n;
  int* X=inter.X;
  int* Y=inter.Y;
  int* W=inter.W;
  int m=n/k;
  int z,j,i;
  for(z=0; z<k; z++){
    for(j=0; j<m; j++){
      // for less ops
      int w_step=(d+z*N)/k;
      int y_new=0;
      for(i=0; i<k; i++){
	printf("(%d %d %d) X:%d W_i:%d\n",z,j,i,X[i*m+j],(w_step*i));
	y_new+=(X[i*m+j]*W[w_step*i])%p;
      }
      Y[z*m+j]=y_new%p;
    }
  }
}

void Phi_inv(interface_t inter){
  printf("Phi_inv n:%d k:%d d:%d N:%d p:%d\n",inter.n,inter.k,inter.d,inter.N,inter.p);
  int d=inter.d;
  int k=inter.k;
  int N=inter.N;
  int p=inter.p;
  int n=inter.n;
  int* X=inter.X;
  int* Y=inter.Y;
  int* W=inter.W;
  int m=n/k;
  int z,j,i;
  int k_inv=inverse(k,p);
  for(i=0; i<k; i++){
    for(j=0; j<m; j++){
      // for less ops
      int w_step=(d+z*N)/k;
      int y_new=0;
      for(z=0; z<k; z++){
	//printf("(%d %d %d) X:%d W_i:%d\n",i,j,z,X[z*m+j],(N-(d+z*N)/k*i));
	y_new+=(X[z*m+j]*W[(N-(d+z*N)/k*i)])%p;
      }
      Y[i*m+j]=(y_new*k_inv)%p;
      //printf("<- %d\n",bad);
    }
  }
}

