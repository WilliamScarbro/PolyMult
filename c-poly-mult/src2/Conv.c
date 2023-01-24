#include <stdio.h>

#include "Conv.h"


void Conv(interface_t inter){
  int* A=inter.X;
  int* B=inter.X+inter.n;
  for (int i=0; i<inter.n; i++){
    int j=0;
    int t1=0;
    int t2=0;
    for (; j<i+1; j++){
      t1+=(A[j]*B[i-j])%inter.p;
    }
    for (; j<inter.n; j++){
      t2+=(A[j]*B[inter.n+i-j])%inter.p;
    }
    printf("(%d) <- %d + %d * %d\n",i,t1,inter.W[inter.d],t2);
    inter.Y[i]=(t1+inter.W[inter.d]*t2)%inter.p;
    // zero rest of Y
    inter.Y[i+inter.n]=0;
  }
}
