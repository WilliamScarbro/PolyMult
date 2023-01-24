
#include <stdio.h>
#include <stdlib.h>

#include "colors.h"

#include "LPerm.h"

void LPerm(int n, int k, int* X, int *Y){
  #ifdef DEBUG
  printf("LPerm %d %d\n",n,k); 
  #endif
  int m=n/k;
  for (int i=0; i<m; i++){
    for (int j=0; j<k; j++){
      Y[j+k*i]=X[m*j+i];
    }
  }
}
  
