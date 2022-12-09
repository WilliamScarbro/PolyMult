
#include <stdio.h>
#include <stdlib.h>

#include "colors.h"

#include "LPerm.h"

void LPerm(int n,int* args, int* X, int* Y){
  int k=args[0];
  if (n%k!=0){
    fprintf(stdout, "k: %d does not divide n: %d\n",k,n);
    exit(1);
  }
  int m=n/k;
  for (int i=0; i<m; i++){
    for (int j=0; j<k; j++){
      Y[j+k*i]=X[m*j+i];
    }
  }
}
  
