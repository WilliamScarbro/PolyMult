
#include <stdio.h>
#include <stdlib.h>

#include "colors.h"

#include "LPerm.h"

void LPerm(interface_t inter){
  printf("LPerm %d %d\n",inter.n,inter.k); 
  int m=inter.n/inter.k;
  for (int i=0; i<m; i++){
    for (int j=0; j<inter.k; j++){
      inter.Y[j+inter.k*i]=inter.X[m*j+i];
    }
  }
}
  
