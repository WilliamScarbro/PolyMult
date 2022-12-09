#include "Phi.h"

void Phi(int n,int* args, int* X, int* Y,int* W){
  int d=args[0];
  int k=args[1];
  int N=args[2];
  int p=args[3];
  int m=n/k;
  int z,j,i;
  for(z=0; z<m; z++){
    for(j=0; j<k; j++){
      Y[z*m+j]=0;
      for(i=0; i<k; i++){
	Y[z*m+j]+=(a[i*m+j]*W[(d+z*N)*i/k])%p;
      }
      Y[z*m+j]%=p;
    }
  }
}
