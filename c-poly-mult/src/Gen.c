#include "Gen.h"

int pow(int x, int e, int p){
  while (e!=0){
    if (e%2==1)
      x=(x*x)%p;
    e/=2;
  }
  return x;
}

// bad things will happen if p is not prime, but we do not check
int generator(int p){
  for (i=2; i<p; i++){
    int x=i;
    bool gen=True;
    for (j=1; j<p-1; j++){
      if (x==1){
	gen=False;
	break;
      }
      x=(x*i)%p;
    }
    if (gen)
      return i;
  }
}

int Nth_root(int p, int gen, int N){
  if (N%(p-1)!=0){
    printf("%d does not divide %d-1",N,p);
    exit(1);
  }
  return pow(gen,(p-1)/N,p);
}

void root_powers(int p, int N, int* W){
  int w=Nth_root(p,generator(p),N);
  W[0]=1;
  for (int i=1; i<N; i++){
    W[i]=(W[i-1]*w)%p;
  }
}
