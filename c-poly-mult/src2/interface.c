#include <stdlib.h>
#include <stdio.h>

#include "interface.h"
#include "Gen.h"

// n|N and n|d
void check_strict(interface_t inter){
  if (inter.N%inter.n!=0){
    printf("Error: n:%d does not divide N:%d\n",inter.n,inter.N);
    exit(1);
  }
  if (inter.d%inter.n!=0){
    printf("Error: n:%d does not divide d:%d\n",inter.n,inter.d);
    exit(1);
  }
  //if (! is_prime(inter.p)){
  //  printf("Error: p:%d is not prime\n",inter.p);
  //  exit(1);
  //}
}

// k|N and k|d
// k|n 
void check(interface_t inter){
  if (inter.N%inter.k!=0){
    printf("Error: k:%d does not divide N:%d\n",inter.k,inter.N);
    exit(1);
  }
  if (inter.d%inter.k!=0){
    printf("Error: k:%d does not divide d:%d\n",inter.k,inter.d);
    exit(1);
  }
  if (inter.n%inter.k!=0){
    printf("Error: k:%d does not divide n:%d\n",inter.k,inter.n);
    exit(1);
  }
}

void initialize(interface_t *inter, int n, int p, int N, int k, int d, int* X, int* Y){
  inter->n=n;
  inter->p=p;
  inter->N=N;
  inter->k=k;
  inter->d=d%N; // inforces same ordering in outputs
  inter->X=X;
  inter->Y=Y;
  check(*inter);
  check_strict(*inter);
  inter->W=malloc((N+1)*sizeof(int));
  root_powers(p,N,inter->W);
}

void destroy(interface_t* inter){
  if (inter->W!=NULL)
    free(inter->W);;
}

void copy_context(interface_t* from, interface_t* to){
  to->n=from->n;
  to->p=from->p;
  to->N=from->N;
  to->k=from->k;
  to->d=from->d;
  to->X=from->X;
  to->Y=from->Y;
  to->W=from->W;
}
