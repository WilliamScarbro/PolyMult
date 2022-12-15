#include <stdlib.h>

#include "interface.h"
#include "Gen.h"

void initialize(interface_t *inter, int n, int p, int N, int k, int d, int* X, int* Y){
  inter->n=n;
  inter->p=p;
  inter->N=N;
  inter->k=k;
  inter->d=d;
  inter->X=X;
  inter->Y=Y;
  inter->W=malloc((N+1)*sizeof(int));
  root_powers(p,N,inter->W);
}

void destroy(interface_t* inter){
  if (inter->W!=NULL)
    free(inter->W);;
}

void copy_context(interface_t* from, interface_t* to){
  to->W=from->W;
  to->p=from->p;
  to->N=from->N;
}
