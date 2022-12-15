#ifndef INTERFACE_H
#define INTERFACE_H

enum oper{
  Id_op,
  LPerm_op,
  Phi_op
};

struct interface{
  enum oper op;
  int n;
  int p;
  int N;
  int k;
  int d;
  int *X;
  int *Y;
  int *W;
};

typedef enum oper oper_t;
typedef struct interface interface_t;

void initialize(interface_t*,int n, int p, int N, int k, int d, int* X, int* Y);
void destroy(interface_t*);
// coppies p,N,W
void copy_context(interface_t* from, interface_t* to);


#endif
