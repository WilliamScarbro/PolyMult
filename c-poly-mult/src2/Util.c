#include "Util.h"


void Id(interface_t inter){
  for (int i=0; i<inter.n; i++){
    inter.Y[i]=inter.X[i];
  }
}
  
void swapInOut(interface_t* inter){
  int* temp=inter->X;
  inter->X=inter->Y;
  inter->Y=temp;
}

void print_array(char* name,int* arr,int len){
  printf("%s: ",name);
  for(int i=0; i<len; i++){
    printf("%d ",arr[i]);
  }
  printf("\n");
}
