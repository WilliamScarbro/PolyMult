/*
 * ============================================================================
 *
 *       Filename:  main.c
 *
 *    Description:  Main file of the project
 *
 *        Created:  03/24/2016 19:40:56
 *
 *         Author:  Gustavo Pantuza
 *   Organization:  Software Community
 *
 * ============================================================================
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "args.h"
#include "colors.h"
#include "Kernels.h"

//#include "Interface.h"

int* read_inputs(char* fname, int len){
  FILE* fp = fopen(fname,"r");
  int* res=malloc(len*sizeof(int));
  if (fp==NULL){
    printf("Cannot open file %s\n",fname);
    exit(1);
  }
  int i=0;
  int num;
  //printf("FLAG1\n");
  while(i<len && 1==fscanf(fp,"%d",&num)){
    //printf("read %d\n",num);
    res[i++]=num;
  }
  if (i!=len){
    printf("Expected more elements in file. Expected %d got %d\n",len,i);
    exit(1);
  }
  return res;
}

int* parse_ints(char* str, int len){
  int* res=malloc(len*sizeof(int));
  char* tok=strtok(str," ");
  int i=0;
  while(tok!=NULL && i<len){
    //printf("token: %s\n",tok);
    res[i++]=atoi(tok);
    if (res[i-1]==0 && strcmp(tok,"0")!=0){
      printf("'%s' is not number\n",tok);
      exit(1);
    }
    tok=strtok(NULL," ");
  }
  return res;
}

void trace(char* kernel, int size){
  fprintf(stdout,GREEN "Executing %s size %d\n",kernel,size);
}

void check_size(int actual, int expected){
  if (actual!=expected){
    printf("incorrect number of inputs, expected %d got %d",expected,actual);
    exit(1);
  }
}

void print_array(char* name,int* arr,int len){
  printf("%s: ",name);
  for(int i=0; i<len; i++){
    printf("%d ",arr[i]);
  }
  printf("\n");
}

int main (int argc, char* argv[])
{

    /* Read command line options */
    options_t options;
    options_parser(argc, argv, &options);


#ifdef DEBUG
    fprintf(stdout, BLUE "Command line options:\n" NO_COLOR);
    fprintf(stdout, BROWN "help: %d\n" NO_COLOR, options.help);
    fprintf(stdout, BROWN "version: %d\n" NO_COLOR, options.version);
    fprintf(stdout, BROWN "kernel_name: %s\n" NO_COLOR, options.kernel_name);
    fprintf(stdout, BROWN "input_size: %d\n" NO_COLOR, options.input_size);
    fprintf(stdout, BROWN "use colors: %d\n" NO_COLOR, options.use_colors);
    fprintf(stdout, BROWN "filename: %s\n" NO_COLOR, options.file_name);
      
#endif

      int n = options.input_size;
      int* X = read_inputs(options.file_name,n);
      int* Y = malloc(n*sizeof(int));

      int matched=0;
      
      if (strcmp(options.kernel_name,"LPerm16_4")==0){
	matched=1;
	check_size(16,n);
	trace(options.kernel_name,n);
	print_array("input",X,n);
	LPerm16_4(X,Y);
	print_array("result",Y,n);
      }

      if (strcmp(options.kernel_name,"Phi4")==0){
	matched=1;
	check_size(4,n);
	trace(options.kernel_name,n);
	print_array("input",X,n);
	Phi4(X,Y);
	print_array("result",Y,n);
      }
      if (strcmp(options.kernel_name,"Phi4_inv")==0){
	matched=1;
	check_size(4,n);
	trace(options.kernel_name,n);
	print_array("input",X,n);
	Phi4_inv(X,Y);
	print_array("result",Y,n);
      }
      if (strcmp(options.kernel_name,"Phi8_id")==0){
	matched=1;
	check_size(8,n);
	trace(options.kernel_name,n);
	print_array("input",X,n);
	Phi8_id(X,Y);
	print_array("result",Y,n);
      }

      
      if (!matched){
	printf("kernel name %s did not match known kernels\n",options.kernel_name);
	exit(1);
      }
      free(X);
      free(Y);

      
    /* Do your magic here :) */
    /* inputs
      kernel_name: char*
      input-size : int
      input : int[input-size]
    */
    
    
    return EXIT_SUCCESS;
}

