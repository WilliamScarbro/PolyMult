#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>

#include "interface.h"

void Id(int n, int* X, int *Y);

void swap(int** X,int** Y);

void swapInOut(interface_t* inter);

void print_array(char*,int*,int);

#endif
