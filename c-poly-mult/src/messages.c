/*
 * ============================================================================
 *
 *       Filename:  messages.c
 *
 *    Description:  Program messages implementation
 *
 *        Created:  24/03/2016 22:48:39
 *       Compiler:  gcc
 *
 *         Author:  Gustavo Pantuza
 *   Organization:  Software Community
 *
 * ============================================================================
 */


#include <stdio.h>
#include <stdlib.h>


#include "colors.h"
#include "messages.h"



/*
 * Help message
 */
void
help ()
{
    fprintf(stdout, BLUE __PROGRAM_NAME__ "\n\n" NO_COLOR);
    usage();
    description();
    options();
    author();
    version();
}



/*
 * Usage message
 */
void
usage ()
{
    fprintf(stdout, BROWN "Usage: " NO_COLOR);
    fprintf(stdout, "%s [options] input file\n\n", __PROGRAM_NAME__);
}



/*
 * Description message
 */
void
description ()
{
    fprintf(stdout, BROWN "Description: " NO_COLOR);
    fprintf(stdout, "Various kernels for performing polynomial multiplication.");
}



/*
 * Options message
 */
void
options ()
{
    fprintf(stdout, BROWN "Options:\n\n" NO_COLOR);
    fprintf(stdout, GRAY "\t-v|--version\n" NO_COLOR
                    "\t\tPrints %s version\n\n", __PROGRAM_NAME__);
    fprintf(stdout, GRAY "\t-h|--help\n" NO_COLOR
                    "\t\tPrints this help message\n\n");
    fprintf(stdout, GRAY "\t--no-color\n" NO_COLOR
                    "\t\tDoes not use colors for printing\n\n");
    fprintf(stdout, GRAY "\t-k|--kernel\n" NO_COLOR
	    "\t\tSelect which kernel to execute\n\n");
    fprintf(stdout, GRAY "\t-s|--input_size\n" NO_COLOR
	    "\t\tSize of input\n\n");
    fprintf(stdout, GRAY "\t-p|--param\n" NO_COLOR
	    "\t\tParameters to use for kernel\n\n");
}



/*
 * Author message
 */
void
author ()
{
    fprintf(stdout, BROWN "Written by: " GRAY "%s\n\n" NO_COLOR,
            __PROGRAM_AUTHOR__);
}



/*
 * Version message
 */
void
version ()
{
    fprintf(stdout, __PROGRAM_NAME__ " version: " GRAY "%s\n" NO_COLOR,
            __PROGRAM_VERSION__);
}
