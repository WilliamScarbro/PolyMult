/*
 * ============================================================================
 *
 *       Filename:  args.c
 *
 *    Description:  Command line options parser using GNU getopt
 *
 *        Created:  24/03/2015 22:00:09 PM
 *       Compiler:  gcc
 *
 *         Author:  Gustavo Pantuza
 *   Organization:  Software Community
 *
 * ============================================================================
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "messages.h"
#include "args.h"
#include "colors.h"



/*
 * Sets the default options
 */
static void set_default_options(options_t* options)
{
    options->help = false;
    options->version = false;
    options->use_colors = true;
    options->kernel_name="";
    options->input_size=0;
    strncpy(options->params,"",PARAM_SIZE);
    strncpy(options->file_name,"",FILE_NAME_SIZE);
}



/*
 * Finds the matching case of the current command line option
 */
void
switch_options (int arg, options_t* options, char* optarg)
{
    switch (arg)
    {
        case 'h':
            options->help = true;
            help();
            exit(EXIT_SUCCESS);

        case 'v':
            options->version = true;
            version();
            exit(EXIT_SUCCESS);

        case 's':
            options->input_size = atoi(optarg);
            break;

        case 'k':
            options->kernel_name = optarg;
            break;

        case 'p':
	    strncpy(options->params, optarg, PARAM_SIZE);
            break;

        case 0:
            options->use_colors = false;
            break;

        case '?':
            usage();
            fprintf(stdout, "bad usage: %d\n", arg);
            exit(EXIT_FAILURE);

        default:
            usage();
            abort();
    }
}



/*
 * Tries to get the file name. Otherwise, gets stdin
 */
void
get_file_name (int argc, char* argv[], options_t* options)
{

    /* If there is more arguments, probably, it is an input file */
    if (optind < argc) {
        strncpy(options->file_name, argv[optind++], FILE_NAME_SIZE);

    /* Otherwise, assumes stdin as the input file */
    } else {
        strncpy(options->file_name, "-", FILE_NAME_SIZE);
    }
}



/*
 * Public function that loops until command line options were parsed
 */
void
options_parser (int argc, char* argv[], options_t* options)
{
    set_default_options(options);

    int arg; /* Current option */

    /* getopt allowed options */
    static struct option long_options[] =
    {
        {"help", no_argument, 0, 'h'},
        {"version", no_argument, 0, 'v'},
        {"kernel_name", 1, 0, 'k'},
        {"input_size", 1, 0, 's'},
	{"params",1,0,'p'},
	{"no-colors", no_argument, 0, 0},
    };

    while (true) {

        int option_index = 0;
        arg = getopt_long(argc, argv, "hvk:s:t:p:", long_options, &option_index);

        /* End of the options? */
        if (arg == -1) break;

        /* Find the matching case of the argument */
        switch_options(arg, options, optarg);
    }

    /* Gets the file name or assumes stdin */
    get_file_name(argc, argv, options);
}
