#include <pal_math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

//-i   input file name
//-o   output file name
//-c   #columns in data file
//-m   options  (integers pushed into an array) 
//-f   function to test

#define EPSILON 0.00001f

int main(int argc, char **argv){

    //Test vars
    int n,i;
    float *a,*b,*c,*g;
    float delta;
    double runtime;
    int passed=1;
    struct timeval timer[4];
    
    //Getopt vars
    int opt;
    extern char *optarg; 
    extern int optind; 

    char *input_file    = "default";
    char *output_file   = "default";
    char *function_name = "default";
    int  test_opts[8];
    int columns;

    while ( (opt = getopt(argc, argv, "i:o:c:m:f:")) != -1) {
        switch (opt) {      
        case 'i':           
	    input_file = optarg;
            break;
        case 'o':
	    output_file = optarg;
            break;
	case 'f':
	    function_name = optarg;
            break;  
        case 'c':
	    columns =  atoi(optarg);
            break;
        case 'm':
            break;
        case '?':
            break;
        default:
            printf ("??getopt returned character code 0%o??\n", opt);
        }
    }

    //Read stimulus
    int lines=get_filesize(input_file);

    //Allocating memory
    a=malloc(lines*sizeof(float));
    b=malloc(lines*sizeof(float));
    c=malloc(lines*sizeof(float));
    g=malloc(lines*sizeof(float));

    //Reading stimulus
    load_float_data(input_file,lines,a,b,g);

    //All the functions
    if (strcmp (function_name, "p_add_f32") == 0) { gettimeofday(&timer[0], NULL);  p_add_f32(a,b,c,lines); } 
    if (strcmp (function_name, "p_mul_f32") == 0) { gettimeofday(&timer[0], NULL);  p_mul_f32(a,b,c,lines); } 
    if (strcmp (function_name, "p_sub_f32") == 0) { gettimeofday(&timer[0], NULL);  p_sub_f32(a,b,c,lines); } 
    
    //stop clock 
    gettimeofday(&timer[1], NULL);

    //check result
    for(i=0;i<lines;i++){
	if(!(*(c+i) == *(g+i))){
	    if(!(fabs( (*(c+i) - *(g+i)) / *(g+i)) < EPSILON)){
		printf("FAILED! a=%f b=%f result=%f golden=%f\n", *(a+i), *(b+i), *(c+i), *(g+i));
		passed=0;
	    }
	}
    }
    //print run time
    runtime = (timer[1].tv_sec - timer[0].tv_sec) * 1000000 + ((double) (timer[1].tv_usec - timer[0].tv_usec));
    if(passed){
	printf( "%s (%f usec)\n", function_name, runtime);
    }
    else{
	printf( "%s FAILED\n", function_name);
    }
    free(a);
    free(b);
    free(g);
    return 0;
}

//Floating point data
int load_float_data(char *filename, int total_lines, float *a, float *b, float *g){

    //Local variables
    FILE *sourceFile;
    int  i=0;
    char line[128];

    sourceFile = fopen(filename, "r");
    
    if(sourceFile==NULL) {
	printf("ERROR: CAN'T ACCESS %s\n", filename);
    }
    else{
	//Allocating memory	

	//Parsing through file one line at a time
	while (fgets(line, 128, sourceFile)!=NULL){
	    sscanf(line,"%f,%f,%f", (a+i), (b+i), (g+i));	    
	    //printf("a=%f b=%f c=%f\n", *(a+i), *(b+i), *(g+i));	    
	    i++;
	}	
	fclose(sourceFile);
    }
    return i;
}

int get_filesize(char *filename){
    
    FILE *sourceFile;
    char line[128];
    int total_lines;

    sourceFile = fopen(filename, "r");
    while (fgets(line, 128, sourceFile)!=NULL){
	total_lines++; 
    }    
    fclose(sourceFile);
    return total_lines;	
}
