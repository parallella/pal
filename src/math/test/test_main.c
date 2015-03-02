#include <pal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <stdint.h>

//-i   input file name
//-o   output file name
//-c   #columns in data file
//-m   options  (integers pushed into an array) 
//-f   function to test

#define EPSILON_MAX     0.001f
#define EPSILON_RELMAX  0.00001f
int get_filesize(char *filename);
int load_float_data(char *filename, int lines, float *a, float *b, float *c, float *g);
void dump_results(char *filename,float *ai, float *bi,float *ci,float* res, int lines);

//Two arguments
#define TEST(FN)  {	   		        \
                     if(strcmp(function_name,#FN) == 0)  \
                     {	                                \
 	               gettimeofday(&timer[0], NULL);   \
                       FN(ai,res,lines);		\
                       gettimeofday(&timer[1], NULL);   \
                      }                                 \
                }

#define TEST2(FN)  {	   		        \
                     if(strcmp(function_name,#FN) == 0)  \
                     {	                                \
 	               gettimeofday(&timer[0], NULL);   \
                       FN(ai,bi,res,lines);		\
                       gettimeofday(&timer[1], NULL);   \
                      }                                 \
                }                 
int main(int argc, char **argv){

    //Test vars
    int n,i;
    float *ai,*bi,*ci,*gold, *res;
    float delta;
    double runtime;
    int passed=1;
    int scalar=0;
    int size;
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

    while ( (opt = getopt(argc, argv, "i:o:c:m:f:s")) != -1) {
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
	case 's':
	    scalar=1;
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
    ai=malloc(lines*sizeof(float));
    bi=malloc(lines*sizeof(float));
    ci=malloc(lines*sizeof(float));
    res=malloc(lines*sizeof(float));
    gold=malloc(lines*sizeof(float));

    //Reading stimulus
    load_float_data(input_file,lines,ai,bi,ci, gold);

    //Needed for mac testing
    for(i=0;i<lines;i++){
	*(res+i)=*(ci+i);
    }
    //one vector input, one vector output
    TEST(p_abs_f32);
    TEST(p_acos_f32);
    TEST(p_acosh_f32);
    TEST(p_asin_f32);
    TEST(p_asinh_f32);
    TEST(p_atan_f32);
    TEST(p_atanh_f32);
    TEST(p_cbrt_f32);
    TEST(p_cos_f32);
    TEST(p_cosh_f32);
    TEST(p_exp_f32);
    TEST(p_inv_f32);
    TEST(p_invcbrt_f32);
    TEST(p_invsqrt_f32);
    TEST(p_ln_f32);
    TEST(p_log10_f32);
    TEST(p_sin_f32);
    TEST(p_sinh_f32);
    TEST(p_sqrt_f32);
    TEST(p_tan_f32);
    TEST(p_tanh_f32);

    //vector input, scalar output
    
    TEST(p_mean_f32);
    TEST(p_median_f32);
    TEST(p_mode_f32);
    TEST(p_sum_f32);    
    TEST(p_sumsq_f32);
    //two vector inputs, one vector output
    TEST2(p_sub_f32);
    TEST2(p_add_f32);
    TEST2(p_mul_f32);
    TEST2(p_absdiff_f32);
    TEST2(p_atan2_f32);
    TEST2(p_div_f32);
    TEST2(p_pow_f32);
    TEST2(p_mac_f32);
    
    //scalar output
    TEST2(p_dot_f32);

    //check result    
    if(scalar){
	size=1;	
    }
    else{
	size=lines;
    }
    float err;
    for(i=0;i<size;i++){       
	if(fabs(*(res+i) - *(gold+i)) > EPSILON_MAX){
	    printf("ERR=%f\n", fabs(*(res+i) - *(gold+i)));
	    if(fabs(*(res+i)) > fabs(*(gold+i))){
		err= fabs( (*(res+i) - *(gold+i)) / *(res+i));
	    }
	    else{
		err= fabs( (*(res+i) - *(gold+i)) / *(gold+i));
	    }
	    if(!(err <= EPSILON_RELMAX)){
		printf("%s FAILED! a=%lf b=%lf result=%lf golden=%lf\n", function_name, *(ai+i), *(bi+i), *(res+i), *(gold+i));
		passed=0;
	    }
	}
    }

    //print run time
    runtime = (timer[1].tv_sec - timer[0].tv_sec) * 1000000 + 
	      ((double) (timer[1].tv_usec - timer[0].tv_usec));

    if(passed){
	printf( "%s (%f usec)\n", function_name, runtime);
    }   
    
    //dump results
    dump_results(output_file,ai,bi,ci,res,lines);

    free(ai);
    free(bi);
    free(ci);
    free(res);
    free(gold);
    return 0;
}

//Floating point data
int load_float_data(char *filename, int lines, float *a, float *b, float *c, float *g){

    //Local variables
    FILE *sourceFile;
    int  i=0;
    char line[128];

    sourceFile = fopen(filename, "r");
    
    if(sourceFile==NULL) {
	printf("ERROR: CAN'T ACCESS %s\n", filename);
    }
    else{
	//Parsing through file one line at a time
	//Commm separate file with 4 mandatory entries
	while (fgets(line, 128, sourceFile)!=NULL){
	    sscanf(line,"%f,%f,%f,%f", (a+i), (b+i), (c+i), (g+i));	    
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
 

void dump_results(char *filename,float *ai, float *bi,float *ci,float* res, int lines){
    int i;
    FILE *ofp;
    ofp = fopen(filename, "w");
    for(i=0;i<lines;i++){
	fprintf(ofp, "%f,%f,%f,%f\n", *(ai+i),*(bi+i),*(ci+i),*(res+i));
    }
    fclose(ofp);
}
