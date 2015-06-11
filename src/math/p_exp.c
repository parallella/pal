#include <pal.h>

/**
 *
 * Calculate exponent (e^a), where e is the base of the natural logarithm
 * (2.71828.) Using series expansion of function
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */
static const int precision = 0.1e-6;

float inverse(float a) //function to calucalte inverse without division operator using Newton Raphson method
{
	float result = precision;
	while(result * a > 1 + precision || result * a < 1 - precision)
	{
		result *= (2 - a * result);
	}
	return result;
}

float single_exp(float b)
{
	float a = b;
	if(b < 0) //if exponent is negative then calculate e^b and return 1/e^b
	{
		a = -a;
	}
	float result = 1;
	float counter = 1;
	float next_term = 1;
	float old_result;
	float error;
	do
	{
		old_result = result;
		next_term = next_term * a * inverse(counter);
		result += next_term;
		counter += 1;
		error = (old_result - result) / result;
		if(error < 0)
		{
			error = -error;
		}
	}while(error > precision);
	if(b < 0)
		return inverse(result);
	return result;
}
void p_exp_f32(const float *a, float *c, int n, int p, p_team_t team)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = single_exp(*(a + i));
    }
}
