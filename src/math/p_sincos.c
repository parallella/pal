#include <pal.h>
#include <pal_math.h>
#include <pal_base.h>

#define SIN_COS_ITERATIONS 17

//Cordic in 32 bit signed fixed point math
//Function is valid for arguments in range -pi/2 -- pi/2
//for values pi/2--pi: value = half_pi-(theta-half_pi) and similarly for values -pi---pi/2
//
// 1.0 = 1073741824
// 1/k = 0.6072529350088812561694
// pi = 3.1415926535897932384626
//Constants
#define cordic_1K 0x26DD3B6A
#define half_pi 0x6487ED51
#define MUL 1073741824.000000
#define MUL_RECIP 1.0/MUL
#define CORDIC_NTAB 32
int cordic_ctab[] = { 0x3243F6A8, 0x1DAC6705, 0x0FADBAFC, 0x07F56EA6,
		0x03FEAB76, 0x01FFD55B, 0x00FFFAAA, 0x007FFF55, 0x003FFFEA, 0x001FFFFD,
		0x000FFFFF, 0x0007FFFF, 0x0003FFFF, 0x0001FFFF, 0x0000FFFF, 0x00007FFF,
		0x00003FFF, 0x00001FFF, 0x00000FFF, 0x000007FF, 0x000003FF, 0x000001FF,
		0x000000FF, 0x0000007F, 0x0000003F, 0x0000001F, 0x0000000F, 0x00000008,
		0x00000004, 0x00000002, 0x00000001, 0x00000000, };

void cordic(int theta, int *s, int *c, int n) {
	int k, d, tx, ty, tz;
	int x = cordic_1K, y = 0, z = theta;
	n = (n > CORDIC_NTAB) ? CORDIC_NTAB : n;
	for (k = 0; k < n; ++k) {
		d = z >> 31;
		//get sign. for other architectures, you might want to use the more portable version
		//d = z>=0 ? 0 : -1;
		tx = x - (((y >> k) ^ d) - d);
		ty = y + (((x >> k) ^ d) - d);
		tz = z - ((cordic_ctab[k] ^ d) - d);
		x = tx;
		y = ty;
		z = tz;
	}
	*c = x;
	*s = y;
}

float normalizeRadiansToPlusMinusM_PI(float radians) {
	unsigned int *radiansBits = (unsigned int *)&radians;
	unsigned int signBit = *radiansBits & 0x80000000u;
	*radiansBits = *radiansBits ^ signBit; //remove any negative bit

	int revolutions = (int) (radians * M_1_PI) + 1;
	revolutions = revolutions >> 1; // div 2

	radians = radians - revolutions * (2 * M_PI); //subtract whole revolutions, now in range [-pi..pi]

	*radiansBits = *radiansBits ^ signBit; //if was negative, flip negative bit
	return radians;
}

int radiansToPlusMinusM_PI_2(float *radians) {
	int flip = 0;
	*radians = normalizeRadiansToPlusMinusM_PI(*radians);

	if (*radians < -M_PI_2 || *radians > M_PI_2) {
		if (*radians < 0) {
			*radians += M_PI;
		} else {
			*radians -= M_PI;
		}
		flip = 1; //flip the sign for second or third quadrant
	}
	return flip;
}

void cordicF(float theta, float *s, float *c, int n) {
	int sint, cint;
	int flip = radiansToPlusMinusM_PI_2(&theta);
	cordic(theta * MUL, &sint, &cint, n);
	*s = sint * MUL_RECIP;
	*c = cint * MUL_RECIP;
	if (flip) {
		*s = -*s;
		*c = -*c;
	}
}

/**
 *
 * Compute the sine and cosine of the vector 'a'.
 *
 * Angles are specified in * radians.
 * The radian number must be in the range 0 to 2pi,
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to sine output vector
 *
 * @param z     Pointer to cosine output vector
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
void p_sincos_f32(const float *a, float *c, float *z, int n, int p,
		p_team_t team) {
	int i;
	for (i = 0; i < n; i++) {
		const float angle = a[i];
		float *pcos = z + i;
		float *psin = c + i;
		cordicF(angle, psin, pcos, SIN_COS_ITERATIONS);
	}
}

