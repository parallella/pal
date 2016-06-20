#include <pal.h>

/**
 *
 * Calculates the natural logarithm of 'a', (where the base is 'e'=2.71828)
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */


/* remez.cpp

-8<-------

// http://lolengine.net/wiki/oss/lolremez
#include "lol/math/real.h"
#include "lol/math/remez.h"

using lol::real;
using lol::RemezSolver;

// See the tutorial at http://lol.zoy.org/wiki/doc/maths/remez
real f(real const &x)
{ real one = 1.0;
  return one + static_cast<real>(log2(x));
}

int main(int argc, char **argv)
{
    RemezSolver<3, real> solver;
    solver.Run(1, 2, f, 40);
    return 0;
}
->8-------

Polynomial estimate:
x**0*-1.153620710492991324275207336525758107522
+x**1*3.047884146894374561531585118219466680216
+x**2*-1.051875021717643104028593360237358100725
+x**3*1.582487026083649643648849946418010888515e-1

*/

void p_ln_f64(const double *a, double *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        union {
            double f;
            uint64_t i;
        } u = { *(a + i) };

        // Calculate the exponent (which is the floor of the logarithm) minus one
        int e = ((u.i >> 52) & 0x7ff) - 0x400;

        // Mask off the exponent, leaving just the mantissa
        u.i = (u.i & 0xfffffffffffffULL) + 0x3ff0000000000000ULL;

        // Interpolate using a cubic minimax polynomial derived with
        // the Remez exchange algorithm. Coefficients courtesy of Alex Kan.
        // This approximates 1 + log2 of the mantissa.
        double r =
            ((0.1582487026083649643648 * u.f
                - 1.0518750217176431040285) * u.f
                    + 3.0478841468943745615315) * u.f
                        - 1.1536207104929913242752;

        // The log2 of the complete value is then the sum
        // of the previous quantities (the 1's cancel), and
        // we find the natural log by scaling by log2(e).
        *(c + i) = (e + r) * PCONST(M_LN2);
    }
}
