#ifndef TEST_FUNCTION
#error TEST_FUNCTION must be defined
#endif

#include <common.h>
#include <stdio.h>

int main()
{
#ifndef __epiphany
    fprintf(stderr, "WARNING: %s: Test not implemented yet\n",
            XSTRING(TEST_FUNCTION));
#endif

    /* Skipped test. see:
     * https://www.gnu.org/software/automake/manual/html_node/Scripts_002dbased-Testsuites.html
     */
    return 77;
}
