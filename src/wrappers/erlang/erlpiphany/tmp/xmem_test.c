#include <stdio.h>
#include <string.h>
#include <e-hal.h>

#define BUF_OFFSET 0x01000000


int main(int argc, char *argv[])
{
  int ret, size, ssize;
  e_platform_t eplat;
  e_epiphany_t edev;
  e_mem_t emem;
  char *test="Hello, Parallella!", buf[32];

  printf("Test alloc/read/write/free functions\n");
  ret = e_init(NULL);
  printf("e_init return code = %d\n", ret);
  ret = e_reset_system();
  printf("e_reset_system return code = %d\n", ret);
  ret = e_alloc(&emem, 0x01000000, 64);
  printf("e_alloc return code = %d\n", ret);
  ssize = e_write(&emem, 0, 0, 0, test, strlen(test));
  printf("Wrote %d bytes for string %s\n", ssize, test);
  ssize = e_read(&emem, 0, 0, 0, buf, strlen(test));
  buf[ssize] = 0;
  printf("read back string '%s' length %d\n", buf, ssize);
  ret = e_free(&emem);
  printf("e_free return code = %d\n", ret);
  ret = e_finalize();
  printf("e_init return code = %d\n", ret);
  return 0;
}
