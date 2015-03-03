#include <stdio.h>
#include <string.h>
#include <e-hal.h>

#define BUF_OFFSET 0x01000000


int main(int argc, char *argv[])
{
  int ret;
  e_platform_t eplat;
  e_epiphany_t edev;
  e_mem_t emem;

  printf("Test open/close functions\n");
  ret = e_init(NULL);
  printf("e_init return code = %d\n", ret);
  ret = e_reset_system();
  printf("e_reset_system return code = %d\n", ret);
  ret = e_get_platform_info(&eplat);
  printf("e_get_platform_info return code = %d\n", ret);
  ret = e_open(&edev, 0, 0, 4, 4);
  printf("e_open return code = %d\n", ret);
  ret = e_close(&edev);
  printf("e_close return code = %d\n", ret);
  ret = e_finalize();
  printf("e_init return code = %d\n", ret);
  return 0;
}
