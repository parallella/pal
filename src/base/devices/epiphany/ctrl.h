#define STATUS_NONE 0
#define STATUS_SCHEDULED 1 // Set by host in device.c:dev_run()
#define STATUS_RUNNING 2   // Set by device in boilerplate.c:init()
#define STATUS_DONE 3      // Set by device in boilerplate.c:fini()

#define CTRL_MEM_SIZE 4096
#define CTRL_MEM_OFFSET (0x2000000-CTRL_MEM_SIZE)
#define CTRL_MEM_EADDR (0x90000000-CTRL_MEM_SIZE)

struct epiphany_ctrl_mem {
    uint32_t status[16]; // status field for team
} __attribute__((packed)); // since this is referenced cross-architecture we
                           // need to pack struct explicitly to avoid surprises


