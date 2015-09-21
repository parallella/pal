/* TODO: All addresses etc. are hardcoded. Assumes Parallella-16 memory map */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
#include <alloca.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/stat.h>

#include <libelf.h>
#include <gelf.h>

#include "config.h"

#include <pal.h>
#include <common.h>
#include "../../pal_base_private.h"
#include "dev_epiphany.h"
#include "epiphany-abi.h"

#define MMR_R0         0xf0000
#define MMR_DEBUGCMD   0xf0448
#define MMR_MEMPROTECT 0xf0608
#define MMR_MEMSTATUS  0xf0604
#define MMR_MESHCONFIG 0xf0700
#define MMR_DMA0CONFIG 0xf0500
#define MMR_DMA1CONFIG 0xf0520
#define MMR_CONFIG     0xf0400
#define MMR_STATUS     0xf0404
#define MMR_FSTATUS    0xf0440
#define MMR_CTIMER0    0xf0438
#define MMR_CTIMER1    0xf043c
#define MMR_PC         0xf0408
#define MMR_MULTICAST  0xf0704
#define MMR_LC         0xf0414
#define MMR_LS         0xf0418
#define MMR_LE         0xf041c
#define MMR_IRET       0xf0420
#define MMR_IMASK      0xf0424
#define MMR_ILATST     0xf042c
#define MMR_ILATCL     0xf0430
#define MMR_IPEND      0xf0434
#define MMR_DMASTART   MMR_DMA0CONFIG
#define MMR_DMAEND     0xf0540

#define E_SYS_BASE     0x70000000
#define E_REG_LINKCFG  0xf0300
#define E_REG_LINKTXCFG 0xf0304
#define E_REG_LINKRXCFG 0xf0308

// Epiphany system registers
typedef enum {
    E_SYS_RESET     = 0x0040,
    E_SYS_CFGTX     = 0x0044,
    E_SYS_CFGRX     = 0x0048,
    E_SYS_CFGCLK    = 0x004c,
    E_SYS_COREID    = 0x0050,
    E_SYS_VERSION   = 0x0054,
    E_SYS_GPIOIN    = 0x0058,
    E_SYS_GPIOOUT   = 0x005c
} e_sys_reg_id_t;

typedef union {
    unsigned int reg;
    struct {
        unsigned int enable:1;
        unsigned int mmu:1;
        unsigned int mode:2;      // 0=Normal, 1=GPIO
        unsigned int ctrlmode:4;
        unsigned int clkmode:4;   // 0=Full speed, 1=1/2 speed
        unsigned int resvd:20;
    };
} e_syscfg_tx_t;

typedef union {
    unsigned int reg;
    struct {
        unsigned int enable:1;
        unsigned int mmu:1;
        unsigned int path:2;    // 0=Normal, 1=GPIO, 2=Loopback
        unsigned int monitor:1;
        unsigned int resvd:27;
    };
} e_syscfg_rx_t;

typedef union {
    unsigned int reg;
    struct {
        unsigned int divider:4;  // 0=off, 1=F/64 ... 7=F/1
        unsigned int pll:4;      // TBD
        unsigned int resvd:24;
    };
} e_syscfg_clk_t;

typedef union {
    unsigned int reg;
    struct {
        unsigned int col:6;
        unsigned int row:6;
        unsigned int resvd:20;
    };
} e_syscfg_coreid_t;

typedef union {
    unsigned int reg;
    struct {
        unsigned int revision:8;
        unsigned int type:8;
        unsigned int platform:8;
        unsigned int generation:8;
    };
} e_syscfg_version_t;

#define EM_ADAPTEVA_EPIPHANY   0x1223  /* Adapteva's Epiphany architecture.  */

#define COREID(_addr) ((_addr) >> 20)

struct symbol_info {
    const char *name;
    bool found;
    GElf_Sym sym;
};

static inline bool is_local(uint32_t addr)
{
    return COREID(addr) == 0;
}

static bool is_on_chip(uint32_t addr)
{
    uint32_t row, col;
    row = (COREID(addr) >> 6) & 0x3f;
    col = (COREID(addr)     ) & 0x3f;

    return is_local(addr)
        || ((0x20 <= row && row < 0x24) && (0x08 <= col && row < 0x0c));
}

static inline bool is_in_eram(uint32_t addr)
{
    return (0x8e000000 <= addr && addr < 0x90000000);
}

static inline bool is_valid_addr(uint32_t addr, uint32_t coreid)
{
    /* Only allow loading to current core */
    return is_local(addr)
        || (is_on_chip(addr) && COREID(addr) == coreid)
        || is_in_eram(addr);
}

static inline bool is_epiphany_exec_elf(Elf32_Ehdr *ehdr)
{
    return ehdr
        && memcmp(ehdr->e_ident, ELFMAG, SELFMAG) == 0
        && ehdr->e_ident[EI_CLASS] == ELFCLASS32
        && ehdr->e_type == ET_EXEC
        && ehdr->e_version == EV_CURRENT
        && ehdr->e_machine == EM_ADAPTEVA_EPIPHANY;
}

/* Assumes 32 bit ... */
/* Assumes core is valid */
/* Assumes core mem and regs are cleared, core is idle / halted */
/* Assumes valid elf file */
static int process_elf(const void *file, struct epiphany_dev *epiphany,
                       unsigned coreid)
{
    Elf32_Ehdr    *ehdr;
    Elf32_Phdr    *phdr;
    void          *dst;
    const uint8_t *src = (uint8_t *) file;

    ehdr = (Elf32_Ehdr *) &src[0];
    phdr = (Elf32_Phdr *) &src[ehdr->e_phoff];

    for (unsigned i = 0; i < ehdr->e_phnum; i++) {
        // TODO: should this be p_paddr instead of p_vaddr?
        if (!is_valid_addr(phdr[i].p_vaddr, coreid))
            return -EINVAL;
    }

    for (unsigned i = 0; i < ehdr->e_phnum; i++) {
        // TODO: should this be p_paddr instead of p_vaddr?
        uint32_t addr = phdr[i].p_vaddr;
        dst = is_local(addr) ?
            (void *) ((coreid << 20) | addr) : (void *) addr;

        memcpy(dst, &src[phdr[i].p_offset], phdr[i].p_filesz);
        /* This is where we would have cleared .bss (p_memsz - p_filesz), but
         * since we assume SRAM is already cleared there's no need for that.
         */
    }

    return 0;
}

static void ecore_soft_reset(struct epiphany_dev *epiphany, unsigned coreid)
{
    /* (TODO: Wait for dma to complete??? istead of cancelling transfers ???) */

    /* Assumes no Write-after-Write or Read-after-Write hazards */

    union {
        void *v;
        uint8_t *u8;
        /* Make volatile so compiler can't reorder so we don't have to issue
         * barrier after *every* access (some of the ordering does matter) */
        volatile uint32_t *u32;
    } corep = { .v = (void *) (coreid << 20) };

    /* Halt core */
    corep.u32[MMR_DEBUGCMD   >> 2] = 0x00000001;

    /* Stop DMA transfers ??? */
    corep.u32[MMR_DMA0CONFIG >> 2] = 0x00000000;
    corep.u32[MMR_DMA1CONFIG >> 2] = 0x00000000;

    /* Reset config, enable clock gating */
    corep.u32[MMR_CONFIG     >> 2] = 0x00400000;

    /* Clear all bits STATUS */
    corep.u32[MMR_FSTATUS    >> 2] = 0x00000000;

    /* Set PC to SYNC IVT */
    corep.u32[MMR_PC         >> 2] = 0x00000000;

    /* Hardware loops */
    corep.u32[MMR_LC         >> 2] = 0x00000000;
    corep.u32[MMR_LS         >> 2] = 0x000effff;
    corep.u32[MMR_LE         >> 2] = 0x000effff;

    /* Reset interrupt regs */
    /* IRET */
    corep.u32[MMR_IRET       >> 2] = 0x00000000;
    /* ??? IMASK: Temporarily enable all to be able to clear pending */
    corep.u32[MMR_IMASK      >> 2] = 0x00000000;
    /* ILATCL */
    corep.u32[MMR_ILATCL     >> 2] = 0xffffffff;
    /* ??? IPEND: Clear pending (does it work???) */
    corep.u32[MMR_IPEND      >> 2] = 0x00000000;
    /* IMASK: Mask all but SYNC */
    corep.u32[MMR_IMASK      >> 2] = 0xfffffffe;

    /* Reset MULTICAST */
    corep.u32[MMR_MULTICAST  >> 2] = 0x00000000;

    /* Reset timers */
    corep.u32[MMR_CTIMER0    >> 2] = 0x00000000;
    corep.u32[MMR_CTIMER1    >> 2] = 0x00000000;

    /* Clear DMA regs */
    memset(&corep.u8[MMR_DMA0CONFIG], 0, MMR_DMAEND - MMR_DMASTART);

    /* Enable clock gating */
    corep.u32[MMR_MESHCONFIG >> 2] = 0x00000002;

    /* Clear mem protection bits */
    corep.u32[MMR_MEMPROTECT >> 2] = 0x00000000;
    /* Clear faults */
    corep.u32[MMR_MEMSTATUS  >> 2] = 0x00000000;

    /* Clear r0 - r63 */
    memset(&corep.u8[MMR_R0], 0 , 64 * 4);

    /* Clear SRAM */
    memset(corep.v, 0, 32768);

    /* Set all IVT vectors to "b.l 0" */
    for (unsigned i = 0; i < 10; i++)
        corep.u32[i] = 0xe8000000;

    /* Core still halted here */
}

static inline struct epiphany_dev *to_epiphany_dev(struct dev *dev)
{
    return container_of(dev, struct epiphany_dev, dev);
}

void epiphany_soft_reset(struct team *team, int start, int size)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);
    for (unsigned i = (unsigned) start; i < (unsigned) (start + size); i++) {
        unsigned row = 32 + i / 4;
        unsigned col =  8 + i % 4;
        unsigned coreid = (row << 6) | col;
        ecore_soft_reset(epiphany, coreid);
    }
}

static void lookup_symbols(const void *file, size_t file_size,
                           struct symbol_info *tbl, size_t tbl_size)
{
    Elf        *elf;
    Elf_Scn    *scn = NULL;
    Elf_Data   *edata = NULL;
    GElf_Shdr  shdr;
    GElf_Sym   sym;
    const char *sym_name;

    if (elf_version(EV_CURRENT) == EV_NONE)
        return;

    elf = elf_memory((char *) file, file_size);

    /* Find loader symbols */
    while ((scn = elf_nextscn(elf, scn)) != NULL) {
        gelf_getshdr(scn, &shdr);

        if (shdr.sh_type != SHT_SYMTAB)
            continue;

        edata = elf_getdata(scn, edata);

        int symbol_count = shdr.sh_size / shdr.sh_entsize;

        for (unsigned i = 0; i < symbol_count; i++) {
            gelf_getsym(edata, i, &sym);

            /* Only accept global or weak symbols */
            switch (ELF32_ST_BIND(sym.st_info)) {
            default:
                continue;
            case STB_GLOBAL:
            case STB_WEAK:
                ;
            }

            sym_name = elf_strptr(elf, shdr.sh_link, sym.st_name);
            for (unsigned j = 0; j < tbl_size; j++) {
                if (strcmp(sym_name, tbl[j].name) != 0)
                    continue;

                if (tbl[j].found
                    && ELF32_ST_BIND(tbl[j].sym.st_info) == STB_GLOBAL)
                    continue;

                tbl[j].found = true;
                memcpy(&tbl[j].sym, &sym, sizeof(sym));
            }
        }
    }

    elf_end(elf);
}

/* Data needed by device (e-lib and crt0) */
static int set_core_config(struct epiphany_dev *epiphany, unsigned coreid,
                           const void *file, size_t file_size)
{
    uint8_t *corep = (uint8_t *) (coreid << 20);
    uint8_t *nullp = (uint8_t *) 0;
    e_group_config_t *e_group_config;
    e_emem_config_t  *e_emem_config;

    struct symbol_info tbl[] = {
        { .name = "e_group_config"    },
        { .name = "e_emem_config"     },
    };
    lookup_symbols(file, file_size, tbl, ARRAY_SIZE(tbl));
    for (unsigned i; i < ARRAY_SIZE(tbl); i++) {
        if (!tbl[i].found)
            return -ENOENT;

        /* ???: Should perhaps only allow local addresses */
        if (!is_valid_addr(tbl[i].sym.st_value, coreid))
            return -EINVAL;
    }
    e_group_config = is_local(tbl[0].sym.st_value) ?
        (e_group_config_t *) &corep[tbl[0].sym.st_value] :
        (e_group_config_t *) &nullp[tbl[0].sym.st_value];
    e_emem_config = is_local(tbl[1].sym.st_value) ?
        (e_emem_config_t *) &corep[tbl[1].sym.st_value] :
        (e_emem_config_t *) &nullp[tbl[1].sym.st_value];

    /* No trivial way to emulate workgroups??? Pretend each core is its own
     * separate group for now. */

    e_group_config->objtype    = E_EPI_GROUP;
    e_group_config->chiptype   = E_E16G301; /* TODO: Or E_64G501 */
    e_group_config->group_id   = coreid;
    e_group_config->group_row  = coreid >> 6;
    e_group_config->group_col  = coreid & 0x3f;
    e_group_config->group_rows = 1;
    e_group_config->group_cols = 1;
    e_group_config->core_row   = 0;
    e_group_config->core_col   = 0;
    e_group_config->alignment_padding = 0xdeadbeef;

    e_emem_config->objtype = E_EXT_MEM;
    e_emem_config->base    = 0x8e000000;

    return 0;
}

int epiphany_load(struct team *team, struct prog *prog,
                  int start, int size, int flags)
{
    int rc;
    int fd;
    struct stat st;
    void *file;

    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);

    /* TODO: File opening should be done in p_load() */
    fd = open(prog->path, O_RDONLY);
    if (fd == -1)
        return -errno;

    if (fstat(fd, &st) == -1) {
        close(fd);
        return -errno;
    }

    file = mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (file == MAP_FAILED) {
        close(fd);
        return -errno;
    }

    if (!is_epiphany_exec_elf((Elf32_Ehdr *) file)) {
        rc = -EINVAL;
        goto out;
    }

    for (unsigned i = (unsigned) start; i < (unsigned) (start + size); i++) {
        unsigned row = 32 + i / 4;
        unsigned col =  8 + i % 4;
        unsigned coreid = (row << 6) | col;
        rc = process_elf(file, epiphany, coreid);
        if (rc)
            goto out;
        rc = set_core_config(epiphany, coreid, file, st.st_size);
        if (rc)
            goto out;
    }

out:
    munmap(file, st.st_size);
    close(fd);
    return rc;
}

void epiphany_start(struct team *team, int start, int size, int flags)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);

    for (unsigned i = (unsigned) start; i < (unsigned) (start + size); i++) {
        unsigned row = 32 + i / 4;
        unsigned col =  8 + i % 4;
        unsigned coreid = (row << 6) | col;
        volatile uint32_t *corep = (uint32_t *) (coreid << 20);

        corep[MMR_ILATST   >> 2] = 1;
        corep[MMR_DEBUGCMD >> 2] = 0;
    }
}

int epiphany_reset_system(struct epiphany_dev *epiphany)
{
    e_syscfg_tx_t txcfg;

    union ptr {
        void *v;
        uint8_t *u8;
        /* Make volatile so compiler can't reorder so we don't have to issue
         * barrier after *every* access (ordering matters). */
        volatile uint32_t *u32;
    } host_elink, east_elink;

    host_elink.v = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                   epiphany->epiphany_fd, E_SYS_BASE);

    if (host_elink.v == MAP_FAILED)
        return -errno;

    /* FPGA elink reset routine */
    {
        e_syscfg_rx_t rxcfg;
        e_syscfg_clk_t clkcfg;
        uint32_t resetcfg;

        resetcfg = 1;
        host_elink.u32[E_SYS_RESET >> 2] = resetcfg;
        usleep(1000);

        txcfg.reg = 0;
        host_elink.u32[E_SYS_CFGTX >> 2] = txcfg.reg;
        usleep(1000);

        rxcfg.reg = 0;
        host_elink.u32[E_SYS_CFGRX >> 2] = rxcfg.reg;
        usleep(1000);

        clkcfg.divider = 7; // Full speed
        host_elink.u32[E_SYS_CFGCLK >> 2] = clkcfg.reg;
        usleep(1000);

        clkcfg.divider = 0; // Stop clock
        host_elink.u32[E_SYS_CFGCLK >> 2] = clkcfg.reg;
        usleep(1000);

        resetcfg = 0;
        host_elink.u32[E_SYS_RESET >> 2] = resetcfg;
        usleep(1000);

        clkcfg.divider = 7; // Full speed
        host_elink.u32[E_SYS_CFGCLK >> 2] = clkcfg.reg;
        usleep(1000);

        txcfg.clkmode = 0; // Full speed
        txcfg.enable  = 1;
        host_elink.u32[E_SYS_CFGTX >> 2] = txcfg.reg;
        usleep(1000);

        rxcfg.enable = 1;
        host_elink.u32[E_SYS_CFGRX >> 2] = rxcfg.reg;
        usleep(1000);
    }

    /* Set chip link TX divider */
    {
        uint32_t divider;

        /* Chip east elink core */
        east_elink.u8 = (uint8_t *) epiphany->chip + ((2 << 6 | 3) << 20);

        txcfg.ctrlmode = 0x5; /* Force east */
        host_elink.u32[E_SYS_CFGTX >> 2] = txcfg.reg;
        usleep(1000);

        divider = 1; /* Divide by 4, see data sheet */
        east_elink.u32[E_REG_LINKCFG >> 2] = divider;
        usleep(1000);

        txcfg.ctrlmode = 0x0;
        host_elink.u32[E_SYS_CFGTX >> 2] = txcfg.reg;
        usleep(1000);
    }

    /* Reset cores */
    for (unsigned i = 0; i < 16; i++) {
        unsigned row = 32 + i / 4;
        unsigned col =  8 + i % 4;
        unsigned coreid = (row << 6) | col;
        ecore_soft_reset(epiphany, coreid);
    }

    /* Disable disconnected chip elinks */
    {
        unsigned data = 0xfff;
        struct {
            union ptr elink;
            unsigned  ctrlmode;
        } disable[] = {
            {
                /* north */
                .elink = {
                    .u8 = (uint8_t *) epiphany->chip + ((0 << 6 | 2) << 20)
                },
                .ctrlmode = 0x1,
            },
            {
                /* south */
                /* TODO: Different coords on E64... */
                .elink = {
                    .u8 = (uint8_t *) epiphany->chip + ((3 << 6 | 2) << 20)
                },
                .ctrlmode = 0x9,
            },
            {
                /* west */
                .elink = {
                    .u8 = (uint8_t *) epiphany->chip + ((2 << 6 | 0) << 20)
                },
                .ctrlmode = 0xd,
            },
        };
        for (unsigned i = 0; i < ARRAY_SIZE(disable); i++) {
            txcfg.ctrlmode = disable[i].ctrlmode;
            host_elink.u32[E_SYS_CFGTX >> 2] = txcfg.reg;
            usleep(1000);
            disable[i].elink.u32[E_REG_LINKTXCFG >> 2] = data;
            disable[i].elink.u32[E_REG_LINKRXCFG >> 2] = data;
            usleep(1000);
        }
        txcfg.ctrlmode = 0x0;
        host_elink.u32[E_SYS_CFGTX >> 2] = txcfg.reg;
        usleep(1000);
    }

    if (-1 == munmap(host_elink.v, 4096))
        return -errno;

    return 0;
}
