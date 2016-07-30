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
#include <sys/ioctl.h>

#include <elf.h>

#include "config.h"

#include <pal.h>
#include <common.h>
#include "../../../pal_base_private.h"
#include "dev_epiphany.h"
#include "epiphany-abi.h"
#include "epiphany-kernel-abi.h"

#define MMR_R0           0xf0000
#define MMR_CONFIG       0xf0400
#define MMR_STATUS       0xf0404
#define MMR_PC           0xf0408
#define MMR_DEBUGSTATUS  0xf040c
#define MMR_LC           0xf0414
#define MMR_LS           0xf0418
#define MMR_LE           0xf041c
#define MMR_IRET         0xf0420
#define MMR_IMASK        0xf0424
#define MMR_ILAT         0xf0428
#define MMR_ILATST       0xf042c
#define MMR_ILATCL       0xf0430
#define MMR_IPEND        0xf0434
#define MMR_CTIMER0      0xf0438
#define MMR_CTIMER1      0xf043c
#define MMR_FSTATUS      0xf0440
#define MMR_DEBUGCMD     0xf0448
#define MMR_DMA0CONFIG   0xf0500
#define MMR_DMA0STRIDE   0xf0504
#define MMR_DMA0COUNT    0xf0508
#define MMR_DMA0SRCADDR  0xf050c
#define MMR_DMA0DSTADDR  0xf0510
#define MMR_DMA0AUTODMA0 0xf0514
#define MMR_DMA0AUTODMA1 0xf0518
#define MMR_DMA0STATUS   0xf051c
#define MMR_DMA1CONFIG   0xf0520
#define MMR_DMA1STRIDE   0xf0524
#define MMR_DMA1COUNT    0xf0528
#define MMR_DMA1SRCADDR  0xf052c
#define MMR_DMA1DSTADDR  0xf0530
#define MMR_DMA1AUTODMA0 0xf0534
#define MMR_DMA1AUTODMA1 0xf0538
#define MMR_DMA1STATUS   0xf053c
#define MMR_MEMSTATUS    0xf0604
#define MMR_MEMPROTECT   0xf0608
#define MMR_MESHCONFIG   0xf0700
#define MMR_MULTICAST    0xf0704

#define MMR_DMASTART     MMR_DMA0CONFIG
#define MMR_DMAEND       0xf0540

#define E_SYS_BASE       0x70000000
#define MMR_LINKCFG      0xf0300
#define MMR_LINKTXCFG    0xf0304
#define MMR_LINKRXCFG    0xf0308



#define EM_ADAPTEVA_EPIPHANY   0x1223  /* Adapteva's Epiphany architecture.  */

#define COREID(_addr) ((_addr) >> 20)

/* Convenience macros. Remember to name
 * int foo(struct epiphany_dev *epiphany, ...)
 */
#define reg_read(base, offset) \
    epiphany->loader_ops.reg_read(epiphany, (base), (offset))
#define reg_write(base, offset, val) \
    epiphany->loader_ops.reg_write(epiphany, (base), (offset), (val))

#define mem_read(dst, src, n) \
    epiphany->loader_ops.mem_read(epiphany, (dst), (src), (n))
#define mem_write(dst, src, n) \
    epiphany->loader_ops.mem_write(epiphany, (dst), (src), (n))

struct symbol_info {
    const char *name;
    bool present;
    Elf32_Sym sym;
};

struct section_info {
    const char *name;
    bool present;
    Elf32_Addr	sh_addr; /* Section virtual addr at execution */
};

enum loader_sections {
    SEC_WORKGROUP_CFG,
    SEC_EXT_MEM_CFG,
    SEC_LOADER_CFG,
    SEC_NUM,
};

static inline bool is_local(uint32_t addr)
{
    return COREID(addr) == 0;
}

static bool is_on_chip(struct epiphany_dev *epiphany, uint32_t addr)
{
    uint32_t row, col;
    row = (COREID(addr) >> 6) & 0x3f;
    col = (COREID(addr)     ) & 0x3f;

    return is_local(addr)
        || ((epiphany->dev.start.row <= row
             && row < epiphany->dev.start.row + epiphany->dev.size.row)
        &&  (epiphany->dev.start.col <= col
             && row < epiphany->dev.start.col + epiphany->dev.size.col));
}

static inline bool is_in_eram(struct epiphany_dev *epiphany, uint32_t addr)
{
    return (epiphany->eram_base <= addr && addr - epiphany->eram_base < epiphany->eram_size);
}

static inline bool is_valid_addr(struct epiphany_dev *epiphany, uint32_t addr,
                                 uint32_t coreid)
{
    /* Only allow loading to current core */
    return is_local(addr)
        || (is_on_chip(epiphany, addr) && COREID(addr) == coreid)
        || is_in_eram(epiphany, addr);
}

/* TODO: Only supports addresses in eram */
static bool translate(struct epiphany_dev *epiphany, void *addr,
                      unsigned coreid, uint32_t *eaddr)
{
    uintptr_t uaddr = (uintptr_t) addr;
    uintptr_t ueram = (uintptr_t) epiphany->eram;
    uintptr_t ueeram = (uintptr_t) epiphany->eram_base + epiphany->eram_size;
    if (uaddr < ueram || ueeram <= uaddr)
        return false;

    *eaddr = epiphany->eram_base + ((uint32_t) uaddr - ueram);

    return true;
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
    uintptr_t     dst;
    const uint8_t *src = (uint8_t *) file;

    ehdr = (Elf32_Ehdr *) &src[0];
    phdr = (Elf32_Phdr *) &src[ehdr->e_phoff];

    for (unsigned i = 0; i < ehdr->e_phnum; i++) {
        // TODO: should this be p_paddr instead of p_vaddr?
        if (!is_valid_addr(epiphany, phdr[i].p_vaddr, coreid))
            return -EINVAL;
    }

    for (unsigned i = 0; i < ehdr->e_phnum; i++) {
        // TODO: should this be p_paddr instead of p_vaddr?
        uint32_t addr = phdr[i].p_vaddr;
        dst = is_local(addr) ? (coreid << 20) | addr : addr;

        mem_write(dst, &src[phdr[i].p_offset], phdr[i].p_filesz);
        /* This is where we would have cleared .bss (p_memsz - p_filesz), but
         * since we assume SRAM is already cleared there's no need for that.
         */
    }

    return 0;
}

int ecore_soft_reset_dma(struct epiphany_dev *epiphany, unsigned coreid)
{
    uint32_t config;
    bool fail0, fail1;
    int i;
    /* HACK: Depends on that we do an explicit mmap of Epiphany addresses space
     * in device.c:mmap_chip_mem() */
    uintptr_t core = coreid << 20;

    /* pause DMA */
    config = reg_read(core, MMR_CONFIG) | 0x01000000;
    reg_write(core, MMR_CONFIG, config);

    reg_write(core, MMR_DMA0CONFIG, 0);
    reg_write(core, MMR_DMA0STRIDE, 0);
    reg_write(core, MMR_DMA0COUNT, 0);
    reg_write(core, MMR_DMA0SRCADDR, 0);
    reg_write(core, MMR_DMA0DSTADDR, 0);
    reg_write(core, MMR_DMA0STATUS, 0);
    reg_write(core, MMR_DMA1CONFIG, 0);
    reg_write(core, MMR_DMA1STRIDE, 0);
    reg_write(core, MMR_DMA1COUNT, 0);
    reg_write(core, MMR_DMA1SRCADDR, 0);
    reg_write(core, MMR_DMA1DSTADDR, 0);
    reg_write(core, MMR_DMA1STATUS, 0);

    /* unpause DMA */
    config &= ~0x01000000;
    reg_write(core, MMR_CONFIG, config);

    fail0 = true;
    for (i = 0; i < 1000; i++) {
        if (!(reg_read(core, MMR_DMA0STATUS) & 7)) {
            fail0 = false;
            break;
        }
        usleep(10);
    }
    if (fail0)
        /* warnx("%s(): (%d) DMA0 NOT IDLE after dma reset", __func__, coreid); */
        ;

    fail1 = true;
    for (i = 0; i < 1000; i++) {
        if (!(reg_read(core, MMR_DMA1STATUS) & 7)) {
            fail1 = false;
            break;
        }
        usleep(10);
    }
    if (fail1)
        /* warnx("%s(): (%d) DMA1 NOT IDLE after dma reset", __func__, coreid); */
        ;

    return (fail0 || fail1) ? -EIO : 0;
}

int ecore_reset_regs(struct epiphany_dev *epiphany, unsigned coreid,
                     bool reset_dma)
{
    unsigned i;
    /* HACK: Depends on that we do an explicit mmap of Epiphany addresses space
     * in device.c:mmap_chip_mem() */
    uintptr_t core = coreid << 20;

    const uint32_t gpr = { 0 };

    /* General purpose registers */
    for (i = 0; i < 64; i++)
        reg_write(core, i, MMR_R0 + (i << 2));

    if (reset_dma)
        if (ecore_soft_reset_dma(epiphany, coreid))
            return -EIO;

    /* Enable clock gating */
    reg_write(core, MMR_CONFIG, 0x00400000);
    reg_write(core, MMR_FSTATUS, 0);
    /* reg_write(core, MMR_PC, 0); */
    reg_write(core, MMR_LC, 0);
    reg_write(core, MMR_LS, 0);
    reg_write(core, MMR_LE, 0);
    reg_write(core, MMR_IRET, 0);
    /* Mask all but SYNC irq */
    reg_write(core, MMR_IMASK, ~1);
    reg_write(core, MMR_ILATCL, ~0);
    reg_write(core, MMR_CTIMER0, 0);
    reg_write(core, MMR_CTIMER1, 0);
    reg_write(core, MMR_MEMSTATUS, 0);
    reg_write(core, MMR_MEMPROTECT, 0);
    /* Enable clock gating */
    reg_write(core, MMR_MESHCONFIG, 2);

    return 0;
}

void ecore_clear_sram(struct epiphany_dev *epiphany, unsigned coreid)
{
    const unsigned ivt_size = 9 * 4;
    const uint32_t insn = 0xffe017e2; /* entry: trap #5; b.s entry; */
    unsigned i;
    union {
        void     *v;
        uint32_t *u32;
        uint8_t  *u8;
    } core = { .v = (void *) (((uintptr_t) coreid) << 20) };
    uint8_t zeroes[32768] = { 0 };

    /* Set IVT entries to a safe instruction */
    for (i = 0; i < ivt_size / 4; i++)
        mem_write((uintptr_t) &core.u32[i], &insn, sizeof(insn));

    mem_write((uintptr_t) &core.u8[ivt_size], zeroes, 32768 - ivt_size);
}


static uint8_t soft_reset_payload[] = {
    0xe8, 0x16, 0x00, 0x00, 0xe8, 0x14, 0x00, 0x00, 0xe8, 0x12, 0x00, 0x00,
    0xe8, 0x10, 0x00, 0x00, 0xe8, 0x0e, 0x00, 0x00, 0xe8, 0x0c, 0x00, 0x00,
    0xe8, 0x0a, 0x00, 0x00, 0xe8, 0x08, 0x00, 0x00, 0xe8, 0x06, 0x00, 0x00,
    0xe8, 0x04, 0x00, 0x00, 0xe8, 0x02, 0x00, 0x00, 0x1f, 0x15, 0x02, 0x04,
    0x7a, 0x00, 0x00, 0x03, 0xd2, 0x01, 0xe0, 0xfb, 0x92, 0x01, 0xb2, 0x01,
    0xe0, 0xfe
};

/*
 *        ivt:
 *   0:              b.l     clear_ipend
 *   4:              b.l     clear_ipend
 *   8:              b.l     clear_ipend
 *   c:              b.l     clear_ipend
 *  10:              b.l     clear_ipend
 *  14:              b.l     clear_ipend
 *  18:              b.l     clear_ipend
 *  1c:              b.l     clear_ipend
 *  20:              b.l     clear_ipend
 *  24:              b.l     clear_ipend
 *  28:              b.l     clear_ipend
 *        clear_ipend:
 *  2c:              movfs   r0, ipend
 *  30:              orr     r0, r0, r0
 *  32:              beq     1f
 *  34:              rti
 *  36:              b       clear_ipend
 *        1:
 *  38:              gie
 *  3a:              idle
 *  3c:              b       1b
 */

static int ecore_soft_reset(struct epiphany_dev *epiphany, unsigned coreid)
{
    int i;
    uint32_t status;
    bool fail;
    uintptr_t core = coreid << 20;

    /* (TODO: Wait for dma to complete??? istead of cancelling transfers ???) */

    /* Assumes no Write-after-Write or Read-after-Write hazards */

    if (!(reg_read(core, MMR_DEBUGSTATUS) & 1)) {
        /* WARN: No clean previous exit */
        reg_write(core, MMR_DEBUGCMD, 1);
    }

    /* Wait for external fetch */
    fail = true;
    for (i = 0; i < 1000; i++) {
        if (!(reg_read(core, MMR_DEBUGSTATUS) & 2)) {
            fail = false;
            break;
        }
        usleep(10);
    }
    if (fail) {
        /* warnx("%s(): (%d) stuck. Full system reset needed", __func__, coreid); */
        return -EIO;
    }

    if (reg_read(core, MMR_DMA0STATUS) & 7)
        /* warnx("%s(): (%d) DMA0 NOT IDLE", __func__, coreid); */
        ;

    if (reg_read(core, MMR_DMA1STATUS) & 7)
        /* warnx("%s(): (%d) DMA1 NOT IDLE", __func__, coreid); */
        ;

    /* Abort DMA transfers */
    if (ecore_soft_reset_dma(epiphany, coreid))
        return -EIO;

    /* Disable timers */
    reg_write(core, MMR_CONFIG, 0);

    reg_write(core, MMR_ILATCL, ~0);

    reg_write(core, MMR_IMASK, 0);

    reg_write(core, MMR_IRET, 0x2c); /* clear_ipend */

    reg_write(core, MMR_PC, 0x2c); /* clear_ipend */

    mem_write(core, soft_reset_payload, sizeof(soft_reset_payload));

    /* Set active bit */
    reg_write(core, MMR_FSTATUS, 1);

    reg_write(core, MMR_DEBUGCMD, 0);

    fail = true;
    for (i = 0; i < 10000; i++) {
        if (!reg_read(core, MMR_IPEND) &&
                !reg_read(core, MMR_ILAT) &&
                !(reg_read(core, MMR_STATUS) & 1)) {
            fail = false;
            break;
        }
        usleep(10);
    }
    if (fail) {
        /* warnx("%s: (%d) Not idle", __func__, coreid); */
        return -EIO;
    }

    /* Reset regs, excluding DMA (already done above) */
    if (ecore_reset_regs(epiphany, coreid, false))
        return -EIO;

    ecore_clear_sram(epiphany, coreid);

    return 0;
}

static inline struct epiphany_dev *to_epiphany_dev(struct dev *dev)
{
    return container_of(dev, struct epiphany_dev, dev);
}

int epiphany_soft_reset(struct team *team, int start, int size)
{
    int ret;
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);
    for (unsigned i = (unsigned) start; i < (unsigned) (start + size); i++) {
        unsigned row = epiphany->dev.start.row + i / epiphany->dev.size.col;
        unsigned col = epiphany->dev.start.col + i % epiphany->dev.size.col;
        unsigned coreid = (row << 6) | col;
        ret = ecore_soft_reset(epiphany, coreid);
        if (ret)
            return ret;
    }
    return 0;
}

static void lookup_symbols(const void *file, size_t file_size,
                           struct symbol_info *tbl, size_t tbl_size)
{
    uint8_t *elf;
    unsigned shnum;
    Elf32_Ehdr *ehdr;
    Elf32_Phdr *phdr;
    Elf32_Shdr *shdrs, *shdr, *shdr_symstrtab;
    const char *strtab;

    elf   = (uint8_t *) file;
    ehdr  = (Elf32_Ehdr *) &elf[0];
    phdr  = (Elf32_Phdr *) &elf[ehdr->e_phoff];
    shdr  = (Elf32_Shdr *) &elf[ehdr->e_shoff];
    shdrs = (Elf32_Shdr *) &elf[ehdr->e_shoff];

    shnum = ehdr->e_shnum;

    for (shnum = ehdr->e_shnum; shnum; shnum--, shdr++) {
        Elf32_Sym  *sym;
        const char *sym_strtab;
        int symbol_count;

        if (shdr->sh_type != SHT_SYMTAB)
            continue;

        if (!shdr->sh_size || !shdr->sh_entsize)
            continue;

        shdr_symstrtab = &shdrs[shdr->sh_link];
        sym_strtab     = &elf[shdr_symstrtab->sh_offset];

        symbol_count   = shdr->sh_size / shdr->sh_entsize;
        sym            = (Elf32_Sym *) &elf[shdr->sh_offset];

        for (; symbol_count; symbol_count--, sym++) {
            const char *sym_name = &sym_strtab[sym->st_name];

            if (sym->st_shndx == SHN_UNDEF)
                continue;

            switch (ELF32_ST_BIND(sym->st_info)) {
            default:
                continue;
            case STB_GLOBAL:
            case STB_WEAK:
                ;
            }

            for (unsigned i = 0; i < tbl_size; i++) {
                if (strcmp(sym_name, tbl[i].name) != 0)
                    continue;

                if (tbl[i].present
                    && ELF32_ST_BIND(tbl[i].sym.st_info) == STB_GLOBAL)
                    continue;

                tbl[i].present = true;
                memcpy(&tbl[i].sym, sym, sizeof(*sym));
            }
        }
    }
}

static void lookup_sections(const void *file, struct section_info *tbl,
        size_t tbl_size)
{
    int i;
    size_t j;
    Elf32_Ehdr *ehdr;
    Elf32_Phdr *phdr;
    Elf32_Shdr *shdr, *sh_strtab;
    const char *strtab;
    uint8_t *src = (uint8_t *) file;

    ehdr = (Elf32_Ehdr *) &src[0];
    phdr = (Elf32_Phdr *) &src[ehdr->e_phoff];
    shdr = (Elf32_Shdr *) &src[ehdr->e_shoff];
    int shnum = ehdr->e_shnum;

    sh_strtab = &shdr[ehdr->e_shstrndx];
    strtab = (char *) &src[sh_strtab->sh_offset];

    for (i = 0; i < shnum; i++) {
        for (j = 0; j < tbl_size; j++) {
            if (tbl[j].present)
                continue;

            if (strcmp(&strtab[shdr[i].sh_name], tbl[j].name) != 0)
                continue;

            tbl[j].present = true;
            tbl[j].sh_addr = shdr[i].sh_addr;
        }
    }
}

static inline bool is_passed_by_value(const p_arg_t *arg)
{
    return (arg->is_primitive && arg->size <= 8);
}

static void setup_function_args(struct epiphany_dev *epiphany, unsigned coreid,
                                int argn, const p_arg_t *args,
                                uint32_t function_addr,
                                uintptr_t loader_args_ptr_addr)
{
    unsigned rel_row = (coreid - 0x808) >> 6;
    unsigned rel_col = (coreid - 0x808) & 0x3f;
    unsigned rel_coreid = (rel_row << 2) + rel_col;
    uint32_t eaddr;
    struct loader_args *loader_args = &epiphany->ctrl->loader_args[rel_coreid];
    /* epiphany address */
    uintptr_t loader_args_addr =
        CTRL_MEM_EADDR +
        (uintptr_t) &epiphany->ctrl->loader_args[rel_coreid] -
        (uintptr_t) epiphany->ctrl;
    uint32_t *regs = &loader_args->r0;
    unsigned arg = 0;
    /* TODO: Obviously this doesn't work with more than one core */
    uint8_t *argstackp = (uint8_t *) epiphany->ctrl;

    /* Set cores's args ptr */
    mem_write(loader_args_ptr_addr,
              &loader_args_addr,
              sizeof(loader_args_addr));

    memset(loader_args, 0, sizeof(*loader_args));

    loader_args->function_ptr = function_addr;

    /* Set up register args */
    for (unsigned reg = 0; reg < 4 && arg < argn; /* nop */) {
        if (is_passed_by_value(&args[arg])) {
            if (reg & 1 && args[arg].size > 4) {
                reg++;
                if (reg > 2)
                    break;

                memcpy(&regs[reg], args[arg].ptr, args[arg].size);
                arg++;

                /* No more register slots */
                break;
            } else {
                memcpy(&regs[reg], args[arg].ptr, args[arg].size);
                reg += args[arg].size > 4 ? 2 : 1;
                arg++;
            }
        } else {
            /* Argument is passed by reference */

            /* No need to copy arg if it's already accessible by core */
            if (translate(epiphany, args[arg].ptr, coreid, &eaddr) &&
                /* On the off chance the host ptr is in the first 1M */
                !is_local((uint32_t) (uintptr_t) args[arg].ptr)) {

                regs[reg] = eaddr;

            } else {
                argstackp -= (args[arg].size + 7) & (~7);
                memcpy(argstackp, args[arg].ptr, args[arg].size);
                translate(epiphany, argstackp, coreid, &eaddr);
                regs[reg] = eaddr;
            }
            reg++;
            arg++;
        }
    }

    /* No stack spill */
    if (arg == argn)
        return;

    uint32_t ptrslot[P_RUN_MAX_ARGS];

    /* Copy rest of args that are passed by reference onto argsstack */
    for (unsigned i = arg; i < argn; i++) {
        /* Don't need to copy arguments passed by value */
        if (is_passed_by_value(&args[i]))
            continue;

        /* No need to copy arg if it's already accessible by core */
        if (translate(epiphany, args[i].ptr, coreid, &eaddr) &&
            /* On the off chance the host ptr is in the first 1M */
            !is_local((uint32_t) (uintptr_t) args[i].ptr)) {

            ptrslot[i] = (uint32_t) eaddr;
        } else {
            argstackp -= (args[i].size + 7) & (~7);
            memcpy(argstackp, args[i].ptr, args[i].size);
            translate(epiphany, argstackp, coreid, &eaddr);
            ptrslot[i] = eaddr;
        }
    }

    /* Allocate the maximum possible size for actual stack arguments,
     * 8 bytes / entry. */
    argstackp -= (argn - arg) * 8;

    translate(epiphany, argstackp, coreid, &eaddr);
    loader_args->stack_spill_ptr = eaddr;

    for (; arg < argn; arg++) {
        if (is_passed_by_value(&args[arg])) {
            if (args[arg].size > 4)
                argstackp = (uint8_t *) (((uintptr_t) &argstackp[7]) & (~7));

            memcpy(argstackp, args[arg].ptr, args[arg].size);
            argstackp += (args[arg].size + 3) & (~3);
        } else {
            memcpy(argstackp, &ptrslot[arg], 4);
            argstackp += 4;
        }
    }
    /* Align size on 8-byte boundary, */
    argstackp = (uint8_t *) (((uintptr_t) &argstackp[7]) & (~7));

    loader_args->stack_spill_size =
        (uint32_t) ((uintptr_t) argstackp - loader_args->stack_spill_ptr);
}

/* Data needed by device (e-lib and crt0) */
static int set_core_config(struct team *team, unsigned coreid, unsigned rank,
                           const void *file, size_t file_size, int argn,
                           const p_arg_t *args, const char *function)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);

    uint8_t *corep = (uint8_t *) (uintptr_t) (coreid << 20);
    uint8_t *nullp = (uint8_t *) 0;
    uint32_t loader_flags, function_addr;
    e_group_config_t e_group_config;
    e_emem_config_t  e_emem_config;
    struct loader_cfg loader_cfg;
    uintptr_t e_group_config_addr, e_emem_config_addr, loader_cfg_addr;
    uintptr_t loader_flags_addr, loader_args_ptr_addr;
    char *function_plt;

    {
        size_t function_len = strlen(function);
        function_plt = alloca(function_len + sizeof("@PLT"));
        memcpy(&function_plt[function_len], "@PLT", sizeof("@PLT"));
    }

    struct section_info sections[] = {
        { .name = "workgroup_cfg" },
        { .name = "ext_mem_cfg"   },
        { .name = "loader_cfg"    },
    };
    lookup_sections(file, sections, ARRAY_SIZE(sections));

    struct symbol_info symbols[] = {
        { .name = function     },
        { .name = function_plt },
    };
    lookup_symbols(file, file_size, symbols, ARRAY_SIZE(symbols));

    for (unsigned i; i < ARRAY_SIZE(sections); i++) {
        if (!sections[i].present)
            return -ENOENT;

        /* ???: Should perhaps only allow local addresses */
        if (!is_valid_addr(epiphany, sections[i].sh_addr, coreid))
            return -EINVAL;
    }

    e_group_config_addr = is_local(sections[SEC_WORKGROUP_CFG].sh_addr) ?
        (uintptr_t) &corep[sections[SEC_WORKGROUP_CFG].sh_addr] :
        (uintptr_t) &nullp[sections[SEC_WORKGROUP_CFG].sh_addr];
    e_emem_config_addr = is_local(sections[SEC_EXT_MEM_CFG].sh_addr) ?
        (uintptr_t) &corep[sections[SEC_EXT_MEM_CFG].sh_addr] :
        (uintptr_t) &nullp[sections[SEC_EXT_MEM_CFG].sh_addr];
    loader_cfg_addr = is_local(sections[SEC_LOADER_CFG].sh_addr) ?
        (uintptr_t) &corep[sections[SEC_LOADER_CFG].sh_addr] :
        (uintptr_t) &nullp[sections[SEC_LOADER_CFG].sh_addr];

    loader_flags_addr = loader_cfg_addr + offsetof(struct loader_cfg, flags);
    loader_args_ptr_addr =
        loader_cfg_addr + offsetof(struct loader_cfg, args_ptr);

    /* Check everything except PLT entry (only exists when compiled w/ -fpic) */
    /* Prefer PLT entry */
    /* No need to adjust function address if it's local */
    if (!symbols[0].present && !symbols[1].present)
        return -ENOENT;

    if (symbols[1].present && is_valid_addr(epiphany, symbols[1].sym.st_value, coreid))
        function_addr = symbols[1].sym.st_value;
    else if (is_valid_addr(epiphany, symbols[0].sym.st_value, coreid))
        function_addr = symbols[0].sym.st_value;
    else
        return -EINVAL;

    if (sections[SEC_WORKGROUP_CFG].present) {
        unsigned glob_row0 = epiphany->dev.start.row + team->start.id / epiphany->dev.size.col;
        unsigned glob_col0 = epiphany->dev.start.col + team->start.id % epiphany->dev.size.col;
        unsigned col0 = team->start.id % epiphany->dev.size.col;
        unsigned cole = (team->start.id + team->size.id - 1) % epiphany->dev.size.col;
        unsigned cols = 1 + cole - col0;
        unsigned rows = team->size.id / cols;
        /* No trivial way to emulate workgroups??? Pretend each core is its own
         * separate group for now. */
        e_group_config.objtype    = E_EPI_GROUP;
        e_group_config.chiptype   = E_E16G301; /* TODO: Or E_64G501 */
        e_group_config.group_id   = glob_row0 * 64 + glob_col0;
        e_group_config.group_row  = glob_row0;
        e_group_config.group_col  = glob_col0;
        e_group_config.group_rows = rows;
        e_group_config.group_cols = cols;
        e_group_config.core_row   = rank / epiphany->dev.size.col;
        e_group_config.core_col   = rank % epiphany->dev.size.col;
        e_group_config.alignment_padding = 0xdeadbeef;
        mem_write(e_group_config_addr, &e_group_config, sizeof(e_group_config));
    }

    if (sections[SEC_EXT_MEM_CFG].present) {
        e_emem_config.objtype = E_EXT_MEM;
        e_emem_config.base    = epiphany->eram_base;
        mem_write(e_emem_config_addr, &e_emem_config, sizeof(e_emem_config));
    }

    setup_function_args(epiphany, coreid, argn, args, function_addr,
                        loader_args_ptr_addr);

    // Instruct crt0 .bss is cleared and that we've provided custom args.
    loader_flags = LOADER_BSS_CLEARED_FLAG | LOADER_CUSTOM_ARGS_FLAG;
    mem_write(loader_flags_addr, &loader_flags, sizeof(loader_flags));

    return 0;
}

int epiphany_load(struct team *team, int start, int count,
                  struct prog *prog, const char *function,
                  int argn, const p_arg_t *args)

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

    for (unsigned i = (unsigned) start; i < (unsigned) (start + count); i++) {
        unsigned row = epiphany->dev.start.row + (team->start.id + i) / epiphany->dev.size.col;
        unsigned col = epiphany->dev.start.col + (team->start.id + i) % epiphany->dev.size.col;
        unsigned coreid = (row << 6) | col;
        rc = process_elf(file, epiphany, coreid);
        if (rc)
            goto out;
        rc = set_core_config(team, coreid, i, file, st.st_size, argn, args,
                             function);
        if (rc)
            goto out;
    }

out:
    munmap(file, st.st_size);
    close(fd);
    return rc;
}

void epiphany_start(struct team *team, int start, int count)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);

    for (unsigned i = (unsigned) start; i < (unsigned) (start + count); i++) {
        unsigned row = epiphany->dev.start.row + i / epiphany->dev.size.col;
        unsigned col = epiphany->dev.start.col + i % epiphany->dev.size.col;
        unsigned coreid = (row << 6) | col;
        uintptr_t core_base = coreid << 20;

        reg_write(core_base, MMR_ILATST, 1);
    }
}

int epiphany_reset_system(struct epiphany_dev *epiphany)
{
    if (ioctl(epiphany->epiphany_fd, E_IOCTL_RESET))
        return -errno;

    return 0;
}

/* Return true if all CPU's in team have exited */
bool epiphany_is_team_done (struct team *team)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);

    for (unsigned i = team->start.id; i < team->start.id + team->size.id; i++) {
        unsigned row = epiphany->dev.start.row + i / epiphany->dev.size.col;
        unsigned col = epiphany->dev.start.col + i % epiphany->dev.size.col;
        uint32_t core = ((row << 6) | col) << 20;
        uint32_t debugstatus, pc;
        uint16_t insn;

        debugstatus = reg_read(core, MMR_DEBUGSTATUS);

        if (!(debugstatus & 1))
            return false;


        pc = reg_read(core, MMR_PC);

        if (pc >= 2)
            pc -= 2;

        if (pc < 0x100000)
            pc |= core;

        mem_read(&insn, pc, sizeof(insn));
        switch (insn) {
#define TRAP3 0x0fe2
#define TRAP4 0x13e2
#define TRAP5 0x17e2
        case TRAP3:
        case TRAP4:
        case TRAP5:
            continue;
        default:
            return false;
        }
    }
    return true;
}
