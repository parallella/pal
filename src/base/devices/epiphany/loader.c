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
#include "../../pal_base_private.h"
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

struct symbol_info {
    const char *name;
    bool found;
    Elf32_Sym sym;
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

static inline uint32_t reg_read(volatile void *base, uintptr_t offset)
{
    volatile uint32_t *reg = (uint32_t *) ((uintptr_t) base + offset);
    return *reg;
}

static inline uint32_t reg_write(volatile void *base, uintptr_t offset,
                                 uint32_t val)
{
    volatile uint32_t *reg = (uint32_t *) ((uintptr_t) base + offset);
    *reg = val;
}

int ecore_soft_reset_dma(struct epiphany_dev *epiphany, unsigned coreid)
{
    uint32_t config;
    bool fail0, fail1;
    int i;
    /* HACK: Depends on that we do an explicit mmap of Epiphany addresses space
     * in device.c:mmap_chip_mem() */
    volatile void *core = (void *) (coreid << 20);

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
    volatile void *core = (void *) (coreid << 20);

    /* General purpose registers */
    memset((void *) ((uintptr_t) core + MMR_R0), 0, 64 * 4);

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
    /* HACK: Depends on that we do an explicit mmap of Epiphany addresses space
     * in device.c:mmap_chip_mem() */
    union {
        void     *v;
        uint32_t *u32;
        uint8_t  *u8;
    } core = { .v = (void *) (coreid << 20) };

    /* Set IVT entries to a safe instruction */
    for (i = 0; i < ivt_size / 4; i++)
        core.u32[i] = insn;

    memset(&core.u8[ivt_size], 0, 32768 - ivt_size);
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
    /* HACK: Depends on that we do an explicit mmap of Epiphany addresses space
     * in device.c:mmap_chip_mem() */
    volatile void *core = (void *) (coreid << 20);

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

    memcpy((void *) core, soft_reset_payload, sizeof(soft_reset_payload));

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
        unsigned row = 32 + i / 4;
        unsigned col =  8 + i % 4;
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

                if (tbl[i].found
                    && ELF32_ST_BIND(tbl[i].sym.st_info) == STB_GLOBAL)
                    continue;

                tbl[i].found = true;
                memcpy(&tbl[i].sym, sym, sizeof(*sym));
            }
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
                                uint32_t *loader_args_ptr)
{
    unsigned rel_row = (coreid - 0x808) >> 6;
    unsigned rel_col = (coreid - 0x808) & 0x3f;
    unsigned rel_coreid = (rel_row << 2) + rel_col;
    struct loader_args *loader_args = &epiphany->ctrl->loader_args[rel_coreid];
    uint32_t *regs = &loader_args->r0;
    unsigned arg = 0;
    /* TODO: Obviously this doesn't work with more than one core */
    uint8_t *argstackp = (uint8_t *) epiphany->ctrl;

    /* Set cores's args ptr */
    *loader_args_ptr = (uint32_t) loader_args;

    memset(loader_args, 0 , sizeof(*loader_args));

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
            if (is_valid_addr((uint32_t) args[arg].ptr, coreid) &&
                /* On the off chance the host ptr is in the first 1M */
                !is_local((uint32_t) args[arg].ptr)) {

                regs[reg] = (uint32_t) args[arg].ptr;

            } else {
                argstackp -= (args[arg].size + 7) & (~7);
                memcpy(argstackp, args[arg].ptr, args[arg].size);
                regs[reg] = (uint32_t) argstackp;
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
        if (is_valid_addr((uint32_t) args[i].ptr, coreid) &&
            /* On the off chance the host ptr is in the first 1M */
            !is_local((uint32_t) args[i].ptr)) {

            ptrslot[i] = (uint32_t) args[i].ptr;
        } else {
            argstackp -= (args[i].size + 7) & (~7);
            memcpy(argstackp, args[i].ptr, args[i].size);
            ptrslot[i] = (uint32_t) argstackp;
        }
    }

    /* Allocate the maximum possible size for actual stack arguments,
     * 8 bytes / entry. */
    argstackp -= (argn - arg) * 8;

    loader_args->stack_spill_ptr = (uint32_t) argstackp;

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
static int set_core_config(struct epiphany_dev *epiphany, unsigned coreid,
                           const void *file, size_t file_size, int argn,
                           const p_arg_t *args, const char *function)
{
    uint8_t *corep = (uint8_t *) (coreid << 20);
    uint8_t *nullp = (uint8_t *) 0;
    e_group_config_t *e_group_config;
    e_emem_config_t  *e_emem_config;
    uint32_t *loader_flags, *loader_args_ptr;
    uint32_t function_addr;
    char *function_plt;

    {
        size_t function_len = strlen(function);
        function_plt = alloca(function_len + sizeof("@PLT"));
        memcpy(&function_plt[function_len], "@PLT", sizeof("@PLT"));
    }

    struct symbol_info tbl[] = {
        { .name = "e_group_config"    },
        { .name = "e_emem_config"     },
        { .name = "__loader_args_ptr" },
        { .name = "__loader_flags"    },
        { .name = function            },
        { .name = function_plt        },
    };
    lookup_symbols(file, file_size, tbl, ARRAY_SIZE(tbl));

    /* Check everything except PLT entry (only exists when compiled w/ -fpic) */
    for (unsigned i; i < ARRAY_SIZE(tbl) - 1; i++) {
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
    loader_args_ptr = is_local(tbl[2].sym.st_value) ?
        (uint32_t *) &corep[tbl[2].sym.st_value] :
        (uint32_t *) &nullp[tbl[2].sym.st_value];
    loader_flags = is_local(tbl[3].sym.st_value) ?
        (uint32_t *) &corep[tbl[3].sym.st_value] :
        (uint32_t *) &nullp[tbl[3].sym.st_value];

    /* Prefer PLT entry */
    /* No need to adjust function address if it's local */
    if (tbl[5].found && is_valid_addr(tbl[5].sym.st_value, coreid))
        function_addr = tbl[5].sym.st_value;
    else
        function_addr = tbl[4].sym.st_value;

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

    setup_function_args(epiphany, coreid, argn, args, function_addr,
                        loader_args_ptr);

    // Instruct crt0 .bss is cleared and that we've provided custom args.
    *loader_flags = LOADER_BSS_CLEARED_FLAG | LOADER_CUSTOM_ARGS_FLAG;

    return 0;
}

int epiphany_load(struct team *team, struct prog *prog,
                  int start, int size, int flags, int argn,
                  const p_arg_t *args, const char *function)

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
        rc = set_core_config(epiphany, coreid, file, st.st_size, argn, args,
                             function);
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

        reg_write(corep, MMR_ILATST, 1);
    }
}

int epiphany_reset_system(struct epiphany_dev *epiphany)
{
    if (ioctl(epiphany->epiphany_fd, E_IOCTL_RESET))
        return -errno;

    return 0;
}
