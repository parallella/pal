%%% @author  Mark A Fleming
%%% @copyright (C) 2015, 
%%% @doc
%%% Constants defined for the Epiphany eHAL SDK.
%%% 
%%% @end
%%% Created : 12 Jan 2015 by  Mark A Fleming

%% Enumerated constants and macros
-define(E_FALSE, false).
-define(E_TRUE,  true).

-define(E_OK,   ok).
-define(E_ERR,  error).
-define(E_WARN, warning).

-define(H_D0, 0).
-define(H_D1, 1).
-define(H_D2, 2).
-define(H_D3, 3).
-define(H_D4, 4).

-define(L_D0, 0).
-define(L_D1, 1).
-define(L_D2, 2).
-define(L_D3, 3).
-define(L_D4, 4).


%% General purpose registers
-define(E_REG_R0,  0).
-define(E_REG_R1,  1).
-define(E_REG_R2,  2).
-define(E_REG_R3,  3).
-define(E_REG_R4,  4).
-define(E_REG_R5,  5).
-define(E_REG_R6,  6).
-define(E_REG_R7,  7).
-define(E_REG_R8,  8).
-define(E_REG_R9,  9).
-define(E_REG_R10, 10).
-define(E_REG_R11, 11).
-define(E_REG_R12, 12).
-define(E_REG_R13, 13).
-define(E_REG_R14, 14).
-define(E_REG_R15, 15).
-define(E_REG_R16, 16).
-define(E_REG_R17, 17).
-define(E_REG_R18, 18).
-define(E_REG_R19, 19).
-define(E_REG_R20, 20).
-define(E_REG_R21, 21).
-define(E_REG_R22, 22).
-define(E_REG_R23, 23).
-define(E_REG_R24, 24).
-define(E_REG_R25, 25).
-define(E_REG_R26, 26).
-define(E_REG_R27, 27).
-define(E_REG_R28, 28).
-define(E_REG_R29, 29).
-define(E_REG_R30, 30).
-define(E_REG_R31, 31).
-define(E_REG_R32, 32).
-define(E_REG_R33, 33).
-define(E_REG_R34, 34).
-define(E_REG_R35, 35).
-define(E_REG_R36, 36).
-define(E_REG_R37, 37).
-define(E_REG_R38, 38).
-define(E_REG_R39, 39).
-define(E_REG_R40, 40).
-define(E_REG_R41, 41).
-define(E_REG_R42, 42).
-define(E_REG_R43, 43).
-define(E_REG_R44, 44).
-define(E_REG_R45, 45).
-define(E_REG_R46, 46).
-define(E_REG_R47, 47).
-define(E_REG_R48, 48).
-define(E_REG_R49, 49).
-define(E_REG_R50, 50).
-define(E_REG_R51, 51).
-define(E_REG_R52, 52).
-define(E_REG_R53, 53).
-define(E_REG_R54, 54).
-define(E_REG_R55, 55).
-define(E_REG_R56, 56).
-define(E_REG_R57, 57).
-define(E_REG_R58, 58).
-define(E_REG_R59, 59).
-define(E_REG_R60, 60).
-define(E_REG_R61, 61).
-define(E_REG_R62, 62).
-define(E_REG_R63, 63).

%% eCore Special Registers
%% Control Registers
-define(E_REG_CONFIG,        0).
-define(E_REG_IRET,          1).
-define(E_REG_STATUS,        2).
-define(E_REG_IMASK,         3).
-define(E_REG_FSTATUS,       4).
-define(E_REG_ILAT,          5).
-define(E_REG_PC,            6).
-define(E_REG_ILATST,        7). 
-define(E_REG_DEBUGSTATUS,   8).
-define(E_REG_ILATCL,        9).
-define(E_REG_DEBUGCMD,     10).
-define(E_REG_IPEND,        11).
-define(E_REG_LC,           12).
-define(E_REG_LS,           13).
-define(E_REG_LE,           14).
%% DMA registers 
-define(E_REG_DMA0CONFIG,   15).
-define(E_REG_DMA1CONFIG,   16).
-define(E_REG_DMA0STRIDE,   17).
-define(E_REG_DMA1STRIDE,   18).
-define(E_REG_DMA0COUNT,    19).
-define(E_REG_DMA1COUNT,    20).
-define(E_REG_DMA0SRCADDR,  21).
-define(E_REG_DMA1SRCADDR,  22).
-define(E_REG_DMA0DSTADDR,  23).
-define(E_REG_DMA1DSTADDR,  24).
-define(E_REG_DMA0AUTODMA0, 25).
-define(E_REG_DMA1AUTODMA0, 26).
-define(E_REG_DMA0AUTODMA1, 27).
-define(E_REG_DMA1AUTODMA1, 28).
-define(E_REG_DMA0STATUS,   29).
-define(E_REG_DMA1STATUS,   30).
%% Event Timer Registers    31).
-define(E_REG_CTIMER0,      32).
-define(E_REG_CTIMER1,      33).
%% Processor Control Registers 
-define(E_REG_MEMPROTECT,   34).
-define(E_REG_MESH_CONFIG,  35).
-define(E_REG_COREID,       36).
-define(E_REG_CORE_RESET,   37).

%% Chip Registers 
%% (see Epiphany Chip Datasheets for details) 
-define(E_REG_IO_LINK_MODE_CFG, 0).
-define(E_REG_IO_LINK_TX_CFG,   1).
-define(E_REG_IO_LINK_RX_CFG,   2).
-define(E_REG_IO_LINK_DEBUG,    3).
-define(E_REG_IO_GPIO_CFG,      4).
-define(E_REG_IO_FLAG_CFG,      5).
-define(E_REG_IO_SYNC_CFG,      6).
-define(E_REG_IO_HALT_CFG,      7).
-define(E_REG_IO_RESET,         8).

%% Epiphany system registers
%% (see Board manual for details) 
-define(E_SYS_CONFIG,  0).
-define(E_SYS_RESET,   1).
-define(E_SYS_VERSION, 2).
-define(E_SYS_FILTERL, 3).
-define(E_SYS_FILTERH, 4).
-define(E_SYS_FILTERC, 5).
