BLAS
==================================

## Why?

-Fast BLAS optimized to run in cache, parallelism from top layer
-Atlas and others have all been optimized for caches
-Based on inputs from Robet van de Geijn this should be easy
-Assembly optimized for Epiphany, but with a clean C reference implementation
-Small code size, less error checking, boundaries, size checks, corner cases.

## Goals
-Tiny code size footprint
-Very fast out of the box with C
-Architeture specific assembly/generated versions (asm/blis)

