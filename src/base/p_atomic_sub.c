#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

uint8_t p_atomic_sub_u8(uint8_t * atom, uint8_t n)
{
	return __sync_fetch_and_sub(atom, n);
}

uint16_t p_atomic_sub_u16(uint16_t * atom, uint16_t n)
{
	return __sync_fetch_and_sub(atom, n);
}

uint32_t p_atomic_sub_u32(uint32_t * atom, uint32_t n)
{
	return __sync_fetch_and_sub(atom, n);
}

uint64_t p_atomic_sub_u64(uint64_t * atom, uint64_t n)
{
	return __sync_fetch_and_sub(atom, n);
}

int8_t p_atomic_sub_i8(int8_t * atom, int8_t n)
{
	return __sync_fetch_and_sub(atom, n);
}

int16_t p_atomic_sub_i16(int16_t * atom, int16_t n)
{
	return __sync_fetch_and_sub(atom, n);
}

int32_t p_atomic_sub_i32(int32_t * atom, int32_t n)
{
	return __sync_fetch_and_sub(atom, n);
}

int64_t p_atomic_sub_i64(int64_t * atom, int64_t n)
{
	return __sync_fetch_and_sub(atom, n);
}
