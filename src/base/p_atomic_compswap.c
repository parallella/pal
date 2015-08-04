#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

uint8_t p_atomic_compswap_u8(uint8_t * atom, uint8_t oldval, uint8_t newval)
{
	return __sync_bool_compare_and_swap(atom, oldval, newval);
}

uint16_t p_atomic_compswap_u16(uint16_t * atom, uint16_t oldval,
			       uint16_t newval)
{
	return __sync_bool_compare_and_swap(atom, oldval, newval);
}

uint32_t p_atomic_compswap_u32(uint32_t * atom, uint32_t oldval,
			       uint32_t newval)
{
	return __sync_bool_compare_and_swap(atom, oldval, newval);
}

uint64_t p_atomic_compswap_u64(uint64_t * atom, uint64_t oldval,
			       uint64_t newval)
{
	return __sync_bool_compare_and_swap(atom, oldval, newval);
}

int8_t p_atomic_compswap_i8(int8_t * atom, int8_t oldval, int8_t newval)
{
	return __sync_bool_compare_and_swap(atom, oldval, newval);
}

int16_t p_atomic_compswap_i16(int16_t * atom, int16_t oldval, int16_t newval)
{
	return __sync_bool_compare_and_swap(atom, oldval, newval);
}

int32_t p_atomic_compswap_i32(int32_t * atom, int32_t oldval, int32_t newval)
{
	return __sync_bool_compare_and_swap(atom, oldval, newval);
}

int64_t p_atomic_compswap_i64(int64_t * atom, int64_t oldval, int64_t newval)
{
	return __sync_bool_compare_and_swap(atom, oldval, newval);
}
