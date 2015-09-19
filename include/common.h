#pragma once
#include <stddef.h>

#define CONCAT2(_a, _b)                  _a##_b
#define CONCAT3(_a, _b, _c)              _a##_b##_c
#define CONCAT4(_a, _b, _c, _d)          _a##_b##_c##_d
#define CONCAT5(_a, _b, _c, _d, _e)      _a##_b##_c##_d##_e
#define CONCAT6(_a, _b, _c, _d, _e, _f)  _a##_b##_c##_d##_e##_f

#define XCONCAT2(_a, _b)                 CONCAT2(_a, _b)
#define XCONCAT3(_a, _b, _c)             CONCAT3(_a, _b, _c)
#define XCONCAT4(_a, _b, _c, _d)         CONCAT4(_a, _b, _c, _d)
#define XCONCAT5(_a, _b, _c, _d, _e)     CONCAT5(_a, _b, _c, _d, _e)
#define XCONCAT6(_a, _b, _c, _d, _e, _f) CONCAT6(_a, _b, _c, _d, _e, _f)

#define STRING(_s) #_s
#define XSTRING(_s) STRING(_s)

#define ARRAY_SIZE(_a) (sizeof(_a) / sizeof((_a)[0]))
#define FIELD_SIZEOF(_t, _f) (sizeof(((_t*)0)->_f))

// For an explanation, see:
// http://www.linuxjournal.com/files/linuxjournal.com/linuxjournal/articles/067/6717/6717s2.html
#define container_of(ptr, type, member)\
    ({\
        const typeof( ((type *)0)->member ) *__mptr = (ptr);\
        (type *)( (char *)__mptr - offsetof(type,member) );\
    })
