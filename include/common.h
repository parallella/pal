#pragma once

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

