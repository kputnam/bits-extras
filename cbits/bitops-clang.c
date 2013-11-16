#include "bitops.h"

unsigned int ffs (unsigned int x) { return __builtin_ffs (x); }
unsigned int clz (unsigned int x) { return __builtin_clz (x); }
unsigned int ctz (unsigned int x) { return __builtin_ctz (x); }
unsigned int popcount (unsigned int x) { return __builtin_popcount (x); }
unsigned int parity (unsigned int x) { return __builtin_parity (x); }
unsigned int bswap32 (unsigned int x) { return __builtin_bswap32(x); }

unsigned int ffsll (unsigned long long x) { return __builtin_ffsll (x); }
unsigned int clzll (unsigned long long x) { return __builtin_clzll (x); }
unsigned int ctzll (unsigned long long x) { return __builtin_ctzll (x); }
unsigned int popcountll (unsigned long long x) { return __builtin_popcountll (x); }
unsigned int parityll (unsigned long long x) { return __builtin_parityll (x); }
unsigned long long bswap64 (unsigned long long x) { return __builtin_bswap64(x); }

unsigned int ffsl (unsigned long x) { return __builtin_ffsll (x); }
unsigned int clzl (unsigned long x) { return __builtin_clzll (x); }
unsigned int ctzl (unsigned long x) { return __builtin_ctzll (x); }
unsigned int popcountl (unsigned long x) { return __builtin_popcountll (x); }
unsigned int parityl (unsigned long x) { return __builtin_parityll (x); }
