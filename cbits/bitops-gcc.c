#include "bitops-gcc.h"

inline unsigned int ffs (unsigned int x) { return __builtin_ffs (x); }
inline unsigned int clz (unsigned int x) { return __builtin_clz (x); }
inline unsigned int ctz (unsigned int x) { return __builtin_ctz (x); }
inline unsigned int popcount (unsigned int x) { return __builtin_popcount (x); }
inline unsigned int parity (unsigned int x) { return __builtin_parity (x); }
inline unsigned int bswap32 (unsigned int x) { return __builtin_bswap32(x); }

inline unsigned int ffsll (unsigned long long x) { return __builtin_ffsll (x); }
inline unsigned int clzll (unsigned long long x) { return __builtin_clzll (x); }
inline unsigned int ctzll (unsigned long long x) { return __builtin_ctzll (x); }
inline unsigned int popcountll (unsigned long long x) { return __builtin_popcountll (x); }
inline unsigned int parityll (unsigned long long x) { return __builtin_parityll (x); }
inline unsigned long long bswap64 (unsigned long long x) { return __builtin_bswap64(x); }

inline unsigned int ffsl (unsigned long x) { return __builtin_ffsll (x); }
inline unsigned int clzl (unsigned long x) { return __builtin_clzll (x); }
inline unsigned int ctzl (unsigned long x) { return __builtin_ctzll (x); }
inline unsigned int popcountl (unsigned long x) { return __builtin_popcountll (x); }
inline unsigned int parityl (unsigned long x) { return __builtin_parityll (x); }
