{-# LANGUAGE ForeignFunctionInterface #-}
-- | Extended bit operations, implemented using GCC builtins (see
-- <http://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html>).
module Data.Bits.Extras (
    ExtraBits(..)
) where
import Data.Bits
import Data.Word
import Data.Int
import Foreign.C.Types

-- | Instances provided: 'Word', 'Word8', 'Word16', 'Word32', 'Word64', 'Int', 'Int8', 'Int16', 'Int32', 'Int64'
class Bits x => ExtraBits x where
    -- | Returns one plus the index of the least significant 1-bit of x, or if
    -- x is zero, returns zero. 
    lowestBitPlus1 :: x -> Word32
    -- | Returns the number of leading 0-bits in x, starting at the most
    -- significant bit position. If x is 0, the result is undefined. 
    leadingZeros :: x -> Word32
    -- | Returns the number of trailing 0-bits in x, starting at the least
    -- significant bit position. If x is 0, the result is undefined. 
    trailingZeros :: x -> Word32
    -- | Returns the number of 1-bits in x. 
    populationCount :: x -> Word32
    -- | Returns the parity of x, i.e. the number of 1-bits in x modulo 2. 
    parity :: x -> Word32
    -- | Returns x with the order of the bytes reversed; for example,
    -- 0xaabbccdd becomes 0xddccbbaa. Byte here always means exactly 8 bits. 
    byteSwap :: x -> x

instance ExtraBits Word where
    lowestBitPlus1 = c_ffsl
    {-# INLINE lowestBitPlus1 #-}
    leadingZeros = c_clzl
    {-# INLINE leadingZeros #-}
    trailingZeros = c_ctzl
    {-# INLINE trailingZeros #-}
    populationCount = c_popcountl
    {-# INLINE populationCount #-}
    parity = c_parityl
    {-# INLINE parity #-}
    byteSwap = if bitSize (undefined :: Word) == 32 
                then fromIntegral . c_bswap32 . fromIntegral
                else fromIntegral . c_bswap64 . fromIntegral
    {-# INLINE byteSwap #-}

instance ExtraBits Word8 where
    lowestBitPlus1 = c_ffs . fromIntegral
    {-# INLINE lowestBitPlus1 #-}
    leadingZeros w = (c_clz $ fromIntegral w) - 24
    {-# INLINE leadingZeros #-}
    trailingZeros = c_ctz . (0xff00 .|.) . fromIntegral
    {-# INLINE trailingZeros #-}
    populationCount = c_popcount . fromIntegral
    {-# INLINE populationCount #-}
    parity = c_parity . fromIntegral
    {-# INLINE parity #-}
    byteSwap = id
    {-# INLINE byteSwap #-}

instance ExtraBits Word16 where
    lowestBitPlus1 = c_ffs . fromIntegral
    {-# INLINE lowestBitPlus1 #-}
    leadingZeros w = (c_clz $ fromIntegral w) - 16
    {-# INLINE leadingZeros #-}
    trailingZeros = c_ctz . (0xffff0000 .|.) . fromIntegral
    {-# INLINE trailingZeros #-}
    populationCount = c_popcount . fromIntegral
    {-# INLINE populationCount #-}
    parity = c_parity . fromIntegral
    {-# INLINE parity #-}
    byteSwap w = w `shiftR` 8 .|. w `shiftL` 8
    {-# INLINE byteSwap #-}

instance ExtraBits Word32 where
    lowestBitPlus1 = c_ffs
    {-# INLINE lowestBitPlus1 #-}
    leadingZeros = c_clz
    {-# INLINE leadingZeros #-}
    trailingZeros = c_ctz
    {-# INLINE trailingZeros #-}
    populationCount = c_popcount
    {-# INLINE populationCount #-}
    parity = c_parity
    {-# INLINE parity #-}
    byteSwap = c_bswap32
    {-# INLINE byteSwap #-}

instance ExtraBits Word64 where
    lowestBitPlus1 = c_ffsll
    {-# INLINE lowestBitPlus1 #-}
    leadingZeros = c_clzll
    {-# INLINE leadingZeros #-}
    trailingZeros = fromIntegral . c_ctzll
    {-# INLINE trailingZeros #-}
    populationCount = c_popcountll
    {-# INLINE populationCount #-}
    parity = c_parityll
    {-# INLINE parity #-}
    byteSwap = c_bswap64
    {-# INLINE byteSwap #-}

instance ExtraBits Int where
    lowestBitPlus1 = c_ffsl . fromIntegral
    {-# INLINE lowestBitPlus1 #-}
    leadingZeros = c_clzl . fromIntegral
    {-# INLINE leadingZeros #-}
    trailingZeros = c_ctzl . fromIntegral
    {-# INLINE trailingZeros #-}
    populationCount = c_popcountl . fromIntegral
    {-# INLINE populationCount #-}
    parity = c_parityl . fromIntegral
    {-# INLINE parity #-}
    byteSwap = if bitSize (undefined :: Int) == 32
                then fromIntegral . c_bswap32 . fromIntegral
                else fromIntegral . c_bswap64 . fromIntegral
    {-# INLINE byteSwap #-}

instance ExtraBits Int8 where
    lowestBitPlus1 i = lowestBitPlus1 (fromIntegral i :: Word8)
    leadingZeros i = leadingZeros (fromIntegral i :: Word8)
    trailingZeros i = trailingZeros (fromIntegral i :: Word8)
    populationCount = c_popcount . fromIntegral
    {-# INLINE populationCount #-}
    parity = c_parity . fromIntegral
    {-# INLINE parity #-}
    byteSwap = id

instance ExtraBits Int16 where
    lowestBitPlus1 i = lowestBitPlus1 (fromIntegral i :: Word16)
    leadingZeros i = leadingZeros (fromIntegral i :: Word16)
    trailingZeros i = trailingZeros (fromIntegral i :: Word16)
    populationCount = c_popcount . fromIntegral
    {-# INLINE populationCount #-}
    parity = c_parity . fromIntegral
    {-# INLINE parity #-}
    byteSwap i = fromIntegral . byteSwap $ (fromIntegral i :: Word16)

instance ExtraBits Int32 where
    lowestBitPlus1 = c_ffs . fromIntegral
    {-# INLINE lowestBitPlus1 #-}
    leadingZeros = c_clz . fromIntegral
    {-# INLINE leadingZeros #-}
    trailingZeros = c_ctz . fromIntegral
    {-# INLINE trailingZeros #-}
    populationCount = c_popcount . fromIntegral
    {-# INLINE populationCount #-}
    parity = c_parity . fromIntegral
    {-# INLINE parity #-}
    byteSwap = fromIntegral . c_bswap32 . fromIntegral
    {-# INLINE byteSwap #-}

instance ExtraBits Int64 where
    lowestBitPlus1 = fromIntegral . c_ffsll . fromIntegral
    {-# INLINE lowestBitPlus1 #-}
    leadingZeros = fromIntegral . c_clzll . fromIntegral
    {-# INLINE leadingZeros #-}
    trailingZeros = fromIntegral . c_ctzll . fromIntegral
    {-# INLINE trailingZeros #-}
    populationCount = fromIntegral . c_popcountll . fromIntegral
    {-# INLINE populationCount #-}
    parity = fromIntegral . c_parityll . fromIntegral
    {-# INLINE parity #-}
    byteSwap = fromIntegral . c_bswap64 . fromIntegral
    {-# INLINE byteSwap #-}


-- 32-bit variants
foreign import ccall unsafe "bitops-gcc.h ffs"
    c_ffs :: Word32 -> Word32
foreign import ccall unsafe "bitops-gcc.h clz"
    c_clz :: Word32 -> Word32
foreign import ccall unsafe "bitops-gcc.h ctz"
    c_ctz :: Word32 -> Word32
foreign import ccall unsafe "bitops-gcc.h popcount"
    c_popcount :: Word32 -> Word32
foreign import ccall unsafe "bitops-gcc.h parity"
    c_parity :: Word32 -> Word32
foreign import ccall unsafe "bitops-gcc.h bswap32"
    c_bswap32 :: Word32 -> Word32

-- 64-bit versions
foreign import ccall unsafe "bitops-gcc.h ffsll"
    c_ffsll :: Word64 -> Word32
foreign import ccall unsafe "bitops-gcc.h clzll"
    c_clzll :: Word64 -> Word32
foreign import ccall unsafe "bitops-gcc.h ctzll"
    c_ctzll :: Word64 -> CInt
foreign import ccall unsafe "bitops-gcc.h popcountll"
    c_popcountll :: Word64 -> Word32
foreign import ccall unsafe "bitops-gcc.h parityll"
    c_parityll :: Word64 -> Word32
foreign import ccall unsafe "bitops-gcc.h bswap64"
    c_bswap64 :: Word64 -> Word64

-- Word-sized versions
foreign import ccall unsafe "bitops-gcc.h ffsl"
    c_ffsl :: Word -> Word32
foreign import ccall unsafe "bitops-gcc.h clzl"
    c_clzl :: Word -> Word32
foreign import ccall unsafe "bitops-gcc.h ctzl"
    c_ctzl :: Word -> Word32
foreign import ccall unsafe "bitops-gcc.h popcountl"
    c_popcountl :: Word -> Word32
foreign import ccall unsafe "bitops-gcc.h parityl"
    c_parityl :: Word -> Word32

