import Foreign.Ptr
import Foreign.Marshal.Alloc
import Criterion.Main (defaultMain, bench, bgroup, whnf, nfIO)
import System.IO.Unsafe(unsafePerformIO)
import Data.Word
import Data.Bits

import Data.Bits.Atomic
import Data.Bits.Extras
import GHC.Exts(inline)

p :: Ptr Word
p = unsafePerformIO $ mallocBytes 8
w :: Word
w = 0xdeadbeef

p8 :: Ptr Word8
p8 = unsafePerformIO $ mallocBytes 1
w8 :: Word8
w8 = 220

p32 :: Ptr Word32
p32 = unsafePerformIO $ mallocBytes 4
w32 :: Word32
w32 = 1000

p64 :: Ptr Word64
p64 = unsafePerformIO $ mallocBytes 8
w64 :: Word64
w64 = 670



nothing :: Word -> Word
nothing = (1 +)

nothingIO :: Word -> IO Word
nothingIO i = return $! i + 1

main :: IO ()
main = defaultMain 
    [ bgroup "Extended bitops" 
        [ bench "lowestBitPlus1" (whnf lowestBitPlus1 w)
        , bench "leadingZeros" (whnf leadingZeros w)
        , bench "trailingZeros" (whnf trailingZeros w)
        , bench "populationCount" (whnf populationCount w)
        , bench "parity" (whnf parity w)
        , bench "byteSwap" (whnf byteSwap w)
        , bench "byteSwap 64" (whnf byteSwap w64)
        , bench "bswapW" (whnf bswapW w)
        , bench "bswap64N" (whnf bswap64N w64)
        , bench "bswap32" (whnf bswap32 w32)
        , bench "nothing" (whnf nothing w)
        ]
    , bgroup "Atomic bitops"
        [ bench "fetchAndAdd" (nfIO (fetchAndAdd p w))
        , bench "fetchAndOr" (nfIO (fetchAndOr p w))
        , bench "addAndFetch" (nfIO (addAndFetch p w))
        , bench "compareAndSwap" (nfIO (compareAndSwap p w (w+1)))
        , bench "compareAndSwap 8" (nfIO (compareAndSwap p8 w8 (w8+1)))
        , bench "compareAndSwap 32" (nfIO (compareAndSwap p32 w32 (w32+1)))
        , bench "lockTestAndSet" (nfIO (lockTestAndSet p))
        , bench "lockTestAndSet 8" (nfIO (lockTestAndSet p8))
        , bench "lockTestAndSet 32" (nfIO (lockTestAndSet p32))
        , bench "lockRelease" (nfIO (lockRelease p))
        , bench "lockRelease 8" (nfIO (lockRelease p8))
        , bench "lockRelease 32" (nfIO (lockRelease p32))
        , bench "memoryBarrier" (nfIO memoryBarrier)
        , bench "nothingIO" (nfIO (nothingIO w))
        ]
    ]


bswapW :: Word -> Word
bswapW = if bitSize (undefined :: Word) == 32 
  then fromIntegral . bswap32 . fromIntegral
  else fromIntegral . bswap64 . fromIntegral

bswap32 :: Word32 -> Word32
bswap32 x =     x `shiftL` 24 .&. 0xff000000
            .|. x `shiftL` 8  .&. 0x00ff0000
            .|. 0x0000ff00 .&. x `shiftR` 8  
            .|. 0x000000ff .&. x `shiftR` 24

bswap64 :: Word64 -> Word64
bswap64 x = let low, high :: Word32
                low  = fromIntegral x
                high = fromIntegral $ x `shiftR` 32
            in
                fromIntegral (inline $ bswap32 low) `shiftL` 32
            .|. 
                fromIntegral (inline $ bswap32 high)
{-# INLINE bswap64 #-}

bswap64N :: Word64 -> Word64
bswap64N x =        x  `shiftL` 56   .&. 0xff00000000000000
                .|. x  `shiftL` 40   .&. 0x00ff000000000000
                .|. 0x0000ff0000000000 .&. x  `shiftL` 24
                .|. 0x000000ff00000000 .&. x  `shiftL` 8
                .|. x  `shiftR` 8    .&. 0x00000000ff000000
                .|. x  `shiftR` 24   .&. 0x0000000000ff0000
                .|. 0x000000000000ff00 .&. x  `shiftR` 40
                .|. 0x00000000000000ff .&. x  `shiftR` 56
