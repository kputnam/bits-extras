{-# LANGUAGE ForeignFunctionInterface #-}
-- | Atomic bit operations, using GCC's built-in atomic operations in small C
-- wrapper functions called through the FFI. See
-- <http://gcc.gnu.org/onlinedocs/gcc-4.5.0/gcc/Atomic-Builtins.html> or the
-- version corresponding to your compiler for more detail.
--
-- The C code dynamically links to @libgcc_s@, which can cause problems in
-- GHCi. GHCi does not currently support sonames and tries to open
-- @libgcc_s.so@ while ignoring e.g. @libgcc_s.so.1@. A possible workaround
-- for GHCi on a linux system: @ln -s \/lib\/libgcc_s.so.1 \/lib\/libgcc_s.so@
module Data.Bits.Atomic (
    -- * Atomic bit operations on 'Word'-sized memory locations
    AtomicBits(..),
    -- * Utility
    memoryBarrier
) where
import Data.Bits
import Data.Word
import Data.Int
import Foreign.Ptr

-- | Atomic bit operations on a memory location. 
--
-- Instances: 'Word', 'Word8', 'Word16', 'Word32', 'Word64', 'Int', 'Int8',
-- 'Int16', 'Int32', 'Int64'.
class Bits x => AtomicBits x where
    -- | Atomic @(+)@, returning the original value.
    fetchAndAdd :: Ptr x -> x -> IO x
    -- | Atomic @(-)@, returning the originial value.
    fetchAndSub :: Ptr x -> x -> IO x
    -- | Atomic @(.|.)@, returning the original value.
    fetchAndOr :: Ptr x -> x -> IO x
    -- | Atomic @(.&.)@, returning the original value.
    fetchAndAnd :: Ptr x -> x -> IO x
    -- | Atomic @xor@, returning the original value.
    fetchAndXor :: Ptr x -> x -> IO x
    -- | Atomic @nand@, returning the original value.
    fetchAndNand :: Ptr x -> x -> IO x
    -- | Atomic @(+)@, returning the updated value.
    addAndFetch :: Ptr x -> x -> IO x
    -- | Atomic @(-)@, returning the updated value.
    subAndFetch :: Ptr x -> x -> IO x
    -- | Atomic @(.|.)@, returning the updated value.
    orAndFetch :: Ptr x -> x -> IO x
    -- | Atomic @(.&.)@, returning the updated value.
    andAndFetch :: Ptr x -> x -> IO x
    -- | Atomic @xor@, returning the updated value.
    xorAndFetch :: Ptr x -> x -> IO x
    -- | Atomic @nand@, returning the updated value.
    nandAndFetch :: Ptr x -> x -> IO x
    -- | Atomic @CAS@ with boolean return.
    compareAndSwapBool :: Ptr x  -- ^ The memory location to update
                       -> x  -- ^ Old value
                       -> x  -- ^ Intended new value
                       -> IO Bool -- ^ 'True' if swapped, 'False' otherwise

    -- | Atomic @CAS@, returning the original value.
    compareAndSwap :: Ptr x  -- ^ The memory location to update
                   -> x  -- ^ Old value
                   -> x  -- ^ Intended new value
                   -> IO x -- ^ Original value

    -- | Atomically update the memory location with the value 1 and return the
    -- original value, 0 in case 'lockRelease' was previously called or @1@ if
    -- another call to 'lockTestAndSet' aquired the lock earlier. Implies an
    -- /aquire barrier/.
    lockTestAndSet :: Ptr x -> IO x
    -- | Release the lock by writing a @0@. Includes a /release barrier/.
    lockRelease :: Ptr x -> IO ()


-- | A full memory barrier.
foreign import ccall unsafe "atomic-bitops-gcc.h mem_barrier"
    memoryBarrier :: IO ()

foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_add_8"
    fetch_and_add_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_sub_8"
    fetch_and_sub_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_or_8"
    fetch_and_or_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_and_8"
    fetch_and_and_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_xor_8"
    fetch_and_xor_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_nand_8"
    fetch_and_nand_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h add_and_fetch_8"
    add_and_fetch_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h sub_and_fetch_8"
    sub_and_fetch_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h or_and_fetch_8"
    or_and_fetch_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h and_and_fetch_8"
    and_and_fetch_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h xor_and_fetch_8"
    xor_and_fetch_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h nand_and_fetch_8"
    nand_and_fetch_8 :: Ptr Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h bool_compare_and_swap_8"
    bool_compare_and_swap_8 :: Ptr Word8 -> Word8 -> Word8 -> IO Bool
foreign import ccall unsafe "atomic-bitops-gcc.h val_compare_and_swap_8"
    val_compare_and_swap_8 :: Ptr Word8 -> Word8 -> Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h lock_test_and_set_8"
    lock_test_and_set_8 :: Ptr Word8 -> IO Word8
foreign import ccall unsafe "atomic-bitops-gcc.h lock_release_8"
    lock_release_8 :: Ptr Word8 -> IO ()

instance AtomicBits Word8 where
    fetchAndAdd = fetch_and_add_8
    {-# INLINE fetchAndAdd #-}
    fetchAndSub = fetch_and_sub_8
    {-# INLINE fetchAndSub #-}
    fetchAndOr = fetch_and_or_8
    {-# INLINE fetchAndOr #-}
    fetchAndAnd = fetch_and_and_8
    {-# INLINE fetchAndAnd #-}
    fetchAndXor = fetch_and_xor_8
    {-# INLINE fetchAndXor #-}
    fetchAndNand = fetch_and_nand_8
    {-# INLINE fetchAndNand #-}
    addAndFetch = add_and_fetch_8
    {-# INLINE addAndFetch #-}
    subAndFetch = sub_and_fetch_8
    {-# INLINE subAndFetch #-}
    orAndFetch = or_and_fetch_8
    {-# INLINE orAndFetch #-}
    andAndFetch = and_and_fetch_8
    {-# INLINE andAndFetch #-}
    xorAndFetch = xor_and_fetch_8
    {-# INLINE xorAndFetch #-}
    nandAndFetch = nand_and_fetch_8
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool = bool_compare_and_swap_8
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap = val_compare_and_swap_8
    {-# INLINE compareAndSwap #-}
    lockTestAndSet = lock_test_and_set_8
    {-# INLINE lockTestAndSet #-}
    lockRelease = lock_release_8
    {-# INLINE lockRelease #-}
    
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_add_16"
    fetch_and_add_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_sub_16"
    fetch_and_sub_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_or_16"
    fetch_and_or_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_and_16"
    fetch_and_and_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_xor_16"
    fetch_and_xor_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_nand_16"
    fetch_and_nand_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h add_and_fetch_16"
    add_and_fetch_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h sub_and_fetch_16"
    sub_and_fetch_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h or_and_fetch_16"
    or_and_fetch_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h and_and_fetch_16"
    and_and_fetch_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h xor_and_fetch_16"
    xor_and_fetch_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h nand_and_fetch_16"
    nand_and_fetch_16 :: Ptr Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h bool_compare_and_swap_16"
    bool_compare_and_swap_16 :: Ptr Word16 -> Word16 -> Word16 -> IO Bool
foreign import ccall unsafe "atomic-bitops-gcc.h val_compare_and_swap_16"
    val_compare_and_swap_16 :: Ptr Word16 -> Word16 -> Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h lock_test_and_set_16"
    lock_test_and_set_16 :: Ptr Word16 -> IO Word16
foreign import ccall unsafe "atomic-bitops-gcc.h lock_release_16"
    lock_release_16 :: Ptr Word16 -> IO ()

instance AtomicBits Word16 where
    fetchAndAdd = fetch_and_add_16
    {-# INLINE fetchAndAdd #-}
    fetchAndSub = fetch_and_sub_16
    {-# INLINE fetchAndSub #-}
    fetchAndOr = fetch_and_or_16
    {-# INLINE fetchAndOr #-}
    fetchAndAnd = fetch_and_and_16
    {-# INLINE fetchAndAnd #-}
    fetchAndXor = fetch_and_xor_16
    {-# INLINE fetchAndXor #-}
    fetchAndNand = fetch_and_nand_16
    {-# INLINE fetchAndNand #-}
    addAndFetch = add_and_fetch_16
    {-# INLINE addAndFetch #-}
    subAndFetch = sub_and_fetch_16
    {-# INLINE subAndFetch #-}
    orAndFetch = or_and_fetch_16
    {-# INLINE orAndFetch #-}
    andAndFetch = and_and_fetch_16
    {-# INLINE andAndFetch #-}
    xorAndFetch = xor_and_fetch_16
    {-# INLINE xorAndFetch #-}
    nandAndFetch = nand_and_fetch_16
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool = bool_compare_and_swap_16
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap = val_compare_and_swap_16
    {-# INLINE compareAndSwap #-}
    lockTestAndSet = lock_test_and_set_16
    {-# INLINE lockTestAndSet #-}
    lockRelease = lock_release_16
    {-# INLINE lockRelease #-}
    
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_add_32"
    fetch_and_add_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_sub_32"
    fetch_and_sub_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_or_32"
    fetch_and_or_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_and_32"
    fetch_and_and_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_xor_32"
    fetch_and_xor_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_nand_32"
    fetch_and_nand_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h add_and_fetch_32"
    add_and_fetch_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h sub_and_fetch_32"
    sub_and_fetch_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h or_and_fetch_32"
    or_and_fetch_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h and_and_fetch_32"
    and_and_fetch_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h xor_and_fetch_32"
    xor_and_fetch_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h nand_and_fetch_32"
    nand_and_fetch_32 :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h bool_compare_and_swap_32"
    bool_compare_and_swap_32 :: Ptr Word32 -> Word32 -> Word32 -> IO Bool
foreign import ccall unsafe "atomic-bitops-gcc.h val_compare_and_swap_32"
    val_compare_and_swap_32 :: Ptr Word32 -> Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h lock_test_and_set_32"
    lock_test_and_set_32 :: Ptr Word32 -> IO Word32
foreign import ccall unsafe "atomic-bitops-gcc.h lock_release_32"
    lock_release_32 :: Ptr Word32 -> IO ()

instance AtomicBits Word32 where
    fetchAndAdd = fetch_and_add_32
    {-# INLINE fetchAndAdd #-}
    fetchAndSub = fetch_and_sub_32
    {-# INLINE fetchAndSub #-}
    fetchAndOr = fetch_and_or_32
    {-# INLINE fetchAndOr #-}
    fetchAndAnd = fetch_and_and_32
    {-# INLINE fetchAndAnd #-}
    fetchAndXor = fetch_and_xor_32
    {-# INLINE fetchAndXor #-}
    fetchAndNand = fetch_and_nand_32
    {-# INLINE fetchAndNand #-}
    addAndFetch = add_and_fetch_32
    {-# INLINE addAndFetch #-}
    subAndFetch = sub_and_fetch_32
    {-# INLINE subAndFetch #-}
    orAndFetch = or_and_fetch_32
    {-# INLINE orAndFetch #-}
    andAndFetch = and_and_fetch_32
    {-# INLINE andAndFetch #-}
    xorAndFetch = xor_and_fetch_32
    {-# INLINE xorAndFetch #-}
    nandAndFetch = nand_and_fetch_32
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool = bool_compare_and_swap_32
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap = val_compare_and_swap_32
    {-# INLINE compareAndSwap #-}
    lockTestAndSet = lock_test_and_set_32
    {-# INLINE lockTestAndSet #-}
    lockRelease = lock_release_32
    {-# INLINE lockRelease #-}
    
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_add_64"
    fetch_and_add_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_sub_64"
    fetch_and_sub_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_or_64"
    fetch_and_or_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_and_64"
    fetch_and_and_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_xor_64"
    fetch_and_xor_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_nand_64"
    fetch_and_nand_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h add_and_fetch_64"
    add_and_fetch_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h sub_and_fetch_64"
    sub_and_fetch_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h or_and_fetch_64"
    or_and_fetch_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h and_and_fetch_64"
    and_and_fetch_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h xor_and_fetch_64"
    xor_and_fetch_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h nand_and_fetch_64"
    nand_and_fetch_64 :: Ptr Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h bool_compare_and_swap_64"
    bool_compare_and_swap_64 :: Ptr Word64 -> Word64 -> Word64 -> IO Bool
foreign import ccall unsafe "atomic-bitops-gcc.h val_compare_and_swap_64"
    val_compare_and_swap_64 :: Ptr Word64 -> Word64 -> Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h lock_test_and_set_64"
    lock_test_and_set_64 :: Ptr Word64 -> IO Word64
foreign import ccall unsafe "atomic-bitops-gcc.h lock_release_64"
    lock_release_64 :: Ptr Word64 -> IO ()

instance AtomicBits Word64 where
    fetchAndAdd = fetch_and_add_64
    {-# INLINE fetchAndAdd #-}
    fetchAndSub = fetch_and_sub_64
    {-# INLINE fetchAndSub #-}
    fetchAndOr = fetch_and_or_64
    {-# INLINE fetchAndOr #-}
    fetchAndAnd = fetch_and_and_64
    {-# INLINE fetchAndAnd #-}
    fetchAndXor = fetch_and_xor_64
    {-# INLINE fetchAndXor #-}
    fetchAndNand = fetch_and_nand_64
    {-# INLINE fetchAndNand #-}
    addAndFetch = add_and_fetch_64
    {-# INLINE addAndFetch #-}
    subAndFetch = sub_and_fetch_64
    {-# INLINE subAndFetch #-}
    orAndFetch = or_and_fetch_64
    {-# INLINE orAndFetch #-}
    andAndFetch = and_and_fetch_64
    {-# INLINE andAndFetch #-}
    xorAndFetch = xor_and_fetch_64
    {-# INLINE xorAndFetch #-}
    nandAndFetch = nand_and_fetch_64
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool = bool_compare_and_swap_64
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap = val_compare_and_swap_64
    {-# INLINE compareAndSwap #-}
    lockTestAndSet = lock_test_and_set_64
    {-# INLINE lockTestAndSet #-}
    lockRelease = lock_release_64
    {-# INLINE lockRelease #-}
    
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_add_word"
    fetch_and_add_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_sub_word"
    fetch_and_sub_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_or_word"
    fetch_and_or_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_and_word"
    fetch_and_and_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_xor_word"
    fetch_and_xor_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_nand_word"
    fetch_and_nand_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h add_and_fetch_word"
    add_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h sub_and_fetch_word"
    sub_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h or_and_fetch_word"
    or_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h and_and_fetch_word"
    and_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h xor_and_fetch_word"
    xor_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h nand_and_fetch_word"
    nand_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h bool_compare_and_swap_word"
    bool_compare_and_swap_word :: Ptr Word -> Word -> Word -> IO Bool
foreign import ccall unsafe "atomic-bitops-gcc.h val_compare_and_swap_word"
    val_compare_and_swap_word :: Ptr Word -> Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h lock_test_and_set_word"
    lock_test_and_set_word :: Ptr Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h lock_release_word"
    lock_release_word :: Ptr Word -> IO ()

instance AtomicBits Word where
    fetchAndAdd = fetch_and_add_word
    {-# INLINE fetchAndAdd #-}
    fetchAndSub = fetch_and_sub_word
    {-# INLINE fetchAndSub #-}
    fetchAndOr = fetch_and_or_word
    {-# INLINE fetchAndOr #-}
    fetchAndAnd = fetch_and_and_word
    {-# INLINE fetchAndAnd #-}
    fetchAndXor = fetch_and_xor_word
    {-# INLINE fetchAndXor #-}
    fetchAndNand = fetch_and_nand_word
    {-# INLINE fetchAndNand #-}
    addAndFetch = add_and_fetch_word
    {-# INLINE addAndFetch #-}
    subAndFetch = sub_and_fetch_word
    {-# INLINE subAndFetch #-}
    orAndFetch = or_and_fetch_word
    {-# INLINE orAndFetch #-}
    andAndFetch = and_and_fetch_word
    {-# INLINE andAndFetch #-}
    xorAndFetch = xor_and_fetch_word
    {-# INLINE xorAndFetch #-}
    nandAndFetch = nand_and_fetch_word
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool = bool_compare_and_swap_word
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap = val_compare_and_swap_word
    {-# INLINE compareAndSwap #-}
    lockTestAndSet = lock_test_and_set_word
    {-# INLINE lockTestAndSet #-}
    lockRelease = lock_release_word
    {-# INLINE lockRelease #-}


instance AtomicBits Int where
    fetchAndAdd p v = fmap fromIntegral $ fetch_and_add_word 
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE fetchAndAdd #-}
    fetchAndSub p v = fmap fromIntegral $ fetch_and_sub_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE fetchAndSub #-}
    fetchAndOr p v = fmap fromIntegral $ fetch_and_or_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE fetchAndOr #-}
    fetchAndAnd p v = fmap fromIntegral $ fetch_and_and_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE fetchAndAnd #-}
    fetchAndXor p v = fmap fromIntegral $ fetch_and_xor_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE fetchAndXor #-}
    fetchAndNand p v = fmap fromIntegral $ fetch_and_nand_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE fetchAndNand #-}
    addAndFetch p v = fmap fromIntegral $ add_and_fetch_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE addAndFetch #-}
    subAndFetch p v = fmap fromIntegral $ sub_and_fetch_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE subAndFetch #-}
    orAndFetch p v = fmap fromIntegral $ or_and_fetch_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE orAndFetch #-}
    andAndFetch p v = fmap fromIntegral $ and_and_fetch_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE andAndFetch #-}
    xorAndFetch p v = fmap fromIntegral $ xor_and_fetch_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE xorAndFetch #-}
    nandAndFetch p v = fmap fromIntegral $ nand_and_fetch_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral v)
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool p o n = bool_compare_and_swap_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap p o n = fmap fromIntegral $ val_compare_and_swap_word
                                        (castPtr p :: Ptr Word)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwap #-}
    lockTestAndSet p = fmap fromIntegral $ lock_test_and_set_word 
                                        (castPtr p :: Ptr Word)
    {-# INLINE lockTestAndSet #-}
    lockRelease p = lock_release_word (castPtr p :: Ptr Word)
    {-# INLINE lockRelease #-}

instance AtomicBits Int8 where
    fetchAndAdd p v = fmap fromIntegral $ fetch_and_add_8 
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE fetchAndAdd #-}
    fetchAndSub p v = fmap fromIntegral $ fetch_and_sub_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE fetchAndSub #-}
    fetchAndOr p v = fmap fromIntegral $ fetch_and_or_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE fetchAndOr #-}
    fetchAndAnd p v = fmap fromIntegral $ fetch_and_and_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE fetchAndAnd #-}
    fetchAndXor p v = fmap fromIntegral $ fetch_and_xor_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE fetchAndXor #-}
    fetchAndNand p v = fmap fromIntegral $ fetch_and_nand_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE fetchAndNand #-}
    addAndFetch p v = fmap fromIntegral $ add_and_fetch_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE addAndFetch #-}
    subAndFetch p v = fmap fromIntegral $ sub_and_fetch_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE subAndFetch #-}
    orAndFetch p v = fmap fromIntegral $ or_and_fetch_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE orAndFetch #-}
    andAndFetch p v = fmap fromIntegral $ and_and_fetch_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE andAndFetch #-}
    xorAndFetch p v = fmap fromIntegral $ xor_and_fetch_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE xorAndFetch #-}
    nandAndFetch p v = fmap fromIntegral $ nand_and_fetch_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral v)
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool p o n = bool_compare_and_swap_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap p o n = fmap fromIntegral $ val_compare_and_swap_8
                                        (castPtr p :: Ptr Word8)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwap #-}
    lockTestAndSet p = fmap fromIntegral $ lock_test_and_set_8 
                                        (castPtr p :: Ptr Word8)
    {-# INLINE lockTestAndSet #-}
    lockRelease p = lock_release_8 (castPtr p :: Ptr Word8)
    {-# INLINE lockRelease #-}

instance AtomicBits Int16 where
    fetchAndAdd p v = fmap fromIntegral $ fetch_and_add_16 
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE fetchAndAdd #-}
    fetchAndSub p v = fmap fromIntegral $ fetch_and_sub_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE fetchAndSub #-}
    fetchAndOr p v = fmap fromIntegral $ fetch_and_or_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE fetchAndOr #-}
    fetchAndAnd p v = fmap fromIntegral $ fetch_and_and_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE fetchAndAnd #-}
    fetchAndXor p v = fmap fromIntegral $ fetch_and_xor_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE fetchAndXor #-}
    fetchAndNand p v = fmap fromIntegral $ fetch_and_nand_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE fetchAndNand #-}
    addAndFetch p v = fmap fromIntegral $ add_and_fetch_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE addAndFetch #-}
    subAndFetch p v = fmap fromIntegral $ sub_and_fetch_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE subAndFetch #-}
    orAndFetch p v = fmap fromIntegral $ or_and_fetch_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE orAndFetch #-}
    andAndFetch p v = fmap fromIntegral $ and_and_fetch_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE andAndFetch #-}
    xorAndFetch p v = fmap fromIntegral $ xor_and_fetch_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE xorAndFetch #-}
    nandAndFetch p v = fmap fromIntegral $ nand_and_fetch_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral v)
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool p o n = bool_compare_and_swap_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap p o n = fmap fromIntegral $ val_compare_and_swap_16
                                        (castPtr p :: Ptr Word16)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwap #-}
    lockTestAndSet p = fmap fromIntegral $ lock_test_and_set_16 
                                        (castPtr p :: Ptr Word16)
    {-# INLINE lockTestAndSet #-}
    lockRelease p = lock_release_16 (castPtr p :: Ptr Word16)
    {-# INLINE lockRelease #-}

instance AtomicBits Int32 where
    fetchAndAdd p v = fmap fromIntegral $ fetch_and_add_32 
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE fetchAndAdd #-}
    fetchAndSub p v = fmap fromIntegral $ fetch_and_sub_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE fetchAndSub #-}
    fetchAndOr p v = fmap fromIntegral $ fetch_and_or_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE fetchAndOr #-}
    fetchAndAnd p v = fmap fromIntegral $ fetch_and_and_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE fetchAndAnd #-}
    fetchAndXor p v = fmap fromIntegral $ fetch_and_xor_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE fetchAndXor #-}
    fetchAndNand p v = fmap fromIntegral $ fetch_and_nand_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE fetchAndNand #-}
    addAndFetch p v = fmap fromIntegral $ add_and_fetch_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE addAndFetch #-}
    subAndFetch p v = fmap fromIntegral $ sub_and_fetch_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE subAndFetch #-}
    orAndFetch p v = fmap fromIntegral $ or_and_fetch_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE orAndFetch #-}
    andAndFetch p v = fmap fromIntegral $ and_and_fetch_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE andAndFetch #-}
    xorAndFetch p v = fmap fromIntegral $ xor_and_fetch_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE xorAndFetch #-}
    nandAndFetch p v = fmap fromIntegral $ nand_and_fetch_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral v)
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool p o n = bool_compare_and_swap_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap p o n = fmap fromIntegral $ val_compare_and_swap_32
                                        (castPtr p :: Ptr Word32)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwap #-}
    lockTestAndSet p = fmap fromIntegral $ lock_test_and_set_32 
                                        (castPtr p :: Ptr Word32)
    {-# INLINE lockTestAndSet #-}
    lockRelease p = lock_release_32 (castPtr p :: Ptr Word32)
    {-# INLINE lockRelease #-}

instance AtomicBits Int64 where
    fetchAndAdd p v = fmap fromIntegral $ fetch_and_add_64 
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE fetchAndAdd #-}
    fetchAndSub p v = fmap fromIntegral $ fetch_and_sub_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE fetchAndSub #-}
    fetchAndOr p v = fmap fromIntegral $ fetch_and_or_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE fetchAndOr #-}
    fetchAndAnd p v = fmap fromIntegral $ fetch_and_and_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE fetchAndAnd #-}
    fetchAndXor p v = fmap fromIntegral $ fetch_and_xor_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE fetchAndXor #-}
    fetchAndNand p v = fmap fromIntegral $ fetch_and_nand_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE fetchAndNand #-}
    addAndFetch p v = fmap fromIntegral $ add_and_fetch_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE addAndFetch #-}
    subAndFetch p v = fmap fromIntegral $ sub_and_fetch_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE subAndFetch #-}
    orAndFetch p v = fmap fromIntegral $ or_and_fetch_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE orAndFetch #-}
    andAndFetch p v = fmap fromIntegral $ and_and_fetch_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE andAndFetch #-}
    xorAndFetch p v = fmap fromIntegral $ xor_and_fetch_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE xorAndFetch #-}
    nandAndFetch p v = fmap fromIntegral $ nand_and_fetch_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral v)
    {-# INLINE nandAndFetch #-}
    compareAndSwapBool p o n = bool_compare_and_swap_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwapBool #-}
    compareAndSwap p o n = fmap fromIntegral $ val_compare_and_swap_64
                                        (castPtr p :: Ptr Word64)
                                        (fromIntegral o)
                                        (fromIntegral n)
    {-# INLINE compareAndSwap #-}
    lockTestAndSet p = fmap fromIntegral $ lock_test_and_set_64 
                                        (castPtr p :: Ptr Word64)
    {-# INLINE lockTestAndSet #-}
    lockRelease p = lock_release_64 (castPtr p :: Ptr Word64)
    {-# INLINE lockRelease #-}
