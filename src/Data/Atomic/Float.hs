{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UnliftedFFITypes     #-}
{-# OPTIONS_GHC -fobject-code #-}

module Data.Atomic.Float
  where

import GHC.Base
import GHC.Exts
import GHC.Word


-- Compare-and-swap a (32-bit sized and aligned) floating point value. Returns
-- the old value read. Implies a full memory barrier.
--
atomicCASFloatPtr :: Ptr Float -> Float -> Float -> IO Float
atomicCASFloatPtr ptr expected desired =
  -- Have a look in the module Data.Atomic.Word for inspiration.
  -- atomicCasWord32Addr#
  undefined


-- Read the (32-bit sized and aligned) floating point value @old@ from the given
-- address, compute the minimum of @old@ and the given value, and store the
-- result back to the same memory address. These operations are performed
-- atomically. Implies a full memory barrier. Lock-free progress guarantee.
--
atomicMinFloatPtr :: Ptr Float -> Float -> IO ()
atomicMinFloatPtr ptr val =
  undefined


{-# INLINE floatToWord32 #-}
floatToWord32 :: Float -> Word32
floatToWord32 (F# f32#) = W32# (stgFloatToWord32# f32#)

{-# INLINE word32ToFloat #-}
word32ToFloat :: Word32 -> Float
word32ToFloat (W32# w32#) = F# (stgWord32ToFloat# w32#)

-- Stolen from GHC.Float
--
-- Performs a bitcast / reinterpret_cast from a floating-point value to an
-- unsigned integral value and vice versa.
--
foreign import prim "stg_floatToWord32zh"
    stgFloatToWord32# :: Float# -> Word32#

foreign import prim "stg_word32ToFloatzh"
    stgWord32ToFloat# :: Word32# -> Float#

