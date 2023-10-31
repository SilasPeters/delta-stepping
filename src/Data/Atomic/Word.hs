{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Atomic.Word
  where

import GHC.Base
import GHC.Exts


-- Compare-and-swap a (machine word sized and aligned) unsigned integer value.
-- Returns the old value read. Implies a full memory barrier.
--
atomicCASWordPtr :: Ptr Word -> Word -> Word -> IO Word
atomicCASWordPtr (Ptr addr#) (W# expected#) (W# desired#) = IO $ \s0 ->
  case atomicCasWordAddr# addr# expected# desired# s0 of { (# s1, old# #) ->
    (# s1, W# old# #)
  }

-- Atomically ADD the value to the element at the address. Returns the value of
-- the element before the operation. Implies a full memory barrier.
--
atomicAddWordPtr :: Ptr Word -> Word -> IO Word
atomicAddWordPtr (Ptr addr#) (W# value#) = IO $ \s0 ->
  case fetchAddWordAddr# addr# value# s0 of { (# s1, old# #) ->
    (# s1, W# old# #)
  }

-- Atomically bitwise-AND the value to the element at the given address. Returns
-- the value of the element before the operation. Implies a full memory barrier.
--
atomicAndWordPtr :: Ptr Word -> Word -> IO Word
atomicAndWordPtr (Ptr addr#) (W# value#) = IO $ \s0 ->
  case fetchAndWordAddr# addr# value# s0 of { (# s1, old# #) ->
    (# s1, W# old# #)
  }

-- Atomically bitwise-OR the value to the element at the given address. Returns
-- the value of the element before the operation. Implies a full memory barrier.
--
atomicOrWordPtr :: Ptr Word -> Word -> IO Word
atomicOrWordPtr (Ptr addr#) (W# value#) = IO $ \s0 ->
  case fetchOrWordAddr# addr# value# s0 of { (# s1, old# #) ->
    (# s1, W# old# #)
  }

