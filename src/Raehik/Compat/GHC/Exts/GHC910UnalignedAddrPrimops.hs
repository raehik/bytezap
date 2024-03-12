{-
(index|read|write)<ty>OffAddr# primops fail when unaligned on platforms not
supporting unaligned accesses. GHC 9.10 introduces new primops that handle
cases where platforms need aligned accesses. This module imitates that for
older GHCs, but without the safety. So we still fail when unaligned, but it's
easier to upgrade when GHC 9.10 is out.

Note that GC-managed addresses already have these primops. This is for 'Addr#',
non-GC-managed.

Import this module unqualified along with 'GHC.Exts' .
-}

{-# LANGUAGE CPP #-}

module Raehik.Compat.GHC.Exts.GHC910UnalignedAddrPrimops where

#if MIN_VERSION_base(4,20,0)
-- These should be in base-4.20.0.0.
#else

import GHC.Exts

indexWord8OffAddrAsWord16# :: Addr# -> Int# -> Word16#
indexWord8OffAddrAsWord16# base# os# =
    indexWord16OffAddr# (base# `plusAddr#` os#) 0#
{-
indexWord8OffAddrAsWord32# :: Addr# -> Int# -> Word32#
indexWord8OffAddrAsWord64# :: Addr# -> Int# -> Word64#
indexWord8OffAddrAsWord#   :: Addr# -> Int# -> Word#
indexWord8OffAddrAsInt16#  :: Addr# -> Int# ->  Int16#
indexWord8OffAddrAsInt32#  :: Addr# -> Int# ->  Int32#
indexWord8OffAddrAsInt64#  :: Addr# -> Int# ->  Int64#
indexWord8OffAddrAsInt#    :: Addr# -> Int# ->  Int#
-}

writeWord8OffAddrAsWord16# :: Addr# -> Int# -> Word16# -> State# d -> State# d
writeWord8OffAddrAsWord16# base# os# i# s# =
    writeWord16OffAddr# (base# `plusAddr#` os#) 0# i# s#

writeWord8OffAddrAsWord32# :: Addr# -> Int# -> Word32# -> State# d -> State# d
writeWord8OffAddrAsWord32# base# os# i# s# =
    writeWord32OffAddr# (base# `plusAddr#` os#) 0# i# s#

writeWord8OffAddrAsWord64# :: Addr# -> Int# -> Word64# -> State# d -> State# d
writeWord8OffAddrAsWord64# base# os# i# s# =
    writeWord64OffAddr# (base# `plusAddr#` os#) 0# i# s#

writeWord8OffAddrAsInt16#  :: Addr# -> Int# ->  Int16# -> State# d -> State# d
writeWord8OffAddrAsInt16#  base# os# i# s# =
    writeInt16OffAddr#  (base# `plusAddr#` os#) 0# i# s#

writeWord8OffAddrAsInt32#  :: Addr# -> Int# ->  Int32# -> State# d -> State# d
writeWord8OffAddrAsInt32#  base# os# i# s# =
    writeInt32OffAddr#  (base# `plusAddr#` os#) 0# i# s#

writeWord8OffAddrAsInt64#  :: Addr# -> Int# ->  Int64# -> State# d -> State# d
writeWord8OffAddrAsInt64#  base# os# i# s# =
    writeInt64OffAddr#  (base# `plusAddr#` os#) 0# i# s#

writeWord8OffAddrAsInt#    :: Addr# -> Int# ->  Int#   -> State# d -> State# d
writeWord8OffAddrAsInt#    base# os# i# s# =
    writeIntOffAddr#    (base# `plusAddr#` os#) 0# i# s#

writeWord8OffAddrAsWord#   :: Addr# -> Int# ->  Word#  -> State# d -> State# d
writeWord8OffAddrAsWord#   base# os# i# s# =
    writeWordOffAddr#   (base# `plusAddr#` os#) 0# i# s#
#endif
