{-# LANGUAGE UnboxedTuples #-}

{- | Low-level bytestring builder using continuation parsing.

bytezap's builder is highly performant. However, one thing it can't do is
/fail/. We have no way to flag an error. If you force it, you will either

* write an initial assert, followed by an unsafe builder that relies on it, or
* build a builder as we assert, then execute it once we're ready

The former is inefficient in situations where the check scales similarly with
the build (e.g. both must iterate over the input). And the latter is very silly
since your builder will be allocating all over.

A naive failable builder might use @'Either' e 'Int'@ to flag errors. After
executing, we check the result: if 'Right', we resize to the given actual
length; if 'Left', we discard the buffer with the given error. This is fine...
but it's an extra allocation, and limits us to 'Either'. A shame.

Instead, we design a builder that takes a finalizer continuation @'Int#' ->
'ByteString'@, which is passed the final offset. The builder calls this as it
finishes, wrapping it as needed (or leaving as 'ByteString' for a non-failable
builder). The runner is expected to pass a continuation to perform any buffer
reallocation necessary (if the actual length was less than the max length), and
return a 'ByteString', possibly wrapped in e.g. 'Right'.

This is much harder to use than the regular builder, and they can't be combined
(the regular builder permits sequencing, which this can't support). But it fills
a gap!

Unlike the regular builder we stick with 'IO', because the continuations get
weird otherwise.
-}

module Bytezap.PokeCPS where

import GHC.Exts ( Int#, Addr#, Ptr )

import Data.Text.Internal ( Text(Text) )
import Data.Text.Array qualified as Text
import Data.Word ( Word8 )
import Data.ByteString ( ByteString )
import Data.Primitive.ByteArray ( ByteArray(ByteArray), indexByteArray )
import GHC.Storable ( writeWord8OffPtr )
import Tmp.BSExt qualified as B

type PokeCPS# r = Addr# -> Int# -> (Int# -> IO ByteString) -> IO r

-- | 'PokeCPS#' newtype wrapper.
--
-- Does not permit a 'Semigroup' instance because pokes do not return offset
-- information.
newtype PokeCPS r = PokeCPS { unPokeCPS:: PokeCPS# r }

emptyPokeCPS :: PokeCPS ByteString
emptyPokeCPS = PokeCPS $ \_base# os# finalize -> finalize os#

{- Any unexplained stuff is probably parsing parts of hex bytestrings
Like @xx AA 1a2F@.
-}

{- 2024-08-27 raehik
The best algorithm would probably operate on words (let's assume 8 bytes).
It would involve a shitload of inlining and praying that GHC figures out how to
turn it all into efficient JMPs.
-}

{-
Parse hex bytestring.

Expects that the output buffer can fit the maximum length of the input.

This is a bit overly parametric in the hopes of using it with manual buffering
(e.g. allocate a single buffer and reuse it for every parse). But that's more
complex: we need to carefully set @bufInMax@ so that it also corresponds to the
output buffer as well. But now, we also need to track the input buffer
position... yeah, it's a mess.

Wait, we _are_ tracking input buffer position. We're just not passing it to the
continuation. More reworking, sigh...
-}
full
    :: ByteArray -> Ptr Word8
    -> (Int -> IO r) -> (Word8 -> IO r)
    -> (Word8 -> IO r)
    -> Int -> Int -> Int
    -> IO r
full bufIn bufOut fCont fErrEof fErrNotHexDigit bufInMax = go
  where
    -- each loop writes a single byte or fails
    go = \bufInOs bufOutOs -> do
        let bufInRemaining = bufInMax - bufInOs
        if   bufInRemaining >= 2
        then case indexByteArray @Word8 bufIn bufInOs of
               0x20 -> -- next byte is space
                case indexByteArray @Word8 bufIn (bufInOs+1) of
                  0x20 -> -- and the byte after that: skip both
                    go (bufInOs+2) bufOutOs
                  d1   ->
                    -- next byte is space, then non-space
                    -- could just skip one, but we've already asserted 2 bytes
                    -- so let's copy-paste for just 1 more byte
                    if   bufInRemaining >= 3
                    then do let d0 = indexByteArray @Word8 bufIn (bufInOs+2)
                            withHexNibbles fErrNotHexDigit d1 d0 $ \b -> do
                                writeWord8OffPtr bufOut bufOutOs b
                                go (bufInOs+3) (bufOutOs+1)
                    else fErrEof d1
               d1   -> do
                let d0 = indexByteArray @Word8 bufIn (bufInOs+1)
                withHexNibbles fErrNotHexDigit d1 d0 $ \b -> do
                    writeWord8OffPtr bufOut bufOutOs b
                    go (bufInOs+2) (bufOutOs+1)
        else if   bufInRemaining == 0
             then fCont bufOutOs
             else -- 1 byte remaining
                  case indexByteArray @Word8 bufIn bufInOs of
                    0x20 -> fCont bufOutOs
                    d1   -> fErrEof d1
{-# INLINE full #-}

withHexNibbles ::
    (Word8 -> r) -> Word8 -> Word8 -> (Word8 -> r) -> r
withHexNibbles fFail d1 d0 fCont =
    withByteAsHexDigit d1 fFail $ \n1 ->
        withByteAsHexDigit d0 fFail $ \n0 ->
            fCont $ 0x10*n1 + n0
{-# INLINE withHexNibbles #-}

withByteAsHexDigit :: Word8 -> (Word8 -> r) -> (Word8 -> r) -> r
withByteAsHexDigit c fFail f
  | dec  <= 9 = f dec
  | hexl <= 5 = f $ hexl + 10
  | hexu <= 5 = f $ hexu + 10
  | otherwise = fFail c
  where
    dec   = c - ord_0
    hexl  = c - ord_a
    hexu  = c - ord_A
    ord_0 = 0x30
    ord_a = 0x61
    ord_A = 0x41
{-# INLINE withByteAsHexDigit #-}

textToByteStringUptoIO :: Text -> IO (Either String ByteString)
textToByteStringUptoIO = \(Text (Text.ByteArray tarr) tos tlen) ->
    B.createCPS (tlen `quot` 2) finalizer $ \finalize buf ->
        full (ByteArray tarr) buf finalize fErrEof fErrNotHexDigit tlen tos 0
  where
    fErrNotHexDigit = \b -> pure $ Left ("not a hexadecimal digit: " <> show b)
    finalizer = \fp len -> Right <$> B.mkDeferredByteString fp len
    fErrEof = \_ -> pure $ Left "ended during byte (TODO)"
{-# INLINE textToByteStringUptoIO #-}
