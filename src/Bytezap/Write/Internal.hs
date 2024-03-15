module Bytezap.Write.Internal where

import Bytezap.Poke qualified as P

-- | A 'Poke' with the associated size it pokes.
data Write s = Write { size :: Int, poke :: P.Poke s }

-- | Sequence the 'Poke's, sum the sizes.
instance Semigroup (Write s) where
    -- TODO feels like this might be INLINE[1] or even INLINE[0]?
    Write ll lp <> Write rl rp = Write (ll + rl) (lp <> rp)

-- | The empty 'Write' is the empty 'Poke', which writes zero bytes.
instance Monoid (Write s) where
    mempty = Write 0 mempty
