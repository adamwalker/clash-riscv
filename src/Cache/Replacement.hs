{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Cache.Replacement where

import CLaSH.Prelude

type ReplacementFunc indexBits ways = Signal (BitVector indexBits) -> Signal Bool -> Signal (Index ways) -> Signal (Index ways)

randomReplacement :: ReplacementFunc indexBits 2
randomReplacement _ _ _ = bitCoerce <$> toReplace
    where
    toReplace = register False $ not <$> toReplace

