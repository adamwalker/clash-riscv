{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Cache.Replacement where

import CLaSH.Prelude

type ReplacementFunc indexBits ways 
    =  Signal (BitVector indexBits) --Index being looked up in the current cycle
    -> Signal Bool                  --Hit in next cycle
    -> Signal (Index ways)          --Way that was hit (if any) in the next cycle
    -> Signal (Index ways)          --Way to replace in the current cycle

randomReplacement :: ReplacementFunc indexBits 2
randomReplacement _ _ _ = bitCoerce <$> toReplace
    where
    toReplace = register False $ not <$> toReplace

