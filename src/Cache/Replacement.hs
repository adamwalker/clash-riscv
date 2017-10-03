{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, NoImplicitPrelude, GADTs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Cache.Replacement where

import CLaSH.Prelude

import Cache.PseudoLRUTree

type ReplacementFunc indexBits ways 
    =  Signal (BitVector indexBits) --Index being looked up in the current cycle
    -> Signal Bool                  --Hit in next cycle
    -> Signal (Index ways)          --Way that was hit (if any) in the next cycle
    -> Signal (Index ways)          --Way to replace in the current cycle

randomReplacement :: ReplacementFunc indexBits 2
randomReplacement _ _ _ = bitCoerce <$> toReplace
    where
    toReplace = register False $ not <$> toReplace

pseudoLRUReplacement 
    :: forall indexBits numWaysLog. (KnownNat indexBits, KnownNat numWaysLog, 1 <= numWaysLog) 
    => ReplacementFunc indexBits (2 ^ numWaysLog)
pseudoLRUReplacement index valid way = bitCoerce . getOldestWay <$> readResult
    where
    readResult :: Signal (Vec ((2 ^ numWaysLog) - 1) Bool)
    readResult = readNew (blockRamPow2 (repeat (repeat False))) (unpack <$> index) write

    lastIdx    = register 0 index

    write      :: Signal (Maybe (Unsigned indexBits, Vec ((2 ^ numWaysLog) - 1) Bool))
    write      = mux valid (func <$> index <*> readResult <*> way) (pure Nothing)
        where 
        func index readResult way = Just (unpack index, updateWay (bitCoerce way) readResult)

