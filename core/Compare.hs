{-# LANGUAGE DataKinds #-}
module Compare where

import CLaSH.Prelude

import Data.Bool

branchCompare :: BitVector 3 -> BitVector 32 -> BitVector 32 -> Bool
branchCompare opNeg x y 
    = bool id complement (unpack negate) $ case op of
        0 -> x == y
        2 -> s x < s y
        3 -> u x < u y
        _ -> errorX "branchCompare"
    where
    negate =  slice d0 d0 opNeg
    op     =  slice d2 d1 opNeg
    u      :: BitVector 32 -> Unsigned 32
    u      =  unpack
    s      :: BitVector 32 -> Signed 32
    s      =  unpack

