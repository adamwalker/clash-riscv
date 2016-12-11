{-# LANGUAGE DataKinds #-}
module ALU where

import CLaSH.Prelude

import Data.Bool

data PrimaryOp 
    = ADDSUB
    | SLT
    | SLTU
    | AND
    | OR
    | XOR
    | SLL
    | SR
    deriving (Show)

type SecondaryOp = Bool

alu :: PrimaryOp -> SecondaryOp -> BitVector 32 -> BitVector 32 -> BitVector 32
--TODO: more efficient
alu ADDSUB sop x y = bool (x + y) (x - y) sop
--TODO: can this be more efficient?
alu SLT    _   x y = bool 0 1 ((unpack x :: Signed 32)   < (unpack y :: Signed 32))
alu SLTU   _   x y = bool 0 1 ((unpack x :: Unsigned 32) < (unpack y :: Unsigned 32))
alu AND    _   x y = x .&. y
alu OR     _   x y = x .|. y
alu XOR    _   x y = x `xor` y
--alu SLL  x y = shiftL x (unpack $ slice d4 d0 y)
--alu SR   x y = shiftR x (unpack $ slice d4 d0 y)

