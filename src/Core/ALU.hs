{-# LANGUAGE DataKinds #-}
module Core.ALU where

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

alu :: PrimaryOp -> SecondaryOp -> BitVector 32 -> BitVector 32 -> (BitVector 32, BitVector 32)
alu op sop x y = (addSub, alu' op sop x y)
    where
    alu' ADDSUB _     x y = addSub
    alu' SLT    _     x y = bool 0 1 ((unpack x :: Signed 32)   < (unpack y :: Signed 32))
    alu' SLTU   _     x y = bool 0 1 ((unpack x :: Unsigned 32) < (unpack y :: Unsigned 32))
    alu' AND    _     x y = x .&. y
    alu' OR     _     x y = x .|. y
    alu' XOR    _     x y = x `xor` y
    alu' SLL    _     x y = shiftL x shiftAmt
    alu' SR     False x y = shiftR x shiftAmt
    alu' SR     True  x y = pack $ shiftR (unpack x :: Signed 32) shiftAmt
    addSub   = bool (x + y) (x - y) sop
    shiftAmt = unpack $ resize $ slice d4 d0 y

