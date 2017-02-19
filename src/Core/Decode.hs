{-# LANGUAGE DataKinds, BinaryLiterals, ScopedTypeVariables #-}
module Core.Decode where

import CLaSH.Prelude

import Data.Bool

import Core.ALU

--
--Extract parts of the instruction
--

opcode :: BitVector 32 -> BitVector 7
opcode = slice d6 d0

rOp1 :: BitVector 32 -> BitVector 7
rOp1 = slice d31 d25

rs2 :: BitVector 32 -> BitVector 5
rs2 = slice d24 d20

rs1 :: BitVector 32 -> BitVector 5
rs1 = slice d19 d15

rOp2 :: BitVector 32 -> BitVector 3
rOp2 = slice d14 d12

rd :: BitVector 32 -> BitVector 5
rd = slice d11 d7

--I type immediate
iImm :: BitVector 32 -> BitVector 12
iImm = slice d31 d20

--U type immediate
uImm :: BitVector 32 -> BitVector 20
uImm = slice d31 d12

--UJ type immediate
ujImm :: BitVector 32 -> BitVector 20
ujImm x = slice d31 d31 x ++# slice d19 d12 x ++# slice d20 d20 x ++# slice d30 d21 x

--S type immediate
sImm :: BitVector 32 -> BitVector 12
sImm x = slice d31 d25 x ++# slice d11 d7 x

--SB type immediate - used for branches
sbImm :: BitVector 32 -> BitVector 12
sbImm x = slice d31 d31 x ++# slice d7 d7 x ++# slice d30 d25 x ++# slice d11 d8 x

--
--Opcode predicates
--
rType :: BitVector 32 -> Bool
rType = (== 0b0110011) . opcode

iType :: BitVector 32 -> Bool
iType = (== 0b0010011) . opcode

lui :: BitVector 32 -> Bool
lui = (== 0b0110111) . opcode

jal :: BitVector 32 -> Bool
jal = (== 0b1101111) . opcode

jalr :: BitVector 32 -> Bool
jalr = (== 0b1100111) . opcode

branch :: BitVector 32 -> Bool
branch = (== 0b1100011) . opcode

load :: BitVector 32 -> Bool
load = (== 0b0000011) . opcode

store :: BitVector 32 -> Bool
store = (== 0b0100011) . opcode

auipc :: BitVector 32 -> Bool
auipc = (== 0b0010111) . opcode

sys :: BitVector 32 -> Bool
sys = (== 0b1110011) . opcode

--
--ALU operation decoding
--
decodeAluPrimaryOp :: BitVector 32 -> PrimaryOp
decodeAluPrimaryOp instr 
    | iType instr || rType instr = 
        case rOp2 instr of
            0 -> ADDSUB
            1 -> SLL
            2 -> SLT
            3 -> SLTU
            4 -> XOR
            5 -> SR
            6 -> OR
            7 -> AND
    | auipc instr = ADDSUB
    | otherwise   = ADDSUB --TODO

decodeAluSecondaryOp :: BitVector 32 -> SecondaryOp
decodeAluSecondaryOp instr 
    | rType instr
        = unpack $ slice d30 d30 instr
    | auipc instr
        = False
    | otherwise = False --TODO

--
--Control signal decoding
--

{-
The first operand to the ALU can fome from
  - The first read port of the register file for R and I type instructions
  - The PC for AUIPC
-}
firstOpIsRegister = not . auipc 

{-
The second operand to the ALU can come from 
  - The second read port of the register file for R type instructions
  - The immediate field for I type instructions
-}
secondOpIsRegister instr = rType instr || branch instr

--Does the instruction actually use r1
usesRegister1 instr = rType instr || iType instr || jalr instr || branch instr || store instr || load instr

--Does the instruction actually use r2
usesRegister2 instr = rType instr || branch instr

--A memory write only happens for store instructions
enableMemWrite = store

{-
The instruction results in a register write to rd if the instruction is
  - load
  - lui
  - iType
  - rType
-}
enableRegWrite instr = load instr || iType instr || rType instr || auipc instr || lui instr || jal instr || jalr instr

{-
The source of the register write back is the alu (as opposed to a memory read) if the instruction is
  - iType
  - rType
-}

data DestRegSource 
    = SourceALU
    | SourceMem
    | SourceSpec
    deriving (Show)

decodeDestRegSource instr
    | iType instr || rType instr || auipc instr = SourceALU
    | load  instr                               = SourceMem
    | lui   instr                               = SourceALU
    | jal   instr || jalr  instr                = SourceALU
    | specialReg instr                          = SourceSpec
    | otherwise                                 = SourceALU --TODO

--All immedates in RiscV are sign extended
signExtendImmediate :: forall n. KnownNat n => BitVector n -> BitVector 32
signExtendImmediate x = pack (resize (unpack x :: Signed n))

--Upper immediates
alignUpperImmediate :: BitVector 20 -> BitVector 32
alignUpperImmediate = (++# 0)

--Where does the immediate (always the second alu operand) come from?
extractImmediate :: BitVector 32 -> BitVector 32
extractImmediate instr
    | auipc instr = alignUpperImmediate $ uImm instr
    | load  instr = signExtendImmediate $ iImm instr
    | store instr = signExtendImmediate $ sImm instr
    | iType instr = signExtendImmediate $ iImm instr
    | jalr  instr = signExtendImmediate $ iImm instr
    | otherwise   = 0 --TODO

--
--Load / Store
--
data MemSize
    = Byte
    | HalfWord
    | Word

extractMemSize :: BitVector 32 -> MemSize
extractMemSize x 
    = case slice d13 d12 x of
        0         -> Byte
        1         -> HalfWord
        otherwise -> Word

loadUnsigned :: BitVector 32 -> Bool
loadUnsigned = unpack . slice d14 d14

--
--Branch
--

extractBranchType :: BitVector 32 -> BitVector 3
extractBranchType = slice d14 d12

--
--System instructions
--

specialReg :: BitVector 32 -> Bool
specialReg instr 
    =  slice d31 d28 instr == 0b1100
    && slice d26 d22 instr == 0

data SpecialReg
    = Cycle
    | Time
    | Retired

extractSpecialReg :: BitVector 32 -> BitVector 2
extractSpecialReg = slice d21 d20

decodeSpecialReg :: BitVector 2 -> SpecialReg
decodeSpecialReg 0 = Cycle
decodeSpecialReg 1 = Time
decodeSpecialReg 2 = Retired
decodeSpecialReg _ = Cycle -- TODO

specialRegHigh :: BitVector 32 -> Bool
specialRegHigh  = unpack . slice d27 d27

