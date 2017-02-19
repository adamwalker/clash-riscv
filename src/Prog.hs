module Prog where

import Prelude
import RiscV.RV32I

fib :: [Instr]
fib = [
        --Fibbonnacci
        --Initialize the first 2 fibbonnacci numbers - in X1 and X2
        RIInstr     $ IInstr ADDI (Word12 0) X0 X1,
        RIInstr     $ IInstr ADDI (Word12 1) X0 X2,
        --Initialize the counter
        RIInstr     $ IInstr ADDI (Word12 11) X0 X3,
        --Do the addition
        RRInstr     $ RInstr ADD  X1 X2 X4,
        --Shift the 2 fibs
        RIInstr     $ IInstr ADDI (Word12 0) X2 X1,
        RIInstr     $ IInstr ADDI (Word12 0) X4 X2,
        --Decrement the counter
        RIInstr     $ IInstr ADDI (Word12 (-1)) X3 X3,
        --Branch 
        BranchInstr $ Branch (Word12 (-8)) BNE X3 X0,
        --Write the output in a loop
        MemoryInstr $ STORE  Word (Word12 0xff) X2 X0,
        JumpInstr   $ JAL    (Word20 (-2)) X0
    ]

