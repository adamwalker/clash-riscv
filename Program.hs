{-# LANGUAGE DataKinds #-}
module Program where

import Data.Word

import qualified CLaSH.Prelude as P

import RiscV.RV32I
import RiscV.Encode.RV32I

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
        BranchInstr $ Branch (Word12 (-4)) BNE X3 X0
    ]

fib2 :: [Instr]
fib2 = [
        --Faster fibbonnacci
        --Initialize the first 2 fibbonnacci numbers - in X5 and X6
        RIInstr     $ IInstr ADDI (Word12 0) X0 X5,
        RIInstr     $ IInstr ADDI (Word12 1) X0 X6,
        --Initialize the counter
        RIInstr     $ IInstr ADDI (Word12 5) X0 X3,
        --Do the additions
        RRInstr     $ RInstr ADD  X5 X6 X5,
        RRInstr     $ RInstr ADD  X5 X6 X6,
        --Decrement the counter
        RIInstr     $ IInstr ADDI (Word12 (-1)) X3 X3,
        --Branch 
        BranchInstr $ Branch (Word12 (-3)) BNE X3 X0
    ]

loads :: [Instr]
loads = [
        --Different width loads
        RIInstr     $ LUI   (Word20 0x12345) X25,
        RIInstr     $ IInstr ADDI (Word12 0x678) X25 X25,
        MemoryInstr $ STORE Word (Word12 0) X25 X0,

        MemoryInstr $ LOAD  (Width Word) (Word12 0) X0 X26,
        MemoryInstr $ LOAD  (Width Half) (Word12 0) X0 X27,
        MemoryInstr $ LOAD  (Width Byte) (Word12 0) X0 X28,

        MemoryInstr $ LOAD  HalfUnsigned (Word12 0) X0 X29,
        MemoryInstr $ LOAD  ByteUnsigned (Word12 0) X0 X30,

        MemoryInstr $ LOAD  (Width Half) (Word12 2) X0 X22,
        MemoryInstr $ LOAD  (Width Byte) (Word12 2) X0 X23,

        MemoryInstr $ LOAD  (Width Byte) (Word12 1) X0 X24
    ]

miscInstrs :: [Instr]
miscInstrs = [
        --Other random instructions
        RIInstr     $ LUI   (Word20 0x12345) X11,
        RIInstr     $ AUIPC (Word20 0x12345) X12
    ]

stall :: [Instr]
stall = [
        --Load followed by ALU op
        MemoryInstr $ LOAD  (Width Word) (Word12 16) X0 X15,
        --TODO: add the ability to stall so this NOP isn't needed
        RIInstr     $ IInstr ADDI (Word12 0) X0 X0,
        RIInstr     $ IInstr ADDI (Word12 5) X15 X16
    ]

jal :: [Instr]
jal = [
        --Jumping and linking
        JumpInstr   $ JAL (Word20 2) X18,
        RIInstr     $ IInstr ADDI (Word12 1) X19 X19,
        RIInstr     $ IInstr ADDI (Word12 1) X19 X19,
        RIInstr     $ IInstr ADDI (Word12 1) X19 X19
    ]

jalr :: [Instr]
jalr = [
        --init
        RIInstr     $ IInstr ADDI (Word12 0) X0 X11,
        RIInstr     $ IInstr ADDI (Word12 0) X0 X12,
        RIInstr     $ IInstr ADDI (Word12 0) X0 X13,
        RIInstr     $ IInstr ADDI (Word12 0) X0 X14,

        --This basic block makes the call
        JumpInstr   $ JAL    (Word20 3) X10,
        RIInstr     $ IInstr ADDI (Word12 1) X11 X11,
        RIInstr     $ IInstr ADDI (Word12 1) X12 X12,

        --The subroutine
        RIInstr     $ IInstr ADDI (Word12 1) X13 X13,
        RIInstr     $ IInstr ADDI (Word12 1) X14 X14,
        JumpInstr   $ JALR   (Word12 0)  X10 X0 
    ]

--A Naiive, recursive, exponential time fibbonnacci function
recursiveFib :: [Instr]
recursiveFib = [
        --Calling convention: 
        -- The caller saves all registers it needs. The stack pointer is assumed unclobbered.
        -- -Stack pointer: X1
        -- -Single argument in X2
        -- -Return address in X3
        -- -Return value in X4
        
        --Make the function call
        --Setup the stack pointer. The stack grows upwards.
        RIInstr     $ IInstr ADDI (Word12 0) X0 X1,
        --Load the fib index into register 2. This is the function argument.
        RIInstr     $ IInstr ADDI (Word12 8) X0 X2,
        --Call the function, X3 contains the return address
        JumpInstr   $ JAL    (Word20 2) X3,

        JumpInstr   $ JAL    (Word20 0) X0,
        
        --The function
        --We need to make a recursive call, so we need to store our local vars in a stack frame
        --Layout:
        -- -Function argument
        -- -Return address
        -- -Single local var (the return value of the first recursive fib call)

        -- The base cases
        -- 0
        BranchInstr $ Branch (Word12 3) BNE X0 X2,
        RIInstr     $ IInstr ADDI (Word12 0) X0 X4,
        JumpInstr   $ JALR   (Word12 0)  X3 X0 ,

        -- 1
        RIInstr     $ IInstr ADDI (Word12 (-1)) X2 X6,
        BranchInstr $ Branch (Word12 3) BNE X0 X6,
        RIInstr     $ IInstr ADDI (Word12 1) X0 X4,
        JumpInstr   $ JALR   (Word12 0)  X3 X0 ,

        --Adjust the stack pointer to make space for our local variables
        RIInstr     $ IInstr ADDI (Word12 12) X1 X1,
        --Save the argument and return address
        MemoryInstr $ STORE  Word (Word12 (-12)) X2 X1,
        MemoryInstr $ STORE  Word (Word12 (-8))  X3 X1,
        --Set the argument for the first recursive call
        RIInstr     $ IInstr ADDI (Word12 (-1)) X2 X2,
        --Make the first recursive call
        JumpInstr   $ JAL    (Word20 (-11)) X3,
        --Copy the return value to its place on the stack
        MemoryInstr $ STORE  Word (Word12 (-4)) X4 X1,
        --Restore the argument to this function
        MemoryInstr $ LOAD   (Width Word) (Word12 (-12)) X1 X2,
        RRInstr     $ RInstr ADD X0 X0 X0,
        --Set the argument for the second recursive call
        RIInstr     $ IInstr ADDI (Word12 (-2)) X2 X2,
        --Make the second recursive call
        JumpInstr   $ JAL    (Word20 (-16)) X3,
        --Load the saved result from the first recursive call
        MemoryInstr $ LOAD   (Width Word) (Word12 (-4)) X1 X5,
        RRInstr     $ RInstr ADD X0 X0 X0,
        --Add the results of the recursive call and put it in the result register
        RRInstr     $ RInstr ADD X4 X5 X4,
        --Restore the saved return address
        MemoryInstr $ LOAD   (Width Word) (Word12 (-8)) X1 X3,
        RRInstr     $ RInstr ADD X0 X0 X0,
        --Put the stack pointer back
        RIInstr     $ IInstr ADDI (Word12 (-12)) X1 X1,
        --Return
        JumpInstr   $ JALR   (Word12 0)  X3 X0 

    ]

program :: [Instr]
program = concat [
        --miscInstrs,
        --stall,
        --loads,
        --fib2,
        --jal,
        --jalr
        recursiveFib
    ]

assembled :: [Word32]
assembled = map encodeInstr program

