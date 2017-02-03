{-# LANGUAGE DataKinds #-}
module Program where

import Data.Word

import qualified CLaSH.Prelude as P
import CLaSH.Prelude (Vec((:>), Nil))

import RiscV.RV32I
import RiscV.Encode.RV32I

{-# ANN module ("HLint: ignore Use ++" :: String) #-}

store :: [Instr]
store = [
        RIInstr     $ LUI    (Word20 0x12348) X1,
        RIInstr     $ IInstr ADDI (Word12 0x688) X1 X1,
        MemoryInstr $ STORE  Word (Word12 0xff) X1 X0
    ]

rType :: Word12 -> Word12 -> ROpcode -> Vec 4 Instr
rType x y op 
    =  RIInstr     (IInstr ADDI x X0 X1)
    :> RIInstr     (IInstr ADDI y X0 X2)
    :> RRInstr     (RInstr op X1 X2 X3)
    :> MemoryInstr (STORE  Word (Word12 0xff) X3 X0)
    :> Nil

iType :: Word12 -> Word12 -> IOpcode -> Vec 3 Instr
iType x y op 
    =  RIInstr     (IInstr ADDI x X0 X1)
    :> RIInstr     (IInstr op   y X1 X2)
    :> MemoryInstr (STORE  Word (Word12 0xff) X2 X0)
    :> Nil

branch :: Word12 -> Word12 -> BranchCond -> [Instr]
branch x y cond = [
        RIInstr     $ IInstr ADDI x X0 X1,
        RIInstr     $ IInstr ADDI y X0 X2,
        BranchInstr $ Branch (Word12 4) cond X1 X2,
        RIInstr     $ IInstr ADDI (Word12 1) X0 X3,
        MemoryInstr $ STORE  Word (Word12 0xff) X3 X0
    ]

jalr :: [Instr]
jalr = [
        RIInstr     $ IInstr ADDI (Word12 5) X0 X4,
        JumpInstr   $ JALR   (Word12 11) X4 X3,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        MemoryInstr $ STORE  Word (Word12 0xff) X1 X0
    ]

jalr2 :: [Instr]
jalr2 = [
        RIInstr     $ IInstr ADDI (Word12 5) X0 X4,
        JumpInstr   $ JALR   (Word12 11) X4 X3,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        MemoryInstr $ STORE  Word (Word12 0xff) X3 X0
    ]

jal :: [Instr]
jal = [
        JumpInstr   $ JAL (Word20 6) X3,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        MemoryInstr $ STORE  Word (Word12 0xff) X1 X0
    ]

jal2 :: [Instr]
jal2 = [
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        JumpInstr   $ JAL (Word20 2) X3,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        RIInstr     $ IInstr ADDI (Word12 1) X1 X1,
        MemoryInstr $ STORE  Word (Word12 0xff) X3 X0
    ]

loadSetup :: [Instr]
loadSetup = [
        RIInstr     $ LUI    (Word20 0x12348) X1,
        RIInstr     $ IInstr ADDI (Word12 0x688) X1 X1,
        MemoryInstr $ STORE  Word (Word12 4) X1 X0
    ]

loadWord :: [Instr]
loadWord = concat [
        loadSetup,
        [
            MemoryInstr $ LOAD  (Width Word) (Word12 4) X0 X2,
            --Output
            MemoryInstr $ STORE  Word (Word12 0xff) X2 X0
        ]
    ]

loadHalf :: [Instr]
loadHalf = concat [
        loadSetup,
        [
            MemoryInstr $ LOAD (Width Half) (Word12 4) X0 X2,
            --Output
            MemoryInstr $ STORE Word (Word12 0xff) X2 X0
        ]
    ]

loadHalfUpper :: [Instr]
loadHalfUpper = concat [
        loadSetup,
        [
            MemoryInstr $ LOAD (Width Half) (Word12 6) X0 X2,
            --Output
            MemoryInstr $ STORE Word (Word12 0xff) X2 X0
        ]
    ]

loadHalfUnsigned :: [Instr]
loadHalfUnsigned = concat [
        loadSetup,
        [
            MemoryInstr $ LOAD HalfUnsigned (Word12 4) X0 X2,
            --Output
            MemoryInstr $ STORE Word (Word12 0xff) X2 X0
        ]
    ]

loadByte :: [Instr]
loadByte = concat [
        loadSetup,
        [
            MemoryInstr $ LOAD (Width Byte) (Word12 4) X0 X2,
            --Output
            MemoryInstr $ STORE Word (Word12 0xff) X2 X0
        ]
    ]

loadByteUpper :: [Instr]
loadByteUpper = concat [
        loadSetup,
        [
            MemoryInstr $ LOAD (Width Byte) (Word12 7) X0 X2,
            --Output
            MemoryInstr $ STORE Word (Word12 0xff) X2 X0
        ]
    ]

loadByteUnsigned :: [Instr]
loadByteUnsigned = concat [
        loadSetup,
        [
            MemoryInstr $ LOAD ByteUnsigned (Word12 4) X0 X2,
            --Output
            MemoryInstr $ STORE Word (Word12 0xff) X2 X0
        ]
    ]

stall :: [Instr]
stall = [
        RIInstr     $ LUI    (Word20 0x12345) X1,
        RIInstr     $ IInstr ADDI (Word12 0x670) X1 X1,
        MemoryInstr $ STORE  Word (Word12 0) X1 X0,
        --Load followed by ALU op
        RIInstr     $ IInstr ADDI (Word12 8) X0 X4,
        MemoryInstr $ LOAD   (Width Word) (Word12 0) X0 X2,
        RRInstr     $ RInstr ADD X4 X2 X3,
        --Output
        MemoryInstr $ STORE  Word (Word12 0xff) X3 X0
    ]

stall2 :: [Instr]
stall2 = [
        RIInstr     $ LUI    (Word20 0x12345) X1,
        RIInstr     $ IInstr ADDI (Word12 0x670) X1 X1,
        MemoryInstr $ STORE  Word (Word12 0) X1 X0,
        --Load followed by ALU op
        RIInstr     $ IInstr ADDI (Word12 8) X0 X4,
        MemoryInstr $ LOAD   (Width Word) (Word12 0) X0 X2,
        RRInstr     $ RInstr ADD X2 X4 X3,
        --Output
        MemoryInstr $ STORE  Word (Word12 0xff) X3 X0
    ]

aluForward :: [Instr]
aluForward = [
        RIInstr     $ IInstr ADDI (Word12 1) X0 X1,
        RIInstr     $ IInstr ADDI (Word12 2) X1 X1,
        MemoryInstr $ STORE  Word (Word12 0xff) X1 X0
    ]

aluForward2 :: [Instr]
aluForward2 = [
        RIInstr     $ IInstr ADDI (Word12 1) X0 X1,
        RIInstr     $ IInstr ADDI (Word12 2) X0 X2,
        RRInstr     $ RInstr ADD  X1 X2 X1,
        MemoryInstr $ STORE  Word (Word12 0xff) X1 X0
    ]

aluForward3 :: [Instr]
aluForward3 = [
        RIInstr     $ IInstr ADDI (Word12 1) X0 X1,
        RIInstr     $ IInstr ADDI (Word12 2) X0 X2,
        RRInstr     $ RInstr ADD  X2 X1 X1,
        MemoryInstr $ STORE  Word (Word12 0xff) X1 X0
    ]

memALUForward :: [Instr]
memALUForward = concat [
        loadSetup,
        [
            --Load followed by nop then ALU op
            MemoryInstr $ LOAD   (Width Word) (Word12 4) X0 X2,
            RIInstr     $ IInstr ADDI (Word12 0) X0 X0,
            RIInstr     $ IInstr ADDI (Word12 8) X2 X3,
            --Output
            MemoryInstr $ STORE  Word (Word12 0xff) X3 X0
        ]
    ]

memALUForward2 :: [Instr]
memALUForward2 = concat [
        loadSetup,
        [
            --Load followed by nop then ALU op
            RIInstr     $ IInstr ADDI (Word12 8) X0 X1,
            MemoryInstr $ LOAD   (Width Word) (Word12 4) X0 X2,
            RIInstr     $ IInstr ADDI (Word12 0) X0 X0,
            RRInstr     $ RInstr ADD  X1 X2 X3,
            --Output
            MemoryInstr $ STORE  Word (Word12 0xff) X3 X0
        ]
    ]

memMemForward :: [Instr]
memMemForward = [
        RIInstr     $ LUI    (Word20 0x12345) X1,
        RIInstr     $ IInstr ADDI (Word12 0x678) X1 X1,
        MemoryInstr $ STORE  Word (Word12 0) X1 X0,
        --Load followed by store
        MemoryInstr $ LOAD   (Width Word) (Word12 0) X0 X2,
        MemoryInstr $ STORE  Word (Word12 1) X2 X0,
        --Output
        MemoryInstr $ LOAD   (Width Word) (Word12 1) X0 X3,
        MemoryInstr $ STORE  Word (Word12 0xff) X3 X0
    ]

lui :: [Instr]
lui = [
        RIInstr     $ LUI   (Word20 0x12345) X1,
        MemoryInstr $ STORE Word (Word12 0xff) X1 X0
    ]

auipc :: [Instr]
auipc = [
        RIInstr     $ IInstr ADDI (Word12 0) X0 X0, --NOP
        RIInstr     $ AUIPC  (Word20 0x12345) X1,
        MemoryInstr $ STORE  Word (Word12 0xff) X1 X0
    ]

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

fibUnrolled :: [Instr]
fibUnrolled = [
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
        BranchInstr $ Branch (Word12 (-6)) BNE X3 X0,
        --Write the output in a loop
        MemoryInstr $ STORE  Word (Word12 0xff) X6 X0,
        JumpInstr   $ JAL    (Word20 (-2)) X0
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
        JumpInstr   $ JAL    (Word20 6) X3,

        --Write the output in a loop
        MemoryInstr $ STORE  Word (Word12 0xff) X4 X0,
        JumpInstr   $ JAL    (Word20 (-2)) X0,
        
        --The function
        --We need to make a recursive call, so we need to store our local vars in a stack frame
        --Layout:
        -- -Function argument
        -- -Return address
        -- -Single local var (the return value of the first recursive fib call)

        -- The base cases
        -- 0
        -- PC: 4
        BranchInstr $ Branch (Word12 6) BNE X0 X2,
        RIInstr     $ IInstr ADDI (Word12 0) X0 X4,
        JumpInstr   $ JALR   (Word12 0)  X3 X0 ,

        -- 1
        -- PC: 7
        RIInstr     $ IInstr ADDI (Word12 (-1)) X2 X6,
        BranchInstr $ Branch (Word12 6) BNE X0 X6,
        RIInstr     $ IInstr ADDI (Word12 1) X0 X4,
        JumpInstr   $ JALR   (Word12 0)  X3 X0 ,

        -- PC:11
        --Adjust the stack pointer to make space for our local variables
        RIInstr     $ IInstr ADDI (Word12 12) X1 X1,
        --Save the argument and return address
        MemoryInstr $ STORE  Word (Word12 (-12)) X2 X1,
        MemoryInstr $ STORE  Word (Word12 (-8))  X3 X1,
        --Set the argument for the first recursive call
        RIInstr     $ IInstr ADDI (Word12 (-1)) X2 X2,
        --Make the first recursive call
        JumpInstr   $ JAL    (Word20 (-22)) X3,
        --Copy the return value to its place on the stack
        MemoryInstr $ STORE  Word (Word12 (-4)) X4 X1,
        --Restore the argument to this function
        MemoryInstr $ LOAD   (Width Word) (Word12 (-12)) X1 X2,
        --Set the argument for the second recursive call
        RIInstr     $ IInstr ADDI (Word12 (-2)) X2 X2,
        --Make the second recursive call
        JumpInstr   $ JAL    (Word20 (-30)) X3,
        --Load the saved result from the first recursive call
        MemoryInstr $ LOAD   (Width Word) (Word12 (-4)) X1 X5,
        --Add the results of the recursive call and put it in the result register
        RRInstr     $ RInstr ADD X4 X5 X4,
        --Restore the saved return address
        MemoryInstr $ LOAD   (Width Word) (Word12 (-8)) X1 X3,
        --Put the stack pointer back
        RIInstr     $ IInstr ADDI (Word12 (-12)) X1 X1,
        --Return
        JumpInstr   $ JALR   (Word12 0)  X3 X0 

    ]

