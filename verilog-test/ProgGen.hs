import Data.Word
import RiscV.RV32I
import RiscV.Encode.RV32I

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
        BranchInstr $ Branch (Word12 (-6)) BNE X3 X0,
        --Write the output
        MemoryInstr $ STORE  Word (Word12 (-12)) X6 X0
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
        MemoryInstr $ STORE  Word (Word12 (-12)) X4 X0,
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

assembled :: [Word32]
assembled = map encodeInstr recursiveFib

main = print assembled
