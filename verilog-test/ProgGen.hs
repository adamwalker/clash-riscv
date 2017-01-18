import Data.Word
import RiscV.RV32I
import RiscV.Encode.RV32I

program :: [Instr]
program = [
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
        MemoryInstr $ STORE  Word (Word12 (-12)) X6 X0
    ]

assembled :: [Word32]
assembled = map encodeInstr program

main = print assembled
