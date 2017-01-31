module System where

import CLaSH.Prelude
import Pipeline
import qualified Prelude as P
import RiscV.RV32I
import RiscV.Encode.RV32I
import Prog

system :: Vec (2 ^ 10) (BitVector 32) -> Signal ToDataMem
system program = toDataMem
    where
    --The instruction memory
    instr_0 = romPow2 program (resize . instructionAddress <$> toInstructionMem) 
    --The data memory
    memReadData_3' = readNew (blockRamPow2 (repeat 0 :: Vec (2 ^ 10) (BitVector 32)))
        ((resize . readAddress)  <$> toDataMem) --read address
        (mux ((/=0) . writeStrobe <$> toDataMem) (Just <$> bundle ((resize . writeAddress) <$> toDataMem, writeData <$> toDataMem)) (pure Nothing))
        
    --The processor
    (toInstructionMem, toDataMem, _) = pipeline (FromInstructionMem <$> instr_0 <*> pure False) (FromDataMem <$> memReadData_3')

{-# ANN topEntity
  (defTop
    { t_name     = "riscvPipeline"
    , t_outputs  = ["readAddress", "writeAddress", "writeData", "writeStrobe"]
    }) #-}

topEntity = system ($(listToVecTH (P.map encodeInstr fib)) ++ repeat 0)
