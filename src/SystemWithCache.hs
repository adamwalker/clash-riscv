module System where

import CLaSH.Prelude
import qualified Prelude as P
import RiscV.RV32I
import RiscV.Encode.RV32I
import Core.Pipeline
import Prog
import Cache.ICache

systemWithCache :: Vec (2 ^ 10) (BitVector 32) -> Signal Bool -> Signal ToDataMem
systemWithCache program instrStall = toDataMem
    where
    lines :: Vec (2 ^ 6) (Vec 16 (BitVector 32))
    lines = unconcatI program

    --The instruction memory
    fromMem :: Signal (Vec 16 (BitVector 32))
    fromMem = romPow2 lines ((unpack . resize) <$> memAddr)

    --The instruction cache
    (instrReady, instrData, memReq, memAddr) = iCache (SNat @ 14) (SNat @ 12) (pure True) ((pack . instructionAddress) <$> toInstructionMem) (pure True) fromMem

    --The data memory
    memReadData_3' = readNew (blockRamPow2 (repeat 0 :: Vec (2 ^ 10) (BitVector 32)))
        ((resize . readAddress)  <$> toDataMem) --read address
        (mux ((/=0) . writeStrobe <$> toDataMem) (Just <$> bundle ((resize . writeAddress) <$> toDataMem, writeData <$> toDataMem)) (pure Nothing))
        
    --The processor
    (toInstructionMem, toDataMem, _) = pipeline (FromInstructionMem <$> instrData <*> (not <$> instrReady)) (FromDataMem <$> memReadData_3')

{-# ANN topEntity
  (defTop
    { t_name     = "riscvPipeline"
    , t_outputs  = ["readAddress", "writeAddress", "writeData", "writeStrobe"]
    }) #-}

topEntity = systemWithCache ($(listToVecTH (P.map encodeInstr fib)) ++ repeat 0)
