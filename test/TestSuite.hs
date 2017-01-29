{-# LANGUAGE DataKinds, TypeOperators, NoImplicitPrelude, TemplateHaskell, ScopedTypeVariables, RecordWildCards, BinaryLiterals #-}
import Control.Exception (catch, evaluate)
import System.IO.Unsafe

import CLaSH.Prelude
import qualified Prelude as P
import Test.Hspec
import RiscV.RV32I
import RiscV.Encode.RV32I

import Pipeline
import Program

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

firstCycleDef :: Default a => Signal a -> Signal a
firstCycleDef = mealy step False
    where
    step False _ = (True, def)
    step True  x = (True, x)

system :: Vec (2 ^ 10) (BitVector 32) -> Signal ToDataMem
system program = toDataMem
    where
    --The instruction memory
    instr_0 = firstCycleDef $ romPow2 program (resize . instructionAddress <$> toInstructionMem) 
    --The data memory
    memReadData_3' = firstCycleDef $ readNew (blockRamPow2 (repeat 0 :: Vec (2 ^ 10) (BitVector 32)))
        ((resize . readAddress)  <$> toDataMem) --read address
        (mux ((/=0) . writeStrobe <$> toDataMem) (Just <$> bundle ((resize . writeAddress) <$> toDataMem, writeData <$> toDataMem)) (pure Nothing))
        
    --The processor
    (toInstructionMem, toDataMem, _) = pipeline (FromInstructionMem <$> instr_0 <*> pure False) (FromDataMem <$> memReadData_3')

predX :: (ToDataMem -> Bool) -> ToDataMem -> Bool
predX f x = unsafePerformIO $ catch (f <$> evaluate x) (\(x :: XException) -> return True)

main :: IO ()
main = hspec $ do
    describe "Pipeline" $ do
        it "computes fibonacci correctly" $ do
            let result = sampleN_lazy 2000 $ system $ $(listToVecTH (P.map encodeInstr recursiveFib)) ++ repeat 0
                passed = any (predX (\ToDataMem{..} -> writeAddress == 63 && writeData == 21 && writeStrobe == 0b1111)) result
            passed `shouldBe` True

