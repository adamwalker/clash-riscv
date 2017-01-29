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

runTest :: Vec (2 ^ 10) (BitVector 32) -> Int -> (ToDataMem -> Bool) -> IO ()
runTest instrs cycles pred = do
    let result = sampleN_lazy cycles $ system instrs 
        passed = any (predX pred) result
    passed `shouldBe` True
    where
    predX :: (ToDataMem -> Bool) -> ToDataMem -> Bool
    predX f x = unsafePerformIO $ catch (f <$> evaluate x) (\(x :: XException) -> return True)

outputs :: BitVector 32 -> ToDataMem -> Bool
outputs x ToDataMem{..} = writeAddress == 63 && writeData == x && writeStrobe == 0b1111

main :: IO ()
main = hspec $ do

    describe "Unit tests" $ do
        describe "Pipeline" $ do

            it "lui" $
                runTest ($(listToVecTH (P.map encodeInstr lui)) ++ repeat 0) 100 (outputs 0x12345000)
            it "auipc" $
                runTest ($(listToVecTH (P.map encodeInstr auipc)) ++ repeat 0) 100 (outputs 0x12345004)

            it "stalls" $ 
                runTest ($(listToVecTH (P.map encodeInstr stall)) ++ repeat 0) 100 (outputs 0x12345678)

            describe "Forwarding" $ do
                it "forwards alu to alu" $
                    runTest ($(listToVecTH (P.map encodeInstr aluForward)) ++ repeat 0) 100 (outputs 3)
                it "forwards mem to alu" $ 
                    runTest ($(listToVecTH (P.map encodeInstr memALUForward)) ++ repeat 0) 100 (outputs 0x12345678)
                it "forwards mem to mem" $ 
                    runTest ($(listToVecTH (P.map encodeInstr memMemForward)) ++ repeat 0) 100 (outputs 0x12345678)

            describe "Loads" $ do
                it "load word" $
                    runTest ($(listToVecTH (P.map encodeInstr loadWord)) ++ repeat 0) 100 (outputs 0x12348688)
                it "load half" $
                    runTest ($(listToVecTH (P.map encodeInstr loadHalf)) ++ repeat 0) 100 (outputs 0xffff8688)
                it "load half upper" $
                    runTest ($(listToVecTH (P.map encodeInstr loadHalfUpper)) ++ repeat 0) 100 (outputs 0x1234)
                it "load half unsigned" $
                    runTest ($(listToVecTH (P.map encodeInstr loadHalfUnsigned)) ++ repeat 0) 100 (outputs 0x8688)
                it "load byte" $
                    runTest ($(listToVecTH (P.map encodeInstr loadByte)) ++ repeat 0) 100 (outputs 0xffffff88)
                it "load byte upper" $
                    runTest ($(listToVecTH (P.map encodeInstr loadByteUpper)) ++ repeat 0) 100 (outputs 0x12)
                it "load byte unsigned" $
                    runTest ($(listToVecTH (P.map encodeInstr loadByteUnsigned)) ++ repeat 0) 100 (outputs 0x88)

    describe "Integration tests" $ do
        describe "Pipeline" $ do

            it "computes recursive fibonacci correctly" $
                runTest ($(listToVecTH (P.map encodeInstr recursiveFib)) ++ repeat 0) 2000 (outputs 21)
            it "computes loop fibonacci correctly" $ 
                runTest ($(listToVecTH (P.map encodeInstr fib)) ++ repeat 0)          1000 (outputs 144)
            it "computs unrolled fibonacci correctly" $ 
                runTest ($(listToVecTH (P.map encodeInstr fibUnrolled)) ++ repeat 0)  1000 (outputs 89)

