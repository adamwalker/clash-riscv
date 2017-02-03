{-# LANGUAGE DataKinds, TypeOperators, NoImplicitPrelude, TemplateHaskell, ScopedTypeVariables, RecordWildCards, BinaryLiterals #-}
import Control.Exception (catch, evaluate)
import System.IO.Unsafe

import CLaSH.Prelude
import qualified Prelude as P
import Test.Hspec
import Test.QuickCheck hiding (resize, (.&.))
import RiscV.RV32I
import RiscV.Encode.RV32I
import Data.Bool

import Pipeline
import Program

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

firstCycleDef :: Default a => Signal a -> Signal a
firstCycleDef = mealy step False
    where
    step False _ = (True, def)
    step True  x = (True, x)

system :: Vec (2 ^ 10) (BitVector 32) -> Signal Bool -> Signal ToDataMem
system program instrStall = toDataMem
    where
    --The instruction memory
    instr_0 = firstCycleDef $ romPow2 program (resize . instructionAddress <$> toInstructionMem) 
    --The data memory
    memReadData_3' = firstCycleDef $ readNew (blockRamPow2 (repeat 0 :: Vec (2 ^ 10) (BitVector 32)))
        ((resize . readAddress)  <$> toDataMem) --read address
        (mux ((/=0) . writeStrobe <$> toDataMem) (Just <$> bundle ((resize . writeAddress) <$> toDataMem, writeData <$> toDataMem)) (pure Nothing))
        
    --The processor
    (toInstructionMem, toDataMem, _) = pipeline (FromInstructionMem <$> mux instrStall 0 instr_0 <*> instrStall) (FromDataMem <$> memReadData_3')

runTest :: Vec (2 ^ 10) (BitVector 32) -> Int -> (ToDataMem -> Bool) -> IO ()
runTest instrs cycles pred = do
    let result = sampleN_lazy cycles $ system instrs (pure False)
        passed = any (predX pred) result
    passed `shouldBe` True
    where
    predX :: (ToDataMem -> Bool) -> ToDataMem -> Bool
    predX f x = unsafePerformIO $ catch (f <$> evaluate x) (\(x :: XException) -> return False)

runTestStalls :: Vec (2 ^ 10) (BitVector 32) -> Int -> (ToDataMem -> Bool) -> Property
runTestStalls instrs cycles pred = forAll (vectorOf (10 * cycles) arbitrary) $ \instrStall -> P.length (P.filter id instrStall) > 10 ==>
    let result = sampleN_lazy cycles $ system instrs (fromList instrStall)
        passed = any (predX pred) result
    in passed `shouldBe` True
    where
    predX :: (ToDataMem -> Bool) -> ToDataMem -> Bool
    predX f x = unsafePerformIO $ catch (f <$> evaluate x) (\(x :: XException) -> return False)

outputs :: BitVector 32 -> ToDataMem -> Bool
outputs x ToDataMem{..} = writeAddress == 63 && writeData == x && writeStrobe == 0b1111

testRType :: ROpcode -> (Signed 32 -> Signed 32 -> Signed 32) -> Property
testRType op func = 
    property $ \(x :: Signed 12) (y :: Signed 12) -> 
        runTestStalls
            (map (fromIntegral . encodeInstr) (rType (Word12 (fromIntegral y)) (Word12 (fromIntegral x)) op) ++ repeat 0) 
            100 
            (outputs (fromIntegral ((resize x :: Signed 32) `func` resize y)))

testIType :: IOpcode -> (Signed 32 -> Signed 32 -> Signed 32) -> Property
testIType op func = 
    property $ \(x :: Signed 12) (y :: Signed 12) -> 
        runTestStalls
            (map (fromIntegral . encodeInstr) (iType (Word12 (fromIntegral x)) (Word12 (fromIntegral y)) op) ++ repeat 0) 
            100 
            (outputs (fromIntegral ((resize x :: Signed 32) `func` resize y)))

main :: IO ()
main = hspec $ do

    describe "Unit tests" $ do
        describe "Pipeline" $ do

            it "store" $ 
                runTestStalls ($(listToVecTH (P.map encodeInstr store)) ++ repeat 0) 100 (outputs 0x12348688) 

            it "lui" $
                runTestStalls ($(listToVecTH (P.map encodeInstr lui)) ++ repeat 0) 100 (outputs 0x12345000)
            it "auipc" $
                runTestStalls ($(listToVecTH (P.map encodeInstr auipc)) ++ repeat 0) 100 (outputs 0x12345004)

            describe "rtype" $ do
                it "add"  $ testRType ADD  (+)
                it "slt"  $ testRType SLT  (\x y -> bool 0 1 (x < y))
                it "sltu" $ testRType SLTU (\x y -> bool 0 1 (pack x < pack y))
                it "and"  $ testRType AND  (.&.)
                it "or"   $ testRType OR   (.|.)
                it "xor"  $ testRType XOR  xor
                it "sll"  $ testRType SLL  (\x y -> shiftL x (fromIntegral (slice d4 d0 $ pack y)))
                it "srl"  $ testRType SRL  (\x y -> unpack $ shiftR (pack x) (fromIntegral (slice d4 d0 $ pack y)))
                it "sub"  $ testRType SUB  (-)
                it "sra"  $ testRType SRA  (\x y -> shiftR x (fromIntegral (slice d4 d0 $ pack y)))

            describe "itype" $ do
                it "addi"  $ testIType ADDI  (+)
                it "slti"  $ testIType SLTI  (\x y -> bool 0 1 (x < y))
                it "sltiu" $ testIType SLTIU (\x y -> bool 0 1 (pack x < pack y))
                it "xori"  $ testIType XORI  xor
                it "ori"   $ testIType ORI   (.|.)
                it "andi"  $ testIType ANDI  (.&.)
            
            describe "jal" $ do
                it "jumps to right place" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr jal)) ++ repeat 0) 100 (outputs 1)
                it "puts currect PC in register" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr jal2)) ++ repeat 0) 100 (outputs 8)

            describe "jalr" $ do
                it "jumps to right place" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr jalr)) ++ repeat 0) 100 (outputs 1)
                it "puts currect PC in register" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr jalr2)) ++ repeat 0) 100 (outputs 8)

            describe "branch" $ do
                describe "beq" $ do
                    it "branches" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BEQ)) ++ repeat 0) 100 (outputs 0)
                    it "does not branch" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1235) BEQ)) ++ repeat 0) 100 (outputs 1)

                describe "bne" $ do
                    it "branches" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1235) BNE)) ++ repeat 0) 100 (outputs 0)
                    it "does not branch" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BNE)) ++ repeat 0) 100 (outputs 1)

                describe "blt" $ do
                    it "branches" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 1234) BLT)) ++ repeat 0) 100 (outputs 0)
                    it "does not branch" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BLT)) ++ repeat 0) 100 (outputs 1)
                    it "branches" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 0xf00) BLT)) ++ repeat 0) 100 (outputs 0)

                describe "bge" $ do
                    it "branches" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BGE)) ++ repeat 0) 100 (outputs 0)
                    it "does not branch" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 1234) BGE)) ++ repeat 0) 100 (outputs 1)
                    it "branches" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 0xf00) (Word12 1234) BGE)) ++ repeat 0) 100 (outputs 0)

                describe "bltu" $ do
                    it "branches" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 1234) BLTU)) ++ repeat 0) 100 (outputs 0)
                    it "does not branch" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BLTU)) ++ repeat 0) 100 (outputs 1)
                    it "does not branch" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 0xf00) BLTU)) ++ repeat 0) 100 (outputs 1)

                describe "bgeu" $ do
                    it "branches" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BGEU)) ++ repeat 0) 100 (outputs 0)
                    it "does not branch" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 1234) BGEU)) ++ repeat 0) 100 (outputs 1)
                    it "does not branch" $ 
                        runTestStalls ($(listToVecTH (P.map encodeInstr $ branch (Word12 0xf00) (Word12 1234) BGEU)) ++ repeat 0) 100 (outputs 1)

            describe "stalls" $ do
                it "source 1" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr stall)) ++ repeat 0) 100 (outputs 0x12345678)
                it "source 2" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr stall2)) ++ repeat 0) 100 (outputs 0x12345678)

            describe "Forwarding" $ do
                it "forwards alu to alu" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr aluForward)) ++ repeat 0) 100 (outputs 3)
                it "forwards alu to alu" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr aluForward2)) ++ repeat 0) 100 (outputs 3)
                it "forwards alu to alu" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr aluForward3)) ++ repeat 0) 100 (outputs 3)
                it "forwards mem to alu" $ 
                    runTestStalls ($(listToVecTH (P.map encodeInstr memALUForward)) ++ repeat 0) 100 (outputs 0x12348690)
                it "forwards mem to alu" $ 
                    runTestStalls ($(listToVecTH (P.map encodeInstr memALUForward2)) ++ repeat 0) 100 (outputs 0x12348690)
                it "forwards mem to mem" $ 
                    runTestStalls ($(listToVecTH (P.map encodeInstr memMemForward)) ++ repeat 0) 100 (outputs 0x12345678)

            describe "Loads" $ do
                it "load word" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr loadWord)) ++ repeat 0) 100 (outputs 0x12348688)
                it "load half" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr loadHalf)) ++ repeat 0) 100 (outputs 0xffff8688)
                it "load half upper" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr loadHalfUpper)) ++ repeat 0) 100 (outputs 0x1234)
                it "load half unsigned" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr loadHalfUnsigned)) ++ repeat 0) 100 (outputs 0x8688)
                it "load byte" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr loadByte)) ++ repeat 0) 100 (outputs 0xffffff88)
                it "load byte upper" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr loadByteUpper)) ++ repeat 0) 100 (outputs 0x12)
                it "load byte unsigned" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr loadByteUnsigned)) ++ repeat 0) 100 (outputs 0x88)

    describe "Integration tests" $ do
        describe "Pipeline" $ do

            it "computes recursive fibonacci correctly" $
                runTestStalls ($(listToVecTH (P.map encodeInstr recursiveFib)) ++ repeat 0) 2000 (outputs 21)
            it "computes loop fibonacci correctly" $ 
                runTestStalls ($(listToVecTH (P.map encodeInstr fib)) ++ repeat 0)          1000 (outputs 144)
            it "computes unrolled fibonacci correctly" $ 
                runTestStalls ($(listToVecTH (P.map encodeInstr fibUnrolled)) ++ repeat 0)  1000 (outputs 89)

