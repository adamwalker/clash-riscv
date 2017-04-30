{-# LANGUAGE DataKinds, TypeOperators, NoImplicitPrelude, TemplateHaskell, ScopedTypeVariables, RecordWildCards, BinaryLiterals, TypeApplications #-}
import Control.Exception (catch, evaluate)
import System.IO.Unsafe

import CLaSH.Prelude
import qualified Prelude as P
import Test.Hspec
import Test.QuickCheck hiding (resize, (.&.), (.&&.), (.||.), sample)
import qualified Test.QuickCheck as QC
import RiscV.RV32I
import RiscV.Encode.RV32I
import Data.Bool

import Core.Pipeline
import Cache.ICache

import Program
import CacheTest
import TestUtils

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

{-
 - Systems
 - Take a program, a signal to simulate instructions not being available (e.g, because of a cache miss) 
 - Returns a Signal of data memory accesses which is used to verify correct behaviour of the system
-}

type System = Vec (2 ^ 10) (BitVector 32) -> Signal Bool -> Signal ToDataMem

--Pipeline + instruction memory + data memory. No caches.
system :: System
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

--Pipeline + instruction cache + instruction memory + data memory. No data cache.
systemWithCache :: System
systemWithCache program instrStall = toDataMem
    where
    lines :: Vec (2 ^ 6) (Vec 16 (BitVector 32))
    lines = unconcatI program

    --The instruction memory
    fromMem :: Signal (Vec 16 (BitVector 32))
    fromMem = firstCycleDef $ romPow2 lines ((unpack . resize) <$> memAddr)

    --The instruction cache
    (instrReady, instrData, memReq, memAddr) = 
        iCache 
            (SNat @ 14) 
            (SNat @ 12) 
            (pure True) 
            ((pack . instructionAddress) <$> toInstructionMem) 
            (not <$> instrStall) 
            (mux instrStall (pure $ repeat 0) fromMem)

    --The data memory
    memReadData_3' = firstCycleDef $ readNew (blockRamPow2 (repeat 0 :: Vec (2 ^ 10) (BitVector 32)))
        ((resize . readAddress)  <$> toDataMem) --read address
        (mux ((/=0) . writeStrobe <$> toDataMem) (Just <$> bundle ((resize . writeAddress) <$> toDataMem, writeData <$> toDataMem)) (pure Nothing))
        
    --The processor
    (toInstructionMem, toDataMem, _) = pipeline (FromInstructionMem <$> instrData <*> (not <$> (firstCycleDef' False instrReady))) (FromDataMem <$> memReadData_3')

{-
 - Test runners
 - Takes the contents of instruction memory, the number of cycles to run for, and a predicate on the outgoing memory interface
 - Succeeds if the predicate holds in at least one cycle
 -}

predX :: (ToDataMem -> Bool) -> ToDataMem -> Bool
predX f x = unsafePerformIO $ catch (f <$> evaluate x) (\(x :: XException) -> return False)

type TestRunner = Vec (2 ^ 10) (BitVector 32) -> Int -> (ToDataMem -> Bool) -> Property

--Basic cacheless system
runTest :: TestRunner
runTest instrs cycles pred = property $ do
    let result = sampleN_lazy cycles $ system instrs (pure False)
    any (predX pred) result `shouldBe` True

--System with instruction cache
runTestCache :: TestRunner
runTestCache instrs cycles pred = forAll arbitrary $ \instrStall -> do
    let result = sampleN_lazy cycles $ systemWithCache instrs instrStall
    any (predX pred) result `shouldBe` True

--System without instruction cache, but emulates cache stalls
runTestStalls :: TestRunner
runTestStalls instrs cycles pred = forAll (vectorOf (5 * cycles) arbitrary) $ \instrStall -> 
    P.length (P.filter not instrStall) > cycles ==>
            let result = sampleN_lazy cycles $ system instrs (fromList instrStall)
            in  any (predX pred) result `shouldBe` True

--Is x outputted at least once to address 63 as a full word
outputs :: BitVector 32 -> ToDataMem -> Bool
outputs x ToDataMem{..} = writeAddress == 63 && writeData == x && writeStrobe == 0b1111

{-
 - For unit testing single ALU instructions in isolation
 -}

--Test a single R type instruction
testRType :: TestRunner -> ROpcode -> (Signed 32 -> Signed 32 -> Signed 32) -> Property
testRType runner op func = 
    property $ \(x :: Signed 12) (y :: Signed 12) -> 
        runner
            (map (fromIntegral . encodeInstr) (rType (Word12 (fromIntegral y)) (Word12 (fromIntegral x)) op) ++ repeat 0) 
            100 
            (outputs (fromIntegral ((resize x :: Signed 32) `func` resize y)))

--Test a single I type instruction
testIType :: TestRunner -> IOpcode -> (Signed 32 -> Signed 32 -> Signed 32) -> Property
testIType runner op func = 
    property $ \(x :: Signed 12) (y :: Signed 12) -> 
        runner
            (map (fromIntegral . encodeInstr) (iType (Word12 (fromIntegral x)) (Word12 (fromIntegral y)) op) ++ repeat 0) 
            100 
            (outputs (fromIntegral ((resize x :: Signed 32) `func` resize y)))

--Generates an infinite list of addresses where each address has a 50% chance of being one of the last historySize addresses
withPreviousAccesses :: forall a. (Arbitrary a, Num a) => Int -> Gen [a]
withPreviousAccesses historySize = withPreviousAccesses' [0]
    where
    withPreviousAccesses' :: [a] -> Gen [a]
    withPreviousAccesses' hist = do
        newAddress <- frequency [(1, pure False), (1, pure True)]
        let th     =  P.take historySize hist
        res        <- bool (elements th) arbitrary newAddress
        rest       <- withPreviousAccesses' $ res : th
        return $ res : rest

--Generates an infinite list of addresses. Attempts to test cache corner cases better than random addresses or simply including some previous adresses.
withRealisticAccesses :: Int -> Gen [BitVector 30]
withRealisticAccesses historySize = withRealisticAccesses' [0]
    where
    withRealisticAccesses' :: [BitVector 30] -> Gen [BitVector 30]
    withRealisticAccesses' hist = do

        let th        =  P.take historySize hist
        randomAddress <- frequency [(4, pure False), (1, pure True)]

        res <- case randomAddress of
            --Generate a completely random address
            True  -> arbitrary

            --Randomly pick one of the previous addresses and possibly modify it
            False -> do
                --Pick the prev address to mutate
                prevAddress <- elements th

                mutateAddress <- frequency [(4, pure False), (1, pure True)]

                case mutateAddress of
                    False -> pure prevAddress

                    True  -> do

                        --Randomly choose 3 bits to mutate. 
                        --The idea is that we will generate cases that mutate the
                        --  - line bits only, thus requesting a different word in a line that is already in the cache
                        --  - index bits only, thus requesting a different index
                        --  - tag bits only, hitting the same index as the previous access and thereby forcing the cache to use the other way (if not in use) or evicting a pre-existing entry
                        --Each combination of two of the above
                        --Or, all of them
                        bit1      <- choose (0, 29)
                        bit2      <- choose (0, 29)
                        bit3      <- choose (0, 29)

                        pure $ flip complementBit bit1 $ flip complementBit bit2 $ flip complementBit bit3 prevAddress

        rest <- withRealisticAccesses' $ res : th
        return $ res : rest


main :: IO ()
main = hspec $ do

    describe "Unit tests" $ do

        describe "Instruction cache" $ do
            it "works with random addresses" $
                property $ 
                    forAll (QC.resize 100 arbitrary) $ \addresses -> 
                        forAll arbitrary $ \memValid -> 
                            cacheProp addresses memValid

            it "works with random and previous addresses" $
                property $ 
                    --TODO: figure out how to increase the number of addresses without using up all my RAM
                    forAll (P.take 200 <$> withPreviousAccesses 5) $ \addresses -> 
                        forAll arbitrary $ \memValid -> 
                            cacheProp addresses memValid

            it "tests corner cases" $
                property $ 
                    --TODO: figure out how to increase the number of addresses without using up all my RAM
                    forAll (P.take 200 <$> withRealisticAccesses 5) $ \addresses -> 
                        forAll arbitrary $ \memValid -> 
                            cacheProp addresses memValid

        describe "Pipeline" $ do

            let unitTests runner = do

                    it "store" $ 
                        runner ($(listToVecTH (P.map encodeInstr store)) ++ repeat 0) 100 (outputs 0x12348688) 

                    it "lui" $
                        runner ($(listToVecTH (P.map encodeInstr lui)) ++ repeat 0) 100 (outputs 0x12345000)
                    it "auipc" $
                        runner ($(listToVecTH (P.map encodeInstr auipc)) ++ repeat 0) 100 (outputs 0x12345004)

                    describe "rtype" $ do
                        it "add"  $ testRType runner ADD  (+)
                        it "slt"  $ testRType runner SLT  (\x y -> bool 0 1 (x < y))
                        it "sltu" $ testRType runner SLTU (\x y -> bool 0 1 (pack x < pack y))
                        it "and"  $ testRType runner AND  (.&.)
                        it "or"   $ testRType runner OR   (.|.)
                        it "xor"  $ testRType runner XOR  xor
                        it "sll"  $ testRType runner SLL  (\x y -> shiftL x (fromIntegral (slice d4 d0 $ pack y)))
                        it "srl"  $ testRType runner SRL  (\x y -> unpack $ shiftR (pack x) (fromIntegral (slice d4 d0 $ pack y)))
                        it "sub"  $ testRType runner SUB  (-)
                        it "sra"  $ testRType runner SRA  (\x y -> shiftR x (fromIntegral (slice d4 d0 $ pack y)))

                    describe "itype" $ do
                        it "addi"  $ testIType runner ADDI  (+)
                        it "slti"  $ testIType runner SLTI  (\x y -> bool 0 1 (x < y))
                        it "sltiu" $ testIType runner SLTIU (\x y -> bool 0 1 (pack x < pack y))
                        it "xori"  $ testIType runner XORI  xor
                        it "ori"   $ testIType runner ORI   (.|.)
                        it "andi"  $ testIType runner ANDI  (.&.)
                    
                    describe "jal" $ do
                        it "jumps to right place" $
                            runner ($(listToVecTH (P.map encodeInstr jal)) ++ repeat 0) 100 (outputs 1)
                        it "puts currect PC in register" $
                            runner ($(listToVecTH (P.map encodeInstr jal2)) ++ repeat 0) 100 (outputs 8)

                    describe "jalr" $ do
                        it "jumps to right place" $
                            runner ($(listToVecTH (P.map encodeInstr jalr)) ++ repeat 0) 100 (outputs 1)
                        it "puts currect PC in register" $
                            runner ($(listToVecTH (P.map encodeInstr jalr2)) ++ repeat 0) 100 (outputs 8)

                    describe "branch" $ do
                        describe "beq" $ do
                            it "branches" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BEQ)) ++ repeat 0) 100 (outputs 0)
                            it "does not branch" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1235) BEQ)) ++ repeat 0) 100 (outputs 1)

                        describe "bne" $ do
                            it "branches" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1235) BNE)) ++ repeat 0) 100 (outputs 0)
                            it "does not branch" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BNE)) ++ repeat 0) 100 (outputs 1)

                        describe "blt" $ do
                            it "branches" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 1234) BLT)) ++ repeat 0) 100 (outputs 0)
                            it "does not branch" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BLT)) ++ repeat 0) 100 (outputs 1)
                            it "branches" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 0xf00) BLT)) ++ repeat 0) 100 (outputs 0)

                        describe "bge" $ do
                            it "branches" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BGE)) ++ repeat 0) 100 (outputs 0)
                            it "does not branch" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 1234) BGE)) ++ repeat 0) 100 (outputs 1)
                            it "branches" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 0xf00) (Word12 1234) BGE)) ++ repeat 0) 100 (outputs 0)

                        describe "bltu" $ do
                            it "branches" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 1234) BLTU)) ++ repeat 0) 100 (outputs 0)
                            it "does not branch" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BLTU)) ++ repeat 0) 100 (outputs 1)
                            it "does not branch" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 0xf00) BLTU)) ++ repeat 0) 100 (outputs 1)

                        describe "bgeu" $ do
                            it "branches" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1234) (Word12 1234) BGEU)) ++ repeat 0) 100 (outputs 0)
                            it "does not branch" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 1235) (Word12 1234) BGEU)) ++ repeat 0) 100 (outputs 1)
                            it "does not branch" $ 
                                runner ($(listToVecTH (P.map encodeInstr $ branch (Word12 0xf00) (Word12 1234) BGEU)) ++ repeat 0) 100 (outputs 1)

                    describe "stalls" $ do
                        it "source 1" $
                            runner ($(listToVecTH (P.map encodeInstr stall)) ++ repeat 0) 100 (outputs 0x12345678)
                        it "source 2" $
                            runner ($(listToVecTH (P.map encodeInstr stall2)) ++ repeat 0) 100 (outputs 0x12345678)

                    describe "Forwarding" $ do
                        it "forwards alu to alu" $
                            runner ($(listToVecTH (P.map encodeInstr aluForward)) ++ repeat 0) 100 (outputs 3)
                        it "forwards alu to alu" $
                            runner ($(listToVecTH (P.map encodeInstr aluForward2)) ++ repeat 0) 100 (outputs 3)
                        it "forwards alu to alu" $
                            runner ($(listToVecTH (P.map encodeInstr aluForward3)) ++ repeat 0) 100 (outputs 3)
                        it "forwards mem to alu" $ 
                            runner ($(listToVecTH (P.map encodeInstr memALUForward)) ++ repeat 0) 100 (outputs 0x12348690)
                        it "forwards mem to alu" $ 
                            runner ($(listToVecTH (P.map encodeInstr memALUForward2)) ++ repeat 0) 100 (outputs 0x12348690)
                        it "forwards mem to mem" $ 
                            runner ($(listToVecTH (P.map encodeInstr memMemForward)) ++ repeat 0) 100 (outputs 0x12345678)

                    describe "Loads" $ do
                        it "load word" $
                            runner ($(listToVecTH (P.map encodeInstr loadWord)) ++ repeat 0) 100 (outputs 0x12348688)
                        it "load half" $
                            runner ($(listToVecTH (P.map encodeInstr loadHalf)) ++ repeat 0) 100 (outputs 0xffff8688)
                        it "load half upper" $
                            runner ($(listToVecTH (P.map encodeInstr loadHalfUpper)) ++ repeat 0) 100 (outputs 0x1234)
                        it "load half unsigned" $
                            runner ($(listToVecTH (P.map encodeInstr loadHalfUnsigned)) ++ repeat 0) 100 (outputs 0x8688)
                        it "load byte" $
                            runner ($(listToVecTH (P.map encodeInstr loadByte)) ++ repeat 0) 100 (outputs 0xffffff88)
                        it "load byte upper" $
                            runner ($(listToVecTH (P.map encodeInstr loadByteUpper)) ++ repeat 0) 100 (outputs 0x12)
                        it "load byte unsigned" $
                            runner ($(listToVecTH (P.map encodeInstr loadByteUnsigned)) ++ repeat 0) 100 (outputs 0x88)

            describe "with stalls" $
                unitTests runTestStalls

            describe "with cache" $
                unitTests runTestCache

    describe "Integration tests" $ do
        describe "Pipeline" $ do

            describe "with stalls" $ do
                it "computes recursive fibonacci correctly" $
                    runTestStalls ($(listToVecTH (P.map encodeInstr recursiveFib)) ++ repeat 0) 2000 (outputs 21)
                it "computes loop fibonacci correctly" $ 
                    runTestStalls ($(listToVecTH (P.map encodeInstr fib)) ++ repeat 0)          1000 (outputs 144)
                it "computes unrolled fibonacci correctly" $ 
                    runTestStalls ($(listToVecTH (P.map encodeInstr fibUnrolled)) ++ repeat 0)  1000 (outputs 89)

            describe "with cache" $ do
                --TODO: fails
                --it "computes recursive fibonacci correctly" $
                --    runTestCache ($(listToVecTH (P.map encodeInstr recursiveFib)) ++ repeat 0) 2000 (outputs 21)
                it "computes loop fibonacci correctly" $ 
                    runTestCache ($(listToVecTH (P.map encodeInstr fib)) ++ repeat 0)          1000 (outputs 144)
                it "computes unrolled fibonacci correctly" $ 
                    runTestCache ($(listToVecTH (P.map encodeInstr fibUnrolled)) ++ repeat 0)  1000 (outputs 89)

