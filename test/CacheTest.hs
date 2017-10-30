{-# LANGUAGE DataKinds, NoImplicitPrelude, BinaryLiterals, TypeApplications #-}

-- | Instruction cache testbench
module CacheTest where

import Clash.Prelude
import qualified  Prelude as P
import Data.Bool

import Cache.ICache
import Cache.Replacement

import TestUtils

--Backing mem for cache. Delivers a line at a time
backingMem 
    :: HasClockReset dom sync gated
    => Signal dom Bool
    -> Signal dom (BitVector 30)
    -> Signal dom Bool
    -> Signal dom (Bool, Vec 16 (BitVector 32))
backingMem req addr memValid = register (False, repeat 0) $ readMemory <$> addr <*> memValid
    where
    readMemory addr memValid = (memValid, bool (repeat 0) (map resize (iterateI (+ 1) (addr .&. complement 0b1111))) memValid)

--Test stimulus generation for instruction cache. Requests a sequence of addresses and checks the correct result is returned.
testCache 
    :: HasClockReset dom sync gated
    => [BitVector 30]
    -> Signal dom Bool
    -> Signal dom (BitVector 32)
    -> Signal dom ((Bool, BitVector 30), (Bool, Bool))
testCache addresses instrValid instr = mealy step addresses $ bundle (instrValid, instr)
    where
    step :: [BitVector 30] -> (Bool, BitVector 32) -> ([BitVector 30], ((Bool, BitVector 30), (Bool, Bool)))
    step state (memReady, memData) = (state', ((memReq, memAddress), (P.null state, success)))
        where
        memReq = True
        memAddress 
            = case state' of
                (x:xs) -> x
                []     -> 0
        lastMemAddress 
            = case state of
                (x:xs) -> x
                []     -> 0
        success = not memReady || (memData == resize lastMemAddress)
        state'
            = case memReady of
                False -> state
                True  -> case state of
                    [] -> []
                    x:xs -> xs

--Cache test system consisting of cache, backing ram and stimulus generator
testSystem :: HasClockReset dom sync gated => [BitVector 30] -> Signal dom Bool -> Signal dom (Bool, Bool)
testSystem addresses memValid = result
    where
    (procRespValid, procResp, memReqValid, memReq) = iCache (SNat @ 14) (SNat @ 12) (SNat @ 4) pseudoLRUReplacement cacheReq cacheAddress memRespValid memResp
    (memRespValid, memResp)                        = unbundle $ backingMem memReqValid memReq memValid
    (testReq, result)                              = unbundle $ testCache addresses (firstCycleDef' False procRespValid) procResp
    (cacheReq, cacheAddress)                       = unbundle testReq

--Cache QuickCheck property
--TODO: generate addresses with realistic access patterns
cacheProp addresses memValid  = P.and success && P.or finished
    where
    (finished, success) = P.unzip $ P.take 1000 $ sample $ testSystem addresses memValid

