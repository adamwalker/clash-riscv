{-# LANGUAGE DataKinds, NoImplicitPrelude, BinaryLiterals, TypeApplications #-}

-- | Instruction cache testbench
module CacheTest where

import CLaSH.Prelude
import qualified  Prelude as P

import Cache.ICache

import TestUtils

--Backing mem for cache. Delivers a line at a time
backingMem 
    :: Signal Bool
    -> Signal (BitVector 30)
    -> Signal (Bool, Vec 16 (BitVector 32))
backingMem req addr = register (False, repeat 0) $ (\addr -> (True, map resize $ iterateI (+ 1) (addr .&. complement 0b1111))) <$> addr

--Test stimulus generation for instruction cache. Requests a sequence of addresses and checks the correct result is returned.
testCache 
    :: [BitVector 30]
    -> Signal Bool
    -> Signal (BitVector 32)
    -> Signal ((Bool, BitVector 30), (Bool, Bool))
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
testSystem :: [BitVector 30] -> Signal (Bool, Bool)
testSystem addresses = result
    where
    (procRespValid, procResp, memReqValid, memReq) = iCache (SNat @ 14) (SNat @ 12) cacheReq cacheAddress memRespValid memResp
    (memRespValid, memResp)                        = unbundle $ backingMem memReqValid memReq 
    (testReq, result)                              = unbundle $ testCache addresses (firstCycleDef' False procRespValid) procResp
    (cacheReq, cacheAddress)                       = unbundle testReq

--Cache QuickCheck property
--TODO: generate addresses with realistic access patterns
cacheProp addresses = P.and success && P.or finished
    where
    (finished, success) = P.unzip $ P.take 1000 $ sample $ testSystem addresses

