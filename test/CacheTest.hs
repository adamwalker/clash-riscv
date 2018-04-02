-- | Instruction cache testbench
module CacheTest where

import Clash.Prelude as Clash
import qualified  Prelude as P
import Data.Bool
import Data.List (sort)

import Cache.ICache
import Cache.Replacement
import Cache.PseudoLRUTree

import TestUtils

--Backing mem for cache. Delivers a line at a time
backingMem 
    :: HiddenClockReset dom sync gated
    => Signal dom Bool
    -> Signal dom (BitVector 30)
    -> Signal dom Bool
    -> Signal dom (Bool, Vec 16 (BitVector 32))
backingMem req addr memValid = register (False, repeat 0) $ readMemory <$> addr <*> memValid
    where
    readMemory addr memValid = (memValid, bool (repeat 0) (map resize (iterateI (+ 1) (addr .&. complement 0b1111))) memValid)

--Test stimulus generation for instruction cache. Requests a sequence of addresses and checks the correct result is returned.
testCache 
    :: HiddenClockReset dom sync gated
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
testSystem :: HiddenClockReset dom sync gated => [BitVector 30] -> Signal dom Bool -> Signal dom (Bool, Bool)
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

--Pseudu lru tree pseudo-tests

prop_plru :: Vec 15 Bool -> Bool
prop_plru tree = reordered == [0..15]
    where
    trees     = Clash.iterate (SNat @ 16) func tree
        where
        func tree = updateWay (getOldestWay tree) tree
    reordered = sort $ Clash.toList $ Clash.map (fromIntegral . pack) $ Clash.map getOldestWay trees

prop_plruSame :: Vec 15 Bool -> Bool
prop_plruSame tree = updateOldestWay tree == (oldest, newTree)
    where
    oldest  = getOldestWay tree
    newTree = updateWay oldest tree

prop_plruIdempotent :: Vec 15 Bool -> Vec 4 Bool -> Bool
prop_plruIdempotent tree idx = updateWay idx tree == updateWay idx (updateWay idx tree)

prop_plruSimpleCase :: Vec 1 Bool -> Bool
prop_plruSimpleCase tree = updateWay (getOldestWay tree) tree == Clash.map not tree

