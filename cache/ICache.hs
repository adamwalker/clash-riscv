{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
--module ICache where

import CLaSH.Prelude
import qualified Prelude as P

import Test.QuickCheck hiding (resize, (.&.), (.&&.), (.||.), sample)
import qualified Test.QuickCheck as QC

{-# ANN module ("HLint: ignore Use if" :: String) #-}

data IWay (tagBits :: Nat) (lineBits :: Nat) = IWay {
    valid   :: Bool,
    tag     :: BitVector tagBits,
    line    :: Vec (2 ^ lineBits) (BitVector 32)
}

instance (KnownNat tagBits, KnownNat lineBits) => Default (IWay tagBits lineBits) where
    def = IWay False 0 (repeat 0)

type CacheWrite indexBits tagBits lineBits = Maybe (Unsigned indexBits, IWay tagBits lineBits)

--TODO: get rid of this
firstCycleDef :: Default a => Signal a -> Signal a
firstCycleDef = mealy step False
    where
    step False _ = (True, def)
    step True  x = (True, x)

{- Protocol:
    -The processor requests a memory access by asserting the request signal and setting the request address.
    -The cache will eventually respond by setting the valid signal and providing the data at the requested address.
    -The processor must hold the requested address high, but is not required to hold the request signal high.
 -}
iCache 
    :: forall tagBits indexBits lineBits. ((tagBits + (indexBits + lineBits)) ~ 30, KnownNat indexBits, KnownNat tagBits, KnownNat lineBits)
    => SNat tagBits
    -> SNat indexBits
    -> Signal Bool                                 --request
    -> Signal (BitVector 30)                       --request address
    -> Signal Bool                                 --response from main memory is ready
    -> Signal (Vec (2 ^ lineBits) (BitVector 32))  --respose from memory containing the requested line
    -> (
           Signal Bool,                            --response to processor valid
           Signal (BitVector 32),                  --response to processor data
           Signal Bool,                            --request to memory for line
           Signal (BitVector 30)                   --request to memory address
       )
iCache _ _ req reqAddress fromMemValid fromMemData = (respValid, liftA2 (!!) respLine (register 0 lineBits), busReq, busReqAddress)
    where
    --way 1
    readRes1    = firstCycleDef $ readNew (blockRamPow2 (repeat def :: Vec (2 ^ indexBits) (IWay tagBits lineBits))) (bitCoerce <$> indexBits) write1
    --way 2
    readRes2    = firstCycleDef $ readNew (blockRamPow2 (repeat def :: Vec (2 ^ indexBits) (IWay tagBits lineBits))) (bitCoerce <$> indexBits) write2

    --lru data - random replacement for now
    lru         = register False (not <$> lru)

    --Split the address into tag, index and line bits
    splitAddress :: BitVector 30 -> (BitVector tagBits, BitVector indexBits, BitVector lineBits)
    splitAddress address = (pack tag, pack index, pack line)
        where
        (tag, rest :: Vec (indexBits + lineBits) Bit) = splitAtI (unpack address)
        (index, line)                                 = splitAtI rest

    (tagBits, indexBits, lineBits) = unbundle $ splitAddress <$> reqAddress

    --Combinationally mux the data from the way that contains the address (if any)
    (respValid, respLine) = unbundle $ pickWay <$> readRes1 <*> readRes2 <*> (register 0 tagBits)
        where
        pickWay :: IWay tagBits lineBits -> IWay tagBits lineBits -> BitVector tagBits -> (Bool, Vec (2 ^ lineBits) (BitVector 32))
        pickWay way1 way2 addressTag
            | valid way1 && tag way1 == addressTag = (True,  line way1)
            | valid way2 && tag way2 == addressTag = (True,  line way2)
            | otherwise                            = (False, repeat 0)

    --Was there a miss?
    delayedRequest = register False req
    missLastCycle  = delayedRequest .&&. fmap not respValid

    fromMemValid' = fromMemValid .&&. register False handlingMiss

    --We are handling a miss if we are not in the previous cycle and we have a miss
    --We are not handling a miss if we are in the previous cycle and we get the result from memory
    handlingMiss   = missLastCycle .||. register False (handlingMiss .&&. fmap not fromMemValid')
    missAddress    = register 0 $ mux (fromMemValid' .||. fmap not handlingMiss) reqAddress missAddress
    (missTag, missIndex, missBits) = unbundle $ splitAddress <$> missAddress

    busReq         :: Signal Bool           = handlingMiss
    busReqAddress  :: Signal (BitVector 30) = missAddress

    --Request data from memory and write it back into the cache on a miss
    replacementWay :: Signal (CacheWrite indexBits tagBits lineBits)
    replacementWay =  Just <$> bundle (unpack <$> missIndex, IWay True <$> missTag <*> fromMemData)
    write1         :: Signal (CacheWrite indexBits tagBits lineBits) = mux (lru           .&&. fromMemValid' .&&. handlingMiss) replacementWay (pure Nothing)
    write2         :: Signal (CacheWrite indexBits tagBits lineBits) = mux ((not <$> lru) .&&. fromMemValid' .&&. handlingMiss) replacementWay (pure Nothing)

backingMem 
    :: Signal Bool
    -> Signal (BitVector 30)
    -> Signal (Bool, Vec 16 (BitVector 32))
backingMem req addr = register (False, repeat 0) $ (\addr -> (True, map resize $ iterateI (+ 1) (addr .&. complement 0b1111))) <$> addr

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

testSystem :: [BitVector 30] -> Signal (Bool, Bool)
testSystem addresses = result
    where
    (procRespValid, procResp, memReqValid, memReq) = iCache (SNat @ 14) (SNat @ 12) cacheReq cacheAddress memRespValid memResp
    (memRespValid, memResp)                        = unbundle $ backingMem memReqValid memReq 
    (testReq, result)                              = unbundle $ testCache addresses procRespValid procResp
    (cacheReq, cacheAddress)                       = unbundle testReq

testSystem2 :: Signal (Bool, BitVector 30) -> Signal (Bool, Unsigned 32, Bool, Unsigned 30, Bool, Vec 16 (BitVector 32))
testSystem2 addresses = bundle $ (procRespValid, fromIntegral <$> procResp, memReqValid, fromIntegral <$> memReq, memRespValid, memResp)
    where
    (req, addr) = unbundle addresses
    (procRespValid, procResp, memReqValid, memReq) = iCache (SNat @ 14) (SNat @ 12) req addr memRespValid memResp
    (memRespValid, memResp)                        = unbundle $ backingMem memReqValid memReq 

prop addresses = P.and success && P.or finished
    where
    (finished, success) = P.unzip $ P.take 1000 $ sample $ testSystem addresses

main3 = quickCheck $ forAll (QC.resize 100 arbitrary) prop

main1 = mapM print $ sampleN_lazy 25 $ testSystem list2
    where 
    list2 = [
            0, 
            1, 
            1, 
            1, 
            2, 
            1, 
            257, 
            257, 
            257, 
            257, 
            257,
            3,
            0,
            0,
            0
        ]

main2 = mapM (\p -> print p) $ P.zip list $ sampleN_lazy 16 $ testSystem2 $ fromList list
    where 
    list = [
            (True, 0), 
            (True, 1), 
            (True, 1), 
            (True, 1), 
            (True, 2), 
            (True, 1), 
            (True, 257), 
            (True, 257), 
            (True, 257), 
            (True, 257), 
            (True, 257), 
            (True, 257), 
            (True, 3), 
            (True, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0), 
            (False, 0)
        ]

main = main3 -- main2 >> main1
