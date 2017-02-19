{-# LANGUAGE DataKinds, ScopedTypeVariables, KindSignatures, TypeOperators, GADTs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Cache.ICache where

import CLaSH.Prelude
import qualified Prelude as P

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

