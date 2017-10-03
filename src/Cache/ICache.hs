{-# LANGUAGE DataKinds, ScopedTypeVariables, KindSignatures, TypeOperators, GADTs, RecordWildCards #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Cache.ICache where

import Clash.Prelude
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

{- Protocol:
    -The processor requests a memory access by asserting the request signal and setting the request address.
    -The cache will eventually respond by setting the valid signal and providing the data at the requested address.
    -The processor must hold the requested address high, but is not required to hold the request signal high.
 -}
--TODO: support wrapped burst memory read instead of expecting a whole line to arrive at the same time.
iCache 
    :: forall dom sync gated tagBits indexBits lineBits. (HasClockReset dom sync gated, (tagBits + (indexBits + lineBits)) ~ 30, KnownNat indexBits, KnownNat tagBits, KnownNat lineBits)
    => SNat tagBits
    -> SNat indexBits
    -> Signal dom Bool                                 --request
    -> Signal dom (BitVector 30)                       --request address
    -> Signal dom Bool                                 --response from main memory is ready
    -> Signal dom (Vec (2 ^ lineBits) (BitVector 32))  --respose from memory containing the requested line
    -> (
           Signal dom Bool,                            --response to processor valid
           Signal dom (BitVector 32),                  --response to processor data
           Signal dom Bool,                            --request to memory for line
           Signal dom (BitVector 30)                   --request to memory address
       )
iCache _ _ req reqAddress fromMemValid fromMemData = (respValid, respData, busReq, busReqAddress)
    where

    readVec 
        = map 
            (readNew (blockRamPow2 (repeat def :: Vec (2 ^ indexBits) (IWay tagBits lineBits))) (bitCoerce <$> indexBits)) 
            writes

    --lru data - random replacement for now
    lru' = register False (not <$> lru')
    lru :: Signal (Index 2)
    lru  = bitCoerce <$> lru'

    --Split the address into tag, index and line bits
    splitAddress :: BitVector 30 -> (BitVector tagBits, BitVector indexBits, BitVector lineBits)
    splitAddress address = (pack tag, pack index, pack line)
        where
        (tag, rest :: Vec (indexBits + lineBits) Bit) = splitAtI (unpack address)
        (index, line)                                 = splitAtI rest

    (tagBits, indexBits, lineBits) = unbundle $ splitAddress <$> reqAddress

    --Combinationally mux the data from the way that contains the address (if any)
    (respValid, wayIdx, respData) = unbundle $ topFunc <$> lastTag <*> lastLine <*> sequenceA readVec
        where

        topFunc :: BitVector tagBits -> BitVector lineBits -> Vec 2 (IWay tagBits lineBits) -> (Bool, Index 2, BitVector 32)
        topFunc tagBits lineBits ways = fold merge $ imap (func tagBits lineBits) ways

        lastTag  = register 0 tagBits
        lastLine = register 0 lineBits

        func :: BitVector tagBits -> BitVector lineBits -> Index 2 -> IWay tagBits lineBits -> (Bool, Index 2, BitVector 32)
        func tagBits lineBits idx IWay{..}
            | tag == tagBits = (valid, idx, line !! lineBits)
            | otherwise      = (False, errorX "cache undefined", errorX "cache undefined")

        merge :: (Bool, Index 2, BitVector 32) -> (Bool, Index 2, BitVector 32) -> (Bool, Index 2, BitVector 32)
        merge x@(True, _, _) _              = x
        merge _              y@(True, _, _) = y
        merge _              _              = (False, errorX "cache undefined",  errorX "cache undefined")

    --Was there a miss in the previous cycle?
    delayedRequest = register False req
    missLastCycle  = delayedRequest .&&. fmap not respValid

    --We are expecting a value from mem a cycle after handlingMiss goes high, but not if we received a valid value from mem in the previous cycle
    expectingMem  = register False (handlingMiss .&&. fmap not fromMemValid')
    fromMemValid' = expectingMem .&&. fromMemValid
    handlingMiss  = missLastCycle .||. expectingMem

    missAddress    = register 0 $ mux (fromMemValid' .||. fmap not handlingMiss) reqAddress missAddress
    (missTag, missIndex, missBits) = unbundle $ splitAddress <$> missAddress

    busReq         :: Signal dom Bool           = handlingMiss
    busReqAddress  :: Signal dom (BitVector 30) = missAddress

    --Request data from memory and write it back into the cache on a miss
    replacementWay :: Signal dom (CacheWrite indexBits tagBits lineBits)
    replacementWay =  Just <$> bundle (unpack <$> missIndex, IWay True <$> missTag <*> fromMemData)

    writes :: Vec 2 (Signal dom (CacheWrite indexBits tagBits lineBits))
    writes = imap func $ repeat ()
        where
        func idx _ = mux ((lru .==. pure idx) .&&. fromMemValid') replacementWay (pure Nothing)

