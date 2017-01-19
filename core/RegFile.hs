{-# LANGUAGE DataKinds, NoImplicitPrelude #-}
module RegFile where

import CLaSH.Prelude

type RegFile = Vec 32 (BitVector 32)

regFile 
    :: Signal (Index 32)     --Write address
    -> Signal Bool           --Write enable
    -> Signal (BitVector 32) --Write data
    -> Signal (Vec 32 (BitVector 32))
regFile writeAddr writeEn writeData = file
    where
    file = mealy step (repeat 0) $ bundle (writeAddr, writeEn, writeData)
        where
        step :: RegFile -> (Index 32, Bool, BitVector 32) -> (RegFile, RegFile)
        step regFile (writeAddr, writeEn, writeData) = (regFile', regFile')
            where
            regFile'
                | writeEn   = replace writeAddr writeData regFile
                | otherwise = regFile
            
readReg :: Vec 32 (BitVector 32) -> Index 32 -> BitVector 32
readReg regFile idx
    | idx == 0  = 0
    | otherwise = regFile !! idx
