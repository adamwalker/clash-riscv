{-# LANGUAGE DataKinds, NoImplicitPrelude #-}
module RegFile (regFile) where

import CLaSH.Prelude

type RegFile = Vec 32 (BitVector 32)

regFile 
    :: Signal (Index 32)     --Read address 1
    -> Signal (Index 32)     --Read address 2
    -> Signal (Index 32)     --Write address
    -> Signal Bool           --Write enable
    -> Signal (BitVector 32) --Write data
    -> (Signal (BitVector 32), Signal (BitVector 32), Signal (Vec 32 (BitVector 32)))
regFile rs1 rs2 writeAddr writeEn writeData = (bypass <$> file <*> rs1 <*> writeEn <*> writeAddr <*> writeData, bypass <$> file <*> rs2 <*> writeEn <*> writeAddr <*> writeData, file)
    where
    file = mealy step (repeat 0) $ bundle (writeAddr, writeEn, writeData)
        where
        step :: RegFile -> (Index 32, Bool, BitVector 32) -> (RegFile, RegFile)
        step regFile (writeAddr, writeEn, writeData) = (regFile', regFile)
            where
            regFile'
                | writeEn   = replace writeAddr writeData regFile
                | otherwise = regFile

bypass :: Vec 32 (BitVector 32) -> Index 32 -> Bool -> Index 32 -> BitVector 32 -> BitVector 32
bypass file readAddr writeEn writeAddr writeData 
    | readAddr == 0                    = 0
    | readAddr == writeAddr && writeEn = writeData
    | otherwise                        = file !! readAddr
            
