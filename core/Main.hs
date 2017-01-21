{-# LANGUAGE DataKinds, TemplateHaskell #-}
module Main where

import Data.Word
import qualified Data.Text as Text

import qualified CLaSH.Prelude as P

import Pipeline
import Debug

import Program
import System

assembledVec :: P.Vec 1024 (P.BitVector 32)
assembledVec = $(P.listToVecTH assembled) P.++ P.repeat 0

main = mapM (putStrLn . Text.unpack . prettyPipelineState) $ P.sampleN 2000 $ system assembledVec
