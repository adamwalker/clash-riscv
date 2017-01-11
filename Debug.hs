{-# LANGUAGE DataKinds, RecordWildCards, OverloadedStrings #-}
module Debug where

import CLaSH.Prelude
import Prelude as P

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid

import ALU
import Decode

data ForwardingSource
    = ForwardingSourceALU
    | ForwardingSourceMem
    | NoForwarding
    deriving (Eq, Show)

data Stage0 = Stage0 {
    pc_0     :: Unsigned 32,
    nextPC_0 :: Unsigned 32,
    instr_0  :: BitVector 32
} deriving (Show)

prettyStage0 :: Stage0 -> Text
prettyStage0 Stage0{..} = Text.intercalate "\n" [
        "pc_0:     " <> Text.pack (show pc_0),
        "nextPC_0: " <> Text.pack (show nextPC_0),
        "instr_0:  " <> Text.pack (show instr_0)
    ]

data Stage1 = Stage1 {
    instr_1            :: BitVector 32,
    pc_1               :: Unsigned 32,
    rs1Addr_1          :: Index 32,
    rs2Addr_1          :: Index 32,
    imm_1              :: BitVector 32,
    aluOp1IsRegister_1 :: Bool,
    aluOp2IsRegister_1 :: Bool,
    theRegFile_1       :: Vec 32 (BitVector 32),
    rs1Data_1          :: BitVector 32,
    rs2Data_1          :: BitVector 32,
    forwardALUOp1_1    :: ForwardingSource,
    forwardALUOp2_1    :: ForwardingSource
} deriving (Show)

prettyRegFile :: Vec 32 (BitVector 32) -> Text
prettyRegFile = Text.intercalate "\n" . P.zipWith prettyReg [0..] . toList
    where
    prettyReg idx bv = Text.pack (show idx) <> ": " <> Text.pack (show bv) <> "(" <> Text.pack (show (unpack bv :: Unsigned 32)) <> ")"

prettyStage1 :: Stage1 -> Text
prettyStage1 Stage1{..} = Text.intercalate "\n" [
        prettyRegFile theRegFile_1 <> "\n",

        "instr_1:            " <> Text.pack (show instr_1),
        "pc_1:               " <> Text.pack (show pc_1),
        "rs1Addr_1:          " <> Text.pack (show rs1Addr_1),
        "rs2Addr_1:          " <> Text.pack (show rs2Addr_1),
        "imm_1:              " <> Text.pack (show imm_1),
        "aluOp1IsRegister_1: " <> Text.pack (show aluOp1IsRegister_1),
        "aluOp2IsRegister_1: " <> Text.pack (show aluOp2IsRegister_1),
        "rs1Data_1:          " <> Text.pack (show rs1Data_1),
        "rs2Data_1:          " <> Text.pack (show rs2Data_1),
        "forwardALUOp1_1:    " <> Text.pack (show forwardALUOp1_1),
        "forwardALUOp2_1:    " <> Text.pack (show forwardALUOp2_1)
    ]

data Stage2 = Stage2 {
    pc_2                 :: Unsigned 32,
    instr_2              :: BitVector 32,
    rs1Data_2            :: BitVector 32,
    rs2Data_2            :: BitVector 32,
    aluOp1IsRegister_2   :: Bool,
    aluOp2IsRegister_2   :: Bool,
    imm_2                :: BitVector 32,
    primaryOp_2          :: PrimaryOp,
    secondaryOp_2        :: SecondaryOp,
    memWriteEnable_2     :: Bool,
    regWriteEn_2         :: Bool,
    compareOp_2          :: BitVector 3,

    aluOperand1_2        :: BitVector 32,
    aluOperand2_2        :: BitVector 32,
    execRes_2            :: BitVector 32,
    branchTaken_2        :: Bool,
    forwardALUOp1_2      :: ForwardingSource,
    forwardALUOp2_2      :: ForwardingSource,
    forwardMemToStage3_2 :: Bool
} deriving (Show)

prettyStage2 :: Stage2 -> Text
prettyStage2 Stage2{..} = Text.intercalate "\n" [
        "instr_2:            " <> Text.pack (show instr_2),
        "pc_2:               " <> Text.pack (show pc_2),
        "imm_2:              " <> Text.pack (show imm_2),
        "primaryOp_2:        " <> Text.pack (show primaryOp_2),
        "secondaryOp_2:      " <> Text.pack (show secondaryOp_2),
        "aluOp1IsRegister_2: " <> Text.pack (show aluOp1IsRegister_2),
        "aluOp2IsRegister_2: " <> Text.pack (show aluOp2IsRegister_2),
        "memWriteEnable_2:   " <> Text.pack (show memWriteEnable_2),
        "regWriteEn_2:       " <> Text.pack (show regWriteEn_2),
        "compareOp_2:        " <> Text.pack (show compareOp_2),
        "rs1Data_2:          " <> Text.pack (show rs1Data_2),
        "rs2Data_2:          " <> Text.pack (show rs2Data_2),

        "aluOperand1_2:      " <> Text.pack (show aluOperand1_2),
        "aluOperand2_2:      " <> Text.pack (show aluOperand2_2),
        "execRes_2:          " <> Text.pack (show execRes_2),
        "branchTaken_2:      " <> Text.pack (show branchTaken_2),
        "forwardALUOp1_2     " <> Text.pack (show forwardALUOp1_2),
        "forwardALUOp2_2     " <> Text.pack (show forwardALUOp2_2),
        "forwardMemToStage3_2: " <> Text.pack (show forwardMemToStage3_2)
    ]

data Stage3 = Stage3 {
    pc_3             :: Unsigned 32,
    instr_3          :: BitVector 32,
    execRes_3        :: BitVector 32,
    rs2Data_3        :: BitVector 32,
    memWriteEnable_3 :: Bool,
    regWriteEn_3     :: Bool,
    forwardMemToStage3_3 :: Bool,

    destRegSource_3  :: DestRegSource,
    memReadData_3    :: BitVector 32,
    memDataToWrite_3 :: BitVector 32
} deriving (Show)

prettyStage3 :: Stage3 -> Text
prettyStage3 Stage3{..} = Text.intercalate "\n" [
        "instr_3:            " <> Text.pack (show instr_3),
        "pc_3:               " <> Text.pack (show pc_3),
        "memWriteEnable_3:   " <> Text.pack (show memWriteEnable_3),
        "regWriteEn_3:       " <> Text.pack (show regWriteEn_3),
        "execRes_3:          " <> Text.pack (show execRes_3),
        "rs2Data_3:          " <> Text.pack (show rs2Data_3),
        "forwardMemToStage3_3: " <> Text.pack (show forwardMemToStage3_3),

        "destRegSource_3:    " <> Text.pack (show destRegSource_3),
        "memReadData_3:      " <> Text.pack (show memReadData_3),
        "memDataToWrite_3:   " <> Text.pack (show memDataToWrite_3)
    ]

data Stage4 = Stage4 {
    pc_4            :: Unsigned 32,
    instr_4         :: BitVector 32,
    execRes_4       :: BitVector 32,
    rdAddr_4        :: Index 32,
    regWriteEn_4    :: Bool,
    destRegSource_4 :: DestRegSource,
    memReadData_4   :: BitVector 32,
    rdData_4        :: BitVector 32
} deriving (Show)

prettyStage4 :: Stage4 -> Text
prettyStage4 Stage4{..} = Text.intercalate "\n" [
        "instr_4:            " <> Text.pack (show instr_4),
        "pc_4:               " <> Text.pack (show pc_4),
        "regWriteEn_4:       " <> Text.pack (show regWriteEn_4),
        "execRes_4:          " <> Text.pack (show execRes_4),
        "destRegSource_4:    " <> Text.pack (show destRegSource_4),
        "memReadData_4:      " <> Text.pack (show memReadData_4),

        "rdAddr_4:           " <> Text.pack (show rdAddr_4),
        "rdData_4:           " <> Text.pack (show rdData_4)
    ]

data PipelineState = PipelineState {
    stage0 :: Stage0,
    stage1 :: Stage1,
    stage2 :: Stage2,
    stage3 :: Stage3,
    stage4 :: Stage4
} deriving (Show)

prettyPipelineState :: PipelineState -> Text
prettyPipelineState PipelineState{..} 
    =  "============================================================================================================================\n"
    <> prettyStage0 stage0 <> "\n\n" 
    <> prettyStage1 stage1 <> "\n\n" 
    <> prettyStage2 stage2 <> "\n\n" 
    <> prettyStage3 stage3 <> "\n\n"
    <> prettyStage4 stage4 <> "\n"

