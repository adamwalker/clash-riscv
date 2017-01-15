{-# LANGUAGE DataKinds, NoImplicitPrelude, TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
module Pipeline where

import CLaSH.Prelude

import Data.Bool

import RegFile
import Decode
import ALU
import Compare
import Mem
import qualified Debug as D
import Debug (ForwardingSource(..))

{-# ANN module ("HLint: ignore Functor law" :: String) #-}

topEntity :: Signal (Unsigned 32)
topEntity = D.pc_0 . D.stage0 <$> system (repeat 0)

firstCycleDef :: Default a => Signal a -> Signal a
firstCycleDef = mealy step False
    where
    step False _ = (True, def)
    step True  x = (True, x)

calcForwardingAddress :: Index 32 -> BitVector 32 -> BitVector 32 -> ForwardingSource
calcForwardingAddress sourceAddr instr_2 instr_3
    | sourceAddr == 0                                             = NoForwarding
    | unpack (rd instr_2) == sourceAddr && enableRegWrite instr_2 = ForwardingSourceALU
    | unpack (rd instr_3) == sourceAddr && enableRegWrite instr_3 = ForwardingSourceMem
    | otherwise                                                   = NoForwarding

data FromInstructionMem = FromInstructionMem {
    instruction      :: BitVector 32,
    instructionStall :: Bool
}

data ToInstructionMem = ToInstructionMem {
    instructionAddress :: Unsigned 30
}

data FromDataMem = FromDataMem {
    memoryData :: BitVector 32
}

data ToDataMem = ToDataMem {
    readAddress  :: Unsigned 32,
    writeAddress :: Unsigned 32,
    writeData    :: BitVector 32,
    writeStrobe  :: BitVector 4
}

system :: Vec (2 ^ 10) (BitVector 32) -> Signal D.PipelineState
system program = pipelineState
    where
    --The instruction memory
    instr_0 = firstCycleDef $ romPow2 program (resize . instructionAddress <$> toInstructionMem) --TODO: remove firstCycleDef
    --The data memory
    --TODO: think about readNew
    memReadData_3' = firstCycleDef $ readNew (blockRamPow2 (repeat 0 :: Vec (2 ^ 10) (BitVector 32)))
        ((resize . writeAddress) <$> toDataMem) --write address
        ((resize . readAddress)  <$> toDataMem) --read address
        ((/=0) . writeStrobe <$> toDataMem) 
        (writeData   <$> toDataMem)
    --The processor
    (toInstructionMem, toDataMem, pipelineState) = pipeline (FromInstructionMem <$> instr_0 <*> pure False) (FromDataMem <$> memReadData_3')

pipeline 
    :: Signal FromInstructionMem 
    -> Signal FromDataMem
    -> (Signal ToInstructionMem, Signal ToDataMem, Signal D.PipelineState)
pipeline fromInstructionMem fromDataMem = (ToInstructionMem . unpack . slice d31 d2 . pack <$> nextPC_0, toDataMem, pipelineState)
    where

    ---------------------------------------------
    --Stage 0
    --Instruction fetch
    ---------------------------------------------

    pc_0     :: Signal (Unsigned 32)
    pc_0     =  register (-4) nextPC_0

    nextPC_0 :: Signal (Unsigned 32)
    nextPC_0 =  calcNextPC <$> pc_0 <*> pc_1 <*> instr_1 <*> branchTaken_2 <*> pc_2 <*> isBranching_2 <*> isJumpingViaRegister_2 <*> aluRes_2 <*> stallStage2OrEarlier
        where
        calcNextPC currentPC pc_1 instr branchTaken_2 pc_2 isBranching_2 isJumpingViaRegister_2 aluRes_2 stall
            | stall                              = currentPC
            --Branch predicted incorrectly - resume from branch PC plus 4
            | not branchTaken_2 && isBranching_2 = pc_2      + 4
            --Jumping via register - results is ready in ALU output - load it
            | isJumpingViaRegister_2             = unpack aluRes_2
            --Predict branches taken
            | branch instr                       = pc_1      + unpack (signExtendImmediate (sbImm instr)) `shiftL` 1 :: Unsigned 32
            --Jumps always taken
            | jal    instr                       = pc_1      + unpack (signExtendImmediate (ujImm instr)) `shiftL` 1 :: Unsigned 32
            --Business as usual
            | otherwise                          = currentPC + 4

    instr_0' = instruction <$> fromInstructionMem

    stage0 
        =   D.Stage0 
        <$> pc_0 
        <*> nextPC_0
        <*> instr_0

    --inject nops for all branches and jumps because they are assumed taken
    --Also inject NOP for branch predicted incorrectly
    instr_0 = mux (isJumping_1 .||. isJumpingViaRegister_1 .||. isBranching_1 .||. (not1 branchTaken_2 .&&. isBranching_2) .||. isJumpingViaRegister_2 .||. (instructionStall <$> fromInstructionMem)) 0 instr_0'

    ---------------------------------------------
    --Stage 1
    --Decode / Register access
    ---------------------------------------------
    
    --Delay the signals computed in stage 0
    pc_1    = regEn 0 (not1 stallStage2OrEarlier) pc_0
    instr_1 = regEn 0 (not1 stallStage2OrEarlier) instr_0

    --decode the register addresses and immediate
    rs1Addr_1, rs2Addr_1 :: Signal (Index 32)
    rs1Addr_1 = (unpack . rs1)   <$> instr_1
    rs2Addr_1 = (unpack . rs2)   <$> instr_1
    imm_1     = extractImmediate <$> instr_1

    --The ALU bypass
    aluBypass_1 = mux (lui <$> instr_1) (alignUpperImmediate . uImm <$> instr_1) ((pack . (+ 4)) <$> pc_1)
    bypassALU_1 = lui <$> instr_1 .||. jalr <$> instr_1 .||. jal <$> instr_1 

    --is this instruction a jump, and we therefore need to replace the previous stage with a bubble
    --TODO: replace this with unconditional or backwards jump check
    isJumping_1            = jal <$> instr_1
    isJumpingViaRegister_1 = jalr <$> instr_1
    isBranching_1          = branch <$> instr_1

    --Figure out where the first ALU operand comes from
    aluOp1IsRegister_1 = firstOpIsRegister <$> instr_1

    --Figure out where the second ALU operand comes from
    aluOp2IsRegister_1 = secondOpIsRegister <$> instr_1

    --The regfile
    theRegFile_1 = regFile rdAddr_4 regWriteEn_4 rdData_4
    rs1Data_1    = readReg <$> theRegFile_1 <*> rs1Addr_1
    rs2Data_1    = readReg <$> theRegFile_1 <*> rs2Addr_1

    --Will either of the ALU operands be forwarded?
    forwardALUOp1_1 = calcForwardingAddress <$> rs1Addr_1 <*> instr_2 <*> instr_3
    forwardALUOp2_1 = calcForwardingAddress <$> rs2Addr_1 <*> instr_2 <*> instr_3

    --When the preceeding instruction is a memory load to a register that is used by this instruction, we need to stall
    memToAluHazard_1 
        =   (
                 (((rs1 <$> instr_1) .==. (rd <$> instr_2)) .&&. (usesRegister1 <$> instr_1)) --The previous instruction is writing rs1 and rs1 is used by this instruction
            .||. (((rs2 <$> instr_1) .==. (rd <$> instr_2)) .&&. (usesRegister2 <$> instr_1)) --The previous instruction is writing rs2 and rs2 is used by this instruction
            ) 
        .&&. (load <$> instr_2)                            --And, the previous instruction is a memory load

    stage1 
        =   D.Stage1 
        <$> instr_1 
        <*> pc_1
        <*> rs1Addr_1
        <*> rs2Addr_1
        <*> imm_1
        <*> aluOp1IsRegister_1
        <*> aluOp2IsRegister_1
        <*> theRegFile_1
        <*> rs1Data_1
        <*> rs2Data_1
        <*> forwardALUOp1_1
        <*> forwardALUOp2_1

    ---------------------------------------------
    --Stage 2
    --Execute 
    ---------------------------------------------

    stallStage2OrEarlier   = memToAluHazard_1

    --Delay the signals computed in stage 1 and insert bubbles in the relevant ones if we are stalled
    pc_2                   = register 0      pc_1
    instr_2                = register 0      $ mux stallStage2OrEarlier 0 instr_1
    rs1Data_2              = register 0      rs1Data_1
    rs2Data_2              = register 0      rs2Data_1
    aluOp1IsRegister_2     = register False  aluOp1IsRegister_1
    aluOp2IsRegister_2     = register False  aluOp2IsRegister_1
    imm_2                  = register 0      imm_1
    isBranching_2          = register False  $ mux stallStage2OrEarlier (pure False) isBranching_1
    isJumpingViaRegister_2 = register False  $ mux stallStage2OrEarlier (pure False) isJumpingViaRegister_1
    aluBypass_2            = register 0      aluBypass_1
    bypassALU_2            = register False  bypassALU_1
    forwardALUOp1_2        = register NoForwarding forwardALUOp1_1
    forwardALUOp2_2        = register NoForwarding forwardALUOp2_1

    --decode the alu operation 
    primaryOp_2   = decodeAluPrimaryOp   <$> instr_2
    secondaryOp_2 = decodeAluSecondaryOp <$> instr_2
    --Extract the comparison operator 
    compareOp_2   = extractBranchType <$> instr_2

    --Is the next cycle a register write
    regWriteEn_2       = enableRegWrite <$> instr_2

    --Will the next cycle write to memory
    memWriteEnable_2   = enableMemWrite <$> instr_2

    --Mem stage input forwarding
    forwardMemToStage3_2 = (rs2 <$> instr_2) .==. (rd <$> instr_3) .&&. regWriteEn_3
    --TODO: add to debug and pipeline to earlier stage
    --TODO: maybe combine this with ALU forwarding
    forwardMemToStage2_2 = (rs2 <$> instr_2) .==. (rd <$> instr_4) .&&. regWriteEn_4
    forwardedRs2         = mux forwardMemToStage2_2 rdData_4 rs2Data_2

    regMux :: ForwardingSource -> BitVector 32 -> BitVector 32 -> BitVector 32 ->  BitVector 32
    regMux ForwardingSourceALU _ forwardedAlu _   = forwardedAlu
    regMux ForwardingSourceMem _ _ forwardedMem   = forwardedMem
    regMux NoForwarding        regFileOperand _ _ = regFileOperand

    --Mux the ALU operands
    effectiveR1_2 
        =   regMux 
        <$> forwardALUOp1_2  
        <*> rs1Data_2 
        <*> execRes_3
        <*> rdData_4
    effectiveR2_2
        =   regMux 
        <$> forwardALUOp2_2  
        <*> rs2Data_2
        <*> execRes_3
        <*> rdData_4

    aluOperand1_2 = mux aluOp1IsRegister_2 effectiveR1_2 ((resize . pack) <$> pc_2)
    aluOperand2_2 = mux aluOp2IsRegister_2 effectiveR2_2 imm_2

    --The ALU
    aluRes_2  = alu <$> primaryOp_2 <*> secondaryOp_2 <*> aluOperand1_2 <*> aluOperand2_2
    execRes_2 = mux bypassALU_2 aluBypass_2 aluRes_2

    --The compare unit for branching
    branchTaken_2 = branchCompare <$> compareOp_2 <*> effectiveR1_2 <*> effectiveR2_2 --TODO: rs2Data might have to be forwarded

    stage2 
        =   D.Stage2
        <$> pc_2
        <*> instr_2
        <*> rs1Data_2
        <*> rs2Data_2
        <*> aluOp1IsRegister_2
        <*> aluOp2IsRegister_2
        <*> imm_2
        <*> primaryOp_2
        <*> secondaryOp_2
        <*> memWriteEnable_2
        <*> regWriteEn_2
        <*> compareOp_2
        <*> aluOperand1_2
        <*> aluOperand2_2 
        <*> execRes_2
        <*> branchTaken_2
        <*> forwardALUOp1_2
        <*> forwardALUOp2_2
        <*> forwardMemToStage3_2

    ---------------------------------------------
    --Stage 3
    --Memory
    ---------------------------------------------
    
    --Delay the signals computed in stage 2
    pc_3                 = register 0     pc_2
    instr_3              = register 0     instr_2
    execRes_3            = register 0     execRes_2
    rs2Data_3            = register 0     forwardedRs2
    memWriteEnable_3     = register False memWriteEnable_2
    regWriteEn_3         = register False regWriteEn_2
    forwardMemToStage3_3 = register False forwardMemToStage3_2

    destRegSource_3  = decodeDestRegSource <$> instr_3
    memDataToWrite_3  = mux forwardMemToStage3_3 rdData_4 rs2Data_3

    --Extract the mem word addresses
    wordAddressWrite_3 = slice d31 d2 <$> execRes_3
    wordAddressRead_3  = slice d31 d2 <$> execRes_2

    writeStrobe_3 = calcWriteStrobe <$> (extractMemSize <$> instr_3) <*> (slice d1 d0 <$> execRes_3)

    --The memory
    memReadData_3' = memoryData <$> fromDataMem
    toDataMem      
        =   ToDataMem 
        <$> ((unpack . resize) <$> wordAddressRead_3) --read address
        <*> ((unpack . resize) <$> wordAddressWrite_3) --write address
        <*> memDataToWrite_3
        <*> mux memWriteEnable_3 writeStrobe_3 0

    memReadData_3 = doLoad <$> (extractMemSize <$> instr_3) <*> (loadUnsigned <$> instr_3) <*> (slice d1 d0 <$> execRes_3) <*> memReadData_3'

    stage3 
        =   D.Stage3
        <$> pc_3
        <*> instr_3
        <*> execRes_3
        <*> rs2Data_3
        <*> memWriteEnable_3
        <*> regWriteEn_3
        <*> forwardMemToStage3_3
        <*> destRegSource_3
        <*> memReadData_3
        <*> memDataToWrite_3

    ---------------------------------------------
    --Stage 4
    --Writeback
    ---------------------------------------------

    --Delay the signals computed in stage 3
    pc_4            = register 0 pc_3
    instr_4         = register 0 instr_3
    execRes_4       = register 0 execRes_3
    rdAddr_4        = (unpack . rd)   <$> instr_4
    regWriteEn_4    = register False regWriteEn_3
    destRegSource_4 = register SourceALU destRegSource_3
    memReadData_4   = register 0 memReadData_3

    --Special registers
    cycle, time, retired :: Signal (BitVector 64)
    cycle   = register 0 (cycle + 1)
    time    = register 0 (time + 1)
    retired = register 0 (retired + 1)

    specialRegAll = func <$> instr_4 <*> cycle <*> time <*> retired
        where
        func instr cycle time retired = case decodeSpecialReg (extractSpecialReg instr) of
            Cycle   -> cycle
            Time    -> time
            Retired -> retired

    specialReg = mux (specialRegHigh <$> instr_4) (slice d63 d32 <$> specialRegAll) (slice d31 d0 <$> specialRegAll)

    --Calculate the writeback signals for the register file
    --(used in stage 1)
    rdData_4  = func <$> execRes_4 <*> memReadData_4 <*> specialReg <*> destRegSource_4
        where func a m sr s = 
                case s of
                    SourceALU  -> a
                    SourceMem  -> m
                    SourceSpec -> sr

    stage4
        =   D.Stage4
        <$> pc_4
        <*> instr_4
        <*> execRes_4
        <*> rdAddr_4
        <*> regWriteEn_4
        <*> destRegSource_4
        <*> memReadData_4
        <*> rdData_4

    pipelineState 
        =   D.PipelineState
        <$> stage0
        <*> stage1
        <*> stage2
        <*> stage3
        <*> stage4

