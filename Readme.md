# RISCV-CLaSH

A RiscV processor implementing the RV32I instruction set written in [clash](http://www.clash-lang.org/).

## Features
* 32 bit
* 5 stage pipeline
* In order, single issue
* Operand forwarding

## Navigation

The top level core is in Pipeline.hs.

The core is instantiated with instruction and data memory in System.hs.

Main.hs instantiates this design and simulates it on the program defined in Program.hs. 

The sample program in Program.hs is a recursive function to calculate the ith element of the Fibonacci sequence. 

The verilog-test directory contains a Verilog testbench which simulates the generated Verilog on the recursive Fibonacci program.

## FPGA implementation

Commit bcec694e meets timing on a Virtex 7 device at 270MHz.

## TODO
* Implement data mem/cache stalls
* Exceptions
    * Misaligned instruction fetch
    * Misaligned data memory access
* Run the RiscV testsuite
* Tweak to improve maximum frequency
* DCache, ICache
* Branch prediction
