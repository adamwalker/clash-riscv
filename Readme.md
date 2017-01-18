# RISCV-CLaSH

A RiscV processor implementing the RV32I instruction set written in [clash](http://www.clash-lang.org/).

## Features
* 32 bit
* 5 stage pipeline
* In order, single issue
* Operand forwarding

## Navigation

The top level design is in Pipeline.hs.

Main.hs instantiates this design and simulates it on the program defined in Program.hs. 

The sample program in Program.hs is a recursive function to calculate the ith element of the Fibonacci sequence. 

The verilog-test directory contains a Verilog testbench which simulates the generated Verilog on the recursive Fibonacci program.

## TODO
* Implement data mem/cache stalls
* Exceptions
    * Misaligned instruction fetch
    * Misaligned data memory access
* Run the RiscV testsuite
* Implement on FPGA and tweak to improve maximum frequency
* DCache, ICache
* Branch prediction
