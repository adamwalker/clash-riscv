# RISCV-CLaSH

A RiscV processor implementing the RV32I instruction set written in [clash](http://www.clash-lang.org/).

## Features
* 32 bit
* 5 stage pipeline
* In order, single issue
* Operand forwarding
* Instruction cache

## Navigation

The top level core is in core/Pipeline.hs.

Example programs are defined in test/Program.hs. 

One of the sample programs in Program.hs is a recursive function to calculate the ith element of the Fibonacci sequence. 

The verilog-test directory contains a Verilog testbench which simulates the generated Verilog on the recursive Fibonacci program.

## Testing

To run the testsuite:
```
cabal test --show-details=streaming --test-options=--color
```

## FPGA implementation

Commit bcec694e meets timing on a Virtex 7 device at 270MHz.

## TODO
* Standard AXI/Wishbone bus interface for instruction and data memory accesses
* Implement data mem/cache stalls
* Misaligned memory accesses
* Exceptions
    * Misaligned instruction fetch
* Run the RiscV testsuite
* Tweak to improve maximum frequency
* DCache
* Branch prediction
