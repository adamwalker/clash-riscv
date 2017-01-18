`timescale 1ns/10ps

module main;

    //clock and reset signals
	reg clk = 0;
    reg reset_n = 0;

    //input signals
    reg [31:0] instruction      = 0;
    reg        instructionStall = 0;
    reg [31:0] memoryData       = 0;

    //output signals
    wire [29:0] instructionAddress;
    wire [31:0] readAddress;
    wire [31:0] writeAddress;
    wire [31:0] writeData;
    wire [3:0] writeStrobe;

    //instruction memory
    reg [31:0] instructionMem [255:0];

    initial begin
        instructionMem[0] = 659;
        instructionMem[1] = 1049363;
        instructionMem[2] = 5243283;
        instructionMem[3] = 5440179;
        instructionMem[4] = 5440307;
        instructionMem[5] = 4294017427;
        instructionMem[6] = 4264565475;
        instructionMem[7] = 4267715107;
    end

    //data memory
    reg [31:0] dataMem [65535:0];

    always @ (posedge clk) begin
        instruction = instructionMem[instructionAddress];
    end

    //generate the clock
    always #1 clk = ~clk;

    //generate the reset signal
    initial
    begin
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        reset_n = 1;
    end

    riscvPipeline inst (
        instruction
        ,instructionStall
        ,memoryData
        ,// clock
        clk
        ,// asynchronous reset: active low
        reset_n
        ,instructionAddress
        ,readAddress
        ,writeAddress
        ,writeData
        ,writeStrobe
        );

    //dump the signal waveforms
    initial
    begin
        $dumpfile("wave.vcd");
        $dumpvars(0, clk, reset_n, instruction, instructionStall, memoryData, instructionAddress, readAddress, writeAddress, writeData, writeStrobe);
    end

    //run for 1000 cycles
    initial
    begin
        #1000;
        $finish;
    end

endmodule
