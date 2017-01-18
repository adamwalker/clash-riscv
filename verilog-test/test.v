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

    //initialise the instruction memory
    initial begin
        //non-recursive fib
        //instructionMem[0] = 659;
        //instructionMem[1] = 1049363;
        //instructionMem[2] = 5243283;
        //instructionMem[3] = 5440179;
        //instructionMem[4] = 5440307;
        //instructionMem[5] = 4294017427;
        //instructionMem[6] = 4264565475;
        //instructionMem[7] = 4267715107;
        
        //recursive fib
        instructionMem[0]  = 147;
        instructionMem[1]  = 8388883;
        instructionMem[2]  = 12583407;
        instructionMem[3]  = 4265617955;
        instructionMem[4]  = 4292866159;
        instructionMem[5]  = 71267;
        instructionMem[6]  = 531;
        instructionMem[7]  = 98407;
        instructionMem[8]  = 4293985043;
        instructionMem[9]  = 202339;
        instructionMem[10]  = 1049107;
        instructionMem[11] = 98407;
        instructionMem[12] = 12615827;
        instructionMem[13] = 4263553571;
        instructionMem[14] = 4264602659;
        instructionMem[15] = 4293984531;
        instructionMem[16] = 4250923503;
        instructionMem[17] = 4265651747;
        instructionMem[18] = 4282425603;
        instructionMem[19] = 4292935955;
        instructionMem[20] = 4234146287;
        instructionMem[21] = 4290814595;
        instructionMem[22] = 4358707;
        instructionMem[23] = 4286620035;
        instructionMem[24] = 4282417299;
        instructionMem[25] = 98407;
    end

    //read the instruction memory
    always @ (posedge clk) begin
        instruction = instructionMem[instructionAddress];
    end

    //data memory
    reg [31:0] dataMem [65535:0];

    //read/write the data memory
    always @ (posedge clk) begin

        memoryData = dataMem[readAddress];

        if (writeStrobe != 0) begin
            dataMem[writeAddress] = writeData;
        end

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
        #2500;
        $finish;
    end

endmodule
