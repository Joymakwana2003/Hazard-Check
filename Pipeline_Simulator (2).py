# importing modules for creating tables
from tabulate import tabulate

# A check for Branch Detection
BD = 0

# Sets of arrays containing the instruction executed at the nth clock cycle of each stage
arr_IF = [None] * 200
arr_ID = [None] * 200
arr_EX = [None] * 200
arr_MEM = [None] * 200
arr_WB = [None] * 200

# Register stored in the form of its number, value, symbol and its type
regs = [[0, 0, "$zero", "constant zero"],
        [1, 0, "$at", "assembler temporary"],
        [2, 0, "$v0", "value for function results"],
        [3, 0, "$v1", "value for function results"],
        [4, 0, "$a0", "arguments"],
        [5, 0, "$a1", "arguments"],
        [6, 0, "$a2", "arguments"],
        [7, 0, "$a3", "arguments"],
        [8, 0, "$t0", "temporaries"],
        [9, 0, "$t1", "temporaries"],
        [10, 0, "$t2", "temporaries"],
        [11, 0, "$t3", "temporaries"],
        [12, 0, "$t4", "temporaries"],
        [13, 0, "$t5", "temporaries"],
        [14, 0, "$t6", "temporaries"],
        [15, 0, "$t7", "temporaries"],
        [16, 0, "$s0", "saved temporaries"],
        [17, 0, "$s1", "saved temporaries"],
        [18, 0, "$s2", "saved temporaries"],
        [19, 0, "$s3", "saved temporaries"],
        [20, 0, "$s4", "saved temporaries"],
        [21, 0, "$s5", "saved temporaries"],
        [22, 0, "$s6", "saved temporaries"],
        [23, 0, "$s7", "saved temporaries"],
        [24, 0, "$t8", "temporaries"],
        [25, 0, "$t9", "temporaries"],
        [26, 0, "$k0", "reserved for OS kernel"],
        [27, 0, "$k1", "reserved for OS kernel"],
        [28, 0, "$gp", "global pointer"],
        [29, 0, "$sp", "stack pointer"],
        [30, 0, "$fp", "frame pointer"],
        [31, 0, "$ra", "return address"]]

# Values of the latches in the pipeline

# Current instruction | Next instruction | The Instruction number
IF_ID = [0, 0, 0]

# current READ REG1 | READ REG2 | Sign extend immediate & NEXT READ REG1 | READ REG2 | SIGN EXTEND immediate & The Instruction number
ID_EX = [[0, 0, 0], [0, 0, 0], 0]

# CUrrent ALU result | alu_sr2 and Next ALU result | alu_sr2 and The Instruction number
EX_MEM = [[0, 0], [0, 0], 0]

# MEMORY_READ_DATA | ALU_result | The Instruction number
MEM_WB = [[0, 0], [0, 0], 0]

# The control signals for each stage where they are used
RegDst = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
MemtoReg = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
RegWrite = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
MemRead = [0, 0, 0]  # MEM | EX | DECODE stage
MemWrite = [0, 0, 0, 0]  # WB| MEM | EX | DECODE stage
ALUOp = [0, 0]  # EX | DECODE stage
ALUSrc = [0, 0]  # EX | DECODE stage

# Check for Stall Detection
stallDetected = 0

# values of the registers, function value, Shamt value, OP Code, Branch Control Signal, and the branch target
my_rs = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
my_rt = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
my_rd = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
my_funct = [0, 0]  # EX | DECODE stage
my_shamt = [0, 0]  # EX | DECODE stage
my_op = [0, 0]  # EX | DECODE stage
Branch = [0, 0]  # EX | DECODE stage
branch_target = 0  # EX

# Array storing the instructions in MIPS corresponding to their current stage
inst_assembly = [0, 0, 0, 0, 0]

# Utility function used to display the registers and the values stored


def display_regs():
    x = [x * 2 for x in range(16)]
    for i in x:
        print(regs[i][2] + "=", "%#010x" % (regs[i][1]) + '    ' +
              regs[i + 1][2] + "=", "%#010x" % (regs[i + 1][1]))

    return

# Utility function used to display the data memory


def display_mem():
    print("data Memory")
    for i in range(len(data_mem)):
        print("location %d" % i + " = " + str(data_mem[i]))


# RegDst -- Destination Register selection based upon R or I Format
# ALUSrc -- ALU operand from either register file or sign extended immediate
# MemtoReg -- Loads - selects register write data from memory
# RegWrite -- Write Enable for Register File
# MemRead -- Read Enable for Data Memory
# MemWrite -- Write Enable for Data Memory
# Branch -- Branch instruction used to qualify next PC address
# ALUOp -- ALU operation predecode

# OP Code corresponding to the array storing the control signal values for different instructions
# | RegDst | ALUSrc | MemtoReg | RegWrite | MemRead | MemWrite | Branch | ALUOp |
control = {0b000000: [1, 0, 0, 1, 0, 0, 0, 2],  # R Format
           0b100011: [0, 1, 1, 1, 1, 0, 0, 0],  # lw
           0b101011: [0, 1, 0, 0, 0, 1, 0, 0],  # sw
           0b000100: [0, 0, 0, 0, 0, 0, 1, 1],  # beq
           0b001000: [0, 1, 0, 1, 0, 0, 0, 3], }  # addi

# 4 bits given by the ALU Control and its corresponding operation
ALU = {0b0000: lambda src1, src2: ["and", src1 & src2, "bitwise and"],
       0b0001: lambda src1, src2: ["or", src1 | src2, "bitwise or"],
       0b0010: lambda src1, src2: ["add", src1 + src2, "add signed"],
       0b0110: lambda src1, src2: ["sub", src1 - src2, "sub signed"],
       0b0111: lambda src1, src2: ["slt", 0, "set on less than"], }

# Fucntion value of different operations along with the above 4 bits
decode_funct = {0b100000: ["add", 0b0010],
                0b100010: ["sub", 0b0110],
                0b100100: ["and", 0b0000],
                0b100101: ["or", 0b0001],
                0b101010: ["slt", 0b0111], }

decode_Ifunct = {0b001101: ["or", 0b0001],
                 0b001000: ["addi", 0b0010]}

# Calculating the branch address by checking the beq condition
BranchAddress = {0b000100: lambda Zero: (
    branch_target if (Zero) else PC_plus_4), }


# Generating ALU Control using ALU Op, function value, and the OP Code
def ALU_control(ALUOp, funct, opcode):
    if (ALUOp == 0):  # lw, sw => add
        return (0b0010)
    if (ALUOp == 1):  # beq => sub
        return (0b0110)
    if (ALUOp == 2):  # Rtype
        return (decode_funct[funct][1])
    if ALUOp == 3:  # I type
        return (decode_Ifunct[opcode][1])


# Initializing Instruction Memory
inst_mem = []
# Initialising Data Memory
data_mem = [1, 2, 3, 4, 5, 6, 0, 0]

# Utility function used to read the instruction in MIPS and storing in the Instruction Memory


def read_instr():
    infile = open(
        r"instructions.txt")
    lines = infile.readlines()
    codelines = [x for x in lines if x[0] != "#"]
    for line in codelines:
        words = line.split()
        mem = (int(words[0], 16), int(words[1], 16), words[2])
        inst_mem.append(mem)
    infile.close()
    return


read_instr()  # Read Instruction Memory

# Start of Machine and Initialising the parameters to zero
PC = 0
clock = 0  # a variable holding clock counts

Zero = 0

# Main Loop
for clockcount in range(24):
    print(clock)

    # Fetch Stage
    # Fetching Instructions from the Instruction Memory
    print(PC)
    addr = inst_mem[PC >> 2][0]
    # Instruction assembly storing the MIPS code for the instruction in the IF stage
    inst_assembly[4] = inst_mem[PC >> 2][2]
    # Variable initialised to store the Machine code of the current instruction
    instruction = inst_mem[PC >> 2][1]

    # latch result of this stage into its pipeline register
    IF_ID[1] = instruction
    # Current instruction number
    IF_ID[2] = arr_IF[clock]

    # incrementing the clock by 1 in order to start the next cycle
    clock = clock + 1
    # increment PC if there is no STALL
    if stallDetected == 0:
        PC_plus_4 = PC + 4
        PC = PC_plus_4
        releaseStall = 0
        # assigning the value of the current instruction being excecuted in the IF stage
        arr_IF[clock] = int(PC/4)
    else:
        releaseStall = 1
        arr_IF[clock] = "STALL"

    if(BD == 1):
        # creating a stall incase of a branch instruction in order to prevent control hazard
        arr_IF[clock] = "Stall"

    # decode stage
    if (clock >= 2):
        # in order to make sure that reg write happens before reg read
        if (clock >= 5):
            # register write back depending on the control signal "MemtoReg"
            register_write_data = MEM_WB[0][0] if MemtoReg[0] else MEM_WB[0][1]
            # RegDst selects the write register to be rt or rd
            write_register = my_rd[0] if RegDst[0] else my_rt[0]

        # decode instruction
        # shifts the 32 bit instruction by 26 bits to get the OP code
        my_op[1] = IF_ID[0] >> 26
        # shifting the bits accordingly in order to mask out the required bits
        my_rs[3] = (IF_ID[0] >> 21) & 0x1F
        my_rt[3] = (IF_ID[0] >> 16) & 0x1F
        my_rd[3] = (IF_ID[0] >> 11) & 0x1F
        my_shamt[1] = (IF_ID[0] >> 6) & 0x1F
        my_funct[1] = IF_ID[0] & 0x3F
        my_imm = IF_ID[0] & 0xFFFF

        # control signals mapped to the array named control_word
        control_word = control[my_op[1]]
        ALUSrc[1] = control_word[1]
        RegDst[3] = control_word[0]
        MemtoReg[3] = control_word[2]
        RegWrite[3] = control_word[3]
        MemRead[2] = control_word[4]
        MemWrite[3] = control_word[5]
        Branch[1] = control_word[6]
        ALUOp[1] = control_word[7]

        # Detecting Branch Hazard using the OP Code of beq instructions
        if my_op[1] == 0b000100:
            print("Branch Hazard Detected")
            BD = 1
        else:
            BD = 0

        # register file sources which read the source registers
        read_register1 = my_rs[3]
        read_register2 = my_rt[3]
        # reading the data stored inside the source registers
        read_data1 = regs[read_register1][1]
        read_data2 = regs[read_register2][1]

        # sign extension of immediate data
        sign_bit = (my_imm >> 15) & 0x1
        sign_ext = (my_imm - (0x10000)) if (sign_bit == 1) else my_imm

        # Latch results of that stage into its pipeline reg
        ID_EX[1] = [read_data1, read_data2, sign_ext]

    # Execute Stage
    if (clock >= 3):
        # alu sources when there is no hazard or stall
        # read data 1 which includes two sources of ALU; the second source can be register 2 or the sign extended immediate
        alu_src1 = ID_EX[0][0]
        alu_src2 = ID_EX[0][2] if ALUSrc[0] else ID_EX[0][1]
        # read data 2 which is the second source register
        readData2 = ID_EX[0][1]

        # DataHazard Detection

        # hazard is present if RD of WB stage (RD[0]) == Ex stage's Rs[2] or RT[2]
        # hazard is present if RD of MEM stage (RD[1]) == Ex stage's RS[2] or RT[2]

        # Checks if the data hazard detected is due to Rs or Rt
        hazardDetected_rs = 0
        hazardDetected_rt = 0

        # If the clock value is greater than 5 then we need to check the writebacks
        if (clock > 5):
            # check if there is a write back to any register using appropriate control signals
            if ((RegWrite[0]) or (RegWrite[1])):
                # checks if intruction in WB or MEM writes back to register and WB, MEM, EX is not a store word instruction
                if ((MemWrite[0] == 0) and (MemWrite[1] == 0) and (MemWrite[2] == 0)):
                    # variable that takes the value of destination register or the register rt according to the control signal RegDst
                    WB_hazard_RD_check = my_rd[0] if RegDst[0] else my_rt[0]
                    # Checks if the value of Rd in WB stage is same as that of Ex stage's Rs[2]
                    if (WB_hazard_RD_check == my_rs[2]):
                        # checks if I instruction
                        if ((ALUOp[0] == 3)):
                            # load into alu_src1 the value of ALU result
                            alu_src1_rs = MEM_WB[0][1]
                            # hazard dectected due to Rs
                            # RAW hazard detected
                            hazardDetected_rs = 1
                            print(
                                "RAW hazard detected in clock cycle number " + str(clock))
                            print("Rs forwarded from WB stage")
                        else:
                            # load into alu_src1 the value of the loaded data from memory
                            alu_src1_rs = MEM_WB[0][0]
                            # hazard detected due to Rs
                            # RAW hazard detected
                            hazardDetected_rs = 1
                            print(
                                "RAW hazard detected in clock cycle number " + str(clock))
                            print("Rs forwarded from WB stage")
                    # Checks if the value Rd in WB stage is same as that of Ex stage's Rt[2]
                    if (WB_hazard_RD_check == my_rt[2]):
                        # checks if I instruction
                        if ((ALUOp[0] == 3)):
                            # As in I instruction, Rt is always written, there is no RAW hazard
                            alu_src2 = alu_src2
                        else:
                            # load into alu_src2 the value of the loaded data from memory
                            alu_src2_rt = MEM_WB[0][0]
                            # hazard detected due to Rt
                            hazardDetected_rt = 1
                            print(
                                "RAW hazard detected in clock cycle number " + str(clock))
                            print("Rt forwarded from EB stage")
                    # variable that takes the value of destination register or the register rt according to the control signal RegDst
                    MEM_hazard_RD_check = my_rd[1] if RegDst[1] else my_rt[1]

                    # Checks if the value of Rd in MEM stage is same as that of Ex stage's Rs[2]
                    if (MEM_hazard_RD_check == my_rs[2]):
                        # load into alu_src1 the value of ALU result
                        alu_src1_rs = EX_MEM[0][0]
                        # hazard detected due to Rs
                        hazardDetected_rs = 1
                        print(
                            "RAW hazard detected in clock cycle number " + str(clock))
                        print("Rs forwarded from MEM stage")
                    # Checks if the value of Rd in MEM stage is same as that of Ex stage's Rt[2]
                    # with RT AND source is a reg not signext
                    if ((MEM_hazard_RD_check == my_rt[2]) and (ALUSrc[0] == 0)):
                        alu_src2_rt = EX_MEM[0][0]
                        # hazard detected due to Rt
                        hazardDetected_rt = 1
                        print(
                            "RAW hazard detected in clock cycle number " + str(clock))
                        print("Rt forwarded from MEM stage")

            # Stall Detection
            # stall happens when execute sources needs a valid data which is not yet loaded from memory

            MEM_STALL_RD_check = my_rd[1] if RegDst[1] else my_rt[1]
            #
            if releaseStall == 1:  # if last clock cycle was a stall then release it, otherwise order one
                stallDetected = 0
                # on the next cycle of the stall, re fetch registers.
                alu_src1 = regs[my_rs[2]][1]
                # in case a WB hazard happened with the stall
                alu_src2 = regs[my_rt[2]][1]

            # Fetching the forwarded data
        if (stallDetected == 0):
            if hazardDetected_rs == 1:
                alu_src1 = alu_src1_rs
            if hazardDetected_rt == 1:
                alu_src2 = alu_src2_rt

            # ALUOp is the current instruction
            alu_operation = ALU_control(ALUOp[0], my_funct[0], my_op[0])
            alu_entry = ALU[alu_operation](alu_src1, alu_src2)
            alu_result = alu_entry[1]

            # Branch Target
            branch_target = PC_plus_4 + (ID_EX[0][2] * 4)

            Zero = 1 if (alu_result == 0) else 0

            # ---- Next PC Calculation ----
            # pc_mux1
            pc_mux1 = BranchAddress[my_op[0]](
                Zero) if Branch[0] == 1 else PC_plus_4

            pc_mux2 = pc_mux1

            # Next Instruction coming from the 2nd Multiplexer
            PC = pc_mux2

            # Latch results of that stage into its pipeline reg
            EX_MEM[1] = [alu_result, readData2]

    # MEM Stage
    if (clock >= 4):
        memory_read_data = 0
        # data memory operations
        # For storeword instructions
        if MemWrite[1]:
            data_mem[EX_MEM[0][0] >> 2] = EX_MEM[0][1]
        # For Load word instructions
        if MemRead[0]:
            memory_read_data = data_mem[EX_MEM[0][0] >> 2]

        # Latch results of that stage into its pipeline reg (ALU Result)
        MEM_WB[1] = [memory_read_data, EX_MEM[0][0]]

    # Write Back Stage
    if (clock >= 5):
        pass

    # Updating Pipeline Registers (Shifting)
    my_rd[0] = my_rd[1]
    my_rd[1] = my_rd[2]
    my_rt[0] = my_rt[1]
    my_rt[1] = my_rt[2]
    my_rs[0] = my_rs[1]
    my_rs[1] = my_rs[2]

    # Shifting the latches or Buffer registers
    EX_MEM[0] = EX_MEM[1]
    MEM_WB[0] = MEM_WB[1]

    # Updating Memory control lists
    MemWrite[0] = MemWrite[1]
    MemWrite[1] = MemWrite[2]
    MemRead[0] = MemRead[1]

    # Updating WB control lists
    MemtoReg[0] = MemtoReg[1]
    MemtoReg[1] = MemtoReg[2]
    RegWrite[0] = RegWrite[1]
    RegWrite[1] = RegWrite[2]
    RegDst[0] = RegDst[1]
    RegDst[1] = RegDst[2]

    # Updating Instruction Assembly in the Pipeline
    inst_assembly[0] = inst_assembly[1]
    inst_assembly[1] = inst_assembly[2]

    if stallDetected == 0:
        # if there is a stall, stall these controls and registers
        IF_ID[0] = IF_ID[1]
        ID_EX[0] = ID_EX[1]
        my_rd[2] = my_rd[3]
        my_rt[2] = my_rt[3]
        my_rs[2] = my_rs[3]

        # update next instruction to current instruction for next clock cycle
        # updating the control signals as well
        ALUSrc[0] = ALUSrc[1]
        ALUOp[0] = ALUOp[1]
        MemWrite[2] = MemWrite[3]
        MemRead[1] = MemRead[2]
        RegWrite[2] = RegWrite[3]
        RegDst[2] = RegDst[3]
        MemtoReg[2] = MemtoReg[3]

        # Updating the following
        my_funct[0] = my_funct[1]
        my_shamt[0] = my_shamt[1]
        my_op[0] = my_op[1]
        Branch[0] = Branch[1]

        # Updating Instruction Assembly in the Pipeline
        inst_assembly[2] = inst_assembly[3]
        inst_assembly[3] = inst_assembly[4]
    else:
        # Turn all signals and registers to zero to form a NOP (No operation)
        my_rs[1] = 0
        my_rt[1] = 0
        my_rd[1] = 0
        RegDst[1] = 0
        MemtoReg[1] = 0
        RegWrite[1] = 0
        MemRead[0] = 0
        MemWrite[1] = 0
        inst_assembly[1] = "NOP $zero,$zer0,$zero"

    # Updating the current instruction in all the current instruction arrays
    arr_ID[clock] = IF_ID[2]
    arr_EX[clock] = ID_EX[2]
    arr_MEM[clock] = EX_MEM[2]
    arr_WB[clock] = MEM_WB[2]

    # Updating them with the new instructions in the pipeline
    IF_ID[2] = arr_IF[clock]
    ID_EX[2] = arr_ID[clock]
    EX_MEM[2] = arr_EX[clock]
    MEM_WB[2] = arr_MEM[clock]

# Printing the table with the instructions executed in each stage in each cycle
output = [None]*179
for x in range(179):
    output[x] = [x, arr_IF[x], arr_ID[x], arr_EX[x], arr_MEM[x], arr_WB[x]]


head = ["C_No", "IF", "ID", "EX", "MEM", "RB"]

# Tabulating the results using the tabulate function
print(tabulate(output, headers=head, tablefmt="grid"))


# -- End of Main Loop
