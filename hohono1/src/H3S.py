#!/usr/bin/env python3
import os, sys, time, getch

def file_to_code_array(path):
    tmp_code = []
    with open(path) as f:
        for line in f:
            tmp_code.append(line.split("\n")[0])
    return tmp_code

def file_to_memory_array(path):
    tmp_mem = []
    with open(path, "r") as f:
        byte = f.read(1)
        while byte:
            tmp_mem.append(ord(byte))
            byte = f.read(1)
    while len(tmp_mem) < 0x10000:
        tmp_mem.append(0)
    return tmp_mem

def memory_array_to_memory_file(array, path):
    with open(path, "w") as f:
        for i in range(0x10000):
            if (array[i] != 0):
                f.write("{:04X} {:04X}\n".format(i, array[i]))


class Context:
    def __init__(self):
        self.instruction = None
        self.initial_registers = None

class VM:
    instruction_set = [
            "SET", "AIB", "ASB", "ACB", "PRT", "JMP", 
            "INC", "DEC", "ADD", "SUB", "MUL", "DIV", 
            "MOD", "SWP", "NUL", "CPY", "ALB", "AND", 
            "IOR", "XOR", "NOT", "CMP", "JGT", "JLT", 
            "JEQ", "RTM", "MTR", "GET", "LOD", "SAV"
        ]
    readonly_registers = [ "RES", "CLK", "PTR" ]
    parameters_number = {
            "SET": 2, "AIB": 1, "ASB": 0, "ACB": 1, "PRT": 0, "JMP": 1, 
            "INC": 1, "DEC": 1, "ADD": 2, "SUB": 2, "MUL": 2, "DIV": 2, 
            "MOD": 2, "SWP": 2, "NUL": 0, "CPY": 2, "ALB": 0, "AND": 2, 
            "IOR": 2, "XOR": 2, "NOT": 1, "CMP": 2, "JGT": 1, "JLT": 1, 
            "JEQ": 1, "RTM": 2, "MTR": 2, "GET": 1, "LOD": 1, "SAV": 1
        }

    def __init__(self, debug = False):
        # Registers
        self.registers = dict()
        self.registers["RES"] = 0
        self.registers["PTR"] = 0
        self.registers["CLK"] = 0
        self.registers["G01"] = 0
        self.registers["G02"] = 0
        self.registers["G03"] = 0
        self.registers["G04"] = 0

        # Memory
        self.buf = ""
        self.mem = [0 for i in range(0x10000)]

        # VM-specific settings
        self.ast = []
        self.code = None
        self.context = Context()
        self.debugmode = debug
        self.instruction_per_second = 50
        self.running = False

    def lex(self, code):
        self.code = code
        for line in code:
            tmp_split = line.split("\n")[0].split(" ")
            instruction = tmp_split[0]
            if (len(tmp_split) > 1):
                params = tmp_split[1:]
            else:
                params = []

            # Verify if the instruction is valid
            if (instruction in self.instruction_set):
                self.ast.append([instruction, params])
            elif (instruction == ""):
                continue
            else:
                self.error("unrecognized instruction {:04X}: {}".format(code.index(line), line.split("\n")[0]))

    def run(self):
        self.running = True
        try:
            while True:
                self.do_cycle()
        except IndexError:
            print("Execution completed in {} cycle(s)".format(self.registers["CLK"]))
        self.running = False

    def reset(self):
        tmp_code = self.code
        tmp_ast = self.ast
        self.__init__(self.debugmode)
        self.code = tmp_code
        self.ast = tmp_ast

    def do_cycle(self):
        # Prepare context
        self.context.instruction = self.ast[self.registers["PTR"]]
        self.context.initial_registers = self.registers.copy()

        # Execute the instruction
        self.debug("running {}".format(self.context.instruction))
        ci, cp = self.context.instruction
        if (not self.is_param_count_ok(self.parameters_number[ci], cp)):
            self.error("expected {} parameter(s) for {}, got {}".format(self.parameters_number[ci], ci, len(cp)))

        if (ci == "SET"):
            if (not self.is_register(cp[0])):
                self.error("can't SET {}: not a register".format(cp[0]))
            elif (self.is_register_readonly(cp[0])):
                self.error("can't SET {}: read-only register".format(cp[0]))
            elif (not self.is_constant(cp[1])):
                self.error("can't SET to {}: invalid constant".format(cp[1]))
            else:
                self.registers[cp[0]] = int(cp[1], 16)
        elif (ci == "AIB"):
            if (not self.is_register(cp[0])):
                self.error("can't AIB {}: not a register".format(cp[0]))
            else:
                self.buf += "{:04X}".format(self.registers[cp[0]])
        elif (ci == "ASB"):
            self.buf += " "
        elif (ci == "ACB"):
            if (not self.is_register(cp[0])):
                self.error("can't ACB {}: not a register".format(cp[0]))
            else:
                self.buf += chr(self.registers[cp[0]])
        elif (ci == "PRT"):
            print("{}".format(self.buf), end="", flush=True)
            self.buf = ""
        elif (ci == "JMP"):
            if (not self.is_constant(cp[0])):
                self.error("can't JMP to {}: invalid constant".format(cp[0]))
            else:
                self.registers["PTR"] = int(cp[0], 16) - 1
        elif (ci == "INC"):
            if (not self.is_register(cp[0])):
                self.error("can't INC {}: not a register".format(cp[0]))
            elif (self.is_register_readonly(cp[0])):
                self.error("can't INC {}: read-only register".format(cp[0]))
            else:
                self.registers[cp[0]] = (self.registers[cp[0]] + 1) & 0xFFFF
        elif (ci == "DEC"):
            if (not self.is_register(cp[0])):
                self.error("can't DEC {}: not a register".format(cp[0]))
            elif (self.is_register_readonly(cp[0])):
                self.error("can't DEC {}: read-only register".format(cp[0]))
            else:
                self.registers[cp[0]] = (self.registers[cp[0]] - 1) & 0xFFFF
        elif (ci == "ADD"):
            if (not self.is_register(cp[0])):
                self.error("can't ADD {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't ADD {}: not a register".format(cp[1]))
            else:
                self.registers["RES"] = (self.registers[cp[0]] + self.registers[cp[1]]) & 0xFFFF
        elif (ci == "SUB"):
            if (not self.is_register(cp[0])):
                self.error("can't SUB {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't SUB {}: not a register".format(cp[1]))
            else:
                self.registers["RES"] = (self.registers[cp[0]] - self.registers[cp[1]]) & 0xFFFF
        elif (ci == "MUL"):
            if (not self.is_register(cp[0])):
                self.error("can't MUL {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't MUL {}: not a register".format(cp[1]))
            else:
                self.registers["RES"] = (self.registers[cp[0]] * self.registers[cp[1]]) & 0xFFFF
        elif (ci == "DIV"):
            if (not self.is_register(cp[0])):
                self.error("can't DIV {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't DIV {}: not a register".format(cp[1]))
            else:
                self.registers["RES"] = (self.registers[cp[0]] // self.registers[cp[1]]) & 0xFFFF
        elif (ci == "MOD"):
            if (not self.is_register(cp[0])):
                self.error("can't MOD {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't MOD {}: not a register".format(cp[1]))
            else:
                self.registers["RES"] = (self.registers[cp[0]] % self.registers[cp[1]]) & 0xFFFF
        elif (ci == "SWP"):
            if (not self.is_register(cp[0])):
                self.error("can't SWP {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't SWP {}: not a register".format(cp[1]))
            else:
                self.registers[cp[0]], self.registers[cp[1]] = self.registers[cp[1]], self.registers[cp[0]]
        elif (ci == "NUL"):
            pass
        elif (ci == "CPY"):
            if (not self.is_register(cp[0])):
                self.error("can't CPY {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't CPY {}: not a register".format(cp[1]))
            else:
                self.registers[cp[0]] = self.registers[cp[1]]
        elif (ci == "ALB"):
            self.buf += "\n"
        elif (ci == "AND"):
            if (not self.is_register(cp[0])):
                self.error("can't AND {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't AND {}: not a register".format(cp[1]))
            else:
                self.registers["RES"] = (self.registers[cp[0]] & self.registers[cp[1]])
        elif (ci == "IOR"):
            if (not self.is_register(cp[0])):
                self.error("can't IOR {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't IOR {}: not a register".format(cp[1]))
            else:
                self.registers["RES"] = (self.registers[cp[0]] | self.registers[cp[1]])
        elif (ci == "XOR"):
            if (not self.is_register(cp[0])):
                self.error("can't XOR {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't XOR {}: not a register".format(cp[1]))
            else:
                self.registers["RES"] = (self.registers[cp[0]] ^ self.registers[cp[1]])
        elif (ci == "NOT"):
            if (not self.is_register(cp[0])):
                self.error("can't NOT {}: not a register".format(cp[0]))
            else:
                self.registers["RES"] = (~self.registers[cp[0]]) & 0xFFFF
        elif (ci == "CMP"):
            if (not self.is_register(cp[0])):
                self.error("can't CMP {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't CMP {}: not a register".format(cp[1]))
            elif (self.registers[cp[0]] > self.registers[cp[1]]):
                 self.registers["RES"] = 0x0001
            elif (self.registers[cp[0]] == self.registers[cp[1]]):
                self.registers["RES"] = 0x0000
            elif (self.registers[cp[0]] < self.registers[cp[1]]):
                self.registers["RES"] = 0xFFFF
        elif (ci == "JGT"):
            if (not self.is_constant(cp[0])):
                self.error("can't JGT to {}: invalid constant".format(cp[0]))
            elif (self.registers["RES"] == 0x0001):
                self.registers["PTR"] = int(cp[0], 16) - 1
        elif (ci == "JLT"):
            if (not self.is_constant(cp[0])):
                self.error("can't JLT to {}: invalid constant".format(cp[0]))
            elif (self.registers["RES"] == 0xFFFF):
                self.registers["PTR"] = int(cp[0], 16) - 1
        elif (ci == "JEQ"):
            if (not self.is_constant(cp[0])):
                self.error("can't JEQ to {}: invalid constant".format(cp[0]))
            elif (self.registers["RES"] == 0x0000):
                self.registers["PTR"] = int(cp[0], 16) - 1
        elif (ci == "RTM"):
            if (not self.is_register(cp[0])):
                self.error("can't RTM {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't RTM {}: not a register".format(cp[1]))
            else:
                self.mem[self.registers[cp[0]]] = self.registers[cp[1]]
        elif (ci == "MTR"):
            if (not self.is_register(cp[0])):
                self.error("can't MTR {}: not a register".format(cp[0]))
            elif (not self.is_register(cp[1])):
                self.error("can't MTR {}: not a register".format(cp[1]))
            else:
                self.registers[cp[0]] = self.mem[self.registers[cp[1]]]
        elif (ci == "GET"):
            if (not self.is_register(cp[0])):
                self.error("can't GET {}: not a register".format(cp[0]))
            elif (self.is_register_readonly(cp[0])):
                self.error("can't GET {}: read-only register".format(cp[0]))
            else:
                self.registers[cp[0]] = ord(getch.getche())
        elif (ci == "LOD"):
            if (not self.is_register(cp[0])):
                self.error("can't LOD {}: not a register".format(cp[0]))
            else:
                self.mem = [0 for i in range(0x10000)]
                try:
                    with open("files/{:04X}".format(self.registers[cp[0]])) as f:
                        for line in f:
                            index, value = [int(i, 16) for i in line.split(" ")]
                            if (index >= 0 and index < 0x10000 and value >= 0 and value < 0x10000):
                                self.mem[index] = value
                except:
                    pass
        elif (ci == "SAV"):
            if (not self.is_register(cp[0])):
                self.error("can't SAV {}: not a register".format(cp[0]))
            else:
                try:
                    with open("files/{:04X}".format(self.registers[cp[0]]), "w") as f:
                        for i in range(0x10000):
                            if (self.mem[i] != 0):
                                f.write("{:04X} {:04X}\n".format(i, self.mem[i]))
                except:
                    pass

        # End the cycle
        self.context = Context()
        self.registers["CLK"] += 1
        self.registers["PTR"] += 1
        time.sleep(1/self.instruction_per_second)

    def debug(self, msg):
        if (self.debugmode):
            print("DBG: {}".format(msg))

    def error(self, msg):
        print("ERR: {}".format(msg))
        if self.running: print("CTX: RES={:04X}, PRT={:04X}, CLK={:04X}, G01={:04X}, G02={:04X},\n     G03={:04X}, G04={:04X}".format(self.registers["RES"], self.registers["PTR"], self.registers["CLK"], self.registers["G01"], self.registers["G02"], self.registers["G03"], self.registers["G04"]))
        exit(1)
#        raise Exception(msg + str(self.registers))

    def is_param_count_ok(self, expect, params):
        return (len(params) >= expect)

    def is_register_readonly(self, register):
        return (register in self.readonly_registers)

    def is_register(self, register):
        return (register in self.registers.keys())

    def is_constant(self, constant):
        return (len(constant) == 4 and all(c in "0123456789ABCDEF" for c in constant))

