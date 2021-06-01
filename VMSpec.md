# Register Machine -- Draft

## Hardware

A register machine has some hardware components to achieve the functionality of compute. Basically there two parts, CPU and Memory. We store our program and data in Memory for CPU to execute. CPU will follow the instruction stored in Memory one by one until the program is finished.

### CPU

CPU will simply run the instruction he got, until the program is finised. It mainly has three parts, Controller, Registers and ALU. Controller will parse the instruction and execute it using registers, ALU and Memory. Registers stores the data need to be computed. ALU actually do the math.

#### Controller

Controller accepts a set of instructions, and controls how the input data is passed to the ALU and how the output is stored in register.

#### Registers

there a branch of registers. some of them has specific purpose, some of them can store any data you want. here is the register list.

- PC (Program Counter):
This register stores the current address of instruction.

#### ALU

- arithmatic operations like add sub mul div

- logistic operations like and or

### Memory

Memory is a linear space to store data and code.

```text
| Address | value |
| 0       | 42    |
| 1       | "b"   |
```

## Instruction Set

With a set of instructions, we can control the register machine to calculate. here is the instruction list:

- add, sub: arithmatic operations

- jump: jump to address (set PC register) if cond register is not ZERO

- set, get: set value or get value from/to certain address in memory

