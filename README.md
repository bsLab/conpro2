# ConPro2

*Concurrent Processing and  Programming of Digital Logic with High-level Synthesis*

# Concurrent Programming and Processes

## Overview

> Q: How can complex algorithms be mapped on microchip level?<br> 
> A: By specifying algorithms with a high-level parallel programming language and by using a higher-level synthesis approach!

The ConPro framework consists of a parallel programming language and a synthesis tool able to map the programming level to a hardware behaviour model on RTL supporting parallel data processing and different software models simulating parallel data processing.

## Introduction

### ConPro Programming Language and Synthesis

The ConPro programming language, a new enhanced imperative programming language, was developed to map the programming to Register-Transfer level (RTL) using a higher-level-synthesis approach performed by the synthesis tool ConPro.
In contrast to other approaches using modified existing software languages like C, this language is designed from scratch providing a consistent model for both hardware design and software programming. The programming model and the language provide parallelism on control path level using a multi-process model with communicating sequential processes (CSP), and on data path level using bounded program blocks.

Each process is mapped to a Finite-State Machine (FSM) and is executed concurrently. Additionally, program blocks can be parameterized and can control the synthesis process (scheduling and allocation).
Synthesis is based on a non-iterative, multi-level and constraint selective rule-set based approach, rather than on a traditional constrained iterative scheduling and allocation approach.

Required inter-process communication is provided by a set of primitives, entirely mapped to hardware, already established in parallel software programming (multi-threading), implemented with an abstract data type object model and method-based access.
It can be demonstrated that this synthesis approach is efficient and stable enough to create complex circuits reaching the million gates boundary.

### Modelling and Implementing Parallel Data Processing

Concurrency can be either modeled explicitly (not transparent) or implicitly (transparent) by the synthesis tool:

Explicit Parallelism
: The programming model explicitly describes parallelism which means the programmer is responsible for modeling concurrency using for example processes or threads and synchronization primitives. Usually this is the preferred method for exploration of coarse-grained parallelism, which requires partitioning on algorithmic level, well done by the programmer, rather by the synthesis tool. No further computational effort must be made by the synthesis tool.

Implicit Parallelism
: The compiler tries to explore and derive parallelism from an initially sequential program specification, described with an imperative language, or using functional languages with (hidden) inherent concurrency [SHA98]. Mostly, concurrency is derived from loops using unroll techniques with allocation of resources in parallel, but concurrency can be explored in basic blocks of data-independent expressions, too. For example, both expressions x ← x +1 and y ← y + 1 can be scheduled (using RTL only) in one time step requiring two adders. Usually this is the preferred method for exploration of fine-grained parallelism on data path level. High computational effort must be made for balancing area and time constraints, usually done with an iterative approach [KU92].

There are several advantages of the explicit concurrency model versa the implicit model derived from initially pure sequential code, found in most extended C-like approaches [HLS08], especially in the context of reactive systems. Knowledge based modeling of concurrency can lead to higher degree of concurrency. A multi-process model with communicating sequential processes provides a concise way, 1. to directly map imperative programming languages to RTL, and 2. to provide parallelism on control path level. The multi-process model requires explicit synchronization, shown in figure → Figure 8. Interaction between processes, mainly access of shared resources, is request-acknowledge based.

Figure 8: The multi-process model with request-based synchronization (IPC).

### Interprocess Communication

Concurrency on control path level requires synchronization [AND00] . At least the access of shared resources must be protected using mutual exclusion locks (mutex). Access of all global objects is implicitly protected and serialized by a mutex scheduler. IPC and external communication objects are abstract object types, they can only be modified and accessed with a defined set of methods υ={υ1,υ2,…}, shown in table → Table 1. Queues and channels can be used in expressions and assignments like any other data storage object.

| IPC Object ℑ | Description | Methods υ |
|-|-|-|
| mutex | Mutual Exclusion Lock | lock, unlock |
| semaphore | Counting Semaphore | init, up,down |
| barrier | Counting Barrier | init, await |
| event | Signal Event | init, await, wakeup |
| timer | Periodic Timer Event | init, set, start, stop, await |
| queue (*) | FIFO queue | read, write |
| channel (*) | Handshaken Channel | read, write |

[Table 1: Available IPC objects. Queues and channels belong both to the core and abstract object class, too, and can be used within expressions and assignments (*)]

### Programming Language

The ConPro programming language consist of two classes of statements: 1. process instructions mapped to FSM/RTL, and 2. type, and object definitions. It is an imperative programming language with strong type checking. Imperative programs which describe algorithms that execute sequentially from one statement to the next, are familiar to most programmers. But beneath algorithmic statements the programming language must provide some kind of relation to the hardware circuit synthesized from the programming level.
The syntax and semantics of the programming language is consistently designed and mostly self-explanatory, without cryptic extensions, required in most hardware C-derivatives, like Handel-C or System-C, providing easy access to digital circuit development, also for software programmer.

Additionally there is a requirement to get full programmability of the design activities themselves, that means of the synthesis process, too [RU87], implemented here with constrained rules on block level, providing fine-grained control of the synthesis process. The synthesis process can be parameterized by the programmer globally or locally on instruction block level, for example scheduling and allocation.

The set of objects is splitted into two classes: 1. data storage type set ℜ, and 2. abstract data object type set (ADTO) Θ, with a subset of the IPC objects ℑ. Though it is a traditional imperative programming language, it features true parallel programming both in control and data path, explicitly modelled by the programmer.
Processes provide parallelism on control path level, whereby arbitrary nested bounded blocks inside processes provide parallelism on data path level.

There is an extended interface to connect to external hardware objects.

### Object and Data Types

The set of object types α contains storage ℜ, signals ℘, and abstract objects Θ={ℑ,D,E}: α={ℜ,Θ}. The set D contains data computational objects, for example, random generators and DSP units, and the set E contains external communication objects. Some object definitions are shown in example → Example 2.
Data storage can be implemented with single registers or with variables sharing one or more memory blocks. Choosing one of these object types is a constraint for synthesis, not a suggestion (in contrast to software programming). Registers provide concurrent-read-exclusive-write (CREW) access behaviour, whereby variables provide only exclusive-read-exclusive-write access behaviour (EREW).

Both data storage types can be defined locally on process level or globally on module level. Both registers and variables are true bit-scaled, that means any width ranging from 1 to 64 bit can be used. In the case of variable storage, the data width of the associated memory block is scaled to the widest object stored in this block. Fragmented variable objects are supported.

Beneath data storage objects which can be used within expressions (read and write access), there are abstract objects, which can be accessed with a set of methods υ only. Each abstract object belongs to a module definition and implementation (Module-O), which must be opened before first object definition. A method is applied to an object using the selector operator and a list of arguments passed to method parameters (or empty list for pure reactive methods): Θ.υ(arg1,arg2,...).

An object definition (≡resource allocation) requires the specification of data and object type, β and α, respectively.
The signal type ℘ provides access and control on hardware level. There is no behaviour model behind this signal (in contrast to VHDL), it is just a connection wire with a specified logical signal level and a direction. The signal object can be used in expressions and assignments and provides read and write access like any other storage object, though write access is temporal limited to the activity window of the assignment. The signal type appears in component structures, too.

A strong typed expression model is provided. There is a set of core data types: β={logic, int, bool, char}. Product types, both structures and arrays, can be defined to provide user-defined types.

A structure contains different named elements with defined data types β. The structure type must be defined before an object of this type can be defined: type T: { E1: β1; E2: β2; ...}.
The object type α (register, variable or signal) is associated during object definition. For each structure element a separate storage element is created.

Array definitions consist of object and cell data type specifications in the form: array A: α[N] of β. Arrays can be accessed dynamically selected. In the case of register or object arrays, index-selected multiplexer and demultiplexer are created. Multi-dimensional storage arrays and arrays of abstract objects including processes are supported.
Array and structure cells are accessed using the selector operator already introduced for method access: O.E for structures, and O.[I] for arrays.

### Expressions and Assignments

Expressions contain data storage objects, constants, and operators. Supported are all common arithmetic, Boolean (logical), and relational operators. Most of them are directly mapped to hardware behaviour level (VHDL operators). Initially, assignments to data storage objects are scheduled in one time step, and the order of a sequence of assignments is preserved. A sequence of data-independent assignments can be bound to one time unit either explicitly by the programmer (bounded block), or implicitly evaluated by the basicblock scheduler (preserving data dependencies, but violating sequence order). A semicolon (without further scheduling constraints) schedules an assignment, whereby a colon separated list binds assignments to one time unit, shown in Example 1, e.g. RTL scheduling originally proposed by Barbacci [BAR73]. A time unit requires at least one clock cycle, but can consume more clock cycles in the case of access of guarded (shared) objects. Depending on selected synthesis rules, there are different expression models which can be set on block level using the parameter: expr={“flat","binary","shared"}. The flat model maps operators of a (nested) expression 1:1 to hardware blocks (no shared resources), the binary mode splits nested expressions into single two-operand subexpressions using temporary registers, improving combinational path delay, and the shared model provides resource sharing of functional operators using ALUs.

```
process p1:
begin
 a←1, b←3, z←x-1; Bounded instruction block ⇔
 begin
 a←1; b←3; z←x-1;
 end with bind; Bounded instruction block, too
 x←(a+b)*4;
end;
process p2:
begin
 a←1; b←3; z←x-1;
 x←(a+b)*4;
end with schedule="basicblock";
```
[Example 1: Example of assignments. Lines 3 and 5..9 (parameterized block) reflect equivalent syntax for concurrent statements with identical behaviour. Automatic basic block scheduling is applied to the second process (parameterized process body block).
Control Statements]

There are conditional branches, both Boolean and multi-value branching, conditional, unconditional and counting loops, conditional blocking wait-for statements, function calls, and exceptions. Exceptions are abstract (symbolic) signals which can be raised anywhere inside a process, and caught either inside the process where the signal is raised, or outside from any other process calling this respective process. Exceptions are propagated across process boundaries. Exceptions are the only structural way to leave a control environment, there is no break, continue or goto statement. Tables → Table 2 and → Table 3 summarizes available control statements and their impact on the control path Γ.

The unconditional always-loop is used for request-based server loops (infinite loop). The wait-for statement is used usually with signals, checking the value of an expression of signals and optionally applies a statement (usually signal assignments) until the condition E changes to value true. Because signals are assigned a value to only as long as the assignment is active, a default value can be specified with the optional else-branch. Also time delays can be implemented with the wait-for statement. Is the clock frequency of the system is known, time can be specified in second units.


| Syntax | Statement | Description |
| - | - | - |
| `if E then A1 [else A2];` | Boolean Conditional Branch (false-branch optional) | If the boolean condition E is true, the instruction or instruction sequence A1 is executed, else A2 if present. |
| `match E with begin when E1: A1;  when E2: A2; ... end;` | Multi-Value Conditional Branch | The expression E is compared with given values Ei associated with an instruction Ai (or instruction sequence) executed in the case E=Ei. |

[ Table 2:Available branch statements and impact on state change σ in control path Γ ]

| Syntax | Statement |
|-|-|
| `while E do A; always do A;` | Conditional and unconditional Loop  |
| `for i = a to b do A;` |  Counting Loop (to or down-to direction) |
| `waitfor E [with A1 else A2];` | Conditional Delay (E can be time condition) |
| `try A with begin when ε1: A1; when ε2: A2; ... end;` | Exception Catcher (A with raise ε statements) |

[ Table 3:Available loop statements and impact on state change σ in control path Γ ]

User-defined functions can be implemented in two different ways: 1. as in-lined not-shared function macros and 2. as shared function blocks. In the first case, the function call is replaced by all function instructions, and function parameters are replaced by their respective arguments. In the second case, a function is modelled using the described process with an additional function control block containing a function call lock bound to an access scheduler and registers required for passing function arguments to parameters and returning results. Only call-by-value arguments of atomic objects can be used. The remaining functionality is provided by the underlying process model using the call method.
Functions are restricted to non-recursive calls due to a missing stack environment.
RTL Architecture
Each high-level process is mapped to a FSM and RTL, shown in figure →Figure 9. Process instructions are mapped to states of the FSM. Figure 11 shows the process system interconnect using signals. Access of objects is request-based and requires a request signal fed into a mutex-guarded access scheduler, responsible for serialization of concurrent access by different processes. A guard signal is read by the process FSM, providing a simple and efficient two-signal handshake (REQ↔ACT).
The ConPro synthesis tool maps programming level processes to hardware components (entities in VHDL terminology), each consisting of 1. a FSM (state register and state transition network), 2. combinational data path of RTL (data path multiplexer, demultiplexer, functional units) and 3. transitional data path of RTL (data path multiplexer, demultiplexer, functional units, and local registers), shown in figure → Figure 18. The process block interface and system interconnect shown in figure → Figure 10 require different signals for the control and data path. Shared objects can be connected to different processes, requiring control signals for atomic access (called guards). All processes and objects are sourced by one system clock and reset signal, thus all functional blocks operate synchronously.
Figure 9: The process implementation on hardware architecture level.
Figure 10: The process block interface and system interconnect.
The scheduler interconnect is shown in graph → Graph 1. There are two levels of hand-shakend synchronization: between each process i and the scheduler: {REQ-i,GD-i}, and between the scheduler and the object implementation block: {ACT,ACK}.
Graph 1: Access scheduler block architecture connecting both the port interface of processes (REQ and GD signals) and the interface of shared object (hardware) implementation blocks (activation and acknowledge signals ACT/ACK and possible external IO connections).
Example
Example → Example 2 shows a complete ConPro design. It is the implementation of the dining philosophers problem using semaphores. Five philosophers sit around a circular table. Each philosopher spends his life alternately thinking and eating. In the center of the table is a large platter of spaghetti. Each philosopher needs two forks two eat. But there are only five forks for all. One fork is placed between each pair of philosophers, and they agree that each will use only the forks to the immeadiate left and right [AND00], here implemented with a semaphore array fork.
The read ports of the shared registers eating and thinking are exported to the module toplevel port. The design consists of seven processes. The philosophers are implemented with the process array philosopher. Gate-level synthesis with a standard cell technology [SXLIB, LIP6] results in a circuit with 3919 gates, 235 D-flip-flops, and an estimated longest combinational path of 17 ns (55 MHz maximal clock frequency).

```
open Core;
open Process;
open Semaphore;
open System;
open Event;
object sys: system;
 sys.simu_cycles (500);
object ev: event;
array eating,thinking: reg[5] of logic;
export eating,thinking;
array fork: object semaphore[5] with depth=8 and scheduler="fifo";
process init:
begin
 for i = 0 to 4 do fork.[i].init (1);
 ev.init ();
end;
function eat(n):
begin
 begin
 eating.[n] ← 1;
 thinking.[n] ← 0;
 end with bind;
 wait for 5;
 begin
 eating.[n] ← 0;
 thinking.[n] ← 1;
 end with bind;
end with inline;
array philosopher: process[5] of
begin
 if # < 4 then
 begin
 ev.await ();
 always do
 begin
 -- get left fork then right
 fork.[#].down ();
 fork.[#+1].down ();
 eat (#);
 fork.[#].up ();
 fork.[#+1].up ();
 end;
 end
 else
 begin
 always do
 begin
 -- get right fork then left
 fork.[4].down ();
 fork.[0].down ();
 eat (#);
 fork.[4].up ();
 fork.[0].up ();
 end;
 end;
end;
process main:
begin
 init.call ();
 for i = 0 to 4 do
 begin
 philosopher.[i].start ();
 end;
 ev.wakeup ();
end;
```
[Example 2: A complete ConPro example: the dininig philosopher problem.]

### Bibliography
[SHA98]
: Richard Sharp
 Higher-Level Hardware Synthesis
 Springer, 1998

[KU92]
: David C. Ku, Giovanni De Micheli
 High Level Synthesis of ASICs Under Timing and Synchronization Constraints
 Kluwer, 1993

[HLS08]
: Philippe Coussy, Adam Morawiec (Ed.)
 High-Level Synthesis – from Algorithm to Digital Circuit
 Springer 2008

[AND00]
: Greg Andrews
 Multithreaded, Parallel, and Distributed Programming
 Addison Wesley, 2000

[RU87]
: Steven M. Rubini
 Computer Aids For VLSI Design
 Addision Wesley 1987

[BAR73]
: Mario R. Barbacci
 Automated Exploration of the Design Space For Register Transfer (RT) Systems
 Thesis, 1973, Carnegie-Mellon-University

# Download

ConPro
: Version 2.1, Development Release D197,
(Binaries for Windows, Linux, Solaris)<br>
[ZIP](http://sblab.de/download/ConPro-2.1-D197.all.zip "Download Software, All Binaries")

Example 1
: Memo Game with Numato Lab Mima Board<br>
[ZIP](http://sblab.de/download/cp_memo.zip "Download ConPro Example")
