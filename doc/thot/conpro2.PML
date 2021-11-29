<Book id="0" ssch="Book" psch="BookP" $Equation_Numbers="1" $Algorithm_Numbers="1" $Graph_Numbers="1" $Plot_Numbers="1" $Definition_Numbers="1" $Example_Numbers="1" $Figure_Numbers="1" $Table_Numbers="1" $Appendix_Numbers="4" $Section_Numbers="1" $TableOfContent_label="Table of Content" $Keywordindex_label="Index" $Bibliography_label="References" $Appendix_label="Appendix" $Algorithm_label="Algorithm" $Graph_label="Graph" $Plot_label="Plot" $Definition_label="Definition" $Example_label="Example" $Equation_label="Equation" $Figure_label="Figure" $Table_label="Table" $SectionFileDepth="1" $Color_sty="2" $Text_sty="1" $Size_sty="2" $Language_sty="2" $Langue="EN">
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="1" PageNumber="1">
</PAGE_BREAK>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="1" PageNumber="1">
</PAGE_BREAK>
<Title id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
THE 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
ConPro
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 BOOK
</TEXT_UNIT>
</Simple_paragraphe>
</Title>
<SubTitle id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="131" lang="2">
Rule-Based Mapping of an Imperative Programming Language to RTL for Higher-Lev
el-Synthesis Using Communicating Sequential Processes
</TEXT_UNIT>
</Simple_paragraphe>
</SubTitle>
<Author id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
Dr. Stefan Bosse
</TEXT_UNIT>
</Simple_paragraphe>
</Author>
<Author_address id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
BSSLAB 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
·
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="33" lang="0">
 Independent Research Laboratory 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
·
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 Bremen
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Simple_paragraphe>
</Author_address>
<Publ_Date id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
ConPro
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
·
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 V2.1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
·
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
 22.1.2010 
</TEXT_UNIT>
</Simple_paragraphe>
</Publ_Date>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
Introduction and Overview
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1394" lang="2">
The ConPro programming language, a new enhanced  imperative programming langua
ge is mapped to Register-Transfer-Logic using a  higher-level-synthesis approa
ch performed by the synthesis tool ConPro. In contrast to other approaches usi
ng modified existing software languages like C, this language is designed from
 scratch providing a consistent model for  both hardware design and software p
rogramming. The programming model and the language provide parallelism on cont
rol path level using a multi-process model with communicating sequential proce
sses (CSP), and on data path level using bounded program blocks. Each process 
is mapped to a Finite-State-Machine and is executed concurrently. Additionally
, program blocks can be parameterized and can control the synthesis process (s
cheduling and allocation). Synthesis is  based on a non-iterative, multi-level
 and constraint selective rule-set based  approach,  rather than  on a traditi
onal constrained iterative scheduling and allocation approach.  Required  inte
rprocess communication is provided by a set of primitives, entirely mapped to 
hardware, already established in concurrent softwareprogramming (multi-threadi
ng), implemented with an abstract data type object model and method-based acce
ss. It is demonstrated that this synthesis approach is efficient and stable en
ough to create complex circuits reaching the million gates boundary.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Higher-Level-Synthesis
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
Multi-Process-Architecture
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
Scheduling & Allocation
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Rule Based Scheduling
</TEXT_UNIT>
</Section_title>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="2">
</PAGE_BREAK>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Language Specification
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Introduction
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="514" lang="2">
The ConPro programming language consist of two classes of statements: 1. proce
ss instructions mapped to FSM/RTL, and 2. type, and object definitions. It is 
an imperative programming language with strong type checking. Imperative progr
ams which describe algorithms that execute sequentially from one statement to 
the next, are familiar to most programmers. But beneath algorithmic statements
 the programming language must provide some kind of relation to the hardware c
ircuit synthesized from the programming level.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="284" lang="2">
The syntax and semantics of the programming language is consistently designed 
and mostly self-explanatory, without cryptic extensions, required in most hard
ware C-derivates, like Handel-C or System-C, providing easy access to digital 
circuit development, also for software programmer.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="142" lang="2">
Additionally there is a requirement to get full programmability of the design 
activities themselves, that means of the synthesis process, too 
</TEXT_UNIT>
<Ref_Biblio id="0" ssch="Book" psch="BookP" ref="2b64720">
</Ref_Biblio>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="263" lang="2">
,  implemented here with constrained rules on block level, providing fine-grai
ned control of the synthesis process. The synthesis process can be parameteriz
ed by the programmer globally or locally on instruction block level, for examp
le scheduling and allocation.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="74" lang="2">
The set of objects is splitted into two classes: 1. data storage type set 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Â
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
, and 2. abstract data object type set (ADTO) 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
, with a subset of  the IPC objects 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Á
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="170" lang="2">
. Though it is a traditional imperative programming language, it features true
 parallel programming both in control and data path, explicitly modelled by th
e programmer. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="149" lang="2">
Processes provide parallelism on control path level, whereby arbitrary nested 
bounded blocks inside processes provide parallelism on data path level.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="72" lang="2">
There is an extended interface to connect to external hardware objects. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Modules and Processes
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="546" lang="2">
A ConPro design hierarchy consists of a behavioural module level (Module-B) co
ntaining global (shared) objects and processes. A module is mapped to a circui
t component with a toplevel hardware port interface. Structural modules (Modul
e-S) can be composed of behavioural modules with optional internal interconnec
t components. Each process (process level) consists of local (non-shared) obje
cts and a process instruction sequence, specifying the control and data flow. 
Abstract object types are implemented with abstract object modules (Module-O).
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
Behavioural Modules
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="234" lang="2">
Behavioural modules implement objects and processes. A behavioural module  is 
defined by the source code file itself. Actually there is only one module hier
archy level, the main module. More source code file can be included using the 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="7" lang="2">
include
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
 statement. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="3">
</PAGE_BREAK>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="353" lang="2">
There are two kinds of modules: a module embedding objectes and processes, and
 modules providing access and implementation of abstract data type objects (AD
TO). These are mainly interprocess communication and synchronization objects, 
for example mutex, sempahore, timer and some communication links. Each ADTO mo
dule to be used must be opened using the 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="4" lang="2">
open
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
 statement.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="0" ssch="Book" psch="BookP" $BNF="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
module-b ::= module-b-impl .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
module-b-impl ::= { include-source }+ |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
           { open-module }+ |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
           { object-definition }+ |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
           { process }+ . 
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
include-source ::= 'include' '"' name '"' ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
open-module ::= 'open' name ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="53" lang="2">
Formal syntax specification of a behavioural  module.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
Abstract Object Modules
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="278" lang="2">
This module  provides access and implementation of abstract data type objects 
(ADTO). These are mainly interprocess communication and synchronization object
s, for example mutex, sempahore, timer and some communication links. Each ADTO
 module to be used must be opened using the 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="4" lang="2">
open
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="75" lang="2">
 statement. They are defined by the External Module Interface (see section 
</TEXT_UNIT>
<Ref_Section id="0" ssch="Book" psch="BookP" ref="2b64550">
</Ref_Section>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
).
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
Structural Modules
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="92" lang="2">
Structural modules are used to build System-On-Chip (SoC) circuits from behavi
oural modules.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="0" ssch="Book" psch="BookP" $BNF="1" $FLOAT="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
module-s ::= 'module' name 'begin' module-s-impl 'end' ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
Formal syntax specification of  a structural module.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="4">
</PAGE_BREAK>
<Table id="0" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
module 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
MS
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="6" lang="0">
import
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="9" lang="0">
component
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="10" lang="0">
structtype
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="7" lang="0">
mapping
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
Defines a new structural module with specified name.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
import 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
MB
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
component 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="8" lang="2">
C1,C2,..
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
:
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
MB
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="72" lang="2">
Import of behavioural modules and instantiation of components (ciruits).
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
type 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
ICT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
:{ port...}; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
component 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
IC
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
:
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
ICT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 :=
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
{
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
C1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
TOP
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
S1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
C1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
TOP
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
S2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
,...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
C2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
TOP
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
S1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
,...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
};
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="95" lang="2">
Defines and instantiate a port interface of interconnect component with initia
l signal mapping.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
IC.S1 >> IC.S2;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
Internal interconnect using mapping statements 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
Summary of module-s definition and interconnect.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Core;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
open Process;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Link;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
open System;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
type dev_type:{
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
  port ln_din: input logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
  port ln_din_ack: output logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
  port ln_dout: output logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
  port ln_dout_ack: input logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
};
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
component DEV: dev_type;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
export DEV;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
object sys: system;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  sys.simu_cycles(100);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
-- Async. Link
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
object ln: link with datawidth=4;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="70" lang="2">
  ln.interface(DEV.ln_din,DEV.ln_din_ack,DEV.ln_dout,DEV.ln_dout_ack);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
reg xa: int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
export x,xa;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
exception Exit;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="5">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
-- Producer-Consumer process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p1:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  reg err:bool;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  reg d:logic[4];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  d 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  try
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
    for i = 1 to 10 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
      xa 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
 'w',x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
 to_int(d);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
      ln.write(d,err);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
      if err = true then raise Exit;  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
      xa 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 'r';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
      ln.read (d,err);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
      if err = true then raise Exit;  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
      x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
 to_int(d),d 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
 d + 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  with
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
    when Exit: ln.stop ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  xa 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 '.';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
process main:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  ln.init ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  ln.start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  p1.start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
-- SoC: two identical blocks Lc1 and Lc2 containing 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="66" lang="2">
-- one link ln, process p1 and main from this module Link_simu are
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
-- cross linked (Lc1: dout => Lc2: din, Lc2:dout => Lc1:din)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
module Link2_simu:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  --
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
  -- Instantiate components, import
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  -- THIS toplevel module.
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  --
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  import Link_simu;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
  component Lc1,Lc2: Link_simu;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  --
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
  -- Structural Interconnection
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  --
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  type l_connect: {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
    port Lc1_ln_din: output logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
    port Lc1_ln_din_ack: input logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
    port Lc1_ln_dout: input logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
    port Lc1_ln_dout_ack: output logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
    port Lc2_ln_din: output logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
    port Lc2_ln_din_ack: input logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
    port Lc2_ln_dout: input logic[8];
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="6">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
    port Lc2_ln_dout_ack: output logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  };
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
  component L_c: l_connect :=
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
                  {Lc1.DEV.ln_din,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
                   Lc1.DEV.ln_din_ack,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
                   Lc1.DEV.ln_dout,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
                   Lc1.DEV.ln_dout_ack,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
                   Lc2.DEV.ln_din,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
                   Lc2.DEV.ln_din_ack,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
                   Lc2.DEV.ln_dout,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
                   Lc2.DEV.ln_dout_ack};
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  --
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  -- Interconnect
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  --
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
  L_c.Lc1_ln_din << L_c.Lc2_ln_dout;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
  L_c.Lc1_ln_din_ack >> L_c.Lc2_ln_dout_ack;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
  L_c.Lc1_ln_dout >> L_c.Lc2_ln_din;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
  L_c.Lc1_ln_dout_ack << L_c.Lc2_ln_din_ack;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="218" lang="2">
Example of combined behavioural and structural module definitions. A SoC is co
mposed of two components Lc1 and Lc2, instantiated from behavioural module Lin
k_simu, the actual toplevel module (source file link_simu.cp).
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Processes
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="222" lang="2">
A process definition consists of a unique process name identifier and the proc
ess body. The process body consists of local object definitions (types, data a
nd some abstract objects) and an instruction sequence. Definition 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="266b3d8">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
 shows the formal specification, and table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="266b208">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="69" lang="2">
 summarizes and explains different process defintions and management.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="194" lang="2">
Array of processes can be defined using the array definition statement. To dis
tinguish processes of an array, a process can access its unique process identi
fier number (array index). Definition 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="2592f20">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="67" lang="2">
  shows the formal syntax definition for process array definitions.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="71" lang="2">
After definition, a process (itself an ADTO) must be started using the 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
start
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
 or 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="4" lang="2">
call
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
 method by another process (see table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="2592d50">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
), usually the main process (named 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="4" lang="2">
main
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="77" lang="2">
), which is the only process started some clock cycles after system reset.   
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="266b3d8" ssch="Book" psch="BookP" $BNF="1" $FLOAT="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
process ::= 'process' name ':' 'begin' process-body 'end' 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
            [ 'with' parameter-list ] ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
process-body ::= { object-definition }+
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
                 { process-instruction }+ .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="61" lang="2">
object-definition ::= type-definition | dataobj-definition | 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
                abstrobj-definition .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="53" lang="2">
Formal syntax specification of  a process definition.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="2592f20" ssch="Book" psch="BookP" $BNF="1" $FLOAT="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="71" lang="2">
process-array ::= ( 'array' name ':' 'process' '[' number ']' 'of' ) \>
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="7">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="51" lang="2">
                  ( 'begin' process-body 'end' ) \>
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="53" lang="2">
                  ( [ 'with' parameter-list ] ';' ) .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
process-array-id ::= '#' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
Formal syntax specification of  a process array definition.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Table id="266b208" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
process 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="11" lang="0">
definitions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="12" lang="0">
instructions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
Defines a new process with specified name.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
process 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="11" lang="0">
definitions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="12" lang="0">
instructions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
end with 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
param
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
=
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
value
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="85" lang="2">
Defines a new process with specified name. Additional parameter settings are a
pplied.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
array 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="6" lang="2">
paname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  process[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
size
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
] of
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="11" lang="0">
definitions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="12" lang="0">
instructions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
Defines a new array of 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="4" lang="2">
size
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
 different processes. Optional parameter settings can be applied.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
.start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
.stop ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
.[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
i
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
].start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
myid
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
#;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
Process control using ADTO methods and array selectors.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
Summary of process definitions and access.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="8">
</PAGE_BREAK>
<Table id="2592d50" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Method
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
.start ();
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
Start process 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
. The caller process will not be blocked.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
.stop ();
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
Stop process 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
. The caller process will not be blocked.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
.call ();
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
Call process 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="85" lang="2">
. The caller process will  be blocked untill the called process reaches it end
 state.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Summary of process  management.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
-- Dining philosophers problem using semaphores.
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="73" lang="2">
-- Five philosophers sit around a circular table. Each philosopher spends
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="76" lang="2">
-- his life alternately thinking and eating. In the center of the table is a
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="76" lang="2">
-- large platter of spaghetti. Each philosopher needs two forks two eat. But
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="74" lang="2">
-- there are only five forks for all. One fork is placed between each pair
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="75" lang="2">
-- of philosophers, and they agree that each will use only the forks to the
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
-- immeadiate left and right.
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="71" lang="2">
-- [Andrews 2000, Multihtreaded, Parallel, and Distributed Programming]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Core;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
open Process;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
open Semaphore;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
open System;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
open Event;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
object sys: system;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  sys.simu_cycles (500);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
object ev: event;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
array eating,thinking: reg[5] of logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
export eating,thinking;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
array fork: object semaphore[5] with depth=8 and init=1 and 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="54" lang="2">
                                     scheduler="fifo";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="7" lang="2">
process
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 init:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 0 to 4 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
    fork.[i].init ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  ev.init ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
function eat(n):
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="9">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
    eating.[n] <- 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
    thinking.[n] <- 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  end with bind;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  wait for 5;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
    eating.[n] <- 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
    thinking.[n] <- 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  end with bind;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
end with inline;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="5" lang="2">
array
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
 philosopher: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="7" lang="2">
process
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
[5] of
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="1" lang="2">
#
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
 < 4 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
   ev.await ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
   always do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
   begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
     -- get left fork then right
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
     fork.[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="1" lang="2">
#
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
].down ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
     fork.[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="1" lang="2">
#
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
+1].down ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
     eat (
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="1" lang="2">
#
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
     fork.[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="1" lang="2">
#
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
].up ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
     fork.[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="1" lang="2">
#
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
+1].up ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
   end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
   always do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
   begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
     -- get right fork then left
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
     fork.[4].down ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
     fork.[0].down ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
     eat (
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="1" lang="2">
#
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
     fork.[4].up ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
     fork.[0].up ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
   end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="7" lang="2">
process
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 main:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  init.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="4" lang="2">
call
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
 ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 0 to 4 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
    philosopher.[i].
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="5" lang="2">
start
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
 ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  ev.wakeup ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
An example showing process definitions and process arrays.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
Block Structures
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="259" lang="2">
The ConPro architecture is structured into modules and processes on top level.
 Each process consists of at least one block: the process body, which binds in
structions (and definitions) to the named process environment. Blocks, either 
on same level or nested, 
</TEXT_UNIT>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="10">
</PAGE_BREAK>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="161" lang="2">
can be used to bind instructions to a specific environment with different sche
duling and allocation strategies. Blocks can be parameterized, shown in defini
tion 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="2592178">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
 and table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="2472b40">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
. Additionally, blocks are used in control statements. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="173" lang="2">
In contrast to common imperative programming languages like C, there are no ob
ject defintions and hidden locality inside a block, except in function and pro
cess body blocks.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="74" lang="2">
Predefined block parameters and their possible values are listes in table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="2472970">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="118" lang="2">
. A parameter of type boolean can be used without a value. This assigns automa
tically the true value to the parameter.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="2592178" ssch="Book" psch="BookP" $BNF="1" $FLOAT="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
block ::= 'begin' { process-instruction }+ 'end' 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
            [ 'with' parameter-list ] ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="64" lang="2">
process-instruction ::= assignment | control-statement | block .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="56" lang="2">
parameter-list ::= param-name '=' param-value // 'and' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="51" lang="2">
Formal syntax specification of  a block definition.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Table id="2472b40" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="12" lang="0">
instructions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
Defines a new block containing instructions.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="12" lang="0">
instructions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
end with 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
param
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
=
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
value
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="71" lang="2">
Defines a new block containing instructions with additional parameters.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
expr
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 then 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
while 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
expr
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
 do 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
for 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
i
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 = 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
 to 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
b
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
 do 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="12" lang="0">
instructions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="107" lang="2">
Blocks used with control statements (conditional branch block, conditional loo
p block, counting loop block)
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
Summary of block definitions.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="11">
</PAGE_BREAK>
<Table id="2472970" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Parameter
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Values
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
bind
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
true,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
false
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
Instructions (only assignments) are bound to one time unit. 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
unroll
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
true,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
false
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Unrolls a counting for-loop.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
schedule
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
SL
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
[,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
SL
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
"
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
SL
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
=basicblock |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
   refstack |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
   expr
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="103" lang="2">
Specifies scheduling strategy and optimization (basic block, reference stack a
nd expression scheduler).
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
expr
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
EXPR
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
"
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
EXPR
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
=flat |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
     binary |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
     shared
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="78" lang="2">
Specifies expression model (flat is non-shared, ALU is shared resource model).
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
inline
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
true,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
false
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="107" lang="2">
Defines a function either be placed inline (macro substitution) or  implemente
d as a shared function block.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="87" lang="2">
Summary of block parameters. Highlighted values are default settings of each p
arameter.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
Data Objects and Data Types
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
True bit-scaled data types (
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
TYPE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
b
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
) 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
and storage objects (subset 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Â
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
 of TYPE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
) 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
 are supported. The data width can be choosen in the range 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
w
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
={1,2,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="72" lang="0">
,64} Bit. The formal syntax of object definition is shown in definition 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="2353798">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="57" lang="0">
, and both data and object types are described in tables 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="23535c8">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 and 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="23533f8">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
A data object 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Â
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="42" lang="0">
 is specified by a type cross product of (
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="3" lang="11">
a´b
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
). 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="2353798" ssch="Book" psch="BookP" $BNF="1" $FLOAT="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="61" lang="2">
dataobj-definition ::= obj-type ( name // ',' ) ':' data-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
  [ 'with' parameter-list ] ';' .  
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
obj-type ::= 'reg' | 'var' | 'sig' | 'queue' | 'channel' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
data-type ::=  'logic' |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
   'logic' '[' number ']'  |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
   'int' '[' number ']'  |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
   'bool'  |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
   'char'  .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="12">
</PAGE_BREAK>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
Formal syntax specification of  a data object definition.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Table id="23535c8" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Keyword
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
reg
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Register
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
Single register for CREW-access-behaviour
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
var
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Variable
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
Variable embedded in RAm block with EREW-access-behaviour
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
sig
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Hardware Signal
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="54" lang="2">
Interconnection signal, synthesizing always to a wire.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
queue
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
Data Queue
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
Buffered Queue (FIFO). Depth and data type can be specified.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
channel
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Data Channel
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="90" lang="2">
Buffered (Queue with depth=1) or unbuffered (withour register) synchronized da
ta exchange 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
Summary of provided set of  data (core) objects 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Â
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
, TYPE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="13">
</PAGE_BREAK>
<Table id="23533f8" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Keyword
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Type
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
logic
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Bit Value
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
Single bit type. This data type is commonly  synthesized to VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="9" lang="2">
std_logic
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
 type. 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Value set:Š L={
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="9" lang="2">
0,1,H,L,Z
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
}
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
logic[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
size
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
]
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
Unsigned Bit Vector
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="84" lang="2">
Bit vector type of data width size. This data type is commonly  synthesized to
 VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="33" lang="2">
std_logic_vector(size-1 downto 0)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 type.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Value set: ŠL={
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="9" lang="2">
0,1,H,L,Z
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
}
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
int[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
size
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
]
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Signed Integer Vector
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="108" lang="2">
Signed integer type of data width size (including sign bit). This data type is
 commonly synthesized to VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="23" lang="2">
signed(size-1 downto 0)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 type.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
Value set: I={
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="3" lang="2">
-n,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" !PRFont="3" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="8" lang="0">
,-1,0,1,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" !PRFont="3" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="3" lang="0">
n-1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
}
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
bool
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Boolean
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
Boolean type.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
This data type is commonly synthesized to VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="9" lang="2">
std_logic
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 type
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Value set:Š B={
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="14" lang="2">
0,1,true,false
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
}
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
char
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="120">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
Character (one byte)
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
8-bit vector (unsigned).
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
This data type is commonly  synthesized to VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="28" lang="2">
std_logic_vector(7 downto 0)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 type.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Value set:ŠC= {
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="3" lang="2">
'0'
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" !PRFont="3" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="7" lang="0">
'9','A'
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" !PRFont="3" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="4" lang="0">
'Z',
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" !PRFont="3" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
} 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
È
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
 I
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="51" lang="2">
Summary of provided set of data (core) types, TYPE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
b
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="14">
</PAGE_BREAK>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="172" lang="2">
There are local and global data objects. Global data objects are shared by sev
eral processes concurrently and require an access scheduler providing a mutual
 exclusion lock.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="220f6d0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Shared Objects and Scheduler
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="161" lang="2">
Access of shared objects muts be guarded inherently by a mutex using a mutual 
exclusion scheduler. This scheduler is responsible to serialize concurrent acc
ess. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
There are two different scheduler available:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
Static Priority Scheduler [default]
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="371" lang="2">
This is the simplest scheduler and requires the lowest amount of hardware reso
urces. Each process ever accessing the resource gets a unique ordered priority
. If there are different processes accessing the resource concurrently, the sc
heduler always grants access to the process with the highest priority. There i
s a risk of race conditions using this scheduling strategy.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="154" lang="2">
Commonly, the order of processes appearing in the source code determines their
 priority: the first process gets the highest priority, the last the lowest.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="54" lang="2">
A scheduled access requires at least two clock cycles.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Dynamic FIFO Scheduler
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="347" lang="2">
The dynamic scheduler provides fair scheduling using a process queue. Each pro
cess wanting to access the resource is stored in this FIFO ordered queue. The 
oldest one in the queue is choosen by the scheduler if the resource is release
d by the previous owner. The dynamic scheduler avoids race conditions, but req
uires much more hardware resources.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Graph id="0" ssch="Book" psch="BookP" $FLOAT="1" $GRAPH_width="100">
<Graph_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
digraph H {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  node [shape=record];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
    SCHEDULER [shape=record,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="85" lang="2">
    label="{SCHEDULER|{<r1>REQ-1|<g1>GD-1|<r2>REQ-2|<g2>GD-2|<r3>REQ-N|<g3>GD-
N}}"]; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
    PRO1 [shape=record,label="{{<rp1>REQ|<gp1>GD}|PRO-1}"];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
    PRO2 [shape=record,label="{{<rp2>REQ|<gp2>GD}|PRO-2}"];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
    PRO3 [shape=record,label="{{<rp3>REQ|<gp3>GD}|PRO-N}"];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
    SCHEDULER:g1 -> PRO1:gp1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
    SCHEDULER:g2 -> PRO2:gp2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
    SCHEDULER:g3 -> PRO3:gp3;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
    PRO1:rp1 -> SCHEDULER:r1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
    PRO2:rp2 -> SCHEDULER:r2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
    PRO3:rp3 -> SCHEDULER:r3;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
}
</TEXT_UNIT>
</Text_line>
</Graph_content>
<Graph_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Scheduler Block Architecture
</TEXT_UNIT>
</Simple_paragraphe>
</Graph_desc>
</Graph>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="15">
</PAGE_BREAK>
<Algorithm id="0" ssch="Book" psch="BookP" $FLOAT="1" $Box="8">
<Algorithm_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
if REQ-1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
LOCKED then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
  LOCKED
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
TRUE; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Raise ACT; 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="27" lang="2">
Start Service for Process 1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
else if REQ-2 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
LOCKED then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
  LOCKED
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
TRUE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Raise ACT; 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="27" lang="2">
Start Service for Process 2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
else if ACK 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 REQ-1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
 LOCKED then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  Release GD-1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 FALSE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
else if ACK 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 REQ-2 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
 LOCKED then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  Release GD-2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 FALSE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
</Text_line>
</Algorithm_content>
<Algorithm_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="416" lang="2">
Static Priority Scheduler: From/to process i:{REQ-i,GD-i}, from/to shared reso
urce block:{ACT,ACK}.  A process-i  request activates REQ-i, and if the resour
ce is not locked, the request is granted to the next process in the if-then-el
se cascade. If the request is finished, then ACK is activated and releases the
 locked object and releases GD-i for this respective process indicating that t
he request is finished.   
</TEXT_UNIT>
</Simple_paragraphe>
</Algorithm_desc>
</Algorithm>
<Algorithm id="0" ssch="Book" psch="BookP" $Box="8" $FLOAT="1">
<Algorithm_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
if REQ-1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
 LOCKED=[] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
PRO-1-LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
  LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
 [PRO-1];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  PRO-1-LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TRUE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
OWNER
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
PRO-1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Raise ACT; 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="27" lang="2">
Start Service for Process 1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
else if REQ-2 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
 LOCKED=[] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
PRO-2-LOCKED
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
  LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
 [PRO-2];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  PRO-2-LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TRUE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
OWNER
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
PRO-2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Raise ACT; 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="27" lang="2">
Start Service for Process 2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
else if REQ-1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 [] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
PRO-1-LOCKED then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
  LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
 LOCKED @ [PRO-1]; 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRStyle="3" len="25" lang="0">
Append Process 1 to Queue
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  PRO-1-LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TRUE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
else if REQ-2 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 [] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
PRO-2-LOCKED then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
  LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
 LOCKED @ [PRO-2]; 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRStyle="3" len="25" lang="0">
Append Process 2 to Queue
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  PRO-2-LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TRUE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
else if REQ-1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="20" lang="0">
 Head(LOCKED)=PRO-1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 OWNER
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
PRO-1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Raise ACT; 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="27" lang="2">
Start Service for Process 1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
OWNER
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
PRO-1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
else if REQ-2 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
 Head(LOCKED)=PRO-2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 OWNER
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
PRO-2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Raise ACT; 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="27" lang="2">
Start Service for Process 2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
OWNER
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
PRO-2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="16">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
else if ACK-1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="24" lang="0">
 Head(LOCKED)=PRO-1 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  Release GD-1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  PRO-1-LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 FALSE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
OWNER
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
NONE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
 Tail(LOCKED);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
else if ACK-2 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
 Head(LOCKED)=PRO-2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  Release GD-2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  PRO-2-LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 FALSE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
OWNER
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
NONE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  LOCKED 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
 Tail(LOCKED);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
</Text_line>
</Algorithm_content>
<Algorithm_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="467" lang="2">
Dynamic Queue Scheduler: From/to process i:{REQ-i,GD-i}, from/to shared resour
ce block:{ACT,ACK}. A process-i  request activates REQ-i, and if the resource 
queue is empty or this process is at head of the queue, the request is granted
 to the process in the if-then-else cascade. If the request is finished, then 
ACK is activated and removes the process from the resource queue  and releases
 GD-i for this respective process indicating that  the request is finished.  
</TEXT_UNIT>
</Simple_paragraphe>
</Algorithm_desc>
</Algorithm>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Registers
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="500" lang="2">
Registers are single storage elements either used as a shared global object or
 as a local object inside a process. In the case of a global object, the regis
ter provides concurrent read access (not requiring a mutex guarderd scheduler)
  and exclusive mutex guarded write access. If there is more than process tryi
ng to write to the register, a mutex guarded scheduler serializes the write ac
cesses. There are two different schedulers available: static priority and dyna
mic FIFO scheduled. See section 
</TEXT_UNIT>
<Ref_Section id="0" ssch="Book" psch="BookP" ref="220f6d0">
</Ref_Section>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
 for details.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Table id="220f500" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
reg 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
rname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
];
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
Defines a new register of specified type and width. 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
reg 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
rname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
stype
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
Defines a register of a structure type. 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
reg 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
rname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    with 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
param
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
=
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
value
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="64" lang="2">
Defines a register of specified type with additional parameters.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
rname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 = 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
expr
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 then 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="5" lang="0">
rname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="4" lang="0">
expr
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
Register used in expression and assignment.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
Summary of register definitions and access in expressions.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="17">
</PAGE_BREAK>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="91" lang="2">
Registers can be read in expressions, and can be wrote in assignments, summari
zed in table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="220f500">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
 and shown in example 
</TEXT_UNIT>
<Ref_Example id="0" ssch="Book" psch="BookP" ref="220f330">
</Ref_Example>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Example id="220f330" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
reg xs: int[10];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
reg ys: logic[11];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p1:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  reg x: int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  for i = 1 to 5 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
    x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 xs + x;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ys 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
 to_logic(x);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p2:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  xs 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 xs - 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  while ys < 10 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
    xs 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
 xs + to_int(ys);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="69" lang="2">
Example of global and local register definitions and register access.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Variables
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="231" lang="2">
Variables are storage elements inside a memory block either used as a shared g
lobal object (the memory block itself) or as a local object inside a process. 
A variable provides always  exclusive mutex guarded read and write access. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="291" lang="2">
Different variables concerning both data type and data width are stored in one
 memory block, which is mapped to a RAM. Address management is done automatica
lly during synthesis and is transparent to the programmer. Direct address refe
rences or manipulation (aka pointers) are not supported. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="229" lang="2">
The memory data width, always of type logic/bit-vector, is scaled to the large
st variable stored in memory. To reduce memory data width, variables can be fr
agmented, that means a variable is scattered about several memory cells. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="101" lang="2">
Different memory blocks can be created explicitly, and variables can be assign
ed to different blocks.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="18">
</PAGE_BREAK>
<Table id="0" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
var 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
rname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
];
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
Defines a new register of specified type and width. 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
var 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
rname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
stype
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
Defines a register of a structure type. 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
var 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
vname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    with 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
param
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
=
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
value
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="64" lang="2">
Defines a register of specified type with additional parameters.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
vname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 = 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
expr
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 then 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="5" lang="0">
vname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="4" lang="0">
expr
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
Variable used in expression and assignment.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
Summary of variable definitions and access in expressions.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="102" lang="2">
There are two different schedulers available: static priority and dynamic FIFO
 scheduled. See section 
</TEXT_UNIT>
<Ref_Section id="0" ssch="Book" psch="BookP" ref="220f6d0">
</Ref_Section>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
 for details.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="88" lang="2">
Variables can be read in expressions, and can be wrote in assignments, shown i
n example 
</TEXT_UNIT>
<Ref_Example id="0" ssch="Book" psch="BookP" ref="1f2f2f0">
</Ref_Example>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Example id="1f2f2f0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
block ram1,ram2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
var xs: int[10] in ram1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
var ys: logic[11] in ram1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
var zs: logic[11] in ram2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p1:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  reg x: int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  for i = 1 to 5 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
    x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 xs + x;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ys 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
 to_logic(x);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
  zs 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 ys * 2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p2:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  zs 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 ys;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ys 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 zs - 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  while ys < 10 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
    xs 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
 xs + to_int(ys);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
    ys 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 zs * 2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="53" lang="2">
Example of variable  definitions and variable access.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="19">
</PAGE_BREAK>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Signals
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="163" lang="2">
Signals are interconnection elements without a storage model. They provide an 
interface to external hardware blocks. Signals are used in component structure
s, too.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="97" lang="2">
Signals can be read in expressions, and a value can be assigned in assignments
, shown in example 
</TEXT_UNIT>
<Ref_Example id="0" ssch="Book" psch="BookP" ref="1f0f7a8">
</Ref_Example>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="256" lang="2">
. Reading a signal returns the actual value of a signal, but writing to a sign
al assigns a new value only for the time the assignment is active, otherwise a
 default values is assigned to the signal. Therefore, there may be only one as
signment for a signal.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
Signals can be used in conjunction with 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="4" lang="2">
wait
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="8" lang="2">
wait-for
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
 control statements.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Example id="1f0f7a8" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
type dev_type : {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
  port leds: output logic[4];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  port rd: input logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  port wr: output logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  port we: output logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  port act: input logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
};
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
component DEV: dev_type;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
export DEV;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
reg stat_leds: logic[4];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
DEV.leds << stat_leds;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
signal s1: int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
signal s2: logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
reg xs: int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
export s1,s2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p1:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  reg x: int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  stat_leds[0] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  for i = 1 to 5 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
    x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 x + s1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  xs 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 x;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  stat_leds[0] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p2:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  stat_leds[1] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 1 to 5 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
    wait for DEV.act = 1 with s2 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
    DEV.we 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 1,DEV.wr 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
 to_logic(xs);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  stat_leds[1] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="95" lang="2">
Example of signal definitions and signal access. Component structure elements 
are signals, too.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="20">
</PAGE_BREAK>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
Expressions and Assignments
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="276" lang="2">
Assignments transfer data from expression results to storage elements and they
 can be used with register, variables, queue and channels objects (with specia
l treatment for signals). An assignment finishing with a semicolon at the end 
is usually executed within one time unit. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="205" lang="2">
A group of  data independent assignments with up to one guarded object access 
can be executed within one time unit concurrently. All assignments of this gro
up are separated by a colon, shown in definition 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="1f0f5d8">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
. Tables 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="1ea3bf8">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
, 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="1dcc998">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
, 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="1dcc7c8">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="1a6a820">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
 summarize the available operations.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="1f0f5d8" ssch="Book" psch="BookP" $BNF="1" $FLOAT="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
single-assignment ::= data-obj '<-' expression ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="67" lang="2">
bounded-assignment ::= (( data-obj '<-' expression ) // ',' ) ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
expression ::= arith-expr | logic-expr | bool-expr | relat-expr .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="86" lang="2">
Formal syntax specification of  single and bounded assignments, executed concu
rrently.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Table id="1ea3bf8" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Data Type
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
a + b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Addition
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
a - b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Subtraction
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
-a
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Negation
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
a * b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
Multiplication
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
a / b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
Division
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
a asl n
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
Shifts operand 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="1" lang="0">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="1" lang="0">
n
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
 digits left.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
a asr n
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
Shifts operand 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="1" lang="0">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="1" lang="0">
n
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
 digits right.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
2 ^ n
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
n
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="24" lang="0">
, only constant values n
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
n ~ 2
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
log
</TEXT_UNIT>
</I_Main>
<I_R_Sub id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_R_Sub>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="25" lang="0">
(n), only constant values
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="90" lang="2">
Summary of available arithmetic operators. Supported data type set: {logic[N],
int[N],char}
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="21">
</PAGE_BREAK>
<Table id="1dcc998" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Data Type
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
a lor b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
Or-operation for each bit of 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="1" lang="2">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="1" lang="2">
b
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
a land b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
And-operation for each bit of 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="1" lang="2">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="1" lang="2">
b
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
a lxor b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
Xor-operation for each bit of 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="1" lang="2">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="1" lang="2">
b
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
lnot a
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Negation
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
a lsl n
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
Shifts operand 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="1" lang="0">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="1" lang="0">
n
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
 digits left.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
a lsr n
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
Shifts operand 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="1" lang="0">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="1" lang="0">
n
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
 digits right.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP" %PRVertPos="R0">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" %PRWidth="R160">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="85" lang="2">
Summary of available logic operators. Supported data type set: {logic[N],int[N
],char}
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Table id="1dcc7c8" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Data Type
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
a bor b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
Boolean or-operation
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
a band b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Boolean and-operation 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
a bxor b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Boolean xor-operation 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
bnot a
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Negation
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
Summary of available boolean operators. Supported data type: bool
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Table id="1a6a820" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Data Type
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
a < b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
Lower-than compare
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
a <= b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
Lower-than-equal compare
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
a > b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Greater-than compare  
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
a >= b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Greater-than-equal compare  
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
a = b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Equal compare  
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
a <> b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
Not-Equal compare  
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="87" lang="2">
Summary of available relational  operators. Supported data type: {logic[N],int
[N],char}
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="22">
</PAGE_BREAK>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Types
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
Objects are specified by their object type 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
 and a data type 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
b
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="196" lang="0">
. There are data type  and abstract type objects. User defined types providing
 oroduct types using arrays and structures and restricted sum types with enume
rated symbolic name lists are available.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Data Object Types
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="107" lang="2">
Data objects (including IPC objects queue and channel) can be used directly in
 expressions and assignments.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Description_list id="0" ssch="Book" psch="BookP">
<Description_item id="0" ssch="Book" psch="BookP">
<Description_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Data Object Types
</TEXT_UNIT>
</Description_name>
<Description_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="6" lang="0">
TYPE a
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="3" lang="0">
'={
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="26" lang="2">
reg,var,sig,queue,channel}
</TEXT_UNIT>
</Simple_paragraphe>
</Description_paragr>
</Description_item>
</Description_list>
</Paragraphe>
<Table id="0" ssch="Book" psch="BookP">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
Data Object Type
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
reg
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Register
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
var
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Variable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
 memory block
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
sig
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Signal
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
queue
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Queue (IPC)
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
channel
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Channel 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
(IPC)
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
Summary of available Data Object Types.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
Data Types
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="19b5310">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="226" lang="2">
 lists all available data types which can be used with expressions, function a
rguments and assignments. These data types can be applied to a subset of avail
able object types (data and some interprocess communication objects): 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
See also tables 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="23535c8">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="23533f8">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
 and definition 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="2353798">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
 for more informations.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Description_list id="0" ssch="Book" psch="BookP">
<Description_item id="0" ssch="Book" psch="BookP">
<Description_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
Data Types
</TEXT_UNIT>
</Description_name>
<Description_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="5" lang="0">
TYPE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" !PRFont="3" len="1" lang="11">
b
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="3" lang="0">
'={
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="20" lang="2">
logic,int,char,bool}
</TEXT_UNIT>
</Simple_paragraphe>
</Description_paragr>
</Description_item>
</Description_list>
</Paragraphe>
<Table id="19b5310" ssch="Book" psch="BookP">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Data Type
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
logic
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
Single logic bit
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
logic[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
]
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
Logic vector of width N bit
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
int[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
]
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
Signed integer of width N bit
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
char
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
Character 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 logic[8])
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
bool
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Boolean 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 logic)
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
Summary of available Data Types.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="40">
</PAGE_BREAK>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Abstract Data Object 
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="15cdf50">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
  summarizes all abstract object types 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Á
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="356" lang="2">
used for interprocess communication and synchronization (IPC) and additional o
nes used for communication and storage. Implementation and Interface of each A
DTO are defined using the External Module Interface (EMI). More ADT objects ca
n be added and embedded in a ConPro design using this interface. ADTOs can onl
y be accessed with their respective methods.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Table id="15cdf50" ssch="Book" psch="BookP">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
Abstract Data Type
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
mutex
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
IPC: mutual exclusion
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
semaphore
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
IPC: sempahore
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
barrier
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
IPC: barrier
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
event
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
IPC: event
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
timer
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
IPC: periodical event timer
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
ram
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
Storage: RAM blocks (external and internal)
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
random
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
Pseudo-Random-Generators
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
latch
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
Asynchronous Latch Memory
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
uart
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
COMM: Serial receiver and transmitter
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
process
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
Process object
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
Available Abstract Data Objects Types.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
Product Types: Array
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="41">
</PAGE_BREAK>
<Table id="0" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Statement
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="5" lang="2">
array
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
A
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
OT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
] of 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="68" lang="2">
Define storage array of size N with object type OT and data type DT.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="5" lang="2">
array
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
A
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
OT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
] of 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  with 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="6" lang="2">
PARAMS
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="91" lang="2">
Define storage array of size N with object type OT and data type DT and parame
ter settings.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="5" lang="2">
array
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
A
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  object 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
obj
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  with 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="6" lang="2">
PARAMS
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="113" lang="2">
Define abstract object array of size N. Optional object parameter settings req
uire prefixed ADTO module selector.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="5" lang="2">
array
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
A
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  process[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
] of
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="1" lang="0">
B
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="95" lang="2">
Defines a new array of N different processes. Optional process block parameter
s can be applied.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP" %PRVertPos="R0">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" %PRWidth="R160">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
Summary of array definitions.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
Product Types: Structure
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="364" lang="2">
New user defined types can be used to aggregate objects with heterogene types 
and data widths. In contrast to arrays, a new type structure must be defined f
irst without creation of any data object. After type definition data objects o
f this type can be created (instantiated). Supported data object types are: si
gnal, register, variable, queue, channel, component.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
Structure Subclasses
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="74" lang="2">
There are three different subclasses of structures for different purposes:
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
Multi-Type Structure
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="152" lang="2">
The generic structure type binds different named structure elements with diffe
rent data types to a new user defined data type, the native product type. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
Bit Structure
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="352" lang="2">
This structure subclass provides a bit-index-name mapping for storage objects.
 All structure elements have the same data type. The bit-index is either one b
it number or a range of bits. This structure type provides symbolic/named sele
ction of parts of vector data type (for example logic vector and integer types
) and clarifies bit access of objects.  
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
Component Structure
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="288" lang="2">
This structure defines hardware component ports, either of a ConPro module top
level port, or of an embedded hardware component (modelled on hardware level).
 This structure type can only be used with component object defintions. The co
mponent type has equal behaviour like the signal type.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="170" lang="2">
The structure type defintion therefore contains only data types, and no object
 types. A structure type binds N different structure elements, distinguished b
y their names.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="254" lang="2">
Tip: The member names of structures should begin with a lower case letter, the
 elements of a enumerated symbolic list should begin with an uppercase letter.
 Elements of a structure can be accessed with the object and element name conc
atenated with a dot.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="170" lang="2">
In the case the object type of a structure is a register, just N independent r
egisters are created. In the case of a variable type, N objects are stored int
o a RAM block.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="100" lang="2">
Arrays from structure types can be created. For each structure element a diffe
rent array is created.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="484" lang="2">
Hardware component port types are defined with structures, too, with the diffe
rence that for each structure element the direction of the signal must be spec
ified. Some care must be taken for the direction: if the component is in lower
 hierarchical order (an embedded external hardware component), the direction i
s seen from the external view of the hardware component. If the component is p
art of the toplevel port interface of a ConPro module, it must be seen from th
e internal view.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Table id="0" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Statement
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="4" lang="2">
type
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
ST
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
: { 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
e1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
e2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
};
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="79" lang="2">
Defines a new structure type with data type DT specification for each element.
.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="4" lang="2">
type
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
CT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
: { 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 port 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
e1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
DIR
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 port 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
e2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
DIR
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="1" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
DT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
};
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="113" lang="2">
Defines a new component structure type with data type DT and signal direction 
DIR specification for each element.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="4" lang="2">
type
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
BT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
: { 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
e1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
BN
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
e2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
BN
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
};
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="82" lang="2">
Defines a new bit structure type with bit-index BW specification for each elem
ent.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="3" lang="2">
reg
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
S
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
ST
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
Defines a new storage object of type register.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
OT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
S
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
ST
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="70" lang="2">
Defines a new object of type OT (sig,reg,var,queue,channel,component).
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
ST
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
.e1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
ST
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
.e1 
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="53" lang="2">
Access of structure element objects (write and read).
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="5" lang="2">
array
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
AS
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
:   
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
OT
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
[
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
] of 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
ST
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  with 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="6" lang="2">
PARAMS
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="103" lang="2">
Defines a new array of N different structure objects. Optional process block p
arameters can be applied.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP" %PRVertPos="R0">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" %PRWidth="R160">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="83" lang="2">
Summary of structure type definition, data object definitions and structure ac
cess.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Definition id="0" ssch="Book" psch="BookP" $BNF="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="51" lang="2">
type-definition ::= ( 'type' type-name ':' '{' ) \ 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
  ( { element-name ':' [ port-type ] data-type ';' }+ ) \
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  ( '}' ';' ) . 
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="61" lang="2">
component-type-definition ::= ( 'type' type-name ':' '{' ) \ 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
  ( { 'port' element-name ':' port-type data-type ';' }+ ) \
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  ( '}' ';' ) . 
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
bit-type-definition ::= ( 'type' type-name ':' '{' ) \ 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
  ( { element-name ':' bit-index ';' }+ ) \
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  ( '}' ';' ) . 
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="64" lang="2">
object-definition ::=  object-type obj-name ':' type-name ';' . 
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="75" lang="2">
object-type ::= 'sig' | 'reg' | 'var' | 'queue' | 'channel' | 'component' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
port-type ::= 'input' | 'output' | 'bus' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
bit-index ::= natural | range .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
range ::= A ( 'to' | 'downto' ) B . 
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
element-access ::= obj-name '.' element-name .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="103" lang="2">
Formal syntax definition for structure type definition, object definition and 
structure object  access.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
-- Multi-type structure type definition
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
type registers : {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  ax : logic[32];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  bx : logic[32];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  sp : logic[16];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 };
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
type image : {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  row: logic[32%4];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  col: logic[32%4];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 };
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
-- Component structure type defintion
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
type uart : {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  port rx : input logic[2];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  port tx : output logic[2];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  port re : output logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  port we : input logic; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 };
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
-- Bit-type structure type defintion
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
type command : {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  ack:  0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  cmd: 1 to 2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  data: 3 to 7; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 };
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
block ram1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
reg regs : registers;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
var vegs : registers in ram1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
var vim : image in ram1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
var after : logic[16] in ram1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
component dev1: uart;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
reg cmd: command;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p1:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
  reg x: logic[2];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  type cpu_regs : {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
   ax : logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
   bx : logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
   sp : logic[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  };
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
  var cpu : cpu_regs in ram1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  reg row: logic[32];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  regs.ax 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 row;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  regs.ax
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
 regs.ax + 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  vegs.ax
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
 vegs.ax + intern;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  wait for dev1.re = 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
 dev1.rx;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  dev1.tx 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
 x, dev1.we 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
  cmd 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
  cmd.ack 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
  cmd.cmd 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 x;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="61" lang="2">
Structures with register, variable and component object type.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Sum Types: Enumeration
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
Exceptions
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="504" lang="2">
Exceptions provide the only way to leave a control structure, for example loop
s, conditional branches or functions itself. Exception are abstract signals, w
hich can be raised anywhere and caught within a try-with exception handler env
ironment, either within the process/function where the execption was raised, o
r outside. Thus exceptions are automatically propagated along a call path of p
rocesses and functions using exception state registers if they are not caught 
within the raising process/function.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Table id="0" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Statement
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="9" lang="2">
exception
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
E
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
; 
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
Define a new exception type E.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="5" lang="2">
raise
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
E
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
; 
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="89" lang="2">
Raise exception E. The control flow is directed to exception handler environme
nt, if any.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="3" lang="2">
try
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="1" lang="2">
B
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 with 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP" !PRStyle="2">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  when 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
E1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
B1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  when 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
E2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="2" lang="2">
B2
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="3" lang="2">
end
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="168" lang="2">
Defines an excpetion handler environment. An exception raised in B is caught b
y a particular case in the when list which executes the particluar instruction
 (block) B..
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP" %PRVertPos="R0">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" %PRWidth="R160">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
Summary of array definitions.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
Control Statements
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Counting for-Loop
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Conditional while-Loop
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
Endless always-Loop
</TEXT_UNIT>
</Section_title>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="42">
</PAGE_BREAK>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Blocking wait-for-Loop
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
Conditional if-else-Branch
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Multicase match-Branch
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Exception Handler
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Functions
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="263" lang="2">
A function definition consists of a unique function name identifier, the funct
ion application interface, and the function body. The function body consists o
f local object definitions (types, data and some abstract objects) and an inst
ruction sequence. Definition 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="15cdd80">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
 shows the formal specification, and table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="15cdbb0">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="90" lang="2">
 summarizes and explains different function defintions and function applicatio
n (calling).
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="190" lang="2">
The function application interface specifies function parameters with theit na
me and type.  Function can return values. If there is no value returned, the f
unction behaves like a procedure. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="181" lang="2">
Functions can be used within simple assignments, but not within expressions. I
f a function returns more than one value, a tuple must be used on the left han
d side of the assignment.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="527" lang="2">
Each function parameter and the set of return value paramters are handled like
 registers. Only call-by-value semantic is supported. Values of function argum
ents are copied to the respective parameters on function call, and return valu
es are copied after function call has finished. Within the function body, all 
parameters and the (named) return parameter can be used in assignments and exp
ression like any other register. There is no return statement. The last value 
assigned to the return parameter is automatically returned.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="325" lang="2">
There are two different types of functions: inlined and shared. The inlined fu
nction type is handled likle a macro definition. Each time a function is appli
ed (called), the function call is replaced by the function body, and all funct
ion parameters are replaced by  the function arguments (including return value
 parameters).
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="290" lang="2">
Shared functions are implemented using the process model using the call method
, and with an additional function call wrapper. Each time a shared function is
 called the argument values are passed to the function parameters (global regi
sters), and the return value(s) are passed back, if any.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="15cdd80" ssch="Book" psch="BookP" $BNF="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
function ::= ('function' name func-params func-ret ':' ) \< 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
              ('begin' function-body 'end') \< 
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="43">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
              ([ 'with' parameter-list ] ';') .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
function-params ::= '(' { param-def }+ ')' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="50" lang="2">
function-ret ::= 'return' '(' { param-def }+ ')' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
param-def ::= name ':' data-type .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
function-body ::= { object-definition }+
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
                 { process-instruction }+ .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="61" lang="2">
object-definition ::= type-definition | dataobj-definition | 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
                abstrobj-definition .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="54" lang="2">
Formal syntax specification of  a function definition.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Table id="15cdbb0" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
function 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
 (x:int[8],n:bool):
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="11" lang="0">
definitions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="12" lang="0">
instructions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="62" lang="2">
Defines a new procedure with specified name (no return value).
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
function 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
fname
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
 (x:int[8],n:bool)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
 return (a:int[4]):
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="11" lang="0">
definitions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="12" lang="0">
instructions
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
end with 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
param
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
=
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
value
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="87" lang="2">
Defines a new function with specified name. Additional parameter settings are 
applied (
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="6" lang="2">
inline
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
: inlined function macro).
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
pname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
(i,1);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
y 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
fname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
(x);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
{y1,y2} 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="5" lang="2">
fname
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
(x);
</TEXT_UNIT>
</Text_line>
</Preformatted>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="2" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
Function and procedure application (call with arguments).
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
Summary of function definitions and application (call).
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="44">
</PAGE_BREAK>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="45">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
const div_n: value := 16;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
const div_n1: value := 15;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
const div2_n: value := 32;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
const div2_n1: value := 31;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
-- optimized fast sequentiel division
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
--
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="68" lang="2">
function div (a:logic[div_n],b:logic[div_n]) return(z:logic[div_n]):
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  reg q,b2: logic[div2_n];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
  reg i: logic[5];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  const l0: logic[1] := 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  q <- a;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  b2 <- b lsl div_n;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  i <- 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  while i < div_n do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
      q <- ((q lsl 1)-b2) lor 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
      i <- i + 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
    end with bind;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
    if q[div2_n1] = 1 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
      q <- (q + b2) land 0xFFFFFFFE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  z <- q[0 to div_n1];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="45" lang="2">
function swap(a:logic[div_n],b:logic[div_n]) 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
         return(c:logic[div_n],d: logic[div_n]):
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  c 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 b, d 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 a;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
process calc:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  reg x,y,z: logic[div_n];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 456, y 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 32;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
  z 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 div(x,y);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
  {x,y} 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
 swap(x,y);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
  z 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 div(x,y);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="207" lang="2">
An example showing function definitions and function application. The first fu
nction returns one result value, the second a tuple of two values, assigned a 
tuple of storage objects (of same type) in line 42.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
I/O: Hardware Port Interface
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
Components: Interfacing HDL
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
External Module Interface: Embedding HDL
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
Abstract Data Type Objects
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
Abstract data type objects 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="93" lang="2">
define objects  not directly accessible in expressions like registers (with so
me exceptions).
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="102" lang="2">
Before abstract objects of a particular type can be used, the appropiate modul
e must be opened first: 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="0" ssch="Book" psch="BookP" $BNF="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
open-module ::= 'open' mod-name ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
Opening of a module.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
ADT objects can be accessed by their appropiate method set 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
u
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
={
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
u
</TEXT_UNIT>
</I_Main>
<I_R_Sub id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
1
</TEXT_UNIT>
</I_R_Sub>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
,
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
u
</TEXT_UNIT>
</I_Main>
<I_R_Sub id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_R_Sub>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="190" lang="2">
,...}. A method is applied using the selector operator followed by a list of a
rguments passed to method parameters, with arguments separated by a comma list
 encapsulated between paranthesis:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="0" ssch="Book" psch="BookP" $BNF="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
object-defintion ::= 'object' obj-name ':' obj-type ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="75" lang="2">
object-call ::= obj-name '.' method-name '(' ( argument \\ ',' ) ')' ';' . 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="90" lang="2">
Object method calls. The object must be first created with the object defintio
n statement.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="88" lang="2">
Methods which do not expect arguments are applied with an empty argument list 
(). Table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="15cd9e0">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="68" lang="2">
 summarizes the statements required for using abstract object types.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Table id="15cd9e0" ssch="Book" psch="BookP">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Statement
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
open 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="6" lang="2">
Module
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
Open specified ADTO module
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
object 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
obj
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="7" lang="2">
objtype
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
Defines and instantiates a new object of specified ADT.
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="100">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
obj
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="4" lang="2">
meth
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
Object method access using the selector operator 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="67" lang="2">
Summary of abstract object module inclusion, definition and access.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Pseudo-Notation
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="149" lang="2">
In the following sections, an abstract syntax notation is used to define modul
es of an particular ADTO type with their supported methods. Defintions 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="15cd810">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
 to 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="158ee68">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="271" lang="2">
 show the formal syntax of this notation. The signature declaration specifies 
types and the type signature for each method, the interface declaration define
s signals and the RTL access for each method. The implementation definition sp
ecifie the behaviout for each method.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="15cd810" ssch="Book" psch="BookP" $BNF="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
module ::= 'MODULE' name module-decl ';' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="71" lang="2">
module-decl ::= signature-decl | implementation-decl | interface-decl .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
signature-decl ::= 'SIGNATURE' { signature }+ .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="62" lang="2">
implementation-decl ::= 'IMPLEMENTATION' { implementation }+ .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
interface-decl ::= 'INTERFACE' { interface }+ .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
signature ::= method-decl | type-decl .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
implementation ::= val-def | method-def .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="45" lang="2">
interface ::= signal-def | method-interface .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="107" lang="2">
Pseudo notation for abstract modules: signature and interface declarations, an
d  implementation defintion. 
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="0" ssch="Book" psch="BookP" $BNF="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
type-decl ::= type-abstract | type-struct .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
type-abstract ::= 'TYPE' name .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
type-struct ::= ( 'TYPE' name '=' '{' ) \< ( { struct-elem }+ ) .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="53" lang="2">
struct-elem ::= name ':' ( core-type | symbol-set ) .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="50" lang="2">
core-type ::= 'INT' | 'CHAR' | 'BOOL' | 'FLOAT' . 
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
symbol-set ::= '{' { symbol-name }+ '}' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="64" lang="2">
(Cont.) Pseudo notation for abstract modules: type definition.  
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="0" ssch="Book" psch="BookP" $BNF="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="54" lang="2">
method-decl ::= 'METHOD' name ':' { argument-type }+ .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
argument-type ::= dir name ':' object-type '*' data-type .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
dir ::= read | write | read-write .
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="70" lang="2">
(Cont.) Pseudo notation for abstract modules: method type signature.  
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="158ee68" ssch="Book" psch="BookP" $BNF="1" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
object-type ::=  'reg' | 'var' | 'reg array' | 'var array' 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="56" lang="2">
                 'reg struct' | 'var struct' | 'const' .
</TEXT_UNIT>
</Text_line>
</Definition_content>
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="51" lang="2">
data-type ::= 'logic' | 'int' | 'bool' | 'char' .  
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="69" lang="2">
(Cont.) Pseudo notation for abstract modules: object- and data types.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Table 
</TEXT_UNIT>
<Ref_Table id="0" ssch="Book" psch="BookP" ref="1558b78">
</Ref_Table>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
 explains symbols used in the notation.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Table id="1558b78" ssch="Book" psch="BookP" $FLOAT="1">
<Table_content id="0" ssch="Book" psch="BookP">
<T_Rows id="0" ssch="Book" psch="BookP">
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Statement
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="1" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Decsription
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Control Path
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Data Path
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Abstract Object (Type)
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
u
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
Abstract Object Methods
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Â
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Data Storage Objects 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Á
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
IPC Objects 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
D
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Abstract Computational Objects 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
E
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
Abstract External Communication Objects 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
a
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Set of objects (Type)
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
Data Type Set
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
c
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
Synthesis Rule Set
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
$
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
Parameter variable
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
State
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
+
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
Next State
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
X
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
List of (blocked) Processes
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
­
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
Output/Write 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Input/Read 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Suspend a process
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
Resume a process
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
Empty set or argument list
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
a
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
|
</TEXT_UNIT>
</I_Main>
<I_R_Sub id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
b
</TEXT_UNIT>
</I_R_Sub>
</INDEX>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
Expression a depends on expression b
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
[]
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
List/empty List
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
a :: l
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="2" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
Appends element a to head of list l
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
<T_Row id="0" ssch="Book" psch="BookP">
<T_Columns id="0" ssch="Book" psch="BookP">
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="2" $Border_Left="1" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="150">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
l1 @ l2 
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
<T_Column id="0" ssch="Book" psch="BookP" $Border_Bottom="1" $Border_Right="1" $Border_Left="2" $Border_Top="2" $Alignement_vert="3" $Alignement_horiz="1" $Column_Width="180">
<T_Block id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
Concatenates two lists l1 and l2
</TEXT_UNIT>
</Simple_paragraphe>
</T_Block>
</T_Column>
</T_Columns>
</T_Row>
</T_Rows>
</Table_content>
<Table_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
Summary of symbols used in pseudo notation.
</TEXT_UNIT>
</Simple_paragraphe>
</Table_desc>
</Table>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
Interprocess-Communication
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
The following ADTOs are available for interprocess communication:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Ordered_list id="0" ssch="Book" psch="BookP" $List_Numbers="6">
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Mutex
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Semaphore
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Event
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Barrier
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Timer
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Queue [core]
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
Channel [core]
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
</Ordered_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Mutex
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="288" lang="2">
The Mutex module implements a mutual exclusion lock required for protection of
 shared resources accessed concurrently. All shared atomic objects (both stora
ge and ADTO) are already implicitly guarded by a mutex lock, serializing the a
ccess of the object, including this mutex object, too!
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
The following object methods are available:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
METHODS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
lock
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="387" lang="2">
A process requests the lock with this method. If the mutex is unlocked (not ow
ned by any other process), the calling process gets the lock and continues ope
ration. If the lock is already owned by another process (mutex is locked), the
 calling process is blocked untill the mutex owener release the lock using the
 unlock method. In this case the calling process P is added to a wait-list 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
X
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
unlock
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="189" lang="2">
The unlock method releases a previously locked mutex. If there are blocked pro
cesses awaiting the release. the next waiting process is scheduled and the loc
k is transferred to this process.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
init
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Initialize the mutex object.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
PARAMETERS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
scheduler
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
Selects static or FIFO scheduler policy.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
model
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="182" lang="2">
The model parameter provides two different access models: owner: only the owne
r process of a mutex can unlock the mutex,  group: each member of a process gr
oup can unlock the mutex. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Definitions 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="149ef90">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="1412f18">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="72" lang="2">
 specifiy the signature and the implementation of the mutex module, and 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="1317138">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="105" lang="2">
 the object interface. Two schedulers are available: static priority and dynma
ic priority FIFO scheduler.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="149ef90" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Mutex
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
SIGNATURE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  TYPE mutex
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  TYPE parameters = {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
    scheduler: {static,fifo}
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
    model: {owner,group}
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  } 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  METHOD NEW 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
    object 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="4" lang="2">
name
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
: mutex [with parameters]: mutex
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  METHOD init: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD lock: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
  METHOD unlock: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
Signature of ADTO Module Mutex. 
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="1412f18" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Mutex
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
IMPLEMENTATION
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  VAR lock: bool
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  VAR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
F
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
: process list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  VAR P,P': process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  OBJ S: scheduler
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD init:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    lock 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Î
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 []
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD unlock:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
    if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 [] then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TAIL(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
), P' 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 HEAD(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
P'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
      lock 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
  METHOD lock:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
    P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 SELF
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="24" lang="0">
    if lock = false then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
      lock 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 true
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 @ [P]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
Implementation of ADTO Module Mutex.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="1317138" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Mutex
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
INTERFACE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
  SIGNALS GD,INIT,UNLOCK,LOCK: logic
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD init:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
: INIT 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD unlock:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
: UNLOCK 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
  METHOD lock:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
: UNLOCK 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Interface of ADTO Module Mutex.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Core;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
open Process;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
open Mutex;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
reg x,y: int[10];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
object mu: mutex;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p1:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  for i = 1 to 10 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
    mu.lock ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
    x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 y-1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
    y 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 y+1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
    mu.unlock ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p2:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  for i = 1 to 10 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
    mu.lock ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
    x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 y+1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
    y 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 y-1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
    mu.unlock ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
process main:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  mu.init ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 100;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
  y 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 100;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  p1.start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  p2.start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="75" lang="2">
A mutex lock is used to protect the access of two global registers x and y.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Semaphore
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="50" lang="2">
The Semaphore module implements a guarded counter 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
w
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="331" lang="2">
. A semaphore is used in produce-consumer applications. The counter can be inc
remented (up operation) and decremented (down operation). The value of the cou
nter may never be negative. Thus a down operation with an actual semaphore val
ue zero blocks the requesting process untill another processes increments the 
semaphore counter. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
The following object methods are available:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
METHODS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
down
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
A process decrements the semaphore counter 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
w
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
w
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
-1 iff 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
w
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 > 0
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="211" lang="2">
. In the case the counter is alreayd zero, the calling process  is blocked unt
ill the semaphore was incremented by another process using the up method. In t
his case the calling process P is added to a wait-list 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
X
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
up
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
The up method increments the semaphore counter 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
w
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
w
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
+1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="96" lang="2">
 If there are blocked processes awaiting an increment the next waiting process
 is scheduled and 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
w
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="138" lang="0">
 is still zero (the increment compensates the decrement operation). If there a
re no blocked processes the sempahore counter is incremented
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
init
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="62" lang="2">
Initialize the semaphore object with an initial counter value.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
PARAMETERS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
scheduler
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
Selects static or FIFO scheduler policy.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="118" lang="2">
The depth parameter specifies the bit width of the semaphore counter, thus the
 semaphore value can be in the range [0,
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
-1]
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Definitions 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="1102928">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="1100d80">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="76" lang="2">
 specifiy the signature and the implementation of the semaphore module, and 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="10ff1d8">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="99" lang="2">
  the interface. Two schedulers are available: static priority and dynmaic pri
ority FIFO scheduler.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="1102928" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
MODULE Semaphore
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
SIGNATURE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  TYPE semaphore
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  TYPE constant: natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  TYPE parameters = {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
    scheduler: {static,fifo}
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
    depth: [4 to 16]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  } 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  METHOD NEW 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
    object 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="4" lang="2">
name
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
: semaphore [with parameters]: semaphore
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  METHOD init: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
init-val:(constant | storage-type 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
 integer) 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD down: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
  METHOD up: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
Signature of ADTO Module Semaphore. 
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="1100d80" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
MODULE Semaphore
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
IMPLEMENTATION
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  VAR lock: bool
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  VAL counter: natural [0,
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
-1]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  VAR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
F
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
: process list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  VAR P,P': process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  OBJ S: scheduler
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD init:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    lock 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Î
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 []
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
    counter 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
 init-val
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  METHOD up:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
    if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 = [] then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
      incr counter
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
      lock 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
    else 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TAIL(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
), P' 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 HEAD(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
P'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
  METHOD down:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
    P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 SELF
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="23" lang="0">
    if counter > 0 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
      decr counter
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
    else 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
 @ [P], lock 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 true
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
Implementation of ADTO Module Semaphore.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="10ff1d8" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
MODULE Semaphore
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
INTERFACE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
  SIGNAL GD,INIT,UP,DOWN: logic
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  SIGNAL WR: logic[depth]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD init:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
: INIT 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
GD 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
       WR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 ARG1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  METHOD up:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
: UNLOCK 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
  METHOD down:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
: UNLOCK 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
Interface of ADTO Module Semaphore.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Core;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
open Process;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
open Semaphore;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
open System;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
open Event;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
object sys: system;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  sys.simu_cycles (500);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
object ev: event;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
array eating,thinking: reg[5] of logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
export eating,thinking;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="66" lang="2">
array fork: object semaphore[5] with depth=8 and scheduler="fifo";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
process init:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 0 to 4 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
    fork.[i].init (1);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  ev.init ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
function eat(n):
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
    eating.[n] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
    thinking.[n] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  end with bind;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  wait for 5;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
    eating.[n] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
    thinking.[n] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  end with bind;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
end with inline;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
array philosopher: process[5] of
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  if # < 4 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
   ev.await ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
   always do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
   begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
     -- get left fork then right
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
     fork.[#].down ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
     fork.[#+1].down ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
     eat (#);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
     fork.[#].up ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
     fork.[#+1].up ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
   end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
   always do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
   begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
     -- get right fork then left
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
     fork.[4].down ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
     fork.[0].down ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
     eat (#);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
     fork.[4].up ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
     fork.[0].up ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
   end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
process main:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  init.call ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 0 to 4 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
    philosopher.[i].start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  ev.wakeup ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="575" lang="2">
Semaphores are used to implement a resource negoatation algorithm: Dining phil
osophers problem using semaphores.  Five philosophers sit around a circular ta
ble. Each philosopher spends his life alternately thinking and eating. In the 
center of the table is a large platter of spaghetti. Each philosopher needs tw
o forks two eat. But there are only five forks for all. One fork is placed bet
ween each pair  of philosophers, and they agree that each will use only the fo
rks to the  immeadiate left and right. [Andrews 2000, Multihtreaded, Parallel,
 and Distributed Programming]
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Event
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="325" lang="2">
The Event module implements an event handler (abstract signal) and is used for
 process control flow synchronization (control flow boundary). A group of proc
esses can join the event handler and wait for the occurrence of this event. Th
e processes are blocked untill the event occurs. The event is signaled by anot
her process. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
The following object methods are available:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
METHODS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
await
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="154" lang="2">
A process waits for the event associated with the abstract object.  The callin
g process is blocked untill the event occurs, and P is added to a wait-list 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
X
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
wakeup
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="119" lang="2">
The event associated with this abstract object is signaled. All blocked proces
ses waiting for this event are released. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
init
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Initialize the event object.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
PARAMETERS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
latch
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="236" lang="2">
The latch=1 parameter setting provides a latched event, which causes if an eve
nt occured with empty blocked process list, this event is latched. If a proces
s requests the await method, and the latch is set, it will immediately release
d.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Definitions 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="f84568">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="f80c40">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="72" lang="2">
 specifiy the signature and the implementation of the event module. The 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="7" lang="2">
latch=1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="224" lang="2">
 parameter setting provides a latched event, which means if an event occured w
ith empty blocked process list, this event is latched. If a process requests t
he await method, and the latch is set, it will immediately released.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="f84568" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
MODULE Event:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
SIGNATURE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  TYPE event
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  TYPE parameters = {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
    latch: {0,1}
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  } 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  METHOD NEW 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
    object 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="4" lang="2">
name
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
: event [with parameters]: event
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  METHOD init: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="16" lang="0">
  METHOD await: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
  METHOD wakeup: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Signature of ADTO Module Event.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="f80c40" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
MODULE Event:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
IMPLEMENTATION
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
  if $latch=1 then VAR latch: bool
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  VAR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
F
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
: process list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  VAR P,P': process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  OBJ S: scheduler
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD init:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
    if $latch=1 then latch 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Î
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 []
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD wakeup:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
    if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 [] then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Î
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 []
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="27" lang="0">
    if $latch=1 then latch 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD await:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
    P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 SELF
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="16" lang="0">
    if $latch=1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
 latch = true then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
      latch 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 @ [P]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
Implementation of ADTO Module Event.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="0" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Event
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
INTERFACE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
  SIGNALS GD,INIT,WAKEUP,AWAIT: logic
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD init:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
: INIT 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD wakeup:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
: WAKEUP 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD await:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
: AWAIT 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Interface of ADTO Module Event.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Core;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
open Process;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
open Event;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
open System;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
object sys:system;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  sys.sim_cycles(300);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
object e: event;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
array d: reg[4] of int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
export d;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
array p: process[4] of
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 1 to 5 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
    e.await ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
    d.[#] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
 # + 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
    d.[#] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
process main:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  e.init ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 0 to 3 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
   p.[i].start (); 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 1 to 5 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
    wait for 20;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
    e.wakeup ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="230" lang="2">
An event synchronize the control flow of several processes. The processes of a
rray p are started sequentially, but they are all suspended untill the event i
s signaled by the main process. This happens again in each loop iteration.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Barrier
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="432" lang="2">
The barrier module implements a self synchonization event handler (abstract si
gnal) and is used for process control flow synchronization (control flow bound
ary). A group of processes with defined number N can join the barrier and wait
 for the occurrence of the event. The processes are blocked untill the event o
ccurs. The event is signaled by the last process N joining the barrier. Each t
ime a proces join the barriers, a counter 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="4" lang="2">
join
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
 is incremented.  
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
The following object methods are available:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
METHODS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
await
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="162" lang="2">
A process waits for the barrier event associated with the abstract object.  Th
e calling process is blocked untill the event occurs, and P is added to a wait
-list 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
X
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="25" lang="0">
. The event happens when 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="5" lang="0">
size(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" !PRFont="3" len="1" lang="11">
X
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRFont="3" len="8" lang="0">
)=join=N
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
init
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="184" lang="2">
Initialize the barrier object with size of joining process group. The process 
group size N is automatically determnined by  the number of differernt process
es calling the await method.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
PARAMETERS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_cont>
</Head_Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Definitions 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="f80a70">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="e65d48">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="74" lang="2">
 specifiy the signature and the implementation of the barrier module, and 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="e61d90">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  the process interface. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="f80a70" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
MODULE Barrier:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
SIGNATURE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  TYPE barrier
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  METHOD NEW 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
    object 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="4" lang="2">
name
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
: barrier [with parameters]: barrier
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  METHOD init: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="16" lang="0">
  METHOD await: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
Signature of ADTO Module Barrier.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="e65d48" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
MODULE Barrier:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
IMPLEMENTATION
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  VAR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
F
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
: process list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  VAR P,P': process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  OBJ S: scheduler
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="24" lang="0">
  VAL size,join: natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="0" lang="0">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD init:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
size 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 sizeof(
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
F
</TEXT_UNIT>
</I_Main>
<I_R_Sub id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
await
</TEXT_UNIT>
</I_R_Sub>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
   join 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 P in 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="11">
Q¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD await:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
    if (join+1) = size then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
      join 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 P in 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="11">
Q¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
      P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 SELF
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 @ [P]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
      incr join
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
P 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
      
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
Implementation of ADTO Module Barrier.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="e61d90" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
MODULE Barrier
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
INTERFACE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
  SIGNALS GD,INIT,AWAIT: logic
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD init:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
: INIT 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD await:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
: AWAIT 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
Interface of ADTO Module Barrier.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Core;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
open Process;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
open Barrier;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
open System;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
object sys:system;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  sys.sim_cycles(300);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
object b: barrier;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
array d: reg[4] of int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
export d;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
array p: process[4] of
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 1 to 5 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
    b.await ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
    d.[#] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
 # + 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
    d.[#] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  b.init ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  -- d 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
 64;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 0 to 3 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
   p.[i].start (); 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="130" lang="2">
A group of processes defined in array p wait for the barrier b event. The loop
 iteration of each process starts at the same time. 
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Timer
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="363" lang="2">
The Timer module implements an interval timer and is used for time constrained
 process control flow synchronization. A group of processes can join the timer
 handler and wait for the occurrence of this time event. The processes are blo
cked untill the event occurs. The event is signaled by the timeout of the time
r. The timer can operate one-time and continously. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
The following object methods are available:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
METHODS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
await
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="166" lang="2">
A process waits for the timer event associated with the abstract object.  The 
calling process is blocked untill the timer event occurs, and P is added to a 
wait-list 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
X
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
wakeup
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
Wakeup all blocked processes.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
init
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Initialize the timer object.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
time
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="72" lang="2">
Set interval time of timer in nano seconds (or in time unit specified). 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
start
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
Start the timer.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
stop
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Stop the timer.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
sig_action
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="210" lang="2">
A signal can be used to check the actual state of the timer: enabled or disabl
ed. Can be used in one-shot timer mode to check the timeout. The activity leve
l and the default signal value must be specified, too.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
PARAMETERS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
time
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
The timer interval period time in nano seconds.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
mode
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="83" lang="2">
The mode parameter specifies timer operation: one-time/shot (1) or continously
 (0).
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Definitions 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="e63b48">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="e61bc0">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="68" lang="2">
 specifiy the signature and the implementation of the timer module. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="e63b48" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Timer
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
SIGNATURE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  TYPE timer
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  TYPE parameters = {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
    time: natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
    mode: {0,1}
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  } 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  METHOD NEW 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
    object 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="4" lang="2">
name
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
: timer [with parameters]: timer
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  METHOD init: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="16" lang="0">
  METHOD await: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
  METHOD wakeup: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD time: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="6" lang="0">
period
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
:natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD mode: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="4" lang="0">
mode
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
:natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  METHOD sig_action: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRUnderline="2" len="3" lang="2">
sig
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
:(output signal 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="ISO_latin_1" len="7" lang="0">
 logic)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="21" lang="0">
                     
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="12" lang="0">
action_level
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
:logic 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="21" lang="0">
                     
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="9" lang="0">
def_level
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
:logic
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Signature of ADTO Module Timer.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="e61bc0" ssch="Book" psch="BookP" $Box="8">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Timer
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
IMPLEMENTATION
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  VAR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
F
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
: process list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  VAR P,P': process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  OBJ S: scheduler
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="25" lang="0">
  VAL time,timer: natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="25" lang="0">
  VAL mode: natural {0,1}
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  VAL enabled: bool
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="28" lang="0">
  VAL count,counter: natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD init:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
    counter 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
    count 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
 clkcycles($time)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
    enabled 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
    mode 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 $mode
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Î
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 []
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  METHOD start:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
    counter 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 count
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
    enabled 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 true
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD stop:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
    counter 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
    enabled 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD time:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
    count 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
 clkcycles($ARG1)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD mode:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    mode 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 $ARG1
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD wakeup:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
    if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 [] then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Î
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 []
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD await:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
    P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 SELF
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 @ [P]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="16" lang="0">
  PROCESS timer:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
 CLK'event: 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="28" lang="0">
      if enabled = true then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="27" lang="0">
        if counter = 0 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
          
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Î
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
          
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Q
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 []
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="26" lang="0">
          if mode = 0 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="20" lang="0">
            counter 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 count
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
          else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="20" lang="0">
            enabled 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
        else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="22" lang="0">
          decr counter
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
Implementation of ADTO Module Timer.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="0" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Timer
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
INTERFACE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="56" lang="2">
  SIGNAL GD,INIT,WAKEUP,AWAIT,TIME_SET,START,STOP: logic
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  SIGNAL TIME: natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD init:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
: INIT 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  METHOD start:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
: START 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD stop:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
: STOP 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD time:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
: TIME_SET 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
       TIME 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="47" lang="0">
 index($time,$ARG1) -- List index of time value
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD wakeup:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
: WAKEUP 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD await:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
: AWAIT 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Interface of ADTO Module Timer.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Core;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
open Process;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
open Timer;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
open System;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
object sys:system;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  sys.sim_cycles(300);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
object t: timer;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  t.time (1 microsec);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
array d: reg[4] of int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
export d;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
array p: process[4] of
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
  d.[#] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 1 to 5 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
    t.await ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
    d.[#] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
 # + 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
    d.[#] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
process main:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  t.init ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  t.time (2 microsec);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  for i = 0 to 3 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
    p.[i].start (); 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  t.mode (0);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  t.start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
Timer example.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Queue
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="256" lang="2">
The queue is both a core and abstract object data type. Queues can be used dir
ectly in expressions (read and write). The queue buffers data words written fr
om processes to the queue for processes requesting a read operation. The data 
word order is FIFO.   
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
The following object methods are available:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
METHODS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
read
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
A process 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="70" lang="0">
reading a queue is blocked untill at least one data word is available.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
write
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="105" lang="2">
A process writing to a queue is blocked untill at least one data word cell is 
free to hold the new value.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
unlock
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="89" lang="2">
This method releases all blocked processes (both waiting for read and write co
mpletion). 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
PARAMETERS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
Number of cell of the data queue: 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
-1
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Definition id="0" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Queue
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
SIGNATURE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  TYPE queue
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  TYPE parameters = {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
    depth: natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  } 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  METHOD NEW 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
    queue 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="4" lang="2">
name
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
: data-type [with parameters]: object-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
  METHOD unlock: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD read: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="4" lang="0">
data
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
:data-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD mode: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="4" lang="0">
data
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
:data-type
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Signature of ADTO Module Queue.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="0" ssch="Book" psch="BookP" $Box="8">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Queue
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
IMPLEMENTATION
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  VAR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
F
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
: process list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  VAR P,P': process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  OBJ S: scheduler
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="30" lang="0">
  VAL avail,free,size: natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
  VAL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="16" lang="0">
: data-type list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  VAL d: data-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="0" lang="0">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  METHOD RESET:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
size 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
-1
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
   free 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
-1
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
   avail 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 [] 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 P in 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="ISO_latin_1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
È
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="ISO_latin_1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="11">
Q¯¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
, 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="11">
Q­¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
  METHOD read:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
    if avail > 0 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="27" lang="0">
      decr avail, incr free
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
      d 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 HEAD(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
), 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TAIL(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
) 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
      if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 [] then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
        P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 HEAD(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
), 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TAIL(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
        
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
      $1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 d
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
      P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 SELF
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 @ [P]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
P 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD write:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
    if free > 0 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="27" lang="0">
      incr avail, decr free
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 @ [
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
      if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 [] then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
        P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 HEAD(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
), 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TAIL(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
        
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
      P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 SELF
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 @ [P]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
P 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
  METHOD unlock:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
   free 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
-1
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
   avail 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 [] 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 P in 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
È
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="11">
Q¯¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
, 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="11">
Q­¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
      
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
Implementation of ADTO Module Queue.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="0" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Queue
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
INTERFACE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  SIGNAL GD,RE,WE: logic
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  SIGNAL RD,WR: data-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD read:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
: RE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
$ARG1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 RD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD write:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
: WE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
       $ARG1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 WR
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Interface of ADTO Module Queue.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Channel
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="295" lang="2">
The channel is both a core and abstract object data type. Queues can be used d
irectly in expressions (read and write). A channel is a special case of a queu
e. The depth of the buffer is either 1 (buffered) or 0 (unbuffered). The chann
el provides a handshake for data transfer between processes. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
The following object methods are available:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
METHODS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
read
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
A process 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="63" lang="0">
reading a channel is blocked untill one data word is available.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
write
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="135" lang="2">
A process writing to a queue is blocked untill there is a ready reader (unbuff
ered version) or the channel is empty (buffered version).
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
unlock
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="89" lang="2">
This method releases all blocked processes (both waiting for read and write co
mpletion). 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
PARAMETERS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
Number of cell of the data queue: {0,1}
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Definition id="0" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
MODULE Channel
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
SIGNATURE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  TYPE channel
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  TYPE parameters = {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
    depth: {0,1}
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  } 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  METHOD NEW 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
    channel 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="4" lang="2">
name
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
: data-type [with parameters]: object-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="17" lang="0">
  METHOD unlock: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD read: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="4" lang="0">
data
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
:data-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD mode: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRUnderline="2" len="4" lang="0">
data
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
:data-type
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
Signature of ADTO Module Channel.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="0" ssch="Book" psch="BookP" $Box="8">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
MODULE Channel
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
IMPLEMENTATION
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  VAR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
F
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
: process list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  VAR P,P': process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  OBJ S: scheduler
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="30" lang="0">
  VAL avail,free,size: natural
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
  VAL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="16" lang="0">
: data-type list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  VAL d: data-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="0" lang="0">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  METHOD RESET:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
size 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
-1
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
   free 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
-1
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
   avail 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 [] 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 P in 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
È
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="11">
Q¯¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
, 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="11">
Q­¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
  METHOD read:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
    if avail > 0 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="27" lang="0">
      decr avail, incr free
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
      d 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 HEAD(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
), 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TAIL(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
) 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
      if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 [] then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
        P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 HEAD(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
), 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TAIL(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
        
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
d
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
      P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 SELF
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 @ [P]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
P 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD write:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
    if free > 0 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="27" lang="0">
      incr avail, decr free
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 @ [
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
      if 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¹
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 [] then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
        P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 HEAD(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
), 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 TAIL(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
        
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
      P 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 SELF
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 @ [P]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
      
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
^
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
P 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
  METHOD unlock:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
   free 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
2
</TEXT_UNIT>
</I_Main>
<I_R_Sup id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
depth
</TEXT_UNIT>
</I_R_Sup>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
-1
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
   avail 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
f
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
 [] 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 P in 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
È
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
P
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
   
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="11">
Q¯¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
, 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="11">
Q­¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
[]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
      
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
Implementation of ADTO Module Channel.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="0" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
MODULE Channel
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
INTERFACE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  SIGNAL GD,RE,WE: logic
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  SIGNAL RD,WR: data-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD read:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
 (avail > 0 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ú
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
P)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
: RE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
       $ARG1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 RD
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD write:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="11" lang="0">
 (free > 0 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ú
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
\
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
P) 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
: WE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
       $ARG1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 WR
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
Interface of ADTO Module Channel.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
External Communication
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Link 
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="248" lang="2">
The Link module implements a bidirectional asynchronous parallel communication
 interface using dual rail data encoding and a 4-phase handshake protocol. A l
ink is used to connect different circuit components with different or skewed c
lock domains. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
Arbitrary data widths [1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¼
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="24" lang="0">
64] bits are supported. 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="111" lang="2">
There is an outgoing and incoming link, requiring each 2*datawidth data lines 
D and one acknowledge signal A: D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 and A
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="24" lang="0">
 for the outgoing, and D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 and A
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="22" lang="0">
 for the incoming link
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
The following object methods are available:
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
METHODS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
start
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
Enables the LINK component.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
stop
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Disables the LINK component.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
read
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="219" lang="2">
Read one data byte from the incoming link. This method blocks the calling proc
ess untill a data word was received. There is no receive queue, therefore if m
ore than one data byte was received the old ones are overriden!
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
write
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="146" lang="2">
Write one data byte to the outgoing link. The calling process is blocked until
l the data word was processed and acknowledged by the receiving end.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
interface
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="83" lang="2">
Binds external signals (data and acknowledge, both for incoming and outgoing l
ink).
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Head_Paragraphe id="0" ssch="Book" psch="BookP" $Align_head="2">
<Paragraphe_head id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
PARAMETERS
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe_head>
<Paragraphe_cont id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
datawidth
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
Datawidth of link.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe_cont>
</Head_Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Definitions 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="d26a38">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 and 
</TEXT_UNIT>
<Ref_Definition id="0" ssch="Book" psch="BookP" ref="a33b98">
</Ref_Definition>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="66" lang="2">
 specifiy the signature and the implementation of the link module.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Definition id="d26a38" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Link:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
SIGNATURE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
  TYPE link
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  METHOD NEW 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
º
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
    object 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="3" len="4" lang="2">
name
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
: link: link
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="ISO_latin_1" len="21" lang="0">
  TYPE parameters = {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="ISO_latin_1" len="30" lang="0">
    datawidth: natural=[1,64];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
  }
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="34" lang="0">
  TYPE data-type: logic[datawidth]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="38" lang="0">
  TYPE data-type2: logic[2*data-width]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="43" lang="0">
  TYPE storage-type: register | variable | 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="41" lang="0">
                     reg-array-selector |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="41" lang="0">
                     var-array-selector |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="42" lang="0">
                     reg-struct-selector |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="40" lang="0">
                     var-struct-selector
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="26" lang="0">
  TYPE constant: data-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="0" lang="0">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  METHOD start: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  METHOD stop: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
  METHOD read:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
data:(storage-type 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
 data-type) 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
err:(storage-type 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 bool)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD write:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
data:(storage-type 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="23" lang="0">
 data-type | constant) 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
err:(stoarge-type 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 bool)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  METHOD interface: 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" !PRStyle="3" len="4" lang="0">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" !PRStyle="3" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
data-in:(input signal
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
 data-type2) 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="27" lang="0">
data-in-ack:(output signal 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 logic) 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="24" lang="0">
data-out:(output signal 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
 data-type2) 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="27" lang="0">
data-out-ack:(input signal 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
´
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 logic)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
Signature of ADTO Module Link.  
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="a33b98" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
MODULE Link:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
IMPLEMENTATION
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  VAR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q­
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="2" lang="11">
Q¯
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
,
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
F
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
: process list
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
  VAR P,P': process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="18" lang="0">
  OBJ S: scheduler
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
  VAL enable: bool
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
  VAL din_compl,din_empty: logic
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="0" lang="0">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  METHOD start:  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
    enable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
 true
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD stop:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
    enable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 false
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
  METHOD read:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="52" lang="0">
   wait for din_empty=1 -- await empty set (data-in)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="47" lang="0">
   wait for di_compl=1  -- await data (data-in)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
   d 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="16" lang="0">
 decode(data-in)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
   data-in-ack 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
   $ARG1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
 d
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD write:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
   d 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 $ARG1
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
   data-out 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
 encode(
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Æ
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="31" lang="0">
)  -- send empty set (data-out)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
   data-out 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="35" lang="0">
 encode(d)  -- send data (data-out)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="66" lang="0">
   wait for data-out-ack=0 -- await empty set ack. (data-out-ack 1
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
®
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
0), 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="68" lang="0">
   wait for data-out-ack=1 -- await data acknowledge (data-out-ack 0
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
®
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
1)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
  PROCESS:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
   din_empty 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 i 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Î
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
 [0,data_width-1]: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
P
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="14" lang="0">
data-in.[2*i] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ù
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
data-in.[2*i+1]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="13" lang="0">
   din_compl 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
"
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 i 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="11">
Î
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
 [0,data_width-1]: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
P
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
data-in.[2*i] 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Å
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
 data-in.[2*i+1]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  METHOD interface:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
   data-in
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ü
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 $ARG1
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
   data-in-ack 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ü
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 $ARG2
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="12" lang="0">
   data-out 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ü
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 $ARG3
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="16" lang="0">
   data-out-ack 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ü
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
 $ARG4
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  FUN encode:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  FUN decode:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
Implementation of ADTO Module Link.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Definition id="0" ssch="Book" psch="BookP" $Box="2">
<Definition_contents id="0" ssch="Book" psch="BookP">
<Definition_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
MODULE Link
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
INTERFACE
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
  SIGNAL GD,RE,WE,START,STOP: logic
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  SIGNAL RD,WR: data-type
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
  SIGNAL RD_ERR,WR_ERR: logic (bool)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  METHOD start:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
: START 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD stop:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="8" lang="0">
+ | GD=0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
: STOP 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  METHOD read:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
: RE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="6" lang="0">
$ARG1 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="3" lang="0">
 RD
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
       $ARG2 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 RD_ERR
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="15" lang="0">
  METHOD write:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
G
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
: 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="10" lang="0">
 | GD=1 //
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
       
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
s
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="9" lang="0">
+ | GD=0 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
    
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
D
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="5" lang="0">
: WE 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ø
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="2" lang="0">
GD
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
       WR 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="1" lang="0">
 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
$ARG1 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
       $ARG2 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="7" lang="0">
 WR_ERR
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
 
</TEXT_UNIT>
</Text_line>
</Definition_content>
</Definition_contents>
<Definition_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
Interface of ADTO Module Link.
</TEXT_UNIT>
</Simple_paragraphe>
</Definition_desc>
</Definition>
<Example id="0" ssch="Book" psch="BookP" $Box="8">
<Example_content id="0" ssch="Book" psch="BookP" $LINENUMBER="1">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Core;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
open Process;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
open Link;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
open System;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
type dev_type:{
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
  port ln_din: input logic[20];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
  port ln_din_ack: output logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
  port ln_dout: output logic[20];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
  port ln_dout_ack: input logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
};
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
component DEV: dev_type;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
export DEV;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
object sys: system;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  sys.simu_cycles(100);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
object ln: link with datawidth=10;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="70" lang="2">
  ln.interface(DEV.ln_din,DEV.ln_din_ack,DEV.ln_dout,DEV.ln_dout_ack);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
reg x,y: int[10];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
reg xa,ya: int[8];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
export x,y,xa,ya;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
exception Exit;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p1:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  reg err:bool;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
  reg d:logic[10];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  try
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
    for i = 1 to 10 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
      xa 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 'r';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
      ln.read (d,err);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
      xa 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 '.';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
      if err = true then raise Exit;  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
      x 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
 to_int(d);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  with
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
    when Exit: ln.stop ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
process p2:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  reg err:bool;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
  reg d:logic[10];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  try
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
    for i = 1 to 10 do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
      d 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
 to_logic(i);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
      y 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
 i;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
      ya 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 'w';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
      ln.write(d,err);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
      ya 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="Symbol" len="1" lang="11">
¬
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
 '.';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
      if err = true then raise Exit;  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  with
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
    when Exit: ln.stop ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
process main:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  ln.init ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  ln.start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  p1.start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  p2.start ();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Example_content>
<Example_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="119" lang="2">
More complex example of a Link component used by two processes, one reading fr
om the link, and one writing to the link.
</TEXT_UNIT>
</Simple_paragraphe>
</Example_desc>
</Example>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Data Processing
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Random
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
Hardware and System Architecture
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Introduction
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Modules and Processes
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Modules
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Processes
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
Block Strcutures
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
Data Objects and Data Types
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Registers
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="46">
</PAGE_BREAK>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Variables
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Signals
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
Expressions and Assignments
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
Interprocess-Communication
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Mutex
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Algorithm id="0" ssch="Book" psch="BookP" $Box="8">
<Algorithm_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
MUTEX_$O_SCHED: #process ($scheduler="static")
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  if $CLK then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
    if $RES then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
      MUTEX_$O_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
      foreach $p in $P do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
      begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
        MUTEX_$O_$p_GD <= '1';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
      end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
      foreach $p in $P.lock do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
      begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
        MUTEX_$O_$p_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
      end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
      foreach $p in $P do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
      begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
        MUTEX_$O_$p_GD <= '1';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
      end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
      sequence
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
      begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
        foreach $p in $P.init do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
          if MUTEX_$O_$p_INIT = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
            MUTEX_$O_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
            MUTEX_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
            foreach $l in $P.lock do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
            begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
              if MUTEX_$O_$l_LOCKed = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
              begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
                MUTEX_$O_$l_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
                MUTEX_$O_$l_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
              end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
            end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
        foreach $p in $P.lock do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="67" lang="2">
          if MUTEX_$O_$p_LOCK = '1' and MUTEX_$O_LOCKed  = '0' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
            MUTEX_$O_LOCKed <= '1';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
            MUTEX_$O_$p_LOCKed <= '1';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
            MUTEX_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
        if $model = "owner" then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
          foreach $p in $P.unlock do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
            if MUTEX_$O_$p_UNLOCK = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
            begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
              MUTEX_$O_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
              MUTEX_$O_$p_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
              MUTEX_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
            end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
        if $model = "group" then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
          foreach $p in $P.unlock do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
            if MUTEX_$O_$p_UNLOCK = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
            begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
              MUTEX_$O_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
              if member($P.lock,$p) = true then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
                MUTEX_$O_$p_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
              MUTEX_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
            end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
      end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Algorithm_content>
<Algorithm_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="78" lang="2">
The static priority mutex access scheduler with embedded mutex implementation.
</TEXT_UNIT>
</Simple_paragraphe>
</Algorithm_desc>
</Algorithm>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Semaphore
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Event
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Barrier
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Timer
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Queue
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Channel
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
External Communication
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Link
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="170" lang="2">
A link belongs to the class of external IO components, providing handshaked da
ta exchange with the outside world, requiring interconnect both with ConPro pr
ocesses using 
</TEXT_UNIT>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="47">
</PAGE_BREAK>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
internal signals 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ã
</TEXT_UNIT>
</I_Main>
<I_R_Sub id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
int
</TEXT_UNIT>
</I_R_Sub>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
 and external signals 
</TEXT_UNIT>
<INDEX id="0" ssch="Book" psch="BookP">
<I_Main id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
Ã
</TEXT_UNIT>
</I_Main>
<I_R_Sub id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
ext
</TEXT_UNIT>
</I_R_Sub>
</INDEX>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="63" lang="2">
. Because a link is a sheared resource, a scheduler is invoked.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Graph id="0" ssch="Book" psch="BookP" $GRAPH_width="120" $FLOAT="1">
<Graph_content id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
digraph H {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  PORT [shape=record,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
        label="{PORT|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="67" lang="2">
                {<s1>DOUT-2|<s2>DOUT-ACK|<s3>DIN-2|<s4>DIN-ACK}}"];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  subgraph cluster0 {
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
    LINKOUT [shape=record,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
            label="{{<e1>DOUT-2|<e2>DOUT-ACK}|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
                    LINK-OUT|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="50" lang="2">
                    {<s1>DOUT|<s2>WE|<s3>BUSY}}"];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
    LINKIN [shape=record,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="45" lang="2">
            label="{{<e1>DIN-2|<e2>DIN-ACK}|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
                    LINK-IN|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="50" lang="2">
                    {<s1>DIN|<s2>RE|<s3>AVAIL}}"];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  };
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  SCHED [shape=record,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="70" lang="2">
         label="{{<s1>DOUT|<s2>WE|<s3>BUSY|<s4>DIN|<s5>RE|<s6>AVAIL}|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
                 SCHEDULER|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
                 {<r1>START-i|<r2>STOP-i|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="56" lang="2">
                  <r3>READ-i|<r4>WRITE-i|<r5>DATA_RD-i|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
                  <r6>DATA_WR-i|<g1>GD-i}}"]; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  PRO [shape=record,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
       label="{{<rp1>START|<rp2>STOP|<rp3>READ|<rp4>WRITE|\
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
                <rp5>DATA_RD|<rp6>DATA_WR|<gp>GD}|PRO-i}"];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  SCHED:g1 -> PRO:gp;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  PRO:rp1 -> SCHED:r1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  PRO:rp2 -> SCHED:r2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  PRO:rp3 -> SCHED:r3;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  PRO:rp4 -> SCHED:r4;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  SCHED:r5 -> PRO:rp5; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  PRO:rp6 -> SCHED:r6;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  SCHED:s1 -> LINKOUT:s1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  SCHED:s2 -> LINKOUT:s2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  LINKOUT:s3 -> SCHED:s3;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  SCHED:s5 -> LINKIN:s2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  LINKIN:s1 -> SCHED:s4;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  LINKIN:s3 -> SCHED:s6;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  LINKOUT:e1 -> PORT:s1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  PORT:s2 -> LINKOUT:e2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  PORT:s3 -> LINKIN:e1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  LINKIN:e2 -> PORT:s4;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
}
</TEXT_UNIT>
</Text_line>
</Graph_content>
<Graph_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
Link Block Interconnect
</TEXT_UNIT>
</Simple_paragraphe>
</Graph_desc>
</Graph>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
Types
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
Data Types
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="48">
</PAGE_BREAK>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Abstract Object Types
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
Product Types: Array
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
Product Types: Structure
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Sum Types: Enumeration
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
Exceptions
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
Control Statements
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Counting for-Loop
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Conditional while-Loop
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
Endless always-Loop
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Blocking wait-for-Loop
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
Conditional if-else-Branch
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Multicase match-Branch
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Exception Handler
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
I/O: Hardware Port Interface
</TEXT_UNIT>
</Section_title>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="49">
</PAGE_BREAK>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
Components: Interfacing HDL
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
External Module Interface: Embedding HDL
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="2b64550" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
External Module Interface EMI
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="99" lang="2">
The External Module Interface (EMI) is used to connect and merge VHDL modelled
 components with the 
</TEXT_UNIT>
<NAME id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
CONPRO
</TEXT_UNIT>
</NAME>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
 process framework. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="458" lang="2">
The purpose of the EMI module interface is to embedd and connect external VHDL
 code directly into a ConPro implementation with direct access from inside Con
Pro processes using the Abstract Data Type Object (ADTO) method interface and 
method calls. In contrast to  external VHDL components requiring signal object
s for interconnection with ConPro, here VHDL processes are linked invisible an
d transparent to the programmer view with the ConPro ADTO interface.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="51" lang="2">
An EMI module can be opened and compiled using the 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="4" lang="2">
open
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
 statement.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="135" lang="2">
The EMI module is splitted in the access and implementation part of an abstrac
t object.  The EMI module file (file suffix .mod) defines
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Ordered_list id="0" ssch="Book" psch="BookP" $List_Numbers="6">
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
all methods available for object access,
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="142" lang="2">
the method access, on ConPro process level defining data and control path part
s and the implementation (scheduling) of the object access, too,
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
all required signals for process interconnect and implementation,
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="63" lang="2">
the implementation of the abstract object using VHDL processes.
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
</Ordered_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="95" lang="2">
Each EMI module defines a new abstract object type. Objects can be created fro
m this new type. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="179" lang="2">
The EMI language is a modified subset of VHDL, aligned to ConPro programming l
anguage concepts. The language semantic is layered on  hardware behaviour leve
l using signal objects.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Parameter Section
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#parameter
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#parameter
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  $pname;          -- (1)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  $pname <= m;     -- (2)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
  $pname[n1,n2,n3,...] <= m;  -- (3)
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="50">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
  $pname[a to b] <= m;  -- (4)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="177" lang="2">
This is the paramter section.  Parameters can be used inside the EMI module fi
le. New values can be assigned either on object creation or using method calls
 (set method class). 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="150" lang="2">
This section defines the parameter variables used in an external module interf
ace definition .  Different forms of parameter definitions are provided:
</TEXT_UNIT>
</Simple_paragraphe>
<Ordered_list id="0" ssch="Book" psch="BookP" $List_Numbers="6">
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="274" lang="2">
Giving only the parameter name preceeded by the $ character defines a variable
 without any default and initialized value.  On object instantiation the param
eter must be assigned a value otherwise an error occurs during synthesis, or u
sing the set class method alternatively.
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="121" lang="2">
In this case the parameter variable gets a default value. Parameter value assi
gnment on object instantiation is optional.
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="214" lang="2">
In this case the parameter variable gets a default value. Parameter value assi
gnment on object instantiation is optional. Additional  a set  of allowed valu
es is included in paranthesises after the parameter name. 
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="216" lang="2">
In this case the parameter variable gets a default value. Parameter value assi
gnment on object instantiation is optional. Additional  a range  of allowed va
lues is included in paranthesises after the parameter name. 
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
</Ordered_list>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#parameter
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
  $datawidth[8,10,12,14,16] <= 8;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
  $seed <= 0xffff;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
  $arch001["fifo","static"] <= "fifo"
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Methods Section
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
#methods
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
#methods
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
  mname(exprside:datatype [,exprside:datatype]);          
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="51">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
exprside ::= '#lhs' | '#rhs' | '#lrhs'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
datatype ::= 'logic' | 'logic[' width ']' |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="50" lang="2">
             'int[' width ']' | 'bool' | 'natural'
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="219" lang="2">
This is the method programming interface declaration section of the EMI module
 file. This section defines all exported and accessible methods,  specifying  
the method name and the method call parameter type declaration:
</TEXT_UNIT>
</Simple_paragraphe>
<Ordered_list id="0" ssch="Book" psch="BookP" $List_Numbers="6">
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="218" lang="2">
the way the argument objects are used during method call, either on left-hand-
side (LHS) or right-hand-side (RHS) (or both) of an expression, meaning read o
r write access respectively of the object used as an argument,
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="209" lang="2">
the expected data type of the argument object,  though width scaling of the ac
tual argument used in method call to the expected method paramter used in the 
ADTO implementation is supported by the EMI compiler.
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
</Ordered_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="144" lang="2">
If a method doesn't expect an argument, an empty paranthesis pair () is used i
n the definition. Up to 9 method call parameters can be specified.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#parameter
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  $datawidth[8 to 16] <= 8;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  $addrwidth <= 16;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
#methods
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
  init();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  read(#lhs:logic[12]);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  time(#rhs:natural);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
  read2(#lhs:logic[$datawidth],#rhs:logic[$addrwidth]);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Interface Section
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#interface
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#interface
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
  signal sname_$O : dir datatype;     -- (1)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
  foreach $p in $P do                 -- (2)
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="52">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
    signal sname_$O : dir datatype;      
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
  foreach $p in $P.meth do            -- (3)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
    signal sname_$O : dir datatype;      
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
dir ::= 'in' | 'out' | 'inout'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
datatype ::= 'logic' | 'logic[' width ']' |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
             'int[' width ']' | bool |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
          'std_logic' | 'std_logic_vector[' width ']' |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
          'signed[' width ']'
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="20" lang="2">
ConPro-Process-Level
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="714" lang="2">
This interface section defines the part of the VHDL component port interface r
equired for implementation of ConPro processes accessing methods of this ADT o
bject. ConPro processes are synthesized to VHDL components and RTL and a finit
e-state-machine (FSM). The ConPro module toplevel is synthesized and mapped to
 a VHDL  component, too, implementing ConPro toplevel objects. Additionally, t
he synthesized ConPro-process VHDL-components are structurally connected on th
is module level. An abstract object access requires data and control signals, 
routed up from the ConPro-process level to the ConPro-module level where the a
bstract object is implemented, except of abstract objects defined on ConPro pr
ocess level.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="51" lang="2">
There are two ways for adding process port signals:
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
(1,2)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="207" lang="2">
Generic signals independent on a particular method access. These signals are a
dded to the VHDL process port for each process using the abstract object.  The
 signal name can contain  the object name variable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="2" lang="2">
$O
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(3)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="138" lang="2">
Signals depending  on ConPro processes accessing this object and a specific me
thod. The signal name can contain  the object name variable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="2" lang="2">
$O
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="96" lang="2">
. The signal definifion adds  the signal only  for  ConPro processes  using th
is object method. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
The signal port direction and the signal type must be specified. 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
Supported signal directions are:
</TEXT_UNIT>
</Simple_paragraphe>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="53">
</PAGE_BREAK>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
in
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
The related process  reads from this signal.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
out
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
The related process writes to this signal. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
inout
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="102" lang="2">
The related process both reads from and writes to the signal. This is the bidi
rectional bus behaviour.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="85" lang="2">
Supported signal types are aligned to the core ConPro data type system, and th
ey are:
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
logic
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
ConPro 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
logic
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="72" lang="2">
 data type,  width 1 bit, mapped in general to the VHDL std_logic type. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
logic[n]
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
ConPro 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
logic
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="134" lang="2">
 data type, width n bit, index range is in general [n-1 downto 0], mapped in g
eneral to the VHDL std_logic_vector(n-1 downto 0)  type.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
int[n]
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
ConPro signed integer (
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="3" lang="2">
int
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
) data type, width n bit, index range is in general 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="14" lang="2">
[n-1 downto 0]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
, mapped in general to the VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="20" lang="2">
signed(n-1 downto 0)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  type.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
bool
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
ConPro boolean (bool) data type,   mapped in general to the VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="9" lang="2">
std_logic
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 type.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
std_logic
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="14" lang="2">
std_logic type
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
std_logic_vector[n]
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="30" lang="2">
std_logic_vector(n-1 downto 0)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="75" lang="2">
 type. Index direction and range depends also on ConPro synthesis settings.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
signed[n]
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="20" lang="2">
signed(n-1 downto 0)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="76" lang="2">
  type. Index direction and range depends also on ConPro synthesis settings.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="54">
</PAGE_BREAK>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#interface
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  foreach $p in $P.read do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
    signal F_$O_RE: out std_logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
    signal F_$O_RD: in std_logic_vector[$datawidth];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  foreach $p in $P.init do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
    signal F_$O_INIT: out std_logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  foreach $p in $P do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
    signal F_$O_GD: in std_logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Mapping Section
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
#mapping
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
#mapping
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
begin  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
  foreach $p in $P do        -- (1)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
    signame_$O => signame_$O_$p;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
  foreach $p in $P.meth do   -- (2)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
    signame_$O => signame_$O_$p;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="19" lang="2">
ConPro-Module-Level
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="596" lang="2">
This mapping section defines the part of the VHDL component port mapping requi
red for implementation of ConPro processes accessing methods of this ADT objec
t. ConPro processes are synthesized to VHDL components and RTL and a finite-st
ate-machine (FSM). The ConPro module toplevel is synthesized and mapped to a V
HDL  component, too, implementing ConPro toplevel objects. Additionally, the s
ynthesized ConPro-process VHDL-components are structurally connected and mappe
d on this module level. An abstract object access requires data and control si
gnals, routed up from the ConPro-process level to 
</TEXT_UNIT>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="55">
</PAGE_BREAK>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="125" lang="2">
the ConPro-module level where the abstract object is implemented, except of ab
stract objects defined on ConPro process level.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="374" lang="2">
On the left hand side there is the (local) ConPro-process context level (VHDL 
entity port interface) signal, on the right hand side there is the (global) Co
nPro-module context level signal (VHDL component port mapping on instantiation
).  The object name $O and process name $P are replaced respectively. All sign
al mappings appearing in this section  must be defined in the 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="10" lang="2">
#interface
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  section.
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(1)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="68" lang="2">
The signal mapping is applied to each process accessing this object.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(2)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="92" lang="2">
The signal mapping is applied to each process accessing this object with a spe
cified method.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
#mapping
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  foreach $P.read do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
    F_$O_RE => F_$O_$P_RE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
    F_$O_RD => F_$O_$P_RD;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  foreach $P.init do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
    F_$O_INIT => F_$O_$P_INIT;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  foreach $P do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
    F_$O_GD => F_$O_$P_GD;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
Access Section
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
#access
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
method:#access
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  #data
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="56">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
   signame_$O <= expr1 when $ACC else expr0;  -- (1)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
   $ARG# <= signame_$O when $ACC else expr0; -- (1b)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
   ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
  #control 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
   null;  -- (2)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
   wait for cond-expr;     -- (3)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  #set
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
   $param <= $ARG#;        -- (4)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
   ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="20" lang="2">
ConPro-Process-Level
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="592" lang="2">
ConPro processes are synthesized to VHDL components and RTL and a finite-state
-machine (FSM). The ConPro module toplevel is synthesized and mapped to a VHDL
  component, too, implementing ConPro toplevel objects. Additionally, the synt
hesized ConPro-process VHDL-components are structurally connected and mapped o
n this module level. An abstract object access requires data and control signa
ls, routed up from the ConPro-process level to the ConPro-module level where t
he abstract object is implemented, except of abstract objects defined on ConPr
o process level.For each method defined in the
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="9" lang="2">
 #methods
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
 section there is an access definition. 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="324" lang="2">
An access definition consists of the data and control path of a ConPro-process
 defined in subsections #data and #control respectively.  There are methods on
ly required for setting object parameters on toplevel. In this case the #set s
ubsection is used instead, but can be used additionally to data and control pa
th sections.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="120" lang="2">
The data path defines expression assignments 1. of local signals, 2. of values
 to  object access signals defined in the 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="10" lang="2">
#interface
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="154" lang="2">
 section, or 3. alternatively assigning  these object access signals to a meth
od call argument. The method call arguments are related with the variables  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="22" lang="2">
$ARG1, $ARG2, $ARG3...
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="64" lang="2">
 for the first, the second, the third ... method call argument. 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="156" lang="2">
The control path is used to suspend the ConPro process control state machine (
calling this method) untill a condition is satisfied, mainly the object guard.
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(1)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="202" lang="2">
Expression expr1 is assigned to the LHS signal during access, expression expr0
 otherwise. Independent of the data type of the LHS, expr0 can be the natural 
number 0.  The data type of the LHS object is 
</TEXT_UNIT>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="57">
</PAGE_BREAK>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="84" lang="2">
determined automatically in this case, and hence the VHDL value to be assigned
, too.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="119" lang="2">
The LHS is an object access signal. The RHS can be an object access signal, a 
method call argument or a constant value.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
(1b)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="84" lang="2">
An object access signal or a constant value is assigned to the method call arg
ument 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
$ARG#
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
.  The variable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
$ARG#
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="195" lang="2">
 is substituted with the actual argument signal name. Control signals required
 for the method argument acces (like the write enable signal)  are generated a
utomatically by the synthesis compiler!
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(2)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="141" lang="2">
There is no control path statement. Object access never blocks control path an
d consumes exactly on time unit. Else case (3) must be applied:
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(3)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="198" lang="2">
The method access  blocks the control path of the calling ConPro process until
l condition cond-expr is satisfied. Else case (2) must be applied. Usually the
 object guard signal is used for blocking:
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="19" lang="2">
signame_$o_GD = '0'
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
;
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(4)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="387" lang="2">
The actual argument value is assigned to the parameter variable on the LHS.  T
his is a method call statement used only for configuration of the object, eith
er on toplevel outside processes or inisde a process! Either an integer value 
can be assigned or a data object can be imported and accessed inside the ADTO 
implementation (actually only signals and registers with read access only).
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="740" lang="2">
Environment variables are always array types. Each time the set subsection is 
used to assign argument values to an environment variable, the actual value is
 added to this array. Therefore some array functions exists which can be used 
in expressions. For example it is desired to work with different baud rates of
 a serial communication link, and the baud rate should be changeable during ru
ntime. In this case it is not usefull to pass the original baud rate value eac
ht time a aspecified set method occurs in ConPro-processes, it is more likely 
to pass an index value requiring much less bits to each method call.  Here is 
an example to implement such an method access (time) for the case of a timer (
using the environment variable $time):
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
time: #access
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  #set
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
    $time <= $arg1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="58">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  #data
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
    TIMER_$O_TIME_SET <= '1' when $ACC else '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
    TIMER_$O_TIME <= #index($time,$ARG1) when $ACC else 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  #control
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
    wait for TIMER_$O_GD = '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
end; 
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
init: #access
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  #data
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
    F_$O_INIT <= '1' when $ACC else '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  #control
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    null;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
read: #access
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  #data
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
    F_$O_RE <= '1' when $ACC else '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
    $ARG1 <= F_$O_RD when $ACC else 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  #control
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
    wait for F_$O_GD = '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
time: #access
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  #set
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
    $time <= $ARG1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Signals Section
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
#signals
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="59">
</PAGE_BREAK>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
#signals   [(cond)]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
  signal $signame_$O : datatype;          -- (1)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  foreach $p in $P do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
    signal $signame_$O_$p : datatype;     -- (2)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  foreach $p in $P.meth do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
    signal $signame_$O_$p : datatype;     -- (3)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
  type typname is {                       -- (4)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    el1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    el2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  };
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
  type typname array[range]               -- (5)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
       of datatype;  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
datatype ::= 'logic' | 'logic[' width ']' |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
             'int[' width ']' | bool |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
          'std_logic' | 'std_logic_vector[' width ']' |
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
          'signed[' width ']'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
cond ::= '$' param '=' value [ 'and' '$' param '=' value...]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
range ::= a 'to' b | a 'downto' b | size
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="19" lang="2">
ConPro-Module-Level
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="561" lang="2">
This section defines VHDL signals required for object implementation on global
 module level, and data and control signals required for method access from Co
nPro processes. Remember the ConPro system hierarchy: there is a process level
 and a module level containing processes. Each ConPro process is synthesized i
nto a VHDL component entity. A ConPro module is also synthesized into a VHDL c
omponent entity, providing the interconnections for all contained ConPro proce
sses. Each process accessing this ADT object requires it own set of data and c
ontrol signals.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="177" lang="2">
There can be exist more than one signals section. There are unconditional (the
 usual case) and conditional signals sections, only applied if parameter condi
tions are satisfied. 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
There are three different signal classes:
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(1)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="99" lang="2">
Generic signals independent on ConPro processes and method access, mainly impl
ementation dependent.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="60">
</PAGE_BREAK>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(2)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="122" lang="2">
Signals depending on ConPro processes accessing this object. The signal name c
an contain the ConPro process name variable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="2" lang="2">
$P
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="66" lang="2">
 (array, foreach statement required) and the object name variable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="2" lang="2">
$O
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="159" lang="2">
. The signal definifion adds for each ConPro process accessing this object the
 specified signal, the process variable is replaced by the related process nam
e. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(3)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="119" lang="2">
Signals depending  on ConPro processes and method access. The signal name can 
contain the ConPro process name variable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="2" lang="2">
$P
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="66" lang="2">
 (array, foreach statement required) and the object name variable 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="3" lang="2">
$O.
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="192" lang="2">
 The signal definifion adds for each ConPro process accessing this object and 
applying the specified method the specified signal, the process variable is re
placed by the related process name. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(4)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
Definition of an enumerated symbolic type.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
(5)
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Definition of an array type.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="85" lang="2">
Supported signal types are aligned to the core ConPro data type system, and th
ey are:
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
logic
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
ConPro 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
logic
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="72" lang="2">
 data type,  width 1 bit, mapped in general to the VHDL std_logic type. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
logic[n]
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
ConPro 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
logic
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="134" lang="2">
 data type, width n bit, index range is in general [n-1 downto 0], mapped in g
eneral to the VHDL std_logic_vector(n-1 downto 0)  type.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
int[n]
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
ConPro signed integer (
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="3" lang="2">
int
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
) data type, width n bit, index range is in general 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="14" lang="2">
[n-1 downto 0]
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
, mapped in general to the VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="20" lang="2">
signed(n-1 downto 0)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  type.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
bool
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
ConPro boolean (bool) data type,   mapped in general to the VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="9" lang="2">
std_logic
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
 type.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
std_logic
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="14" lang="2">
std_logic type
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="1" lang="2">
 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="61">
</PAGE_BREAK>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
std_logic_vector[n]
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="30" lang="2">
std_logic_vector(n-1 downto 0)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="75" lang="2">
 type. Index direction and range depends also on ConPro synthesis settings.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
signed[n]
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
VHDL 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="20" lang="2">
signed(n-1 downto 0)
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="76" lang="2">
  type. Index direction and range depends also on ConPro synthesis settings.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
#signals
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  --
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  -- Implementation signals
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
  --
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
  signal F_$O_d_in: std_logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
  signal F_$O_data_shift: std_logic_vector[$datawidth];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="53" lang="2">
  signal F_$O_data: std_logic_vector[$datawidth*2-1];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
  signal F_$O_shift: std_logic; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
  signal F_$O_init: std_logic; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
  signal F_$O_avail: std_logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  foreach $p in $P.read do 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
    signal F_$O_$p_RE: std_logic;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="52" lang="2">
    signal F_$O_$p_RD: std_logic_vector[$datawidth];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  foreach $p in $P.init do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
    signal F_$O_$p_INIT: std_logic;    
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
  end;  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  foreach $p in $P do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
    signal F_$O_$p_GD: std_logic;    
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
#signals ($datawidth=8)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
  signal F_$O_count: std_logic_vector[3];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
#signals ($datawidth=10)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
  signal F_$O_count: std_logic_vector[4];
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Process Section
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="62">
</PAGE_BREAK>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
#process
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
procname:#process   [(cond)]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
cond ::= '$' param '=' value ['and' '$' param '=' value...]
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="19" lang="2">
ConPro-Module-Level
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="447" lang="2">
This section defines a named  VHDL hardware process (procname) required for th
e implementation of an object on hardwae behaviour level. There can be several
 process sections, each defining one process appearing on ConPro-module level,
 or in some limited cases on ConPro-process level iff the object  has only a C
onPro process local context and was defined inside a ConPro process.  A VHDL h
ardware process implementation can be conditional (cond).
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="375" lang="2">
At least one process must exist for the object implementation: the access sche
duler guarding the (usually) shared object. Several ConPro processes can acces
s a shared object, therefore some kind of mutual exclusion lock must be implem
ented. The main object implementation, modelling the behaviour of this ADTO,  
is usually modelled within a separate VHDL process definition. 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="85" lang="2">
Parameter variables are extensively used inside the VHDL hardware process defi
nition:
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
$CLK
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="199" lang="2">
This parameter is used inside conditional expressions and is only true if ther
e is a system clock event. The clock edge is determined by the ConPro program 
and compiler settings, and expands to VHDL:
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="75" lang="2">
$CLK => conpro_system_clk'event and conpro_system_clk = '1' -- rising edge 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="77" lang="2">
$CLK => conpro_system_clk'event and conpro_system_clk = '0' -- falling edge  
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="69" lang="2">
The clock signals are already defined and may not be defined in the  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="8" lang="2">
#signals
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
 section.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
$RES
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="142" lang="2">
This parameter is used inside conditional expressions and is only true if the 
system reset is active. The active reset signal  logic level is 
</TEXT_UNIT>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="63">
</PAGE_BREAK>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="76" lang="2">
determined by the ConPro program and compiler settings, and expands to VHDL:
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
$RES => conpro_system_reset = '1'  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
$RES => conpro_system_reset = '0'   
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="69" lang="2">
The reset signals are already defined and may not be defined in the  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="8" lang="2">
#signals
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
 section.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
$myreg
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="314" lang="2">
ConPro data objects (actually only signals) can be imported into an object mod
ule. For example a module implements a bus interface, than external bus signal
s must be imported. They are attached to a module parameter $myreg (or any oth
er name excpet reserved parameters) using the access set method, defined in th
e 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="8" lang="2">
 #access
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
 section.
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
EMI:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  #methods
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
    set(#rhs:logic[8]);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  set:#access
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    #set:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
      $myreg <= $ARG1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  #process
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
    s <= $myreg;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
    $myreg <= s;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
ConPro: 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  myobj.set(sigx1y);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
VHDL:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
  s <= $myreg; => s <= sigx1y_RD;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
  $myreg <= s; => sigx1y_WR <= s;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="72" lang="2">
The imported signals are already defined and may not be defined in the  
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="8" lang="2">
#signals
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
 section.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
The sensivity list of the VHDL process is computed automatically.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
VHDL Subset
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="118" lang="2">
Only a subset of VHDL is supported, and there are some adjustments on syntax l
evel to the ConPro programming language.
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
if-then-else
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="89" lang="2">
Syntax is slightly modified. A group of statements requires block environment 
begin-end. 
</TEXT_UNIT>
</Simple_paragraphe>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="64">
</PAGE_BREAK>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
if expr then statement ;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
if expr then statement else statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="61" lang="2">
statement ::= single-statement | 'begin' statement-list 'end'
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
if-then-else-cascade
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="106" lang="2">
Either modelled using the elsif VHDL or else if ConPro construct or modelled w
ith the sequence construct. 
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
if expr then statement else if expr then statement ...;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="53" lang="2">
if expr then statement elsif expr then statement ...;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="61" lang="2">
statement ::= single-statement | 'begin' statement-list 'end'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<$30 id="0" ssch="Book" psch="BookP">
</$30>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
sequence
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  if expr then statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  if expr then statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  foreach $p in $P do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
    if expr then statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  foreach $p in $P.meth do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
    if expr then statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  if others then statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="149" lang="2">
The sequence is expanded to a if-then-elsif cascade.  The last case (optional)
 is the default case if no other conditional expression can be applied.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Example:
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
sequence
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  if a = '1' then s <= 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  if b = '1' then s <= 2;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  foreach $p in $P.init do 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
    if c_$p = '1' then s <= 3;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  if others then s <= 4;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="2" len="16" lang="2">
=> expands to =>
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
if a = '1' then s <= 0
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
elsif b = '1' then s <= 2
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
elsif c_p1 = '1' then s <= 3
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
elsif c_p2 = '1' then s <= 3
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
else s <= 4; 
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="260" lang="2">
During synthesis, conditional expressions, containing only constant values and
 environment variables, are tried to be evaluated to constant values. Dependin
g on the result either the true or the false case statements are replaced by t
he conditional statement. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="65">
</PAGE_BREAK>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
case
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="135" lang="2">
Syntax is slightly modified and aligned to the ConPro programming language. A 
group of statements requires block environment begin-end.
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
case expr is
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
begin            
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  when val1 : statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  when val2 : statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
  when others : statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="64" lang="2">
statement ::= single-statement | 'begin' statement-list 'end' | 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
              'null'
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
for
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="291" lang="2">
Syntax is slightly modified and aligned to the ConPro programming language. A 
group of statements requires block environment begin-end.  The loop variable i
 can be used in expressions within the loop body. Environment array variables 
 (preceeded with a $) can be iterated in a for-loop, too.
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
for i = expr dir expr do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
for $i in $array do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
dir ::= 'to' | 'downto' 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
statement = single-statement | 'begin' statement-list 'end' 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
constant values
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Syntax  is slightly modified:  
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
process variables
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="80" lang="2">
VHDL process variables are defined at the beginning of the process section bod
y:
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
#process:
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  variable vname: datatype;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Expressions
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="138" lang="2">
VHDL expressions can contain any VHDL operator (arithmetic, logic, relational,
 boolean), vector subranges [] and the object selector '''. 
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
+ - * / ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
< > = /= <= >= 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
and or xor ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
obj[range]
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="66">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
obj'sel
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
range ::=  a 'to' b | a 'downto' b 
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Environment Variables
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="141" lang="2">
VHDL expressions can contain environment variables, those names are preceeded 
by a $, both on left-hand- and right-hand-side of expressions. 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
Usually values are assigned in 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="10" lang="2">
#parameter
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
 sections, on object creation and within 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="4" lang="2">
#set
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
 subsections of 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="8" lang="2">
#access 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="338" lang="2">
sections. Environment variables are always of array type. That means each time
 a new value is assigned to an environment variable, a new array element is cr
eated an appended.  A scalar read access of an environment variable returns th
e top of the array (the last element stored). But there are builtin functions 
to access array elements. 
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
$name            <=>  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  scalar read access of an 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  environment variable 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
size($array)    <=>  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
  returns number of array elements 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
  actually stored in the array
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
width($array)   <=>  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
  returns number of bits required 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
  for the maximal value in array 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  and weighted binary coding
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
index_width($array)  <=>  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
  returns number of bits required 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  for the index of an array
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  and weighted binary coding
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
index($array,value)  <=> 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
  returns the binary coded index 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  selector for value element
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
  contained in array
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
min($array)     <=>  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
  returns minimal value of all 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  elements in the array
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
max($array)     <=>  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
  returns maximal value of all 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
  elements in the array
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
Printing
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="105" lang="2">
During object synthesis informational text lines can be printed to the standar
d output channel using the 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="5" lang="2">
print
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
 function.
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
#print("Achieved baud rate accuracy [bit/s]: ",
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="63" lang="2">
       "[actual = ",$clock / (($clock / (16 * $ARG1))*16),"] ",
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
       "[requested = ",$ARG1,"] ",
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
       "[error = ",((($clock / (($clock / 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="56" lang="2">
                   (16 * $ARG1))*16))*1000)/$ARG1)-1000,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
                 " %%]" );
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Arg_paragr>
</Arg_item>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="67">
</PAGE_BREAK>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Process Access and Scheduler
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="224" lang="2">
Access of ADT objects  requires control and data signals. In the case of data 
based objects (for example a queue or RAM), ConPro method calls activate contr
ol signals, and either write to or read from data signals, commonly:
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
Control Signals
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
T_$O_RE: Read Request Enable
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
T_$O_WE: Write Request Enable
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
T_$O_GD: Object Guard
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
Data Signals
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
T_$O_RD: Read Data Signal Vector
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
T_$O_WR: Write Data Signal Vector
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="169" lang="2">
In the case of pure control objects (for example a semaphore), only control si
gnals are activated. Of course for special purpose objects different signals a
re required. 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="389" lang="2">
Because several ConPro processes can access a shared object, access serializat
ion and blocking of method caller processes are required. If there is actually
 already an object access, method call from other processes must be blocked un
till the reosurce is available. For this purpose the gurad signal is used. As 
long as the signal is in state '1', the calling process FSM will be blocked. 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
Warning and Limitations
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="350" lang="2">
Only a subset of VHDL is supported, and there are some adjustments on syntax l
evel to the ConPro programming language. Mainly, the EMI-language is context f
ree. That means that function call arguments are enclosed in round paranthesis
, thereby range expressions (both in signal declarations and within expression
) are enclosed in bracket paranthesis!
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
TIMER_$O_SCHED: #process 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
  if $CLK then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
    if $RES then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
      TIMER_$O_ENABLED <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
      TIMER_$O_MODE <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
      TIMER_$O_COUNTER <= 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
       to_logic(0,width((max($time)*$clock)/1000000000));
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
      TIMER_$O_COUNT <= 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
       to_logic((nth($time,1)*$clock)/1000000000,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
                width((max($time)*$clock)/1000000000));
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
      
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
      foreach $p in $P do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
      begin
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="68">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
        TIMER_$O_$p_GD <= '1';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
      end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
      foreach $p in $P.await do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
      begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
        TIMER_$O_$p_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
      end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
    end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
    begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
      foreach $p in $P do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
      begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
        TIMER_$O_$p_GD <= '1';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
      end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
      if $arch002 = 2 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
      begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
        if TIMER_$O_ENABLED = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
          if TIMER_$O_COUNTER = 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="63" lang="2">
             to_logic(0,width((max($time)*$clock)/1000000000)) 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
            foreach $p in $P.await do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
            begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
              if TIMER_$O_$p_LOCKed = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
              begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
                TIMER_$O_$p_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
                TIMER_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
              end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
            end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="39" lang="2">
            if TIMER_$O_MODE = '0' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
            begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
              TIMER_$O_COUNTER <= TIMER_$O_COUNT;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
            end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
            else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="38" lang="2">
              TIMER_$O_ENABLED <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
          end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="53" lang="2">
            TIMER_$O_COUNTER <= TIMER_$O_COUNTER - 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
      end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
      
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
      sequence
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
      begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
        foreach $p in $P.init do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
          if TIMER_$O_$p_INIT = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
            TIMER_$O_COUNTER <= 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="64" lang="2">
              to_logic(0,width((max($time)*$clock)/1000000000));
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
            TIMER_$O_COUNT <= to_logic((nth($tim
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
        if $arch001 = 1 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
          foreach $p in $P.await do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="73" lang="2">
            if TIMER_$O_$p_AWAIT = '1' and TIMER_$O_$p_LOCKed  = '0' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
            begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
              TIMER_$O_$p_LOCKed <= '1';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
            end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
        if $arch001 = 2 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="69">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="62" lang="2">
          if expand($P.await,$p,or,TIME_$O_$p_AWAIT = '1' and 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="51" lang="2">
                    TIMER_$O_$p_LOCKed = '0') then 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
            foreach $p in $P.await do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
            begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="45" lang="2">
              if TIMER_$O_$p_AWAIT = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
              begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
                TIMER_$O_$p_LOCKed <= '1';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
              end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
            end;            
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
        foreach $p in $P.start do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
          if TIMER_$O_$p_START = '1'  then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
            TIMER_$O_COUNTER <= TIMER_$O_COUNT;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
            TIMER_$O_ENABLED <= '1';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
            TIMER_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
        foreach $p in $P.stop do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
          if TIMER_$O_$p_STOP = '1'  then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
            TIMER_$O_COUNTER <= 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="64" lang="2">
              to_logic(0,width((max($time)*$clock)/1000000000));
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
            TIMER_$O_ENABLED <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
            TIMER_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
        foreach $p in $P.time do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="45" lang="2">
          if TIMER_$O_$p_TIME_SET = '1'  then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
            TIMER_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
            sequence
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
            begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
              foreach $this_time in $time do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
              begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="66" lang="2">
                if TIMER_$O_$p_TIME = index($time,$this_time) then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
                  TIMER_$O_COUNT <= 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
                   to_logic(($this_time*$clock)/1000000000,
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="67" lang="2">
                            width((max($time)*$clock)/1000000000));
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
              end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
            end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
        foreach $p in $P.mode do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="45" lang="2">
          if TIMER_$O_$p_MODE_SET = '1'  then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
            TIMER_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
            TIMER_$O_MODE <= TIMER_$O_$p_MODE;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
        if $arch002 = 1 then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
        begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
          if others then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
          begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
            if TIMER_$O_ENABLED = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
            begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
              if TIMER_$O_COUNTER = 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="71" lang="2">
                 to_logic(0,width((max($time)*$clock)/1000000000)) then
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="70">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
              begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
                foreach $p in $P.await do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
                begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="50" lang="2">
                  if TIMER_$O_$p_LOCKed = '1' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
                  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
                    TIMER_$O_$p_LOCKed <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
                    TIMER_$O_$p_GD <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
                  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
                end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
                if TIMER_$O_MODE = '0' then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
                 begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="54" lang="2">
                   TIMER_$O_COUNTER <= TIMER_$O_COUNT;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
                 end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
                 else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
                   TIMER_$O_ENABLED <= '0';
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
              end
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
             else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
              begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
                TIMER_$O_COUNTER <= TIMER_$O_COUNTER - 1;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
              end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
            end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
          end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
        end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
      end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="8" lang="2">
    end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
         
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
Tool Description Interface TDI
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="108" lang="2">
The Tool Description Interface (TDI) is used to emit project specific synthesi
s and technology tool scripts.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="213" lang="2">
Using the TDI tool interface provides a unique and easy way to emit project to
ol script files for post hardware synthesis and simulation. Different target s
cript languages are supported, for example bash and make.
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="45" lang="2">
The TDI tool file (file suffix .tool) defines
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Ordered_list id="0" ssch="Book" psch="BookP" $List_Numbers="6">
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
environment parameters (immutable) and variables (mutable),
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
target and auxilliary functions,
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
and a target scheduler.
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
</Ordered_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="133" lang="2">
Each TDI tool defines a new tool description. There are different output langu
age targets. Actually only BASH scripts are supported. 
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
Parameter section 
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="71">
</PAGE_BREAK>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#parameter
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#parameter
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  $pname <= expr;     -- (1)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
  if expr then statement else statement;  -- (2)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
  foreach $iname in $array do  -- (3)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
    statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
   
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="56" lang="2">
statement ::= assign ';' | 'begin' assign-list 'end' ';'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
assign-list ::= assign ';' assign ';' ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
assign ::= $pname '<=' expr
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="63" lang="2">
expr ::= value | function | arith-expr | bool-expr | relat-expr
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
value ::= string | char | int
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
string ::= "..."
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
char ::= '.'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
int ::= [0..9]+
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
function ::= fname '(' arg-list ')'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
arg-list ::= expr , expr , ...
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="277" lang="2">
Environment parameter and variable definition section. Parameters are assigned
 only one time a (constant) value, variables are assigned different values. Pa
rameters are replaced in expression with their actual value, whereby variables
 are used in expressions as real variables.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="156" lang="2">
Global variables are always arrays. That means each time a new value is assign
ed to this variable a new array element is created and appended to the array. 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
Operations on environment parameters:
</TEXT_UNIT>
</Simple_paragraphe>
<Ordered_list id="0" ssch="Book" psch="BookP" $List_Numbers="6">
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
Assignments and definition of an environment parameter.
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
Conditional assignments.
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
<Item_ordered id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="136" lang="2">
Array iteration. The loop body execution is performed with the iteration varia
ble $iname with actual value from the array element set.  
</TEXT_UNIT>
</Simple_paragraphe>
</Item_ordered>
</Ordered_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
There are some predefined parameters and parameter arrays:
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
$proj
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
This project name
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
$vhdl
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="82" lang="2">
Parameter array containg all  synthesized and imported VHDL files of this proj
ect.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="72">
</PAGE_BREAK>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
$port
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="72" lang="2">
Parameter array containg all VHDL toplevel port signals of this project.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
$target
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="70" lang="2">
Parameter array containg all  hardware target devices of this project.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
$clock_edge
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
System clock signal activity level.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
$reset_state
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
Reset signal activity level.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
$simu_cycles
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
Number of simulation clock cycles.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
$simu_period
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
Period of simulation clock cyclce in nanoseconds.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
$encoding
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
Default encoding format.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#parameter
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  $TOP <= $proj;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  $OBJDIR <= "obj";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  $SRCDIR <= ".";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
  $DESIGN <= $proj;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
  $LOG <= $TOP + ".log";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
  $DUP <= "tee";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  $PWD <= get_env($PWD,"/");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="62" lang="2">
  $ALLIANCE_TOP <= get_env($ALLIANCE_TOP,"/opt/alliance-5.0");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
  $BIN <= $ALLIANCE_TOP + "/bin";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
  $CELLS <= $ALLIANCE_TOP + "/cells";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
  $MBK_CATA_LIB <= $CELLS + "/sxlib";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  $SCRAM <= $BIN + "/scram";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
  $VHD2 <= $BIN + "/vhd2vst";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
  $SXLIB_COMP <= $CELLS +"/sxlib/sxlib_components.vhd";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
  $EXEMPLAR <= get_env($EXEMPLAR,"/export/home/leonardo");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
  $LEONARDO_TOP <= $EXEMPLAR;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
  $SPECTRUM <= $LEONARDO_TOP + "/bin/spectrum";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
  $LIBRARY <= "sxlib";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="43" lang="2">
  $ENCODING <= get_opt($encoding,"binary");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  $res <= 0;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
  $PATH <= get_env($PATH,"/bin");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
  $PATH <= $PATH + ":" + "/usr/ccs/bin";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="45" lang="2">
  $SIM_CYCLES <= get_opt($simu_cycles,"100");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="45" lang="2">
  $SIM_PERIOD <= get_opt($simu_period,"100");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="37" lang="2">
  $SIM_RES <= get_opt($simu_res,"5");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
  $CLOCK_EDGE <= get_opt($clock_edge,"1");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  if $CLOCK_EDGE = "1" then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="26" lang="2">
    $CLOCK_EDGE_NEG <= "0"
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="73">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
    $CLOCK_EDGE_NEG <= "1";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
  $RESET_STATE <= get_opt($reset_state,"1");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  if $RESET_STATE = "1" then
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
    $RESET_STATE_NEG <= "0"
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  else
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
    $RESET_STATE_NEG <= "1";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
#parameter
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
  -- VHDL sources
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  foreach $file in $vhdl do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="40" lang="2">
    $VHDL.[$I] <= chop_extension($file);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="17" lang="2">
Function section 
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
#function
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
label:#fun
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
  statement;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
  statement; 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
  ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="60" lang="2">
statement ::= assign | conditional | loop | function | block
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
block ::= 'begin' statement-list 'end' ';'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
statement-list ::= statement ';' statement;...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
assign ::= $pname '<=' expr
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="65" lang="2">
conditional ::= 'if' expr 'then' statement ['else' statement] ';'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
loop ::= for-loop | foreach-loop
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="62" lang="2">
foreach-loop = 'foreach' $iname 'in' $aname 'do' statement ';'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="55" lang="2">
for-loop = 'for' a 'to' | 'downto' b 'do' statement ';'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="63" lang="2">
expr ::= value | function | arith-expr | bool-expr | relat-expr
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
value ::= string | char | int
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="16" lang="2">
string ::= "..."
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
char ::= '.'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
int ::= [0..9]+
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
function ::= fname(arg-list)
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
arg-list ::= expr , expr , ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="74">
</PAGE_BREAK>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="189" lang="2">
This section defines a user function. User defined  functions can be called fr
om any other function and used inside the #target section. User defined functi
ons actually expect no arguments!
</TEXT_UNIT>
</Simple_paragraphe>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
foreach
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="312" lang="2">
This loop is used to iterate the set of all elements of the array $aname. Each
 loop iteration changes the iteration variable $iname to the actual array elem
ent value. This variable can be used in expressions. There is an additional va
riable $I which holds the actual loop iteration number, starting with value 1.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
for
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="325" lang="2">
This loop is used with numerical ranges [a,b]. Each loop iteration changes the
 iteration variable $iname to the actual value of  the set of range values. Th
is variable can be used in expressions. After each loop iteration, the iterati
on variable is incremented by one (to-direction) or decremented by one (downto
-direction). 
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="12" lang="2">
if-then-else
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="161" lang="2">
The conditional expression expr is evaluated.  If the result is true, the firs
t then-statement is executed, if it is false the else-statement, which is opti
onal.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<Element id="0" ssch="Book" psch="BookP">
</Element>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="24" lang="2">
Warnings and Limitations
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="316" lang="2">
Some target scripting languages don't support lokal variables. Lokal variable 
names therefore are prefixed with the function name. Global variables and para
meters, especially shell environment variables, must be assigned a value outsi
de functions on top level, at least using the get_env or get_opt builtin funct
ion!
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
do_synth:#fun
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
  print ("[Creating Leonardo Spectrum command file...]");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  $TCL <= $TOP  + ".tcl";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  $FILES <= "";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  foreach $file in $VHDL do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
    $SRC <= $file + ".vhdl";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
    $FILES <= "../" + $SRC + " " + $FILES;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  create_file ($TCL);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  $PWD <= get_env($PWD,"/");
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="75">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
  write_line ($TCL,"set_working_dir " + $PWD);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
  write_line ($TCL,"set hierarchy_flatten TRUE");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="54" lang="2">
  write_line ($TCL,"set output_file "+ $TOP + ".vhd");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
  write_line ($TCL,"set novendor_constraint_file FALSE");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
  write_line ($TCL,"set bubble_tristates FALSE");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
  write_line ($TCL,"set encoding "+ $ENCODING);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
  write_line ($TCL,"load_library "+ $LIBRARY);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
  write_line ($TCL,"read -dont_elaborate {"+ $FILES + "}");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  write_line ($TCL,"pd");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="73" lang="2">
  write_line ($TCL,"read -technology " + $LIBRARY + " {" + $FILES + "}");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="98" lang="2">
  write_line ($TCL,"pre_optimize -common_logic -unused_logic -boundary -xor_co
mparator_optimize");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
  write_line ($TCL,"pre_optimize -extract");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  write_line ($TCL,"pd");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
  write_line ($TCL,"optimize .work.MOD_"+ $TOP + 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
  $CMD <= $SPECTRUM + " -f "+$TCL;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
  write_line($LOG,"["+$CMD+"]");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  $res <= exec($CMD);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  check();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
  append_file ("leospec.log",$LOG);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="23" lang="2">
Builtin Core Functions 
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
Name
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19" !PRFont="3">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
TDI core functions
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
Syntax
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
call-fun ::= fname '(' arg-list ')'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
arg-list ::= par1 ',' par2 ',' ...
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="56" lang="2">
infix-fun ::= '+' '-' '/' '*' '=' '<>' '<' '>' '>=' '<='
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="18" lang="2">
prefix-fun ::= '-'
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
type file ::= string
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
type path ::= string
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="11" lang="2">
Description
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="296" lang="2">
There are a large set of builtin functions wich can be used within expressions
. There are functions  with a variable number of arguments indicated with dots
 '...'. All following arguments of same type as the last specified before the 
dots are optional. Files are always identified by their name. 
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="266" lang="2">
The following function type declarations have some special notations: Function
s without returning a result, commonly procedures,  return the dummy type unit
,  polymorph functions are typed with type'a. All function arguments are place
d within paranthesis pairs '()'.
</TEXT_UNIT>
</Simple_paragraphe>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="76">
</PAGE_BREAK>
<Argument_list id="0" ssch="Book" psch="BookP">
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Operators
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
fun +: type'a -> type'a -> type'a 
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
If 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="10" lang="2">
type'a=int
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="63" lang="2">
 this operation adds the two operands with result type integer.
</TEXT_UNIT>
</Simple_paragraphe>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
If 
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRFont="3" len="13" lang="2">
type'a=string
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="76" lang="2">
  this  operation concatenates both string operands with result type string.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
IO
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
fun print: (string,...) -> unit
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="75" lang="2">
Print string arguments concatenated to one line to standard output channel.
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
fun print_line: (string,...) -> unit
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="67" lang="2">
Print string arguments each in one line to standard output channel.
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="36" lang="2">
fun write: (file,string,...) -> unit
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="93" lang="2">
Print string arguments concatenated to one line to a file specified with the f
irst argument. 
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="41" lang="2">
fun write_line: (file,string,...) -> unit
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="84" lang="2">
Print string arguments each in one line to a file specified with the first arg
ument.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
File Management
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="31" lang="2">
fun create_file: (file) -> unit
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="76" lang="2">
Create a file whose file-name and path is specified with the first argument.
</TEXT_UNIT>
</Simple_paragraphe>
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="29" lang="2">
fun open_file: (file) -> unit
</TEXT_UNIT>
</Text_line>
</Preformatted>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="84" lang="2">
Open an existing file whose file-name and path is specified with the first arg
ument.
</TEXT_UNIT>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
</Argument_list>
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<Element id="0" ssch="Book" psch="BookP">
</Element>
</Simple_paragraphe>
</Arg_paragr>
</Arg_item>
<Arg_item id="0" ssch="Book" psch="BookP">
<Arg_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
Example
</TEXT_UNIT>
</Arg_name>
<Arg_paragr id="0" ssch="Book" psch="BookP">
<Preformatted id="0" ssch="Book" psch="BookP">
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="13" lang="2">
do_synth:#fun
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="5" lang="2">
begin
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="57" lang="2">
  print ("[Creating Leonardo Spectrum command file...]");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  $TCL <= $TOP  + ".tcl";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="15" lang="2">
  $FILES <= "";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="27" lang="2">
  foreach $file in $VHDL do
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="7" lang="2">
  begin
</TEXT_UNIT>
</Text_line>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="77">
</PAGE_BREAK>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
    $SRC <= $file + ".vhdl";
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="42" lang="2">
    $FILES <= "../" + $SRC + " " + $FILES;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
  end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
  create_file ($TCL);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="28" lang="2">
  $PWD <= get_env($PWD,"/");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
  write_line ($TCL,"set_working_dir " + $PWD);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
  write_line ($TCL,"set hierarchy_flatten TRUE");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="54" lang="2">
  write_line ($TCL,"set output_file "+ $TOP + ".vhd");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
  write_line ($TCL,"set bubble_tristates FALSE");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="47" lang="2">
  write_line ($TCL,"set encoding "+ $ENCODING);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="46" lang="2">
  write_line ($TCL,"load_library "+ $LIBRARY);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="59" lang="2">
  write_line ($TCL,"read -dont_elaborate {"+ $FILES + "}");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  write_line ($TCL,"pd");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="73" lang="2">
  write_line ($TCL,"read -technology " + $LIBRARY + " {" + $FILES + "}");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="98" lang="2">
  write_line ($TCL,"pre_optimize -common_logic -unused_logic -boundary -xor_co
mparator_optimize");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="44" lang="2">
  write_line ($TCL,"pre_optimize -extract");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
  write_line ($TCL,"pd");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="49" lang="2">
  write_line ($TCL,"optimize .work.MOD_"+ $TOP + 
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="88" lang="2">
                   ".main -target sxlib -macro -area -effort quick -hierarchy 
flatten");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="62" lang="2">
  write_line ($TCL,"optimize_timing .work.MOD_"+$TOP+".main");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="68" lang="2">
  write_line ($TCL,"report_area -cell_usage -hierarchy -all_leafs");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="82" lang="2">
  write_line ($TCL,"report_delay  -num_paths 1 -critical_paths -clock_frequenc
y");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="58" lang="2">
  write_line ($TCL,"auto_write -format VHD "+$TOP+".vhd");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="48" lang="2">
  print_line ("[Forking Leonardo Spectrum...]");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="34" lang="2">
  $CMD <= $SPECTRUM + " -f "+$TCL;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="32" lang="2">
  write_line($LOG,"["+$CMD+"]");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="2" lang="2">
  
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="73" lang="2">
  $res <= exec($  write_line ($TCL,"set novendor_constraint_file FALSE");
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="10" lang="2">
  check();
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="35" lang="2">
  append_file ("leospec.log",$LOG);
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="4" lang="2">
end;
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
<Text_line id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Text_line>
</Preformatted>
</Arg_paragr>
</Arg_item>
</Argument_list>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
m
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="4" lang="0">
Code
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="9" lang="2">
Synthesis
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Sections id="0" ssch="Book" psch="BookP">
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="22" lang="2">
Basic Scheduling Model
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="30" lang="2">
Synthesis RulesSynthesis Rules
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="33" lang="2">
Expression Modells and Allocation
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<PAGE_BREAK id="0" ssch="Book" psch="BookP" PageType="0" PageNumber="78">
</PAGE_BREAK>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Symbol" len="1" lang="11">
m
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" $Langue="x-Latin1" len="19" lang="0">
Code Transformation
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="25" lang="2">
Reference Stack Scheduler
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="21" lang="2">
Basic Block Scheduler
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
<Section id="0" ssch="Book" psch="BookP">
<Section_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
Expression Scheduler
</TEXT_UNIT>
</Section_title>
<Section_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Section_content>
</Section>
</Sections>
</Section_content>
</Section>
</Sections>
<Appendices id="0" ssch="Book" psch="BookP">
<Appendix id="0" ssch="Book" psch="BookP">
<Appendix_title id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="14" lang="2">
Internal Notes
</TEXT_UNIT>
</Appendix_title>
<Appendix_content id="0" ssch="Book" psch="BookP">
<Paragraphe id="0" ssch="Book" psch="BookP" $New_page="2">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="3" lang="2">
...
</TEXT_UNIT>
</Simple_paragraphe>
</Paragraphe>
</Appendix_content>
</Appendix>
</Appendices>
<Bibliography id="0" ssch="Book" psch="BookP">
<Literature_entry id="0" ssch="Book" psch="BookP">
<Biblio_name id="2b64720" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="6" lang="2">
[RU87]
</TEXT_UNIT>
</Biblio_name>
<Biblio_desc id="0" ssch="Book" psch="BookP">
<Simple_paragraphe id="0" ssch="Book" psch="BookP" $Symbols="19">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="19" lang="2">
Steven M. Rubini  Š
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" !PRStyle="4" len="30" lang="2">
Computer Aids For VLSI DesignŠ
</TEXT_UNIT>
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="20" lang="2">
Addision Wesley 1987
</TEXT_UNIT>
</Simple_paragraphe>
</Biblio_desc>
</Literature_entry>
<Literature_entry id="0" ssch="Book" psch="BookP">
<Biblio_name id="0" ssch="Book" psch="BookP">
<TEXT_UNIT id="0" ssch="Book" psch="BookP" len="0" lang="2">
</TEXT_UNIT>
</Biblio_name>
<Biblio_desc id="0" ssch="Book" psch="BookP">
<Paragr id="0" ssch="Book" psch="BookP">
</Paragr>
</Biblio_desc>
</Literature_entry>
</Bibliography>
</Book>
