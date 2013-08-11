[In progress]

#A simplified C-style programming language, parser, and compiler
For my final project at [Hackbright Academy](http://www.hackbrightacademy.com), I decided to write a simple programming language called Waffle, along with a Pratt-style parser and a compiler ("Crumpet"), both coded by hand, in order to gain a better fundamental understanding of how programming languages are implemented. After considering a few different targets for compilation (x86 and LLVM, among others), I ultimately settled on [asm.js](http://www.asmjs.org), a low-level subset of Javascript that is in the process of being formalized by a team at Mozilla.

## Language design
Waffle is a simple, statically typed, C-style programming language. Variable types are explicitly declared; I borrowed Go’s array declaration syntax and C’s structures and statement blocks. Waffle supports both inline and multiline comments as well as the following data types: integers, floats, strings, arrays, structures, and booleans. Because I wanted to focus most heavily on the processes of parsing and code generation, and because Waffle is intended as a personal learning exercise rather than a usable scripting language, I made a few expedient choices (and concessions) when it came to designing the syntax.
