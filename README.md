# Substitute

## Overview

An application that relays the messages exchanged between a user and another application (typically a chess engine, but it can be something else).

## Usage

*Substitute* is a command line tool of the same kind (although much less sophisticated) as [InBetween](https://www.chessprogramming.org/InBetween) by Odd Gunnar Malin, or as [UciFilter](http://www.nnuss.de/Hermann/UciFilter.html) by Volker Annuss. Instead of starting an engine, you start *Substitute* with the engine name as parameter (or written in **substitute.ini**). Substitute creates a file (**substitute.log**) where you can see the messages sent by the user and the messages sent by the engine.

## Compilation

*Substitute* is a Pascal program, for the Free Pascal Compiler. It has been tested only under Linux, but should work also under other environments.

To compile the program you can use the following command:

```
fpc -Mobjfpc -Sh substitute.pas
```
