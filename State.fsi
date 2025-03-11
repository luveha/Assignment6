module Interpreter.State
    open Language
    
    type state = {m: Map<string,int>; mem: Memory.memory}
    val mkState : int -> state 

    val declare : string -> state -> state option
    val getVar : string -> state -> int option
    val setVar: string -> int -> state -> state option

    val alloc : string -> int -> state -> state option
    val free : int -> int -> state -> state option
    val setMem: int -> int -> state -> state option
    val getMem: int -> state -> int option