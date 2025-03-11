module Interpreter.Memory

    type memory = {m: Map<int,int>; next: int}

    val empty: int -> memory

    val alloc : int -> memory -> (memory * int) option
    val free : int -> int -> memory -> memory option
    val setMem: int -> int -> memory -> memory option
    val getMem: int -> memory -> int option