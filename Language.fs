module Interpreter.Language

type error =
    | DivisionByZero
    | ReservedName of string
    | VarNotDeclared of string
    | VarAlreadyExists of string
    | InvalidVarName of string
    | OutOfMemory
    | NegativeMemoryAllocated of int
    | MemoryNotAllocated of int

type aexpr =
    | Num of int
    | Var of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Div of aexpr * aexpr
    | Mod of aexpr * aexpr
    | MemRead of aexpr
    
let (.+.) a b = Add (a, b)
let (.-.) a b = Add (a, Mul (b, Num -1))
let (.*.) a b = Mul (a, b)
let (./.) a b = Div (a, b)
let (.%.) a b = Mod (a, b)    
    
type bexpr =
    | TT
    | Eq of aexpr * aexpr
    | Lt of aexpr * aexpr
    | Conj of bexpr * bexpr
    | Not of bexpr
    
let FF = Not TT
let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)

let (.=.) a b = Eq (a, b)   
let (.<.) a b = Lt (a, b)   
let (.<>.) a b = ~~(a .=. b)                (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)  (* numeric smaller than or equal to *)
let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)


type stmnt =
    | Skip
    | Declare of string
    | If of bexpr * stmnt * stmnt
    | While of bexpr * stmnt
    | Assign of string * aexpr
    | Seq of stmnt * stmnt
    | Alloc of string * aexpr
    | MemWrite of aexpr * aexpr
    | Free of aexpr * aexpr
    
let IT(b, c) = If(b, c, Skip)