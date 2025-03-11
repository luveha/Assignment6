module Interpreter.Eval

    open Result
    open Language
    open State
    
    let apply (n1: 'a option) (n2: 'a option) (operation: 'a -> 'a -> 'b)= 
        match n1,n2 with
        |  Some x, Some y -> Some (operation x y)
        | _ -> None

    let rec arithEval a st = 
        match a with
        | Num n -> Some n
        | Var v when v = "random" -> Some 7 //7 is always random
        | Var v when st.m.ContainsKey v -> Some st.m.[v]
        | Add (x,y) ->
                apply (arithEval x st) (arithEval y st) (fun x y -> x+y)
        | Mul (x,y) -> 
                apply (arithEval x st) (arithEval y st) (fun x y -> x*y)
        | Div (x,y) | Mod (x,y)-> 
                match arithEval y st with
                | Some yInt when not (yInt = 0) -> 
                        match a with
                        | Div _ -> apply (arithEval x st) (arithEval y st) (fun x y -> x/y)
                        | _ -> apply (arithEval x st) (arithEval y st) (fun x y -> x%y)
                | _ -> None
        | MemRead (e1) -> 
                match (arithEval e1 st) with 
                | Some n ->
                        match State.getMem n st with 
                        | Some x -> Some x
                        | _ -> None
                | None -> None
        | Var(x) -> 
                match st.m.ContainsKey(x) with
                | true -> State.getMem st.m.[x] st 
                | false -> None

    let rec boolEval b st = 
        match b with
        | TT -> Some true
        | Eq (a,c)-> 
                apply (arithEval a st) (arithEval c st) (fun x y -> x = y)
        | Lt (a,c) -> 
                apply (arithEval a st) (arithEval c st) (fun x y -> x < y)
        | Conj (a,c) -> 
                apply (boolEval a st) (boolEval c st) (fun x y -> x && y)
        | Not a -> apply (boolEval a st) (Some false) (fun x y -> not x)

    let rec stmntEval s st = 
        match s with 
        | Skip -> Some st
        | Declare s -> declare s st
        | Assign(v,a) -> 
                match arithEval a st with 
                | Some x -> setVar v x st
                | None -> None
        | Seq (s1,s2) -> 
                match stmntEval s1 st with
                | Some st' -> 
                        match stmntEval s2 st' with
                        | None -> None
                        | st'' -> st'' 
                | _ -> None 
        | If (gaurd, s1, s2) -> 
                match boolEval gaurd st with
                | Some true -> stmntEval s1 st
                | Some false -> stmntEval s2 st
                | None -> None
        | While (gaurd, s') -> 
                match boolEval gaurd st with
                | Some true -> 
                        match stmntEval s' st with
                        | Some st'' -> stmntEval (While (gaurd, s')) st''
                        | None -> None
                | Some false -> Some st
                | None -> None
        | Alloc (x,e) -> arithEval e st |> Option.bind (fun i -> State.alloc x i st) 
        | Free (e1,e2) -> 
                (arithEval e1 st) |> Option.bind (fun eVal1 -> ((arithEval e2 st) |> Option.bind(fun eVal2 -> State.free eVal1 eVal2 st)))
        | MemWrite(e1,e2) -> 
                (arithEval e1 st) |> Option.bind (fun eVal1 -> ((arithEval e2 st) |> Option.bind(fun eVal2 -> State.setMem eVal1 eVal2 st)))
