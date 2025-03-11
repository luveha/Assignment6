module Interpreter.State

    open Result
    open Language


    let reservedVariableNameList = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
    let reservedVariableName s= List.exists(fun x -> x = s) reservedVariableNameList
    let validVariableName (s: string) = 
        match s.[0] with
        | c when System.Char.IsAsciiLetter(c) || c = '_' -> String.forall(fun c-> System.Char.IsAsciiLetterOrDigit(c) || c = '_') s.[1..]
        | _ -> false

    type state = {m: Map<string,int>; mem: Memory.memory}
    let mkState (memSize: int) = {m = Map.empty<string,int>; mem = Memory.empty memSize}

    let declare x st = 
        match st.m with
        | m when m.ContainsKey x -> None
        | _ when not (validVariableName x) || reservedVariableName x -> None
        | _ -> Some {m = st.m.Add(x,0); mem = st.mem}

    let getVar x st = 
        match st.m.ContainsKey x with
        | true -> Some st.m.[x]
        | _ -> None

    let setVar x v st = 
        match st.m.ContainsKey x with
        | true -> Some {m = st.m.Add(x,v); mem = st.mem}
        | _ -> None

    let random _ = failwith "not implemented"
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"
        
    let alloc (x: string) (size: int) (st: state) = 
        match (Memory.alloc 1 st.mem) with
        | Some(mem, next) -> 
            Some {m=st.m.Add(x,next);mem=mem}
        | None -> None
    
    let free (ptr: int) (size: int) (st: state) = 
            match (Memory.free ptr size st.mem) with
            | Some x -> 
                Some (
                    {
                        m = st.m;
                        mem = x
                    }
                )
            | _ -> None    
    let setMem (ptr: int) (v: int) (st: state) =
        match (Memory.setMem ptr v st.mem) with
        | Some x -> Some (
            {
                m = st.m;
                mem = x
            }
            )
        | _ -> None

    let getMem (ptr: int) (st: state) = 
        match (Memory.getMem ptr st.mem) with 
        | Some x -> Some x
        | _ -> None
    

