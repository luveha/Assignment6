module Interpreter.Memory
    
    type memory = {m: Map<int,int>; next: int}

    let empty (memsize:int) = {m = Map.empty<int,int>; next=0}

    let rec alloc size mem =
        match size with
        | x when x > 0 -> 
            Some(
                {m = 
                    (
                    [mem.next .. mem.next+size-1] |> List.fold (fun map input -> Map.add input 0 map) mem.m
                );
                next = mem.next + size
                },
                mem.next
            )
        | _ -> None
    let free (ptr: int) (size: int) (mem: memory) = 
        match (ptr + size) with
        | x when x < mem.next -> 
            Some(
                {m = 
                    (
                    [mem.next .. mem.next+size-1] |> List.fold (fun map input -> Map.remove input map) mem.m
                );
                next = mem.next
                }
            )
        | _ -> None
        
    let setMem (ptr: int) (v: int) (mem: memory) = 
        match mem.m.ContainsKey(ptr) with
        | true -> 
            Some (
                {m=mem.m.Add(ptr,v);next=mem.next}
            )
        | false -> None
        
    let getMem (ptr: int) (mem: memory) = 
        match mem.m.ContainsKey(ptr) with
        | true -> Some (mem.m.[ptr])
        | false -> None