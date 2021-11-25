module Unify

exception UnifyError of string

type Term = 
    | V of string
    | F of string * Term list

let isV t = match t with 
            | V _ -> true
            | _ -> false
let isF t = isV t |> not

type UnionFindSet = Map<string, Term>

let rec find (s:UnionFindSet) (t:Term) =
    match t with
    | (V v) ->
        match Map.tryFind v s with
        | Some r -> 
            match r with
            | V v2 -> find s r
            | F _ -> r
        | None -> t
    | F _ -> t

let rec union (s:UnionFindSet) (v:string) (t:Term) = Map.add v t s

let occurCheck (v1:Term) (v2:Term) = 
    let rec strOccur (s:string) (v:Term) = 
        match v with
        | V s1 -> s = s1
        | F (_, args) -> List.map (strOccur s) args |> List.fold (||) false
    match v1, v2 with
    | V v1S, F _ -> strOccur v1S v2
    | F _, V v2S -> strOccur v2S v1
    | _, _ -> false


let rec unifyToSet (s:UnionFindSet) (v1:Term) (v2:Term) =
    let v1N = find s v1
    let v2N = find s v2
    if occurCheck v1N v2N then UnifyError "circle!" |> raise
    match v1N, v2N with
    | V v1S, V v2S -> if v1S = v2S then s else union s v1S v2
    | V v1S, _ -> union s v1S v2
    | _, V v2S -> union s v2S v1
    | F (f1, args1), F (f2, args2) ->
        if not (f1 = f2) 
        then UnifyError "no such unification!" |> raise
        else 
            List.zip args1 args2 |>
            List.fold (fun s (t1, t2) ->  unifyToSet s t1 t2) s

let rec subst (s:UnionFindSet) (v1:Term) =
    match v1 with
    | V s1 -> 
        match find s v1 with
        | V s2 -> V s2
        | F (op, l) -> F (op, List.map (subst s) l)
    | F (f, args) -> F (f, List.map (subst s) args)
