module Inf

open Ast
open Unify

exception TypeException of string

let genTypeVar = 
    let mutable i = 0
    fun () -> 
        let now = i
        i <- i + 1
        sprintf "%i" now
let mutable sset : UnionFindSet = Map.ofList []

type Gamma = Map<string, Type>
let vdash (g:Gamma) (x:string) = Map.tryFind x g

let rec instaceWith (g:Map<string, string>) (t:Type) = 
    match t with
    | Basic (Var v) ->
        match Map.tryFind v g with
        | Some t -> (Var t)
        | None -> (Var v)
    | Basic (Arrow (t1, t2)) -> 
        Arrow (instaceWith g (Basic t1) , instaceWith g (Basic t2))
    | Basic a -> a
    | Forall (alpha, t) ->
        instaceWith (Map.add alpha (genTypeVar ()) g) t
let instance = instaceWith (Map.ofList [])

let getIType (t:Type) = 
    match t with
    | Basic i -> i
    | Forall _ -> TypeException (sprintf "%A should be iType, but got forall..." t) |> raise

let rec fromIType (i:IType) =
    match i with
    | I _ -> F ("int", [])
    | B _ -> F ("bool", [])
    | Arrow (t1, t2) -> F ("->", [fromIType t1; fromIType t2])
    | Var v -> V v

let rec toIType (t:Term) =
    match t with
    | V v -> Var v
    | F ("int", []) -> I ()
    | F ("bool", []) -> B ()
    | F ("->", [t1; t2]) -> Arrow (toIType t1, toIType t2)
    | _ -> TypeException (sprintf "%A is not proper type term" t) |> raise


let rec getCleanIType (t:IType) = subst sset (fromIType t) |> toIType

let rec getCleanType (t:Type) =
    match t with
    | Basic i -> getCleanIType i |> Basic
    | Forall (x, t1) -> Forall (x, getCleanType t1)

let rec free (t:Type) = 
    match t with
    | Basic (Var v) -> Set.ofList [v]
    | Basic (Arrow (t1, t2)) -> Set.union (free (Basic t1)) (free (Basic t2))
    | Basic _ -> Set.ofList []
    | Forall (x, t1) -> Set.difference (free t1) (Set.ofList [x])

let freeGamma (g:Gamma) = 
    Map.toList g |>
    List.map (fun x -> snd x |> free) |>
    List.fold (Set.union) (Set.ofList [])

let compGammaT (g:Gamma) (t:Type) = 
    let t1 = getCleanType t
    Set.difference (free t1) (freeGamma g) |>
    Set.toList |> fun l -> 
    List.foldBack (fun x s -> Forall (x, s)) l t1

let rec infJ (e:Expr) (gamma:Gamma) : TypedExpr * Type = 
    match e with
    | Int x -> TInt x, I () |> Basic
    | Bool b -> TBool b, B () |> Basic
    | Id x -> 
        match vdash gamma x with
        | Some t ->
            let i = instance t
            (TId x, i |> Basic)
        | None -> TypeException (sprintf "no such var %s" x) |> raise
    | Lambda (var, exp) -> 
        let tVar = genTypeVar () |> Var
        let bTVar = tVar |> Basic
        let (exp', texp) = infJ exp (Map.add var bTVar gamma)
        TLambda (var, exp'), Arrow (tVar, getIType texp) |> Basic
    | Let (v, exp1, exp2) ->
        let exp1', tExp1 = infJ exp1 gamma
        let tgExp1 = compGammaT gamma tExp1
        let exp2', tExp2 = infJ exp2 (Map.add v tgExp1 gamma)
        TLet (v, tgExp1, exp1', exp2'), tExp2
    | App (exp1, exp2) ->
        let exp1', tExp1 = infJ exp1 gamma
        let exp2', tExp2 = infJ exp2 gamma
        let newVar = genTypeVar () |> Var
        sset <- unifyToSet sset 
                (fromIType (Arrow (getIType tExp2, newVar))) 
                (fromIType (getIType tExp1))
        TApp (exp1', exp2'), newVar |> Basic

let rec getCleanTypedAst (exp:TypedExpr) =
    match exp with
    | TLet (v, tv, exp1, exp2) -> TLet (v, getCleanType tv, exp1, exp2)
    | TLambda (v, exp1) -> TLambda (v, getCleanTypedAst exp1)
    | TApp (exp1, exp2) -> TApp (getCleanTypedAst exp1, getCleanTypedAst exp2)
    | _ -> exp

let initInfEngine () = 
    sset <- Map.ofList []