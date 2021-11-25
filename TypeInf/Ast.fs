module Ast

type Expr = 
    | Int of int
    | Bool of bool
    | Id of string
    | Lambda of (string * Expr)
    | Let of string * Expr * Expr
    | App of Expr * Expr

type IType = 
    | I of unit
    | B of unit
    | Var of string
    | Arrow of IType * IType

type Type = 
    | Basic of IType
    | Forall of string * Type

type TypedExpr = 
    | TInt of int
    | TBool of bool
    | TId of string
    | TLambda of string * TypedExpr
    | TLet of string * Type * TypedExpr * TypedExpr
    | TApp of TypedExpr * TypedExpr

let iT = I () |> Basic
let bT = B () |> Basic 
let arrowT (t1, t2) = Basic (Arrow (t1, t2))