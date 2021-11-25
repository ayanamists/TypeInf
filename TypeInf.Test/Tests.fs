module Tests

open Xunit
open logic.core.Logic
open Inf
open Ast
open Parser
open Unify

let testInf (x:Expr) = 
    initInfEngine ()
    let t, _ = infJ x (Map.ofList [])
    getCleanTypedAst t

let testInfT (x:Expr) = 
    initInfEngine ()
    infJ x (Map.ofList []) |> snd

[<Fact>]
let ``Int`` () =
    Assert.Equal(
        testInf (Int 10),
        TInt 10
    )

[<Fact>]
let ``Bool`` () =
    Assert.Equal(
        testInf (Bool true),
        TBool true
    )

[<Fact>]
let ``Id`` () = 
    Assert.Equal(
        testInf (Let ("x", Ast.Int 10, Id "x")),
        TLet ("x", iT, TInt 10, TId "x")
    )

[<Fact>]
let ``Id function`` () =
    Assert.Equal(
        testInf (Let ("f", (Lambda ("x", Ast.Id "x")), App (Ast.Id "f", Ast.Int 10))),
        TLet ("f"
             ,Forall ("0", arrowT (Var "0", Var "0"))
             ,TLambda ("x", TId "x")
             ,TApp (TId "f", TInt 10))
    )

[<Fact>]
let ``Id is poly`` () =
    let prg = Let ("f", (Lambda ("x", Ast.Id "x"))
                  ,Let ("a", (App (Ast.Id "f", Ast.Int 10))
                       ,(App (Ast.Id "f", Ast.Bool true))))
    let expected = TLet ("f"
                        ,Forall ("3", arrowT (Var "3", Var "3"))
                        ,TLambda ("x", TId "x")
                        ,TLet ("a", iT
                              ,TApp (TId "f", TInt 10)
                              ,TApp (TId "f", TBool true)))
    Assert.Equal(
        expected, testInf prg
    )

[<Fact>]
let ``combinator logic`` () = 
    let prg = Let (
        "S", Lambda ("f", (Lambda ("g", 
                                    Lambda ("x", App (App (Ast.Id "g", Ast.Id "x"), 
                                                      App (Ast.Id "f", Ast.Id "x")))))),
        Let ("K", Lambda ("x", Lambda ("y", Ast.Id "x")),
             Let ("I", App (App (Ast.Id "S", Ast.Id "K"), Ast.Id "K"), 
                  Ast.Id "I"))
    ) 
    Assert.Equal(
        Forall ("18", Basic (Arrow (Var "18", Var "18"))),
        testInfT prg
    )

[<Fact>]
let ``union test`` () =
    let t1 = F ("->", [V "a1"; F ("->", [F ("->", [V "b"; V "a1"]); V "c"])])
    let t2 = F ("->", [V "a2"; F ("->", [V "b2"; V "a2"])])
    let sset = Map.ofList []
    let usset = unifyToSet sset t2 t1
    Assert.Equal(
        F ("->", [V "c"; V "c"]),
        subst usset (F ("->", [V "a1"; V "c"]))
    )