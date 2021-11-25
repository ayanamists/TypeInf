module Parser

open Ast
open Farkle
open Farkle.Builder
open Farkle.Builder.Regex
open System

exception ParseException of string

let curry2 f = fun a b -> f (a, b)
let curry3 f = fun a b c -> f (a, b, c)

let integer = Terminals.int "int"
let boolean = 
    regexString @"true|false" 
    |> terminal "bool" (T (fun _ x -> x.ToString() |> Convert.ToBoolean))
let binding = 
    regexString @"[a-z]"
    |> terminal "binding" (T (fun _ x -> x.ToString()))

let untyped = 
    let untyped = nonterminal "untyped"
    untyped.SetProductions(
        !@ integer => Int,
        !@ boolean => Bool,
        !@ binding => Id,
        !& "let" .>>. binding .>> ":=" .>>. untyped .>> "in" .>>. untyped => curry3 Let,
        !& "fun" .>>. binding .>> "=>" .>>. untyped => curry2 Lambda,
        !@ untyped .>>. untyped => curry2 App
    )
    untyped

let runtimeUT = RuntimeFarkle.build untyped

let parseUntyped s = 
    match RuntimeFarkle.parseString runtimeUT s with
    | Ok r -> r
    | Error err -> 
        printfn "%O" err
        ParseException (sprintf "%O" err) |> raise