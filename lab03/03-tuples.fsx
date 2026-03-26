// ============================================================================
// 03 - Extend the type checker with tuple types
// ============================================================================

type Type =
  | Number
  | String
  | Function of Type * Type
  // NOTE: 'Tuple(t1, t2)' is the type of a pair where the first element
  // has type t1 and the second element has type t2.
  | Tuple of Type * Type

type Expression =
  | StringConst of string
  | NumberConst of int
  | Binary of string * Expression * Expression
  | Variable of string
  | If of Expression * Expression * Expression
  | Let of string * Expression * Expression
  | Lambda of string * Type * Expression
  | Application of Expression * Expression
  // NOTE: Added MakeTuple (constructor) and GetTuple (getter)
  | MakeTuple of Expression * Expression
  | GetTuple of bool * Expression

type TypingContext = Map<string, Type>

// ----------------------------------------------------------------------------
// Type checker
// ----------------------------------------------------------------------------

let rec typeCheck (ctx:TypingContext) expr =
  match expr with
  | StringConst _ -> failwith "implemented in step 1"
  | NumberConst _ -> failwith "implemented in step 1"
  | Binary _ -> failwith "implemented in step 1"
  | Variable _ -> failwith "implemented in step 1"
  | If _ -> failwith "implemented in step 1"
  | Let _ -> failwith "implemented in step 2"
  | Lambda _ -> failwith "implemented in step 2"
  | Application _ -> failwith "implemented in step 2"

  // TODO: Add type checking for MakeTuple(e1, e2) and GetTuple(b, e)!
  // b=true returns the first element; b=false indicates the second.
  | MakeTuple _ | GetTuple _ ->
      failwith "not implemented"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let vars = Map.ofList ["num", Number]

// Correctly typed: ("hello world", num+10) => Tuple(String, Number)
let et1 =
  MakeTuple(StringConst("hello world"),
    Binary("+", Variable "num", NumberConst 10))

typeCheck vars et1

// Correctly typed: let t = ("hello world", num+10) in t#2 + 1 => Number
let et2 =
  Let("t",
    MakeTuple(StringConst("hello world"),
      Binary("+", Variable "num", NumberConst 10)),
    Binary("+", GetTuple(false, Variable("t")), NumberConst 1) )

typeCheck vars et2

// Type error: + applied to string and a number
let et3 =
  Let("t",
    MakeTuple(StringConst("hello world"),
      Binary("+", Variable "num", NumberConst 10)),
    Binary("+", GetTuple(true, Variable("t")), NumberConst 1) )

typeCheck vars et3

// Type error: 't' is bound to a Number, not a tuple
let et4 =
  Let("t", Binary("+", Variable "num", NumberConst 10),
    GetTuple(false, Variable("t")) )

typeCheck vars et4
