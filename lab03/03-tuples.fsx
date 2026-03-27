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
  | StringConst _ ->
      String

  | NumberConst _ ->
      Number

  | Binary(op, l, r) ->
      let supportedOps = set ["*"; "/"; "+"; "-"]
      if not (supportedOps.Contains op) then
        failwith $"Unknown operator: {op}"
      else
        let leftType = typeCheck ctx l
        let rightType = typeCheck ctx r
        if leftType <> Number then
          failwith $"Left argument of '{op}' must be a Number, but got {leftType}"
        elif rightType <> Number then
          failwith $"Right argument of '{op}' must be a Number, but got {rightType}"
        else
          Number

  | Variable v ->
      if ctx.ContainsKey v then
        ctx.[v]
      else
        failwith $"Variable '{v}' is unbound"

  | If(e1, e2, e3) ->
      let conditionType = typeCheck ctx e1
      if conditionType <> Number then
        failwith $"Condition of 'if' must be a Number, but got {conditionType}"
      else
        let branch1Type = typeCheck ctx e2
        let branch2Type = typeCheck ctx e3
        if branch1Type <> branch2Type then
          failwith $"Branches of 'if' must have the same type, but got {branch1Type} and {branch2Type}"
        else
          branch1Type

  | Lambda(v, t, e) ->
      let newCtx = Map.add v t ctx
      let bodyType = typeCheck newCtx e
      Function(t, bodyType)

  | Application(e1, e2) ->
      let funcType = typeCheck ctx e1
      let argType = typeCheck ctx e2
      match funcType with
      | Function(t1, t2) when t1 = argType -> t2
      | Function(t1, t2) -> failwith $"Argument type mismatch: expected {t1}, got {argType}"
      | _ -> failwith $"Expected a function, but got {funcType}"

  | Let(v, e1, e2) ->
      let bindingType = typeCheck ctx e1
      let newCtx = Map.add v bindingType ctx
      typeCheck newCtx e2

  // TODO: Add type checking for MakeTuple(e1, e2) and GetTuple(b, e)!
  // b=true returns the first element; b=false indicates the second.
  | MakeTuple(e1, e2) ->
      Tuple(typeCheck ctx e1, typeCheck ctx e2)

  | GetTuple(b, e) ->
      let tupleType = typeCheck ctx e
      match tupleType with
      | Tuple(t1, t2) when b -> t1
      | Tuple(t1, t2) when not b -> t2
      | _ -> failwith $"Expected a tuple, but got {tupleType}"


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
