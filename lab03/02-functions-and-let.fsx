// ============================================================================
// 02 - Extend the type checker with let bindings and functions
// ============================================================================

type Type =
  | Number
  | String
  // NOTE: 'Function(t1, t2)' is the type of a function from t1 to type t2.
  // For example, a function that takes a number and returns a string
  // has a type Function(Number, String).
  | Function of Type * Type

type Expression =
  | StringConst of string
  | NumberConst of int
  | Binary of string * Expression * Expression
  | Variable of string
  | If of Expression * Expression * Expression
  | Let of string * Expression * Expression
  // NOTE: Lambda carries a type annotation for its argument - when 
  // writing 'fun x -> ...' the programmer must say what type 'x' has. 
  | Lambda of string * Type * Expression
  | Application of Expression * Expression

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
      // TODO: Type-check the lambda body 'e' in a context extended with
      // variable 'v' having the annotated type 't'. Note that this is
      // why we had to add type to 'Lambda'!
      // failwith "not implemented"
      let newCtx = Map.add v t ctx
      let bodyType = typeCheck newCtx e
      Function(t, bodyType)

  | Application(e1, e2) ->
      // TODO: Type-check e1 and e2. e1 must have a Function(t1, t2) type -
      // its argument type must match the type of e2 and the result is t2.
      // failwith "not implemented"
      let funcType = typeCheck ctx e1
      let argType = typeCheck ctx e2
      match funcType with
      | Function(t1, t2) when t1 = argType -> t2
      | Function(t1, t2) -> failwith $"Argument type mismatch: expected {t1}, got {argType}"
      | _ -> failwith $"Expected a function, but got {funcType}"

  | Let(v, e1, e2) ->
      // TODO: Type check 'let v = e1 in e2' 
      // failwith "not implemented"
      let bindingType = typeCheck ctx e1
      let newCtx = Map.add v bindingType ctx
      typeCheck newCtx e2

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let vars = Map.ofList ["num", Number]

// Correctly typed: let x = 10+20 in x => Number
let ef1 =
  Let("x", Binary("+", NumberConst 10, NumberConst 20),
    Variable("x"))

typeCheck Map.empty ef1

// Type error: 'x' is not in scope in the binding expression
let ef2 =
  Let("x", Variable("x"),
    Binary("+", NumberConst 10, NumberConst 20))

typeCheck Map.empty ef2

// Correctly typed: fun (x:Number) -> x+20 => Function(Number, Number)
let ef3 =
  Lambda("x", Number,
    Binary("+", Variable "x", NumberConst 20))

typeCheck Map.empty ef3

// Type error: '+' applied to a String argument (x has type String)
let ef4 =
  Lambda("x", String,
    Binary("+", Variable "x", NumberConst 20))

typeCheck Map.empty ef4

// Correctly typed: (fun (x:Number) -> x+10) 32 => Number
let ef5 =
  Application(
    Lambda("x", Number, Binary("+", Variable "x", NumberConst 10)),
    NumberConst(32) )

typeCheck Map.empty ef5

// Type error: function expects Number but called with String
let ef6 =
  Application(
    Lambda("x", Number, Binary("+", Variable "x", NumberConst 10)),
    StringConst("32") )

typeCheck Map.empty ef6

// Type error: 32 is not a function
let ef7 =
  Application(
    NumberConst(32),
    Lambda("x", Number, Binary("+", Variable "x", NumberConst 10)))

typeCheck Map.empty ef7
