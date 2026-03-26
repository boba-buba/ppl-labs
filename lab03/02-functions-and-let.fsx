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
  | StringConst _ -> failwith "implemented in step 1"
  | NumberConst _ -> failwith "implemented in step 1"
  | Binary _ -> failwith "implemented in step 1"
  | Variable _ -> failwith "implemented in step 1"
  | If _ -> failwith "implemented in step 1"

  | Lambda(v, t, e) ->
      // TODO: Type-check the lambda body 'e' in a context extended with
      // variable 'v' having the annotated type 't'. Note that this is
      // why we had to add type to 'Lambda'!
      failwith "not implemented"

  | Application(e1, e2) ->
      // TODO: Type-check e1 and e2. e1 must have a Function(t1, t2) type -
      // its argument type must match the type of e2 and the result is t2.
      failwith "not implemented"

  | Let(v, e1, e2) ->
      // TODO: Type check 'let v = e1 in e2' 
      failwith "not implemented"

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
