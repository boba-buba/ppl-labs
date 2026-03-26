// ============================================================================
// 01 - A simple type checker for expressions with strings and numbers
// ============================================================================

// Types of expressions in our language - we have numbers and strings
type Type =
  | Number
  | String

// Expressions (similar as in lab02) - constants, operators, variables and if
// (The type checker will figure out the type of each expression.)
type Expression =
  | StringConst of string
  | NumberConst of int
  | Binary of string * Expression * Expression
  | Variable of string
  | If of Expression * Expression * Expression

// A typing context maps variable names to their types
// (note, VariableContext in lab02 mapped strings to values!)
type TypingContext = Map<string, Type>

// ----------------------------------------------------------------------------
// Type checker - a recursive function taking a typing context and expression;
// returns the Type of the expression, or fails with a type error.
// ----------------------------------------------------------------------------

let rec typeCheck (ctx:TypingContext) expr =
  match expr with
  | StringConst _ ->
      // TODO: Return the type of a string constant.
      failwith "not implemented"

  | NumberConst _ ->
      // TODO: Return the type of a number constant.
      failwith "not implemented"

  | Binary(op, l, r) ->
      // TODO: Type-check binary expressions. The supported operators are
      // "*", "/", "+", "-". Both arguments must be Number and the result is
      // Number. Fail with an error for unknown operators or for non-number
      // arguments. (Hint: use set ["*"; "/"; "+"; "-"] to define the set.)
      failwith "not implemented"

  | Variable v ->
      // TODO: Look up the variable type in the context using ctx.ContainsKey
      // and ctx[v]. If the variable is not in the context, fail with an error.
      failwith "not implemented"

  | If(e1, e2, e3) ->
      // TODO: Type-check the condition 'e1' and both branches 'e2', 'e3'.
      // * The condition must be Number
      // * Both branches must have the same type
      // * The overall type is the type of the branches
      failwith "not implemented"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let vars = Map.ofList ["num", Number]

// Type error: condition is a String, not a Number
let e1 =
  If(StringConst("oops"),
    Binary("+", NumberConst 40, NumberConst 2),
    Variable("num"))

typeCheck vars e1

// Type error: '+' applied to a String argument
let e2 =
  If(NumberConst 0,
    Binary("+", StringConst "40", NumberConst 2),
    Variable("num"))

typeCheck vars e2

// Type error: variable 'nummmm' is unbound
let e3 =
  If(NumberConst 0,
    Binary("+", NumberConst 40, NumberConst 2),
    Variable("nummmm"))

typeCheck vars e3

// Correctly typed: if 0 then 40+2 else num => result is Number
let e4 =
  If(NumberConst 0,
    Binary("+", NumberConst 40, NumberConst 2),
    Variable("num"))

typeCheck vars e4
