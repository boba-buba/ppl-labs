// ============================================================================
// 06 - Functions and application - now with proper lexical scoping
// ============================================================================

type Value = 
  | ValNum of int 
  // NOTE: The right way to handle lexical scoping is to remember the
  // variable context as it was available when the function was defined.
  // We do this by adding VariableContext to our closure value.
  // (Compilers for C# and similar languages with lambdas need to
  // capture variables when you define function in the same way!)
  | ValClosure of string * Expression * VariableContext

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Log of string * Expression
  | Let of string * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression

and VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant _ -> failwith "implemented in step 1"
  | Binary _ -> failwith "implemented in step 1"
  | Variable _ -> failwith "implemented in step 1"
  | Unary _ -> failwith "implemented in step 2"
  | If _ -> failwith "implemented in step 2"
  | Log _ -> failwith "implemented in step 3"
  | Let _ -> failwith "implemented in step 3"

  | Lambda(v, e) ->
      // TODO: Now capture the variable context when creating a closure!
      failwith "not implemented"

  | Application(e1, e2) ->
      // TODO: The body of the closure needs to be evaluated with a context
      // that adds the variable to the captured evaluation context
      failwith "not implemented"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Basic function declaration (should return closure)
//   (fun x -> x * 2) 
let ef1 = 
  Lambda("x", Binary("*", Variable("x"), Constant(2)))
evaluate Map.empty ef1

// Basic function calls (should return number)
//   (fun x -> x * 2) 21
let ef2 = 
  Application(
    Lambda("x", Binary("*", Variable("x"), Constant(2))),
    Constant(21)
  )
evaluate Map.empty ef2

// This did not work with dynamic scoping, but it works now.
// The variable 'n' is captured when creating a closure and
// so you should get 42.
//
//   let f = 
//     (let n = 21 in (fun x -> n*x)) 
//   f 2
//
let efunarg =
  Let("f", 
    Let("n", Constant 21, 
      Lambda("x", Binary("*", Variable "n", Variable "x"))),
    Application(Variable "f", Constant 2)
  )

evaluate Map.empty efunarg

// On the other hand, the following no longer works with lexical
// scoping, because 'n' is not defined when we create the closure!
//
//   let f = (fun x -> n*x)
//   let n = 21
//   f 2
//
let edyn =
  Let("f", Lambda("x", Binary("*", Variable "n", Variable "x")),
    Let("n", Constant 21, 
      Application(Variable "f", Constant 2)))

evaluate Map.empty edyn
