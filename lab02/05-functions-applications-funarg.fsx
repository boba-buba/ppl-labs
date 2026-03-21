// ============================================================================
// 05 - Functions and application - but with the funarg problem!
// ============================================================================

type Value = 
  | ValNum of int 
  // NOTE: A function value such as '(fun x -> ...)' is a value - this means
  // that we can pass them around. We added 'ValClosure' here to represent
  // functions at runtime. The closure stores the variable name of the lambda
  // and the body of the lambda. (We will need to modify this later to support 
  // lexical scoping.)
  | ValClosure of string * Expression

// NOTE: 'ValClosure' above needs to refer to 'Expression'. 
// To make such recursive references, we define 
// the types using 'type .. and .. and' from now on!
and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Log of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added application 'e1 e2' and lambda 'fun v -> e'
  | Application of Expression * Expression
  | Lambda of string * Expression

// ============================================================================
// NOTE: We are going to use eager evaluation in our interpreter, 
// so you should continue from STEP 03 and ignore the changes from STEP 04
// ============================================================================

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
      // TODO: Evaluate a lambda - create a closure value
      failwith "not implemented"

  | Application(e1, e2) ->
      // TODO: Evaluate a function application. Recursively
      // evaluate 'e1' and 'e2'; 'e1' must evaluate to a closure.
      // You can then evaluate the closure body.
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

// Wrong function call (the first argument is not a function)
//   21 (fun x -> x * 2)
let ef3 = 
  Application(
    Constant(21),
    Lambda("x", Binary("*", Variable("x"), Constant(2)))
  )
evaluate Map.empty ef3

// Wrong binary operator (it is now possible to apply '+'
// to functions; this makes no sense and should fail!)
//   21 + (fun x -> x * 2)
let ef4 = 
  Binary("+",
    Constant(21),
    Lambda("x", Binary("*", Variable("x"), Constant(2)))  
  )
evaluate Map.empty ef4

// The FUNARG problem - our variables are dynamically scoped. When
// we call a function, we call it with current variables as they
// are in context when making the call. In the following, 'n' is
// available when the function 'f' is defined, but not when we call it!
// This means that the example crashes!
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

// On the other hand, the following works with dyanmic scoping!
// The variable 'n' is not defined when we create the lambda, but
// it is in scope when we call it - we get 42!
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
