// ============================================================================
// 04 - Modifying let to use lazy evaluation
// ============================================================================

type Value = 
  | ValNum of int 

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Let of string * Expression * Expression
  | Log of string * Expression

// NOTE: Modified from the previous step. Rather than storing the 
// evaluated value, we now store unevaluated expressions
type VariableContext = 
  Map<string, Expression>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant _ -> failwith "implemented in step 1"
  | Binary _ -> failwith "implemented in step 1"
  | Unary _ -> failwith "implemented in step 2"
  | If _ -> failwith "implemented in step 2"  
  | Log _ -> failwith "implemented in step 3"

  | Variable _ -> 
      // TODO: Context now contains unevaluated expressions and so
      // you need to evaluate them when variable is accessed!
      failwith "todo"

  | Let(v, earg, ebody) ->
      // TODO: Now we need to store the unevaluated 'earg'!
      failwith "todo"      

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Simple let: let x = 3*7 in x+x => 42
let eletx = 
  Let("x", Binary("*", Constant(3), Constant(7)),
    Binary("+", Variable("x"), Variable("x")))

evaluate Map.empty eletx

// Testing the 'log' function: + evaluates before *
let elog = 
  Log("evaluating *",
    Binary("*",
      Log("evaluating +", Binary("+", Constant(1), Constant(2))),
      Constant(10) ))

evaluate Map.empty elog


// NOTE! Lazy evaluation - this should now print 'evaluating *' twice!
let elog1 = 
  Let("x", Log("evaluating *", Binary("*", Constant(3), Constant(7))),
    Binary("+", Variable("x"), Variable("x")))

evaluate Map.empty elog1
