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
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
  | Unary(op, e) ->
      // TODO: Implement the case for 'Unary' here!
      let v = evaluate ctx e
      match v with 
      | ValNum n -> 
          match op with 
          | "-" -> ValNum(-n)
          | _ -> failwith "unsupported unary operator"
      | _ -> failwith "unary operator applied to non-numeric value"
  | If(cond, thenExpr, elseExpr) ->
      // TODO: Implement the case for 'If' here!
      let vCond = evaluate ctx cond
      match vCond with
      | ValNum n when n <> 0 -> evaluate ctx thenExpr
      | ValNum 0 -> evaluate ctx elseExpr
      | _ -> failwith "condition evaluated to non-numeric value"
  | Log(msg, e) -> 
      // TODO: Evaluate the expression 'e', print the result using 
      // printf "%s: %A" (%s for string argument, %A for any argument)
      // and return the evaluated result.
      // failwith "todo"
      let v = evaluate ctx e
      printf "%s: %A\n" msg v
      v
  | Variable(v) -> 
      // TODO: Context now contains unevaluated expressions and so
      // you need to evaluate them when variable is accessed!
      // failwith "todo"
      match ctx.TryFind v with 
      | Some res -> evaluate ctx res
      | _ -> failwith ("unbound variable: " + v)

  | Let(v, earg, ebody) ->
      // TODO: Now we need to store the unevaluated 'earg'!
      // failwith "todo"      
      let newCtx = ctx.Add(v, earg)
      evaluate newCtx ebody

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
