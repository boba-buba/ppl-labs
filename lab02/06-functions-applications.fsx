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
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)
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

  | Let(v, earg, ebody) ->
      // TODO: Evaluate the argument, add it to the current 'ctx' to get
      // a new context and then evalaute body with the new context.
      // failwith "todo"      
      let vArg = evaluate ctx earg
      let newCtx = ctx.Add(v, vArg)
      evaluate newCtx ebody

  | Lambda(v, e) ->
      // TODO: Now capture the variable context when creating a closure!
      ValClosure(v, e, ctx)

  | Application(e1, e2) ->
      // TODO: The body of the closure needs to be evaluated with a context
      // that adds the variable to the captured evaluation context
      // failwith "not implemented"
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1 with
      | ValClosure(param, body, capturedCtx) ->
          let newCtx = capturedCtx.Add(param, v2)
          evaluate newCtx body
      | _ -> failwith "application of non-function"


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
