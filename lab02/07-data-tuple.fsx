// ============================================================================
// 07 - Add a simple data type - tuples
// ============================================================================

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  // NOTE: A tuple value consisting of two other values.
  // (Think about why we have 'Value' here but 'Expression'
  // in the case of 'ValClosure' above!)
  | ValTuple of Value * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Log of string * Expression
  // NOTE: 'Tuple' represents two-element tuple constructor
  // and 'TupleGet' the destructor (accessing a value)
  // Use 'true' for #1 element, 'false' for #2. This is not
  // particularly descriptive, but it works OK enough.
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

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

  | Tuple(e1, e2) ->
      // TODO: Construct a tuple value here!
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      ValTuple(v1, v2)

  | TupleGet(b, e) ->
      // TODO: Access #1 or #2 element of a tuple value.
      // (If the argument is not a tuple, this fails.)
      let v = evaluate ctx e
      match v with
      | ValTuple(x, y) when b -> x
      | ValTuple(x, y) when not b -> y
      | _ -> failwith "not a tuple value"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1 = 
  TupleGet(true, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed1

let ed2 = 
  TupleGet(false, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed2

// Data types - trying to get a first element of a value
// that is not a tuple (This makes no sense and should fail)
//   (42)#1
let ed3 = 
  TupleGet(true, Constant(42))
evaluate Map.empty ed3
