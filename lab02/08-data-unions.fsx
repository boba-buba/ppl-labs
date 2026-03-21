// ============================================================================
// 08 - Add more data types - unions
// ============================================================================

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  // NOTE: Value representing a union case. Again, we use 'bool':
  // 'true' for 'Case1' and 'false' for 'Case2'
  | ValCase of bool * Value

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
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  // NOTE: 'Case' represents creating a union value and 'Match' pattern 
  // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching 
  // of the form: 'match e with v -> e1 | v -> e2'
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

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

  | Match(e, v, e1, e2) ->
      // TODO: Implement pattern matching. Note you need to
      // assign the right value to the variable of name 'v'!
      // failwith "not implemented"
      let vMatch = evaluate ctx e
      match vMatch with
      | ValCase(b, matchedValue) when b -> 
          let newCtx = ctx.Add(v, matchedValue)
          evaluate newCtx e1
      | ValCase(b, matchedValue) when not b -> 
          let newCtx = ctx.Add(v, matchedValue)
          evaluate newCtx e2
      | _ -> failwith "not a case value"


  | Case(b, e) ->
      // TODO: Create a union value.
      //failwith "not implemented"
      let v = evaluate ctx e
      ValCase(b, v)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec2

let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec3
