// ============================================================================
// 04 - Adding limited polymorphism via unification
// ============================================================================

type Expression =
  | StringConst of string
  | NumberConst of int
  | Binary of string * Expression * Expression
  | Variable of string
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Type * Expression
  | Let of string * Expression * Expression
  | MakeTuple of Expression * Expression
  | GetTuple of bool * Expression

and Type =
  | Number
  | String
  | Function of Type * Type
  | Tuple of Type * Type
  // NOTE: We add 'TypeVariable' to represent an unknown type to be determined
  // by unification. For example, the identity function can be given type
  // Function(TypeVariable "a", TypeVariable "a"), meaning it works for any 'a'.
  | TypeVariable of string

type TypingContext = Map<string, Type>

// ----------------------------------------------------------------------------
// Unification
// ----------------------------------------------------------------------------


// TODO: Implement 'unify'. It takes a list of type constraints (pairs of
// types that must be equal) and tries to find an assignment of type variables
// to concrete types. It returns 'Some assignment' for success or 'None' for
// failure. Note that only the FIRST type in each pair may contain
// TypeVariables - the second is always a concrete type.
let rec unify (constraints:list<Type * Type>) : option<Map<_, _>> =
  match constraints with 
  // EXAMPLE: If the first constraint unifies number & number, 
  // we remove it and solve the remaining constraints...
  | (Number, Number)::constraints -> unify constraints
  
  //  1. (String, String) - types match, continue with the rest
  //  2. (Function(ta1,ta2), Function(tb1,tb2)) 
  //     -> Add (ta1,tb1) and (ta2,tb2) to the remaining constraints
  //  3. Handle tuples similarly to (2) and numbers similarly to (1)
  //  4. (TypeVariable v, t) - First solve the remaining constraints. 
  //     -> If that worked, add mapping from 'v' to 't' to the returned assignment
  //     -> If the assignment already contains type that's not 't', that is an error
  //  5. [] - all constraints solved, return Some Map.empty
  //  6. anything else - types are incompatible, return None
  //| _ -> failwith "todo: implement me!"
  | (String, String)::constraints -> unify constraints

  | (Function(ta1, ta2), Function(tb1, tb2))::constraints ->
      unify ([(ta1, tb1); (ta2, tb2)] @ constraints)
  | (Tuple(ta1, ta2), Tuple(tb1, tb2))::constraints ->
      unify ([(ta1, tb1); (ta2, tb2)] @ constraints)
  | (TypeVariable v, t)::constraints ->
      match unify constraints with
      | Some subst when Map.containsKey v subst ->
          if Map.find v subst = t then
              Some subst
          else
              None
      | Some subst -> Some (Map.add v t subst)
      | None -> None
  | [] -> Some Map.empty
  | _ -> None



// Success: Some [] - Number matches Number, String matches String
unify [(Function(Number, String), Function(Number, String))]

// Success: Some ["a", Function(Number, String)]
unify [(TypeVariable "a", Function(Number, String))]

// Success: Some ["a", Number; "b", String]
unify [(Tuple(TypeVariable "a", TypeVariable "b"), Tuple(Number, String))]

// None: String does not match Number
unify [(Function(String, Number), Function(Number, String))]

// None: Function does not match Tuple
unify [(Function(Number, String), Tuple(Number, String))]

// None: cannot assign two different types to 'a'
unify [(Tuple(TypeVariable "a", TypeVariable "a"), Tuple(Number, String))]


// ----------------------------------------------------------------------------
// Substitution
// ----------------------------------------------------------------------------

// TODO: Implement 'substitute'. Given a substitution map and a type, replace
// every TypeVariable in the type with its assigned type from the map.
// Leave Number and String unchanged. Recurse into Function and Tuple.
// You can assume all TypeVariables in the type are present in the map.

let rec substitute (subst:Map<string, Type>) typ =
  //failwith "not implemented"
  match typ with
  | Number -> Number
  | String -> String
  | Function(t1, t2) -> Function(substitute subst t1, substitute subst t2)
  | Tuple(t1, t2) -> Tuple(substitute subst t1, substitute subst t2)
  | TypeVariable v -> Map.find v subst


let ab = Map.ofList ["a", Number; "b", String]

// Substitute 'a' -> Number in TypeVariable "a" => Number
substitute ab (TypeVariable "a")

// Substitute 'a' -> Number in Function(TypeVariable "a", String) 
// => Function(Number, String)
substitute ab (Function(TypeVariable "a", String))

// Substitute a and b in Tuple(TypeVariable "a", TypeVariable "b")
// => Tuple(Number, String)
substitute ab (Tuple(TypeVariable "a", TypeVariable "b"))


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
      let newCtx = Map.add v t ctx
      let bodyType = typeCheck newCtx e
      Function(t, bodyType)

  | Let(v, e1, e2) ->
      let bindingType = typeCheck ctx e1
      let newCtx = Map.add v bindingType ctx
      typeCheck newCtx e2

  | MakeTuple(e1, e2) ->
      Tuple(typeCheck ctx e1, typeCheck ctx e2)

  | GetTuple(b, e) ->
      let tupleType = typeCheck ctx e
      match tupleType with
      | Tuple(t1, t2) when b -> t1
      | Tuple(t1, t2) when not b -> t2
      | _ -> failwith $"Expected a tuple, but got {tupleType}"

  | Application(e1, e2) ->
      // TODO: Implement function application with unification.
      //
      // Unlike in step 2, functions can now be polymorphic - e.g. an identity
      // function has type Function(TypeVariable "a", TypeVariable "a").
      // Note our unification is one-sided and can have type variables on the left.
      //
      // Steps:
      //  1. Type-check e1 and match the type against Function(t1a, t2). Fail otherwise.
      //  2. Type-check e2 to get the concrete argument type t1b.
      //  3. Call 'unify [(t1a, t1b)]' - this matches the (possibly polymorphic)
      //     expected argument type against the concrete actual argument type.
      //  4. If unification returns Some subst, call 'substitute subst t2' to
      //     instantiate any type variables in the return type. Return the result.
      //  5. If unification returns None, fail with a type mismatch error.
      //failwith "not implemented"
      let funcType = typeCheck ctx e1
      let argType = typeCheck ctx e2
      match funcType with
      | Function(t1a, t2) ->
          match unify [(t1a, argType)] with
          | Some subst -> substitute subst t2
          | None -> failwith $"Argument type mismatch: expected {t1a}, got {argType}"
      | _ -> failwith $"Expected a function, but got {funcType}"


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Context with some useful polymorphic operations
let ops = Map.ofList [
  "id",   Function(TypeVariable "a", TypeVariable "a")
  "dup",  Function(TypeVariable "a", Tuple(TypeVariable "a", TypeVariable "a"))
  "swap", Function(Tuple(TypeVariable "a", TypeVariable "b"),
            Tuple(TypeVariable "b", TypeVariable "a")) ]

// id 42 => Number  (a is instantiated to Number)
let e0 = Application(Variable("id"), NumberConst(42))

typeCheck ops e0

// swap 42 => type error: 42 is a Number, not a Tuple
let e1 = Application(Variable("swap"), NumberConst(42))

typeCheck ops e1

// dup "hello!" => Tuple(String, String), then get first element => String
let e2 = GetTuple(true, Application(Variable("dup"), StringConst "hello!"))

typeCheck ops e2

// fun (tup : Number * String) -> (swap tup)#1 => Function(Tuple(Number,String), String)
let e3 =
  Lambda("tup", Tuple(Number, String),
    GetTuple(true, Application(Variable("swap"), Variable "tup")))

typeCheck ops e3

// (fun (f : Number -> Number) -> f 10) (fun x -> x+1) => Number
let elf = Lambda("f", Function(Number, Number),
  Application(Variable "f", NumberConst 10))

let elf1 = Application(elf,
  Lambda("x", Number, Binary("+", Variable "x", NumberConst 1)))

typeCheck ops elf1

// (fun (f : Number -> Number) -> f 10) id
// In a more sophisticated system, we could do this! But because our 
// unification is one-sided (only allows type variables on the left), this
// currently fails. (It would have to unify 'Number -> Number' with 'a -> a' ).
let elf2 = Application(elf, Variable "id")

typeCheck ops elf2
