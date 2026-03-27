// ============================================================================
// 05 - Adding records with structural subtyping
// ============================================================================

// NOTE: Records are a new kind of value. A record type lists the field
// names and their types. Unlike tuples, fields are accessed by name.
//
// We also want a limited form of subtyping: a function expecting a record with
// field {age:Number} can accept any record that has at least that field
// (e.g. {name:String, age:Number} is fine). This is checked in the unification.

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
  // NOTE: MakeRecord constructs a record from a map of field names to
  // expressions; GetRecord accesses a named field of a record expression.
  | MakeRecord of Map<string, Expression>
  | GetRecord of Expression * string

and Type =
  | Number
  | String
  | Function of Type * Type
  | Tuple of Type * Type
  | TypeVariable of string
  // NOTE: Record type is a map from field names to their types. For example: 
  // {name:String, age:Number} is Record(Map.ofList ["name",String; "age",Number])
  | Record of Map<string, Type>

type TypingContext = Map<string, Type>

// ----------------------------------------------------------------------------
// Useful F# functions
// ----------------------------------------------------------------------------

// For checking record types, you may need functions for working with sets, e.g.:
Set.isSubset (set ["a";"b"]) (set ["a";"b";"c"])  // true
Set.isSubset (set ["a";"d"]) (set ["a";"b";"c"])  // false

// You can create a set from a collection of values, such as Keys of a map:
let m1 = Map.ofList ["x", Number; "y", String]
let s1 = set m1.Keys 

// When you have a map, you can transform its values using 'Map.map", e.g.:
Map.map (fun k v -> Tuple(v, v)) m1

// If you need to turn any collection to a list, you can use 'List.ofSeq':
List.ofSeq s1
List.ofSeq m1.Keys

// You can concatenate two lists using the '@' operator:
let l1 = [1;2;3]
let l2 = [4;5;6]
let l = l1 @ l2

// You can iterate over lists using 'List.map', e.g.: this fetches the 
// value for each key, which is useless, but shows the syntax!
List.map (fun k -> m1[k]) (List.ofSeq m1.Keys)

// ----------------------------------------------------------------------------
// Unification
// ----------------------------------------------------------------------------

// NOTE: Extend 'unify' with a case for records. This adds limited subtyping, i.e.:
// (Record r1, Record r2) can be unified when r2 has at least all
// the fields that r1 requires (r1's keys are a subset of r2's keys). 
// Fields present in r1 must have types that unify pairwise.

let rec unify constraints : option<Map<_, _>> =
  match constraints with
  | (Record r1, Record r2) :: constraints ->
      // TODO: Check that every field name in r1 also appears in r2 using
      // Set.isSubset and on Keys. If not, return None.
      //
      // Otherwise build a list of pairwise field-type constraints for each
      // key in r1: (r1[k], r2[k]). Append these to 'constraints' (use @)
      // and call unify on the combined list.
      //
      // (Hint: use List.ofSeq r1.Keys to iterate over the keys of r1.)
      //failwith "not implemented"
      let r1Keys = set r1.Keys
      let r2Keys = set r2.Keys
      if not (Set.isSubset r1Keys r2Keys) then
        None
      else
        let fieldConstraints =
          List.map (fun k -> (r1[k], r2[k])) (List.ofSeq r1.Keys)
        unify (fieldConstraints @ constraints)

  // TODO: Copy your implementation from the previous step
  | (Number, Number)::constraints -> unify constraints

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

// Success: r2 has all fields of r1 (and more) - Some ["a", Number]
unify [Record(Map.ofList ["thing", TypeVariable "a"]),
       Record(Map.ofList ["thing", Number; "more", String])]

// None: r1 requires "more" but r2 does not have it
unify [Record(Map.ofList ["thing", Number; "more", String]),
       Record(Map.ofList ["thing", Number])]


// ----------------------------------------------------------------------------
// Substitution
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Type>) typ =
  match typ with
  | Record r ->
      // TODO: Apply substitute to every field type in r using Map.map.
      // (Hint: Map.map takes a function (key -> value -> result))
      //failwith "not implemented"
      Record (Map.map (fun _ v -> substitute subst v) r)

  // TODO: Copy your implementation from the previous step
  | Number -> Number
  | String -> String
  | Function(t1, t2) -> Function(substitute subst t1, substitute subst t2)
  | Tuple(t1, t2) -> Tuple(substitute subst t1, substitute subst t2)
  | TypeVariable v -> Map.find v subst

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
      let funcType = typeCheck ctx e1
      let argType = typeCheck ctx e2
      match funcType with
      | Function(t1a, t2) ->
          match unify [(t1a, argType)] with
          | Some subst -> substitute subst t2
          | None -> failwith $"Argument type mismatch: expected {t1a}, got {argType}"
      | _ -> failwith $"Expected a function, but got {funcType}"

  | MakeRecord(fields) ->
      // TODO: Type-check every field expression and collect the results into
      // a map of field types. Return Record of that map.
      // failwith "not implemented"
      let fieldTypes = Map.map (fun _ v -> typeCheck ctx v) fields
      Record fieldTypes

  | GetRecord(e, field) ->
      // TODO: Type-check e - it must be a Record type. Check that 'field'
      // is present in the record's field map (use .ContainsKey). Return the
      // type of that field. Fail if it is not there.
      // failwith "not implemented"
      let recordType = typeCheck ctx e
      match recordType with
      | Record fieldMap when fieldMap.ContainsKey field ->
          fieldMap.[field]
      | Record _ ->
          failwith $"Field '{field}' does not exist in the record"
      | _ ->
          failwith $"Expected a record, but got {recordType}"


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Success: {name:"Yoda", age:700}.age + 1 => Number
let e1 =
  Binary("+",
    GetRecord(
      MakeRecord(Map.ofList ["name", StringConst "Yoda"; "age", NumberConst 700]),
      "age"),
    NumberConst 1)

typeCheck Map.empty e1

// Fail: {name:"Yoda"}.age => type error: field "age" does not exist
let e2 =
  GetRecord(
    MakeRecord(Map.ofList ["name", StringConst "Yoda"]),
    "age")

typeCheck Map.empty e2

// Success: (fun (x:{age:Number}) -> x.age + 1) {name:"Yoda", age:700} => Number
// The function requires only 'age'; the argument has 'name' too - that is fine.
let e3 =
  Application(
    Lambda("x", Record(Map.ofList ["age", Number]),
      Binary("+",
        GetRecord(Variable "x", "age"),
        NumberConst 1)),
    MakeRecord(Map.ofList ["name", StringConst "Yoda"; "age", NumberConst 700]))

typeCheck Map.empty e3

// let r = (fun (x:{age:Number}) -> x) {name:"Yoda", age:700}
// in r.name
//
// Fail: This fails even though the argument had a "name" field! The lambda's
// return type is Record({"age":Number}) - it only promises to return a record
// with "age". The extra "name" field is forgotten. 
let e4 =
  Let("r",
    Application(
      Lambda("x", Record(Map.ofList ["age", Number]),
        Variable "x"),
      MakeRecord(Map.ofList ["name", StringConst "Yoda"; "age", NumberConst 700])),
    GetRecord(Variable "r", "name"))

typeCheck Map.empty e4
