// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list
type Substitution = Map<string, Term>

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

let appendSubstitutions sub1 sub2 = 
  Map.fold (fun sub2 key value -> Map.add key value sub2) sub1 sub2

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Substitution) term = 
  match term with
  | Atom _ -> term
  | Variable v -> Map.tryFind v subst |> Option.defaultValue term
  | Predicate(p, args) -> Predicate(p, List.map (substitute subst) args)



let substituteSubst (newSubst:Substitution) (subst:Substitution) = 
  Map.map (fun var term -> substitute newSubst term) subst


let substituteTerms subst (terms:list<Term>) = 
  List.map (substitute subst) terms

let rec unifyLists l1 l2 = 
  match l1, l2 with 
  | [], [] -> Some(Map.empty)
  | h1::t1, h2::t2 -> 
      match unify h1 h2 with
      | Some sub1 ->
          let t1' = substituteTerms sub1 t1
          let t2' = substituteTerms sub1 t2
          match unifyLists t1' t2' with
          | Some sub2 -> Some(appendSubstitutions (substituteSubst sub2 sub1) sub2)
          | None -> None
      | None -> None
  | _ -> None
and unify t1 t2 = 
    match t1, t2 with
    | Atom a1, Atom a2 when a1 = a2 -> Some(Map.empty)
    | Predicate(p1, args1), Predicate(p2, args2) when p1 = p2 -> unifyLists args1 args2
    | Variable v, term | term, Variable v -> Some(Map.ofList [(v, term)])
    | _ -> None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  // TODO: Return a list of all variables that appear in 'term'
  // (this may contain duplicates, we will eliminate them below)
  // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
  // failwith "not implemented"
  match term with
  | Atom _ -> []
  | Variable v -> [v]
  | Predicate(_, args) -> List.collect freeVariables args


let withFreshVariables (clause:Clause) : Clause =
  // TODO: Get a list of distinct variables in the clause (using 
  // 'freeVariables' and 'List.distinct'), generate a substitution 
  // that append a number 'n' obtained by 'nextNumber()' to the end
  // of all the variable names, and apply the substitutions to the 
  // head and body of the clause.
  //
  // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
  // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
  //
  // This may not be correct if the user-provided names of variables
  // had numbers in them in a certain format, but that's OK for now! 
  // failwith "not implemented"
  let vars =
    freeVariables clause.Head
    @ List.collect freeVariables clause.Body
    |> List.distinct

  let subst =
    vars
    |> List.map (fun v -> (v, Variable(v + string (nextNumber()))))
    |> Map.ofList

  { Head = substitute subst clause.Head
    Body = substituteTerms subst clause.Body }


let query (program:list<Clause>) (query:Term) : list<Clause * Substitution> =
  // TODO: Return all clauses from 'program' whose 'Head' can be
  // unified with the specified 'query' and return the resulting
  // substitutions. Before unifying, rename variables in the program
  // rule using 'withFreshVariables'. You can do this using 'List.choose' 
  // or by using list comprehension.
  // 
  // The return type of this is a list of tuples consisting of the matching
  // clause and a substitution (list<string * Term>). Calling 'unify'
  // gives you 'option<list<string * Term>>', so you need to pattern match
  // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.
  // failwith "not implemented"
  program
  |> List.choose (fun clause ->
      let freshClause = withFreshVariables clause
      match unify freshClause.Head query with
      | Some subst -> Some(freshClause, subst)
      | None -> None)


// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [Variable("X")]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [Variable("X"); Atom("William")]))
