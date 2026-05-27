// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
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
  match term with
  | Atom _ -> []
  | Variable v -> [v]
  | Predicate(_, args) -> List.collect freeVariables args

let withFreshVariables (clause:Clause) : Clause =
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

let query (program:list<Clause>) (query:Term) =
  program
  |> List.choose (fun clause ->
      let freshClause = withFreshVariables clause
      match unify freshClause.Head query with
      | Some subst -> Some(freshClause, subst)
      | None -> None)

let rec solve (program:list<Clause>) (subst:Substitution) (goals:list<Term>) : unit = 
  match goals with 
  | g::goals -> 
      // TODO: We need to solve the goal (term) 'g'. To do so, find all 
      // matching clauses in the 'program' using 'query' and iterate over
      // the returned list using 'for clause, newSubst in matches do'.
      // For each possible solution, we need to add the 'clause.Body' to 
      // the list of 'goals' and apply the substitution 'newSubst' to the
      // new concatentated list of 'goals'. Then we need to apply the 
      // substitution 'newSubst' to the substitution 'subst' we have so far,
      // append the two and call 'solve' recursively with this new substitution
      // to solve the new goals.
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = substituteTerms newSubst (clause.Body @ goals)
        let newSubst2 = appendSubstitutions (substituteSubst newSubst subst) newSubst
        solve program newSubst2 newGoals
  | [] ->
    // TODO: We solved all goals, which means 'subst' is a possible solution!
    // Print 'subst' (Hint: for var, term in Map.toList subst do ...).
    // failwith "not implemented" 
    printfn "Solution:"
    for var, term in Map.toList subst do
      printfn "  %s -> %A" var term

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family Map.empty [ Predicate("father", [Variable("X"); Atom("William")]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> William, Y -> George, ... ]
solve family Map.empty [ Predicate("father", [Variable("X"); Variable("Y")]) ]

