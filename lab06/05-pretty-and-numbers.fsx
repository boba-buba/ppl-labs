// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to Prolog engine
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec asNumber (term:Term) : option<int> = 
  // TODO: Write a function to recognize numbers in the form used below.
  // If the term is 'Atom("zero")' return Some(0). 
  // If the term is 'Predicate("succ", [n])' where 'n' is itself
  // a term representing number, return the number value +1. 
  // failwith "not implemented"
  match term with 
  | Atom "zero" -> Some(0)
  | Predicate("succ", [n]) -> 
      match asNumber n with
      | Some i -> Some(i + 1)
      | None -> None
  | _ -> None



// This is an active pattern! We can now check for numbers
// inside pattern matching (see 'formatTerm') against Number n!
let (|Number|_|) term = asNumber term

let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      // TODO: format all arguments recursively using 'formatTerm'
      // You can then concatenate the arguments using 'String.concat'
      // failwith "not implemented"
      let args = List.map formatTerm items |> String.concat ", "
      sprintf "%s(%s)" p args

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
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = substituteTerms newSubst (clause.Body @ goals)
        let newSubst2 = appendSubstitutions (substituteSubst newSubst subst) newSubst
        solve program newSubst2 newGoals
  | [] ->
    printfn "Solution:"
    for var, term in Map.toList subst do
      printfn "  %s -> %s" var (formatTerm term)

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family Map.empty [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family Map.empty [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
  // TODO: Write a helper that generates a term representing number.
  // This should return Atom("zero") when n is 0 and otherwise
  // succ(succ(...(zero))) with appropriate number of 'succ's.
  // failwith "not implemented"
  if n = 0 then Atom("zero")
  else Predicate("succ", [num (n - 1)])


// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// Query: add(2, 3, X)
// Output should include: 'X = 5' 
//   (and other variables resulting from recursive calls)
solve nums Map.empty [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3' 
//   (we can use 'add' to calculate subtraction too!)
solve nums Map.empty [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums Map.empty [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
