// ----------------------------------------------------------------------------
// 06 - Lazy search and support for lists
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
  match term with 
  | Atom "zero" -> Some(0)
  | Predicate("succ", [n]) -> 
      match asNumber n with
      | Some i -> Some(i + 1)
      | None -> None
  | _ -> None

let rec asList term : option<list<Term>> = 
  // TODO: If the term represents a list, this should return the 
  // elements of the list collected in an ordinary F# list.
  // If the term is 'Atom("empty")' return Some([])
  // If the term is 'Predicate("cons", [h; tl])' where 'tl' is itself
  // a term representing a list 'l', return Some(h::l).
  // failwith "not implemented"
  match term with
  | Atom "empty" -> Some([])
  | Predicate("cons", [h; tl]) -> 
      match asList tl with
      | Some l -> Some(h :: l)
      | None -> None
  | _ -> None

// Active patterns for use inside 'formatTerm'
let (|Number|_|) term = asNumber term
let (|List|_|) term = asList term

let rec formatTerm term = 
  // TODO: Add a case for 'List(items)' - pretty print this as a list
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | List items ->
    let args = List.map formatTerm items |> String.concat "; "
    sprintf "[%s]" args
  | Predicate(p, items) ->
    // format all arguments recursively using 'formatTerm'
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

let rec solve (program:list<Clause>) (subst:Substitution) (goals:list<Term>) : seq<Substitution> = seq {
  // TODO: We want to change this function to return a lazy sequence
  // of all possible substitutions solving the problem. I already 
  // wrapped the code in 'seq { .. }' block for you. Change the rest
  // to recursively call 'solve' using 'yield!' and return new 
  // solutions using 'yield' (replacing the printing code).
  // failwith "not implemented"
  match goals with 
  | g::goals -> 
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = substituteTerms newSubst (clause.Body @ goals)
        let newSubst2 = appendSubstitutions (substituteSubst newSubst subst) newSubst
        yield! solve program newSubst2 newGoals
  | [] ->
    yield subst
}


let run program query = 
  let vars = Set.ofSeq (freeVariables query)
  for subst in solve program Map.empty [query] do
    // TODO: To avoid cluttered output, we want to only print assignment
    // for variables that appear in the original query (and skip all 
    // variables generated by the various internal matches). You can do
    // this here by iterating over variables and printing them only if
    // they are included in 'vars' (test using 'vars.Contains')
    // failwith "not implemented"
    printfn "Solution:"
    for var, term in Map.toList subst do
      if vars.Contains var then
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

// Queries from previous step (now called using 'run')
run family (Predicate("father", [Variable("X"); Atom("William")]))
run family (Predicate("father", [Variable("X"); Variable("Y")]))


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
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

// Queries from previous step (now called using 'run')
run nums (Predicate("add", [num 2; num 3; Variable("X")]))
run nums (Predicate("add", [num 2; Variable("X"); num 5]))
run nums (Predicate("add", [num 2; Variable("Y"); Variable("X")]))


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

// Helper that generates a term representing a list
let rec makeList l : Term = 
  // TODO: Write a helper that generates a term representing a list.
  // This should return Atom("empty") when 'l' is [] and otherwise
  // cons(t1, .. cons(tN, empty)) when 'l' is [t1; ...; tN]
  // failwith "not implemented"
  match l with
  | [] -> Atom("empty")
  | h::t -> Predicate("cons", [h; makeList t])


// Clauses that represent the 'append' operation on lists
// $ append([X|Y],Z,[X|W]) :- append(Y,Z,W).
// $ append([],X,X).
let append = [ 
  fact (Predicate("append", [Atom("empty"); Variable("X"); Variable("X") ]))
  rule (Predicate("append", [
    Predicate("cons", [Variable("X"); Variable("Y") ])
    Variable("Z"); Predicate("cons", [Variable("X"); Variable("W") ])
  ])) [
    Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ])
  ]
]

let l1to4 = makeList [ for i in 1 .. 4 -> num i ]
let l5to9 = makeList [ for i in 5 .. 9 -> num i ]
let l1to9 = makeList [ for i in 1 .. 9 -> num i ]

// TODO: Test the term formatting - this should print nice outputs!
formatTerm l1to4
formatTerm l5to9
formatTerm l1to9

// Query: append([1..4], [5..9], X)
// Return: X -> [1..9]
run append (Predicate("append", [l1to4; l5to9; Variable "X"]))

// Query: append([1..4], X, [1..9])
// Return: X -> [5..9]
run append (Predicate("append", [l1to4; Variable "X"; l1to9]))

// Query: append(X, Y, [1..9])
// Return: 
//  * X -> [1..9], Y -> []
//  * X -> [1..8], Y -> [9]
//  * X -> [1..7], Y -> [8, 9]
//  * X -> [1..6], Y -> [7 .. 9]
//  * etc.
run append (Predicate("append", [Variable "Y"; Variable "X"; l1to9]))
