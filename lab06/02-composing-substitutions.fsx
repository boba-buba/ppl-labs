// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
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

let rec substitute (subst:Substitution) term : Term = 
  // TODO: Replace variables in 'term' for which there is a
  // replacement specified by 'subst.[var]' with the replacement.
  // You can assume the terms in 'subst' do not contain
  // any of the variables that we want to replace.
  //failwith "not implemented"
  match term with
  | Atom _ -> term
  | Variable v -> Map.tryFind v subst |> Option.defaultValue term
  | Predicate(p, args) -> Predicate(p, List.map (substitute subst) args)



let substituteSubst (newSubst:Substitution) (subst:Substitution) = 
  // TODO: Apply the substitution 'newSubst' to all the terms 
  // in the existing substitiution 'subst' (Hint: use Map.map).
  // failwith "not implemented"
  Map.map (fun var term -> substitute newSubst term) subst


let substituteTerms (subst:Substitution) (terms:list<Term>) = 
  // TODO: Apply substitution 'subst' to all the terms in 'terms'
  // failwith "not implemented"
  List.map (substitute subst) terms


let rec unifyLists l1 l2 = 
  // TODO: Modify the implementation to use 'substituteTerms' and 'substituteSubst'.
  //
  // Let's say that your code calls 'unify h1 h2' to get a substitution 's1'
  // and then it calls 'unifyLists t1 t2' to get a substitution 's2' and then it
  // returns a concatentated list 'appendSubstitutions s1 s2'. Modify the code so that:
  //
  // (1) The substitution 's1' is aplied to 't1' and 't2' before calling 'unifyLists'
  // (2) The substitution 's2' is applied to all terms in substitution 's1' before returning
  // failwith "implemented in step 1"
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
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
  (Predicate("loves", [Atom("narcissus"); Atom("narcissus")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
  (Predicate("add", [Atom("zero"); Predicate("succ", [Atom("zero")])]))
  (Predicate("add", [Variable("Y"); Predicate("succ", [Variable("Y")])]))

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify
  (Predicate("loves", [Variable("X"); Atom("narcissus")]))
  (Predicate("loves", [Variable("Y"); Variable("X")]))

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
  (Predicate("add", 
      [ Predicate("succ", [Variable("X")]); 
        Variable("X") ]))
  (Predicate("add", 
      [ Variable("Y"); 
        Predicate("succ", [Variable("Z")]) ]))

