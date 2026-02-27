// ============================================================================
// STEP #3 - Implementing the call-by-value reduction strategy
// ============================================================================

type Term = 
  | Variable of string
  | Lambda of string * Term
  | Application of Term * Term


// ============================================================================
// Functions implemented in previous step
// ============================================================================

let rec format term = 
  match term with 
  | Lambda(x, t) -> $"(\\{x}.{format t})"
  | Application(t1, t2) -> $"({format t1} {format t2})"
  | Variable x -> x

let tryFormat optTerm = 
  match optTerm with 
  | Some term -> format term
  | None -> ""
  
let rec substitute (var:string) (subst:Term) (term:Term) : Term = 
  failwith "TODO - copy your code from step 1"

// ============================================================================
// Call-by-name reduction
// ============================================================================

let reduceRedexCBN (term:Term) : option<Term> = 
  failwith "TODO - copy your code from step 2"

let rec reduceCBN (term:Term) : option<Term> = 
  failwith "TODO - copy your code from step 2"

let rec reduceAllCBN term = 
  match reduceCBN term with 
  | Some term -> reduceAllCBN term
  | None -> term

// ============================================================================
// Call-by-value reduction
// ============================================================================

// In call-by-value reduction, we want to reduce everything in the argument
// of the redex before doing the substitution. This means that if we have
// 
// (\x.x) ((\y.y) z)
//
// The argument first reduces to 'z' and then the whole term reduces to 'z'.
// (In call-by-name, the term first reduces to '((\y.y) z)' and then to 'z'.)

let rec reduceCBV (term:Term) : option<Term> = 
  match term with 
  | Application(Lambda(v, t1), t2) -> 
      match reduceCBV t2 with 
      | Some t2' -> Some(Application(Lambda(v, t1), t2'))
      | _ -> Some(substitute v t2 t1)
  | Application(t1, t2) -> 
      match reduceCBV t1 with 
      | Some t1' -> Some(Application(t1', t2))
      | _ ->
          match reduceCBV t2 with 
          | Some t2' -> Some(Application(t1, t2'))
          | _ -> None
  | Lambda _ | Variable _ -> None    

// TASK #2: Run implement recursive reduction 
// (this is the same as reduceAllCBN)

let rec reduceAllCBV term = 
  failwith "TODO"


// TESTS: The following are copied from step2. The 
// two reduction strategies behave the same for them!

let tcbn1 = 
  Application(Application(Lambda("x", Lambda("y", 
    Variable("x"))), Variable("z1")), Variable("z2"))
let tcbn2 = 
  Application(Application(Lambda("x", Lambda("y", 
    Variable("y"))), Variable("z1")), Variable("z2"))

// TEST: Reduce redex that is nested in a term: (((\x.(\y.x)) z1) z2) ~~> ((\y.z1) z2)
tryFormat (reduceCBV tcbn1) = "((\\y.z1) z2)"
// TEST: Reduce redex that is nested in a term: (((\x.(\y.y)) z1) z2) ~~> ((\y.y) z2)
tryFormat (reduceCBV tcbn2) = "((\\y.y) z2)"


// TEST: More interesting case where CBN and CBN differ: (\x.x) ((\y.y) z)
let tx = Lambda("x", Variable("x"))
let ty = Lambda("y", Variable("y"))
let t1 = Application(tx, Application(ty, Variable("z")))

// The two strategies proceed in different ways
tryFormat (reduceCBV t1) = "((\\x.x) z)"
tryFormat (reduceCBN t1) = "((\\y.y) z)"

// But if we reduce them fully, the result is the same
format (reduceAllCBN t1) = "z"
format (reduceAllCBV t1) = "z"

// ============================================================================
// Bonus demo - difference between CBN and CBV
// ============================================================================

// Recall that we previously created a term 
// '(\x.xx) (\x.xx)' which can be reduced and reduces to itself!

let txx = Lambda("x",Application(Variable "x", Variable "x"))
let tinf = Application(txx, txx)

format tinf = "((\\x.(x x)) (\\x.(x x)))"
tryFormat (reduceCBN tinf) = format tinf
tryFormat (reduceCBV tinf) = format tinf

// If you try calling 'reduceAllCBN' or 'reduceAllCBV' on 'tinf'
// you get an infinite loop (in both cases)

// But what if we use 'tinf' as an argument to a lambda 
// function that does not use its argument? 
let tnop = Application(Lambda("x", Variable("z")), tinf)

// CBN will do the substitution first and so we get 'z'
reduceAllCBN tnop

// But CBV will run into an infinite loop!
reduceAllCBV tnop
