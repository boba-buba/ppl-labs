// ============================================================================
// STEP #4 (BONUS) - Representing numbers in lambda calculus
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
// Reduction strategies implemented in previous steps
// ============================================================================

let reduceRedexCBN (term:Term) : option<Term> = 
  failwith "TODO - copy your code from step 2"

let rec reduceCBN (term:Term) : option<Term> = 
  failwith "TODO - copy your code from step 2"

let rec reduceAllCBN term = 
  failwith "TODO - copy your code from step 2"

let rec reduceCBV (term:Term) : option<Term> = 
  failwith "TODO - copy your code from step 3"

let rec reduceAllCBV term = 
  failwith "TODO - copy your code from step 3"

// ============================================================================
// You can represent numbers and arithmetic in lambda calculus!
// ============================================================================

// Church numerals encode numbers as lambdas that apply a given function
// to an argument repeatedly - n-times application represents number 'n'

let rec ntimesf n = 
  if n = 0 then Variable "x"
  else Application(Variable "f", ntimesf (n-1))

let num n = Lambda("f", Lambda("x", ntimesf n))

format (num 0) = "(\\f.(\\x.x))"  
format (num 1) = "(\\f.(\\x.(f x)))"  
format (num 2) = "(\\f.(\\x.(f (f x))))"  

// We can apply Church numeral to variables 's' and 'z' to get a more readable output
let normalize n = 
  let t = Application(Application(n, 
    Variable "s"), Variable "z")
  reduceAllCBN t

format (normalize (num 0)) = "z"
format (normalize (num 1)) = "(s z)"
format (normalize (num 2)) = "(s (s z))"


// TASK #1: Can you implement the 'successor' function? See the definition:
// https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals

let succ = failwith "TODO"



// TEST: The successor of the successor of 0 should be 2!

let t2a = Application(succ, Application(succ, num 0)) 
let t2b = num 2

format (normalize t2a) = "(s (s z))"
format (normalize t2b) = "(s (s z))"
