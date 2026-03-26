// ----------------------------------------------------------------------------
// 00 - DEMO: Simple numerical constraint solver
// ----------------------------------------------------------------------------

type Number =
  | Zero
  | Succ of Number
  | Variable of string

let rec solve constraints =
  match constraints with
  | [] -> []
  | (Succ n1, Succ n2)::constraints ->
      solve ((n1, n2)::constraints)
  | (Zero, Zero)::constraints -> solve constraints
  | (Succ _, Zero)::_ | (Zero, Succ _)::_ ->
      failwith "Cannot be solved"
  | (n, Variable v)::constraints
  | (Variable v, n)::constraints ->
      let subst = solve constraints
      (v, n)::subst

// Should work: x = Zero
solve
  [ Succ(Variable "x"), Succ(Zero) ]

// Should work: x = Succ Zero
solve
  [ Succ(Variable "x"), Succ(Succ(Zero)) ]

// Should faild: S(Z) <> Z
solve
  [ Succ(Succ(Zero)), Succ(Zero) ]

// Should fail: No 'x' such that S(S(x)) = S(Z)
solve
  [ Succ(Succ(Variable "x")), Succ(Zero) ]
