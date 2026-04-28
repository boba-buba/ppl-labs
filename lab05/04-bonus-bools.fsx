#nowarn "40"
// ----------------------------------------------------------------------------
// 04 - BONUS: Booleans and blocks as ordinary objects
// ----------------------------------------------------------------------------

// In Smalltalk and Self, even booleans and conditionals are just objects.
// There is no built-in 'if' statement - instead, 'true' and 'false' are objects
// that respond to the 'if' message with 'then' and 'else' block arguments.

type Slot =
  { Name : string
    Value : Objekt }

and Objekt =
  { Slots : list<Slot>
    Special : option<Special> }

and Special =
  | String of string
  | Code of (Objekt -> Objekt)

#load "objekt-visualizer.fs"

// ----------------------------------------------------------------------------
// Helper functions for constructing objects
// ----------------------------------------------------------------------------

let makeObject (slots : list<string * Objekt>) : Objekt =
  let slots = slots |> List.map (fun (k, v) -> { Name = k; Value = v })
  { Slots = slots; Special = None }

let makeMethod (f : Objekt -> Objekt) : Objekt =
  { Slots = []; Special = Some(Code f) }

let printValue (obj : Objekt) : unit =
  match obj.Special with
  | Some(String s) -> printfn "String: %s" s
  | _ ->
    printfn "Object:"
    for s in obj.Slots do printfn " - %s" s.Name

// ----------------------------------------------------------------------------
// Prototype-based slot lookup
// ----------------------------------------------------------------------------

let getParents (obj : Objekt) : Objekt list =
  failwith "copy from step 1"

let rec findSlots (name : string) (obj : Objekt) : Slot list =
  failwith "copy from step 1"

let send (name : string) (args : list<string * Objekt>) (obj : Objekt) : Objekt =
  failwith "copy from step 3"

// ----------------------------------------------------------------------------
// Booleans and blocks as objects
// ----------------------------------------------------------------------------

// 'makeBool' creates a boolean object with a single 'if' method.
// The method retrieves the 'then' and 'else' arguments from the activation
// record (they are blocks), picks one based on 'b', and invokes it by sending
// 'do' to the 'then' or 'else' block.
let makeBool (b : bool) : Objekt =
  makeObject [
    "if", makeMethod (fun activation ->
      // TODO: Implement boolean conditional as a message send.
      // Retrieve the 'then' and 'else' arguments from 'activation' using 'send'.
      // Pick the branch corresponding to 'b', then invoke it by sending 'do'.
      failwith "not implemented")
  ]

// 'makeBlock' wraps an F# function as an object with a 'do' method.
// Sending 'do' to a block runs the computation and returns its result.
let makeBlock (f : unit -> Objekt) : Objekt =
  makeObject [
    "do", makeMethod (fun _ -> f())
  ]

// ----------------------------------------------------------------------------
// String prototype extended with 'equals'
// ----------------------------------------------------------------------------

let rec stringPrototype : Objekt = makeObject [
  "append", makeMethod (fun o ->
    // TODO: copy from step 3
    failwith "copy from step 3")

  "equals", makeMethod (fun o ->
    // TODO: Implement string equality.
    // Access receiver and argument the same way as in 'append'.
    // Return 'makeBool (s1 = s2)' rather than a string.
    failwith "not implemented")
]

and makeString (s : string) : Objekt =
  { Slots = [ { Name = "prototype*"; Value = stringPrototype } ]
    Special = Some(String s) }

// ----------------------------------------------------------------------------
// DEMO: Prisoner's dilemma using object-level booleans
// ----------------------------------------------------------------------------

// Each player independently chooses "silent" or "testify". The outcome depends
// on the combination of choices. We express the nested conditional entirely
// using 'send "equals"' (returning a bool object) and 'send "if"' (dispatching
// to the chosen block) - no F# 'if' expressions appear in the game logic.
//
// See: https://en.wikipedia.org/wiki/Prisoner's_dilemma
// 
let rnd = System.Random()
let roll () =
  if rnd.Next(2) = 0 then makeString "silent"
  else makeString "testify"

let pA = roll()
let pB = roll()

pA |> send "equals" ["other", makeString "silent"] |> send "if" [
  "then", makeBlock (fun () ->
    pB |> send "equals" ["other", makeString "silent"] |> send "if" [
      "then", makeBlock (fun () -> makeString "A -1, B -1 (both silent)")
      "else", makeBlock (fun () -> makeString "A -3, B 0 (A silent, B testify)")
    ])
  "else", makeBlock (fun () ->
    pB |> send "equals" ["other", makeString "silent"] |> send "if" [
      "then", makeBlock (fun () -> makeString "A 0, B -3 (A testify, B silent)")
      "else", makeBlock (fun () -> makeString "A -2, B -2 (both testify)")
    ])
]
|> printValue
