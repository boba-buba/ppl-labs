// ----------------------------------------------------------------------------
// 05 - Memory-mapped screen in BASIC - the screen now also lives in memory!
// ----------------------------------------------------------------------------
open System

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression =
  | Const of Value
  | Function of string * Expression list
  | Variable of char

type Command =
  | Print of Expression * bool
  | Goto of int
  | Assign of char * Expression
  | If of Expression * Command
  | For of char * Expression * Expression
  | Next of char
  | Poke of Expression * Expression
  | Peek of char * Expression
  // We will store screen (20x60 characters) in memory at address 1024.
  // The Clear command fills every screen cell with a space.
  // The Update command renders the screen region of Memory to the console.
  // (C64 does this automatically, but this is not how modern console works!)
  | Clear
  | Update

type State =
  { Program : list<int * Command>
    Memory : Map<int, int>
    LoopStack : list<char * int * int>
    CurrentLine : int
    // Random is needed to implement the RND(N) function in evalExpression
    Random : System.Random }


// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let gotoNextLine (state:State) line : State option =
  failwith "implemented in step 1"

let getCurrentCommand state : Command =
  failwith "implemented in step 1"

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let getNumberValue value = 
  failwith "implemented in step 3"

let getVariableValue state (name:char) = 
  failwith "implemented in step 4"

let setVariableValue state (name:char) value = 
  failwith "implemented in step 4"

let printValue (value:Value) = 
  failwith "implemented in step 1"

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
// You can use operators as arguments, e.g. binaryRelOp (>) [arg1; arg2]
let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr =
  // TODO: Add support for 'RND(N)' which returns a random number in range 0..N-1
  // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
  // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
  // add helpers for numerical operators and binary Boolean operators to make
  // your code a bit nicer. 
  failwith "implemented in steps 1 and 3"


let rec runCommand state cmd : State option =
  match cmd with
  | Goto _ -> failwith "implemented in step 1"
  | Assign _ -> failwith "implemented in step 4"
  | If _ -> failwith "implemented in step 2"
  | Poke _ -> failwith "implemented in step 4"
  | Peek _ -> failwith "implemented in step 4"
  | For _ -> failwith "implemented in step 3"
  | Next _ -> failwith "implemented in step 3"
  | Print _ -> failwith "implemented in step 1"

  | Update ->
      // TODO: Render the screen region of Memory to the console.
      // For each of the 20 rows, set Console.CursorTop and Console.CursorLeft,
      // then print all 60 characters in that row as a single string
      // (look up each address 1024 + row*60 + col; use ' ' if not found).
      failwith "TODO: not implemented"

  | Clear ->
      // TODO: Write int ' ' into every screen address (1024..1024+20*60-1)
      // in state.Memory, leaving all other addresses untouched, then advance.
      failwith "TODO: not implemented"

let rec runProgram state : unit =
  failwith "implemented in step 1"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------
// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let chr (v:char) = Const(NumberValue (int v))
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.+) a b = Function("+", [a; b])
let (.*) a b = Function("*", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)
let rnd arg = "RND" @ [arg]

let makeProgram prog =
  { Program = List.sortBy fst prog; LoopStack = []; Memory = Map.empty;
    Random = System.Random(); CurrentLine = 10 }

// Hello world program, printing letters letter-by-letter.
let hello = 
  [ 10, Clear
    20, Poke(num 1024, chr 'H')
    30, Poke(num 1025, chr 'E')
    40, Poke(num 1026, chr 'L')
    50, Poke(num 1027, chr 'L')
    60, Poke(num 1028, chr 'O')
    70, Poke(num 1029, chr '!')
    80, Update ]

runProgram (makeProgram hello) |> ignore


// Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ 10, Clear
    20, Poke(num 1024 .+ rnd (num 20) .* (num 60) .+ (rnd (num 60)), chr '*')
    30, For('I', num 1, num 100)
    40, Poke(num 1024 .+ rnd (num 20) .* (num 60) .+ (rnd (num 60)), chr ' ')
    50, Next('I')
    60, Update
    70, Goto(20) ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runProgram (makeProgram stars) |> ignore
