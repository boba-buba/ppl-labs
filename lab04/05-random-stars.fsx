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
  List.tryFind (fun (l, _) -> l > line) state.Program
  |> Option.map (fun (lineNum, cmd) -> { state with CurrentLine = lineNum })

let getCurrentCommand state : Command =
  state.Program
  |> List.find (fun (lineNum, cmd) -> lineNum = state.CurrentLine)
  |> snd

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let getNumberValue value = 
  match value with
  | NumberValue n -> n
  | _ -> failwith "Expected a number"

let getVariableValue state (name:char) = 
  Map.tryFind (int name) state.Memory
  |> Option.map NumberValue
  |> Option.defaultValue (NumberValue 0)

let setVariableValue state (name:char) value = 
  let numericValue = getNumberValue value
  { state with Memory = Map.add (int name) numericValue state.Memory }

let printValue (value:Value) = 
  match value with
  | StringValue s -> printf "%s" s
  | NumberValue n -> printf "%d" n
  | BoolValue b -> printf "%b" b

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
  //failwith "implemented in steps 1 and 3"
  match expr with
  | Const v -> v
  | Variable var -> getVariableValue state var
  | Function(name, args) ->
      match name with
      | "-" ->
        match args |> List.map (fun arg -> evalExpression state (arg)) with
        | [arg1; arg2] ->
          match (arg1, arg2) with
          | (NumberValue n1, NumberValue n2) -> NumberValue (n1 - n2)
          | _ -> failwith "Type error in subtraction"
        | _ -> failwith "Subtraction expects exactly 2 arguments"
      | "+" ->
        match args |> List.map (fun arg -> evalExpression state arg) with
        | [NumberValue n1; NumberValue n2] -> NumberValue (n1 + n2)
        | _ -> failwith "Addition expects exactly 2 numerical arguments"
      | "*" ->
        match args |> List.map (fun arg -> evalExpression state arg) with
        | [NumberValue n1; NumberValue n2] -> NumberValue (n1 * n2)
        | _ -> failwith "Multiplication expects exactly 2 numerical arguments"
      | "=" ->
        match args |> List.map (fun arg -> evalExpression state (arg)) with
        | [arg1; arg2] -> BoolValue (arg1 = arg2)
        | _ -> failwith "Equality expects exactly 2 arguments"
      | "||" ->
        match args |> List.map (fun arg -> evalExpression state (arg)) with
        | [arg1; arg2] ->
          match (arg1, arg2) with
          | (BoolValue b1, BoolValue b2) -> BoolValue (b1 || b2)
          | _ -> failwith "Type error in logical OR"
        | _ -> failwith "Logical OR expects exactly 2 arguments"
      | "<" ->
        args
        |> List.map (evalExpression state)
        |> binaryRelOp (<)
      | ">" ->
        args
        |> List.map (evalExpression state)
        |> binaryRelOp (>)
      | "RND" ->
        match args |> List.map (fun arg -> evalExpression state (arg)) with
        | [NumberValue n] -> NumberValue (state.Random.Next(n))
        | _ -> failwith "RND expects exactly 1 numerical argument"
      | _ -> failwith (sprintf "Unknown function: %s" name)


let rec runCommand state cmd : State option =
  match cmd with
  | Print(expr, newline) ->
      let value = evalExpression state expr
      printValue value
      if newline then printf "\n"
      gotoNextLine state (state.CurrentLine)

  | Goto(target) ->
      { state with CurrentLine = target } |> Some

  | Assign(name, expr) ->
      let value = evalExpression state expr
      let newState = setVariableValue state name value
      gotoNextLine newState state.CurrentLine

  | If (expr, ifCmd) -> 
      let condition = evalExpression state expr
      match condition with
      | BoolValue true -> runCommand state ifCmd
      | _ -> gotoNextLine state (state.CurrentLine)

  | Poke(addr, expr) ->
      let targetAddress = getNumberValue (evalExpression state addr)
      let value = getNumberValue (evalExpression state expr)
      let newState = { state with Memory = Map.add targetAddress value state.Memory }
      gotoNextLine newState state.CurrentLine

  | Peek(name, addr) ->
      // TODO: Evaluate 'addr' to get a memory address, read the int stored
      // there, and store it as the value of variable 'name' (use setVariableValue).
      // This is the read counterpart to Poke.
      let targetAddress = getNumberValue (evalExpression state addr)
      let value = Map.tryFind targetAddress state.Memory |> Option.defaultValue 0
      let newState = setVariableValue state name (NumberValue value)
      gotoNextLine newState state.CurrentLine

  | For (var, start, endExpr) -> 
      let lowerBound = getNumberValue (evalExpression state start)
      let upperBound = getNumberValue (evalExpression state endExpr)
      let newState = setVariableValue state var (NumberValue lowerBound)
      let loopInfo = (var, upperBound, state.CurrentLine)
      gotoNextLine { newState with LoopStack = loopInfo :: newState.LoopStack } (state.CurrentLine)

  | Next (var) -> //failwith "not implemented"
      let currentValue = getNumberValue (getVariableValue state var)
      let incrementedValue = NumberValue (currentValue + 1)
      let newState = setVariableValue state var incrementedValue
      match List.tryFind (fun (v, _, _) -> v = var) state.LoopStack with
      | Some (_, upperBound, forLine) ->
          if getNumberValue incrementedValue <= upperBound then
              gotoNextLine newState forLine
          else
              let newLoopStack = List.filter (fun (v, _, _) -> v <> var) state.LoopStack
              gotoNextLine { newState with LoopStack = newLoopStack } (state.CurrentLine)
      | None -> failwith "NEXT without matching FOR"

  | Update ->
      // TODO: Render the screen region of Memory to the console.
      // For each of the 20 rows, set Console.CursorTop and Console.CursorLeft,
      // then print all 60 characters in that row as a single string
      // (look up each address 1024 + row*60 + col; use ' ' if not found).
      //failwith "TODO: not implemented"
      let rowsToRender = min 20 Console.BufferHeight
      let colsToRender = min 60 Console.BufferWidth
      for row in 0 .. rowsToRender - 1 do
          Console.CursorTop <- row
          Console.CursorLeft <- 0
          let lineChars : char array =
            [ for col in 0 .. colsToRender - 1 ->
              let addr = 1024 + row * 60 + col
              Map.tryFind addr state.Memory
              |> Option.map char
              |> Option.defaultValue ' ' ]
            |> Array.ofList
          printf "%s" (String lineChars)
      gotoNextLine state (state.CurrentLine)

  | Clear ->
      // TODO: Write int ' ' into every screen address (1024..1024+20*60-1)
      // in state.Memory, leaving all other addresses untouched, then advance.
      //failwith "TODO: not implemented"
      let screenAddresses = [1024 .. 1024 + 20 * 60 - 1]
      let newMemory = List.fold (fun mem addr -> Map.add addr (int ' ') mem) state.Memory screenAddresses
      gotoNextLine { state with Memory = newMemory } state.CurrentLine

let rec runCurrentCommand state = 
  runCommand state (getCurrentCommand state) 


let rec runProgram state : unit =
  let rec loop state =
    match runCurrentCommand state with
    | Some newState -> loop newState
    | None -> ()
  loop state
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
