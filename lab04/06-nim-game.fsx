// ----------------------------------------------------------------------------
// 06 - Adding input, implementing NIM game & improving it with GOSUB
// ----------------------------------------------------------------------------
open System

// NOTE: This task has two parts. In the first part, we add INPUT <V> and STOP
// commands to be able to play a little game called NIM (see code at the end).
// In the second part, we can use a better implementation that uses a 
// BASIC subroutine (procedure) call using GOSUB.

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression =
  | Const of Value
  | Function of string * Expression list
  | Variable of char

type Command =
  // Note - I modify PRINT to take a list of expressions (real C64 BASIC allows 
  // this too and it makes the NIM game implementation a bit less horrible)
  | Print of Expression list
  | Goto of int
  | Assign of char * Expression
  | If of Expression * Command
  | For of char * Expression * Expression
  | Next of char
  | Poke of Expression * Expression
  | Peek of char * Expression
  | Clear
  | Update
  
  // INPUT <V> reads a number from the console and stores it as variable 'v'
  // STOP terminates the program without continuing to the next line
  | Input of char
  | Stop
  // GOSUB <N> calls a procedure defined on line <N>. To do this, it pushes
  // the current line onto the call stack and jumps to line 'n'. 
  // RETURN pops the call stack and continues from the line after the GOSUB.
  | GoSub of int
  | Return


type State =
  { Program : list<int * Command>
    Memory : Map<int, int>
    LoopStack : list<char * int * int>
    // Note: CallStack holds return addresses pushed by GOSUB
    CallStack : list<int>
    CurrentLine : int
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

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr =
  // TODO: Extend evalExpression with the 'MIN(E1, E2)' function, which evaluates
  // both arguments and returns the smaller of the two as a NumberValue.
  // (All other cases are implemented in steps 1-5.)
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
      | "MIN" ->
        match args |> List.map (fun arg -> evalExpression state (arg)) with
        | [NumberValue n1; NumberValue n2] -> NumberValue (min n1 n2)
        | _ -> failwith "MIN expects exactly 2 numerical arguments"
      | _ -> failwith (sprintf "Unknown function: %s" name)


let rec runCommand state cmd : State option =
  match cmd with
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
      let screenAddresses = [1024 .. 1024 + 20 * 60 - 1]
      let newMemory = List.fold (fun mem addr -> Map.add addr (int ' ') mem) state.Memory screenAddresses
      gotoNextLine { state with Memory = newMemory } state.CurrentLine


  | Print(exprs) ->
      // TODO: Print now takes a list of expressions rather than a single one.
      // Iterate over 'exprs', evaluate each one and print its value using
      // 'printValue'. Then advance to the next line.
      //failwith "TODO: not implemented"
      let values = exprs |> List.map (evalExpression state)
      values |> List.iter printValue
      gotoNextLine state state.CurrentLine

  | Stop ->
      // TODO: STOP terminates the program immediately.
      // Recall that runCommand returns 'State option': return the value that
      // signals "no more lines to run" to make runProgram stop.
      //failwith "TODO: not implemented"
      None

  | Input(name) ->
      // TODO: INPUT <V> reads a number from the user and stores it in variable 'name'.
      // Use Console.ReadLine() and Int32.TryParse to read an integer. If parsing
      // fails (user typed something that isn't a number), ask again - keep looping
      // until you get a valid number. Then store it and advance to the next line.
      //failwith "TODO: not implemented"
      let rec readNumber() =
          let input = Console.ReadLine()
          match Int32.TryParse input with
          | (true, n) -> n
          | _ -> 
              Console.WriteLine("Invalid input. Please enter a valid number.")
              readNumber()
      let number = readNumber()
      let newState = setVariableValue state name (NumberValue number)
      gotoNextLine newState (state.CurrentLine)

  
  // NOTE: You can skip GoSub and Return now and run the first version of the game!
  // (Return to these later to run the nicer version of the game...)


  | GoSub(target) ->
      // TODO: GOSUB <N> calls a subroutine starting at line <N>.
      // Before jumping, save the current line on the Stack so that RETURN can
      // come back here. Then jump to 'target' (hint: delegate to Goto).
      //failwith "TODO: not implemented"
      let returnAddress = state.CurrentLine
      let newState = { state with CallStack = returnAddress :: state.CallStack }
      gotoNextLine newState target

  | Return ->
      // TODO: RETURN comes back from a subroutine called by GOSUB.
      // Pop the top of the Stack to get the line the GOSUB was on, then use
      // gotoNextLine to continue from the line *after* that GOSUB.
      // If the Stack is empty, there is no matching GOSUB - fail with an error.
      //failwith "TODO: not implemented"
      match state.CallStack with
      | [] -> failwith "RETURN without matching GOSUB"
      | returnAddress :: rest ->
          let newState = { state with CallStack = rest }
          gotoNextLine newState returnAddress

let rec runCurrentCommand state = 
  runCommand state (getCurrentCommand state) 

let rec runProgram state : unit =
  let rec loop state =
    match runCurrentCommand state with
    | Some newState -> loop newState
    | None -> ()
  loop state

// ----------------------------------------------------------------------------
// Test case - NIM game with GOSUB
// ----------------------------------------------------------------------------

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var (n:char) = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let makeProgram prog =
  { Program = List.sortBy fst prog; LoopStack = []; CallStack = [];
    Memory = Map.empty; Random = System.Random(); CurrentLine = 10 }

// NOTE: A simple game you should be able to run now! :-)
// NIM - two players alternate turns (via GoSub) removing 1-5 matches;
// who takes the last match wins the game.

let nimDirect = 
  [ 10, Assign('M', num 20)
    20, Print [ str "THERE ARE "; var 'M'; str " MATCHES LEFT\n" ]
    30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var 'M']; str " MATCHES\n" ]
    40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    50, Input('P')
    60, If((var 'P' .< num 1) .|| (var 'P' .> num 5) .|| (var 'P' .> var 'M'), Goto 40)
    70, Assign('M', var 'M' .- var 'P')
    80, If(var 'M' .= num 0, Goto 200)
    90, Print [ str "THERE ARE "; var 'M'; str " MATCHES LEFT\n" ]
    100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var 'M']; str " MATCHES\n" ]
    110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    120, Input('P')
    130, If((var 'P' .< num 1) .|| (var 'P' .> num 5) .|| (var 'P' .> var 'M'), Goto 110)
    140, Assign('M', var 'M' .- var 'P')
    150, If(var 'M' .= num 0, Goto 220)
    160, Goto 20
    200, Print [str "PLAYER 1 WINS!"]
    210, Stop
    220, Print [str "PLAYER 2 WINS!"]
    230, Stop
  ]

runProgram (makeProgram nimDirect)

// NOTE: NIM - the above version has a lot of repetition for player 1 and 2.
// We can turn this into a subroutine (lines 100-170) that is shared between
// the two players. The program still uses global variables:
//
// 'M' = matches remaining, 'U' = current player number, 'P' = player's pick.
//
let nimGosub = 
  [ 10, Assign('M', num 20)
    
    20, Assign('U', num 1)
    30, GoSub(100)
    40, Assign('U', num 2)
    50, GoSub(100)
    60, Goto(20) 

    100, Print [ str "THERE ARE "; var 'M'; str " MATCHES LEFT\n" ]
    110, Print [ str "PLAYER "; var 'U'; str ": YOU CAN TAKE BETWEEN 1 AND "; 
      Function("MIN", [num 5; var 'M']); str " MATCHES\n" ]
    120, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    130, Input('P')
    140, If((var 'P' .< num 1) .|| (var 'P' .> num 5) .|| (var 'P' .> var 'M'), Goto 120)
    150, Assign('M', var 'M' .- var 'P')
    160, If(var 'M' .= num 0, Goto 200)
    170, Return    
    
    200, Print [str "PLAYER "; var 'U'; str " WINS!"]
  ]


runProgram (makeProgram nimGosub)
