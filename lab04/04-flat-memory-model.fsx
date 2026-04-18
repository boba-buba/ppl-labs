// ----------------------------------------------------------------------------
// 04 - Flat memory model - storing variables in a memory 'array'
// ----------------------------------------------------------------------------

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression =
  | Const of Value
  | Function of string * Expression list
  // We will support only single-character variables and store their
  // value at a memory location determined by their ASCII code
  | Variable of char

type Command =
  | Print of Expression * bool
  | Goto of int
  // Variable name in all of the following also becomes 'char'
  | Assign of char * Expression
  | If of Expression * Command
  | For of char * Expression * Expression
  | Next of char
  // Added two functions for working with the flat memory representation:
  // POKE E1, E2 - sets the value at address 'E1' to the value of 'E2'
  // PEEK X, E - reads the value at address 'E' into a variable named 'X' (like ASSIGN)
  | Poke of Expression * Expression
  | Peek of char * Expression

type State =
  { Program : list<int * Command>
    // Replacing something like "Variables : Map<string, Value>" with 
    // a memory. We will only be able to store numerical values in the memory
    Memory : Map<int, int>
    LoopStack : list<char * int * int>
    CurrentLine : int }


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

let getNumberValue value = 
  match value with
  | NumberValue n -> n
  | _ -> failwith "Expected a number"

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let getVariableValue state (name:char) =
  // TODO: Variables are stored in Memory at the address equal to the ASCII
  // code of the variable name. Look up 'int name' in state.Memory and wrap
  // the result as NumberValue.
  Map.tryFind (int name) state.Memory
  |> Option.map NumberValue
  |> Option.defaultValue (NumberValue 0)

let setVariableValue state (name:char) value =
  // TODO: Set the variable value in state.Memory. Extract the int from 'value'
  // using getNumberValue and store it in Memory at address 'int name'.
  let numericValue = getNumberValue value
  { state with Memory = Map.add (int name) numericValue state.Memory }

let printValue (value:Value) =
  match value with
  | StringValue s -> printf "%s" s
  | NumberValue n -> printf "%d" n
  | BoolValue b -> printf "%b" b

let rec evalExpression state (expr:Expression) : Value =
  match expr with
  | Const v -> v
  | Variable name ->
      // TODO: Use getVariableValue to read the variable's value from Memory.
      getVariableValue state name
  | Function(name, args) ->
      match name with
      | "-" ->
        match args |> List.map (fun arg -> evalExpression state (arg)) with
        | [arg1; arg2] ->
          match (arg1, arg2) with
          | (NumberValue n1, NumberValue n2) -> NumberValue (n1 - n2)
          | _ -> failwith "Type error in subtraction"
        | _ -> failwith "Subtraction expects exactly 2 arguments"
      | "=" ->
        match args |> List.map (fun arg -> evalExpression state (arg)) with
        | [arg1; arg2] -> BoolValue (arg1 = arg2)
        | _ -> failwith "Equality expects exactly 2 arguments"
      | _ -> failwith "Unknown function"

let rec runCommand state cmd : State option =
  match cmd with
  | Print(expr, newline) ->
      // TODO: Modify 'printValue' to use 'printf' (not 'printfn')
      // and print a '\n' character here if required. 
      //failwith "TODO: not implemented"
      let value = evalExpression state expr
      printValue value
      if newline then printf "\n"
      gotoNextLine state (state.CurrentLine)

  | Goto(target) ->
      { state with CurrentLine = target } |> Some

  | If (expr, ifCmd) -> 
      let condition = evalExpression state expr
      match condition with
      | BoolValue true -> runCommand state ifCmd
      | _ -> gotoNextLine state (state.CurrentLine)

  | Assign(name, expr) ->
      // TODO: Evaluate 'expr' and store the result using setVariableValue
      let value = evalExpression state expr
      let newState = setVariableValue state name value
      gotoNextLine newState state.CurrentLine

  | Poke(addr, expr) ->
      // TODO: Evaluate 'addr' to get the target memory address and 'expr' to
      // get the value. Write the value directly into Memory at that address.
      // Unlike Assign, this bypasses the variable name - any address can be
      // written, including one that happens to be a variable's location!
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

  // TODO: NEXT <v> increments the variable <v> by 1. Then look through the 
  // LoopStack to find the loop for this variable. If we are within bounds,
  // use gotoNextLine to jump to the line just after the loop start. If we 
  // finished looping, remove the LoopStack record (hint: List.filter) and 
  // continue (gotoNextLine).
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

let makeProgram prog =
  { Program = List.sortBy fst prog; LoopStack = []; Memory = Map.empty; CurrentLine = 10 }

let testVariables =
  [ 10, Assign('I', Const(NumberValue 1))
    30, Print(Variable 'I', true) ]

// DEMO: Simpler test program with variables
runProgram (makeProgram testVariables)

let helloTen =
  [ 10, Assign('I', Const(NumberValue 10))
    20, If(Function("=", [Variable('I'); Const(NumberValue 1)]), Goto(60))
    30, Print (Const(StringValue "HELLO WORLD"), true)
    40, Assign('I', Function("-", [ Variable('I'); Const(NumberValue 1) ]))
    50, Goto 20
    60, Print (Const(StringValue ""), true) ]

// NOTE: Prints hello world ten times using conditionals
runProgram (makeProgram helloTen)

// DEMO: We can set value in memory at some arbitrary address
// then we can read it into a variable and print the variable value...
let peekPokeDemo =
  [ 10, Poke(Const(NumberValue 100), Const(NumberValue 42))
    20, Peek('X', Const(NumberValue 100))
    30, Print(Variable 'X', true) ]

runProgram (makeProgram peekPokeDemo)

// The following demo shows that we can set variable values using Poke!
// If we know their memory location, we can set them (here we set all three
// using a single for loop).
let pokeVars =
  [ 10, Assign('A', Const(NumberValue 0))
    20, Assign('B', Const(NumberValue 0))
    30, Assign('C', Const(NumberValue 0))
    35, Print(Const(StringValue "hi"), true)
    40, For('I', Const(NumberValue 65), Const(NumberValue 67))
    50, Poke(Variable('I'), Variable('I'))
    60, Next('I')
    70, Print(Variable 'A', true)
    80, Print(Variable 'B', true)
    90, Print(Variable 'C', true) ]

runProgram (makeProgram pokeVars)
