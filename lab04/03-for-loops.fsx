// ----------------------------------------------------------------------------
// 03 - Implementing FOR loops with a stack
// ----------------------------------------------------------------------------

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command =
  // NOTE: Print takes a bool indicating whether to append a newline after
  // the value. Use 'true' for a line on its own, 'false' to keep printing
  // on the same line (e.g. for dots inside a loop body).
  | Print of Expression * bool
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: For(v, lo, hi) initialises variable 'v' to 'lo' and loops until
  // 'v' exceeds 'hi'. Next(v) increments 'v' and jumps back to the body
  // of the loop, or falls through once the limit is passed.
  | For of string * Expression * Expression
  | Next of string

type State =
  { Program : list<int * Command>
    Variables : Map<string, Value>
    // NOTE: LoopStack tracks active FOR loops as (variable, limit, for-line).
    // The for-line is stored so that NEXT can jump back to the line immediately
    // after FOR, which is where the loop body begins.
    LoopStack : list<string * int * int>
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

let getNumberValue (value:Value) : int =
  // TODO: Helper that extracts numerical value or fails
  //failwith "TODO: not implemented"
  match value with
  | NumberValue n -> n
  | _ -> failwith "Expected a number"

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let printValue (value:Value) =
  match value with
  | StringValue s -> printf "%s" s
  | NumberValue n -> printf "%d" n
  | BoolValue b -> printf "%b" b

let rec evalExpression state (expr:Expression) : Value =
  match expr with
  | Const v -> v
  | Variable var -> 
      match Map.tryFind var state.Variables with
      | Some value -> value
      | None -> failwith "Variable not found"
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

  | Assign (var, expr) -> 
      let value = evalExpression state expr
      let newVariables = Map.add var value state.Variables
      gotoNextLine { state with Variables = newVariables } (state.CurrentLine)

  | If (expr, ifCmd) -> 
      let condition = evalExpression state expr
      match condition with
      | BoolValue true -> runCommand state ifCmd
      | _ -> gotoNextLine state (state.CurrentLine)

  // TODO: FOR <v> = <e1> TO <e2> sets the loop variable <V> to the lower bound
  // (obtained by evaluating <e1>). We then need to remember that we started
  // looping! To do this, push information about (i) variable name, (ii) upper
  // bound and (iii) the current line) to LoopStack so that NEXT <v> knows 
  // where to jump and when to stop. Then continue into the loop body.
  // (hint: use getNumberValue helper here!)
  | For (var, start, endExpr) -> 
      let lowerBound = getNumberValue (evalExpression state start)
      let upperBound = getNumberValue (evalExpression state endExpr)
      let newVariables = Map.add var (NumberValue lowerBound) state.Variables
      let loopInfo = (var, upperBound, state.CurrentLine)
      gotoNextLine { state with Variables = newVariables; LoopStack = loopInfo :: state.LoopStack } (state.CurrentLine)

  // TODO: NEXT <v> increments the variable <v> by 1. Then look through the 
  // LoopStack to find the loop for this variable. If we are within bounds,
  // use gotoNextLine to jump to the line just after the loop start. If we 
  // finished looping, remove the LoopStack record (hint: List.filter) and 
  // continue (gotoNextLine).
  | Next (var) -> //failwith "not implemented"
      let incrementedValue = 
        match Map.tryFind var state.Variables with
        | Some (NumberValue n) -> NumberValue (n + 1)
        | _ -> failwith "Loop variable not found or not a number"
      let newVariables = Map.add var incrementedValue state.Variables
      match List.tryFind (fun (v, _, _) -> v = var) state.LoopStack with
      | Some (_, upperBound, forLine) ->
          if getNumberValue incrementedValue <= upperBound then
              gotoNextLine { state with Variables = newVariables } forLine
          else
              let newLoopStack = List.filter (fun (v, _, _) -> v <> var) state.LoopStack
              gotoNextLine { state with Variables = newVariables; LoopStack = newLoopStack } (state.CurrentLine)
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
  { Program = List.sortBy fst prog; Variables = Map.empty; 
    LoopStack = []; CurrentLine = 10 }


let helloTen =
  [ 10, Assign("I", Const(NumberValue 10))
    20, If(Function("=", [Variable("I"); Const(NumberValue 0)]), Goto(60))
    30, Print(Const(StringValue "HELLO WORLD"), true)
    40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    50, Goto 20
    60, Print(Const(StringValue ""), false) ]

// NOTE: Prints hello world ten times using conditionals
runProgram (makeProgram helloTen)

let helloFor =
  [ 10, For("I", Const(NumberValue 1), Const(NumberValue 10))
    20, Print(Const(StringValue "HELLO WORLD"), true)
    30, Next("I") ]

// NOTE: Same result as helloTen but expressed with FOR/NEXT
runProgram (makeProgram helloFor)

let nestedLoops =
  [ 10, For("I", Const(NumberValue 1), Const(NumberValue 10))
    20, For("J", Const(NumberValue 1), Variable("I"))
    30, Print(Const(StringValue "."), false)
    40, Next("J")
    50, Print(Const(StringValue ""), true)
    60, Next("I") ]

// NOTE: Outer loop I from 1 to 10; inner loop prints I dots, then a newline.
// Result is a triangle: one dot on row 1, two on row 2, ..., ten on row 10.
runProgram (makeProgram nestedLoops)
