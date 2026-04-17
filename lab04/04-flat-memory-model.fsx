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
  failwith "implemented in step 1"

let getCurrentCommand state : Command =
  failwith "implemented in step 1"

let getNumberValue value = 
  failwith "implemented in step 3"

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let getVariableValue state (name:char) =
  // TODO: Variables are stored in Memory at the address equal to the ASCII
  // code of the variable name. Look up 'int name' in state.Memory and wrap
  // the result as NumberValue.
  failwith "TODO: not implemented"

let setVariableValue state (name:char) value =
  // TODO: Set the variable value in state.Memory. Extract the int from 'value'
  // using getNumberValue and store it in Memory at address 'int name'.
  failwith "TODO: not implemented"

let printValue (value:Value) =
  failwith "implemented in step 1"

let rec evalExpression state (expr:Expression) : Value =
  match expr with
  | Const _ -> failwith "implemented in step 1"
  | Variable name ->
      // TODO: Use getVariableValue to read the variable's value from Memory.
      failwith "TODO: not implemented"
  | Function _ -> failwith "implemented in step 2"

let rec runCommand state cmd : State option =
  match cmd with
  | Print _ -> failwith "implemented in step 3"
  | Goto _ -> failwith "implemented in step 1"
  | If _ -> failwith "implemented in step 2"
  
  | Assign(name, expr) ->
      // TODO: Evaluate 'expr' and store the result using setVariableValue
      failwith "TODO: not implemented"

  | Poke(addr, expr) ->
      // TODO: Evaluate 'addr' to get the target memory address and 'expr' to
      // get the value. Write the value directly into Memory at that address.
      // Unlike Assign, this bypasses the variable name - any address can be
      // written, including one that happens to be a variable's location!
      failwith "TODO: not implemented"
  
  | Peek(name, addr) ->
      // TODO: Evaluate 'addr' to get a memory address, read the int stored
      // there, and store it as the value of variable 'name' (use setVariableValue).
      // This is the read counterpart to Poke.
      failwith "TODO: not implemented"
  
  | For _ -> failwith "implemented in step 3"
  | Next _ -> failwith "implemented in step 3"

let rec runProgram state : unit =
  failwith "implemented in step 1"

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
