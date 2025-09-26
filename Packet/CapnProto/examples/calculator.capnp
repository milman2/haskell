# Calculator RPC example for Cap'n Proto

@0x9c9c9c9c9c9c9c9c;  # Unique file ID

# Basic calculator operations
interface Calculator {
  # Simple arithmetic operations
  add @0 (a :Float64, b :Float64) -> (result :Float64);
  subtract @1 (a :Float64, b :Float64) -> (result :Float64);
  multiply @2 (a :Float64, b :Float64) -> (result :Float64);
  divide @3 (a :Float64, b :Float64) -> (result :Float64);
  
  # Advanced operations
  power @4 (base :Float64, exponent :Float64) -> (result :Float64);
  sqrt @5 (value :Float64) -> (result :Float64);
  
  # Batch operations
  batchAdd @6 (numbers :List(Float64)) -> (result :Float64);
  
  # Streaming operations
  streamSum @7 (numbers :List(Float64)) -> (sum :Float64);
}

# Advanced calculator with history
struct Calculation {
  operation @0 :Text;
  operands @1 :List(Float64);
  result @2 :Float64;
  timestamp @3 :UInt64;
}

interface AdvancedCalculator {
  # Inherit basic operations
  extends Calculator;
  
  # History management
  getHistory @10 () -> (calculations :List(Calculation));
  clearHistory @11 () -> ();
  
  # Expression evaluation
  evaluateExpression @12 (expression :Text) -> (result :Float64);
  
  # Variable management
  setVariable @13 (name :Text, value :Float64) -> ();
  getVariable @14 (name :Text) -> (value :Float64);
  listVariables @15 () -> (variables :List(Variable));
}

struct Variable {
  name @0 :Text;
  value @1 :Float64;
}
