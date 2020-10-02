-- IntCode Interpreter
with Ada.Containers.Hashed_Maps;
with Ada.Containers;
with Ada.Containers.Vectors;

package IntCode is
  procedure load(s : String);
  procedure load_file(path : String);
  procedure eval;

  procedure poke(addr : Natural; value : Integer);
  function peek(addr : Natural) return Integer;

  function dump return String;

  private
  package Memory_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Integer);
  memory : Memory_Vector.Vector := Memory_Vector.Empty_Vector;

  type OpCode is (Add, Mult, Halt);

  function integer_hash(i: Integer) return Ada.Containers.Hash_Type;

  package Int_to_OpCode_Map is new Ada.Containers.Hashed_Maps
    (
      Key_Type => Integer,
      Element_Type => OpCode,
      Hash => integer_hash,
      Equivalent_Keys => "="
      );
  OpCode_Map : Int_to_OpCode_Map.Map;
  
end IntCode;
