with Ada.Text_IO; use Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers;
use type Ada.Containers.Count_Type;

package body IntCode.Test is

  type Memory_Array is array(Integer range <>) of Integer;
  function to_memory(a : Memory_Array) return Memory_Vector.Vector is
    m : Memory_Vector.Vector;
  begin
    for val of a loop
      m.append(val);
    end loop;
    return m;
  end to_memory;

  procedure Test_Load (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
     Assert(memory.length = 0, "Vector before load should be size 0, got: " & Ada.Containers.Count_Type'Image(memory.length));

     load("99");
     Assert(memory.length = 1, "Vector after load should be size 1, got: " & Ada.Containers.Count_Type'Image(memory.length));
     Assert(memory(0) = 99, "First element of vector should be 99, got: " & Integer'Image(memory(0)));

     load("16,42,0,-1");
     Assert(memory.length = 4, "Vector after load should be size 4, got: " & Ada.Containers.Count_Type'Image(memory.length));
     Assert(memory(0) = 16, "First element of vector should be 16, got: " & Integer'Image(memory(0)));
     Assert(memory(1) = 42, "First element of vector should be 16, got: " & Integer'Image(memory(1)));
     Assert(memory(2) = 0, "First element of vector should be 16, got: " & Integer'Image(memory(2)));
     Assert(memory(3) = -1, "First element of vector should be 16, got: " & Integer'Image(memory(3)));
   end Test_Load;

   procedure Test_Poke (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
     load("16,42,0,-1");
     Assert(memory.length = 4, "Vector after load should be size 4, got: " & Ada.Containers.Count_Type'Image(memory.length));
     Assert(memory(0) = 16, "First element of vector should be 16, got: " & Integer'Image(memory(0)));
     Assert(memory(1) = 42, "First element of vector should be 16, got: " & Integer'Image(memory(1)));
     Assert(memory(2) = 0, "First element of vector should be 16, got: " & Integer'Image(memory(2)));
     Assert(memory(3) = -1, "First element of vector should be 16, got: " & Integer'Image(memory(3)));

     poke(0, -5);
     Assert(memory(0) = -5, "Poked element of vector should be -5, got: " & Integer'Image(memory(0)));
     
     -- XXX - test exception
     -- poke(10, 11);
     
     Assert(peek(3) = -1, "Peeking element of vector should be -1, got: " & Integer'Image(memory(3)));

   end Test_Poke;

   procedure Test_Eval (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      procedure eval_test(input : in Memory_Array; expected : in Memory_Array; desc : in String) is
        use type Memory_Vector.Vector;
        m : Memory_Vector.Vector := to_memory(input);
        e : Memory_Vector.Vector := to_memory(expected);
      begin
        memory := m;
        eval;
        Assert(memory = e, desc & ", got: " & dump);
      end eval_test;

   begin
     eval_test((1, 0, 0, 0, 99), (2, 0, 0, 0, 99), "Simple add test");
     eval_test((2,3,0,3,99), (2,3,0,6,99), "Simple mult test");
     eval_test((2,4,4,5,99,0), (2,4,4,5,99,9801), "Bigger mult test");
     eval_test((1,1,1,4,99,5,6,0,99), (30,1,1,4,2,5,6,0,99), "Multiple write test");
   end Test_Eval;

   procedure Test_Input (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      m : Memory_Vector.Vector := to_memory((3,0,4,0,99));
      val : constant Integer := 17;
      out_val : Integer;
   begin
     memory := m;
     append_input(val);
     eval;
     out_val := take_output;
     Assert(out_val = val, "Expected: " & Integer'IMAGE(val) & ", got: " & Integer'IMAGE(out_val));
   end Test_Input;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("IntCode Package");
   end Name;

   procedure Register_Tests (T : in out Test) is
     use AUnit.Test_Cases.Registration;
   begin
     Register_Routine (T, Test_Load'Access, "Loading");
     Register_Routine (T, Test_Poke'Access, "Peek/Poke");
     Register_Routine (T, Test_Eval'Access, "Evaluation");
     Register_Routine (T, Test_Input'Access, "Input");
   end Register_Tests;

end IntCode.Test;
