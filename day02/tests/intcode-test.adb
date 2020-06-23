with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers;
use type Ada.Containers.Count_Type;

package body IntCode.Test is

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
   end Register_Tests;

end IntCode.Test;
