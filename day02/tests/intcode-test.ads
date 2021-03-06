with AUnit;
with AUnit.Test_Cases;

package IntCode.Test is

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   function Name (T : Test) return AUnit.Message_String;
   procedure Register_Tests (T : in out Test);

   -- Test routines
   procedure Test_Load (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Poke (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Eval (T : in out AUnit.Test_Cases.Test_Case'Class);

end IntCode.Test;
