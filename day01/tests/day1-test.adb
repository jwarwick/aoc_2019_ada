with AUnit.Assertions; use AUnit.Assertions;

package body Day1.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (fuel_required(12) = 2, "Wrong fuel 12 => 2");
      Assert (fuel_required(14) = 2, "Wrong fuel 14 => 2");
      Assert (fuel_required(1969) = 654, "Wrong fuel 1969 => 654");
      Assert (fuel_required(100756) = 33583, "Wrong fuel 100756 => 33583");
   end Test_Part1;

   procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (recursive_fuel_required(14) = 14 + 2, "Wrong total fuel 14 => 2");
      Assert (recursive_fuel_required(1969) = 1969 + 966, "Wrong total fuel 1969 => 966");
      Assert (recursive_fuel_required(100756) = 100756 + 50346, "Wrong total fuel 100756 => 50346");
   end Test_Part2;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Day1 package");
   end Name;

   procedure Register_Tests (T : in out Test) is
     use AUnit.Test_Cases.Registration;
   begin
     Register_Routine (T, Test_Part1'Access, "Test Part 1");
     Register_Routine (T, Test_Part2'Access, "Test Part 2");
   end Register_Tests;

end Day1.Test;
