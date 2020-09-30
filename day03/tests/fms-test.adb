with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers;
use type Ada.Containers.Count_Type;

package body FMS.Test is

   procedure Test_Load (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
     null;
     load("R8,U5,L5,D3", "U7,R6,D4,L4");
     Assert(wire_1.Length = 4, "Expected wire_1 to have 4 elements");
     Assert(wire_2.Length = 4, "Expected wire_2 to have 4 elements");
     Assert(wire_points_1.Length = 21, "Expected wire_1 points to have 21 elements");
     Assert(wire_points_2.Length = 21, "Expected wire_1 points to have 21 elements");

     load("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83");
     Assert(wire_1.Length = 9, "Expected wire_1 to have 9 elements");
     Assert(wire_2.Length = 8, "Expected wire_2 to have 8 elements");

     load("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
     Assert(wire_1.Length = 11, "Expected wire_1 to have 11 elements");
     Assert(wire_2.Length = 10, "Expected wire_2 to have 10 elements");
   end Test_Load;

   procedure Test_Closest (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
     load("R8,U5,L5,D3", "U7,R6,D4,L4");
     Assert(closest_intersection = 6, "Expected closest intersection at distance 6");

     load("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83");
     Assert(closest_intersection = 159, "Expected closest intersection at distance 159");

     load("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
     Assert(closest_intersection = 135, "Expected closest intersection at distance 135");
   end Test_Closest;

   procedure Test_Shortest (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
     load("R8,U5,L5,D3", "U7,R6,D4,L4");
     Assert(shortest_intersection = 30, "Expected shortest intersection at distance 30");

     load("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83");
     Assert(shortest_intersection = 610, "Expected shortest intersection at distance 610");

     load("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
     Assert(shortest_intersection = 410, "Expected shortest intersection at distance 410");
   end Test_Shortest;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("FMS Package");
   end Name;

   procedure Register_Tests (T : in out Test) is
     use AUnit.Test_Cases.Registration;
   begin
     Register_Routine (T, Test_Load'Access, "Loading");
     Register_Routine (T, Test_Closest'Access, "Closest Point");
     Register_Routine (T, Test_Shortest'Access, "Shortest Distance Point");
   end Register_Tests;

end FMS.Test;
