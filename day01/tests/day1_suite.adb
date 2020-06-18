with Day1.Test;

package body Day1_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (new Day1.Test.Test);
      return Ret;
   end Suite;

end Day1_Suite;
