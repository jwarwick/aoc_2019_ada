-- with Day.Test;
with IntCode.Test;

package body AOC_Test_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (new IntCode.Test.Test);
      return Ret;
   end Suite;

end AOC_Test_Suite;
