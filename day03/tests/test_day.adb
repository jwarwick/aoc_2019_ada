with AUnit.Reporter.Text;
with AUnit.Run;
with AOC_Test_Suite; use AOC_Test_Suite;

procedure Test_Day is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Test_Day;
