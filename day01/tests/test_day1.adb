with AUnit.Reporter.Text;
with AUnit.Run;
with Day1_Suite; use Day1_Suite;

procedure Test_Day1 is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Test_Day1;
