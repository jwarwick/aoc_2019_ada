-- AOC, Day 1
with Ada.Text_IO;
use Ada.Text_IO;
with Day1;

procedure main is
  module_fuel_need : Day1.Mass;
  total : Day1.Mass;
begin
  module_fuel_need := Day1.fuel_for_modules("day1_input.txt");
  put_line("Part 1: " & Day1.Mass'Image(module_fuel_need));
  total := Day1.total_fuel(module_fuel_need);
  put_line("Part 2: " & Day1.Mass'Image(total));
end main;
