-- AOC, Day 1
with Ada.Text_IO;
use Ada.Text_IO;
with Day1;

procedure main is
begin
  Day1.load_modules("day1_input.txt");
  put_line("Part 1: " & Day1.Mass'Image(Day1.fuel_for_modules));
  put_line("Part 2: " & Day1.Mass'Image(Day1.total_fuel));
end main;
