-- AOC, Day 6
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  ol : Orbit_List.Vector;
begin
  ol := Day.load_orbits("input.txt");
  put_line("Part 1: " & Orbit_Checksum'Image(Day.orbit_count_checksum(ol)));
  -- put_line("Part 2: " & Day1.Mass'Image(Day1.total_fuel));
end main;
