-- AOC, Day 3
with Ada.Text_IO; use Ada.Text_IO;
with FMS;

procedure main is
begin
  FMS.load_file("day3_input.txt");
  put_line("Part 1: " & Positive'Image(FMS.closest_intersection));
  put_line("Part 2: " & Positive'Image(FMS.shortest_intersection));
end main;
