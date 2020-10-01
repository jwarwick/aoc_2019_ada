-- AOC, Day 4
with Ada.Text_IO; use Ada.Text_IO;
with Password;

procedure main is
    first : constant Natural := 134564;
    last : constant Natural := 585159;
begin
  put_line("Part 1: " & Positive'Image(Password.part1_count(first, last)));
  put_line("Part 2: " & Positive'Image(Password.part2_count(first, last)));
end main;
