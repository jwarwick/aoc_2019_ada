-- AOC, Day 2
with Ada.Text_IO;
use Ada.Text_IO;
with IntCode;

procedure main is
begin
  Intcode.load("1,0,0,0,99");
  Intcode.dump;
  put_line("0 = " & Integer'Image(Intcode.peek(0)));
  put_line("4 = " & Integer'Image(Intcode.peek(4)));
  Intcode.poke(0, 77);
  Intcode.dump;
end main;
