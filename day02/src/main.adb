-- AOC, Day 2
with Ada.Text_IO; use Ada.Text_IO;
with IntCode;

procedure main is
  subtype Input_Int is Integer range 0 .. 99;

  function run_once(noun : in Input_Int; verb : in Input_Int) return Integer is
  begin
    IntCode.load_file("day2_input.txt");
    IntCode.poke(1, Integer(noun));
    IntCode.poke(2, Integer(verb));
    IntCode.eval;
    return IntCode.peek(0);
  end run_once;

  target : constant Integer := 19690720;
  result : Integer;
begin
  put_line("Part 1: " & Integer'Image(run_once(12, 2)));

  for noun in Input_Int'Range loop
    for verb in Input_Int'Range loop
      result := run_once(noun, verb);
      if result = target then
        put_line("Found target, noun: " & Integer'Image(noun) & ", verb: " & Integer'Image(verb));
        put_line("Part 2: " & Integer'Image((100 * noun) + verb));
      end if;
    end loop;
  end loop;
end main;
