-- AoC 2019, Day 1
with Ada.Text_IO;

procedure day1 is
  package TIO renames Ada.Text_IO;
  total : Integer := 0;

  function fuel_required(mass : Positive) return Integer is
    req : Positive;
  begin
    req := integer(Float'Floor(float(mass) / 3.0) - 2.0);
    return req;
  end fuel_required;

  procedure sum_file(filename : String) is
    file : TIO.File_Type;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) Loop
      total := total + fuel_required(Positive'Value(TIO.get_line(file)));
    end loop;
    TIO.close(file);
  end sum_file;

begin
  sum_file("day1_input.txt");
  TIO.put_line("Total: " & Integer'Image(total));
end day1;
