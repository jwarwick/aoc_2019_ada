-- AoC 2019, Day 1
with Ada.Text_IO;

package body Day1 is
  package TIO renames Ada.Text_IO;

  function fuel_required(weight : in Mass) return Integer is
  begin
    return Integer(Float'Floor(Float(weight) / 3.0) - 2.0);
  end fuel_required;

  function fuel_for_modules(filename : in String) return Mass is
    file : TIO.File_Type;
    total : Mass := 0;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      total := total + Mass(fuel_required(weight => Mass'Value(TIO.get_line(file))));
    end loop;
    TIO.close(file);
    return total;
  end fuel_for_modules;

  function total_fuel(weight : in Mass) return Mass is
    total : Mass := weight;
    remaining : Integer := Integer(weight);
  begin
    while remaining > 0 loop
      remaining := fuel_required(Mass(remaining));
      if remaining > 0 then
        total := total + Mass(remaining);
      end if;
    end loop;
    return total;
  end total_fuel;

end day1;
