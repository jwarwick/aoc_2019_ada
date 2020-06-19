-- AoC 2019, Day 1
with Ada.Text_IO;
with Ada.Containers.Vectors;

package body Day1 is
  package TIO renames Ada.Text_IO;

  package Module_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Mass);

  modules : Module_Vectors.Vector;

  procedure load_modules(filename : in String) is
    file : TIO.File_Type;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      modules.append(Mass'Value(TIO.get_line(file)));
    end loop;
    TIO.close(file);
  end load_modules;

  function fuel_required(weight : in Mass) return Integer is
  begin
    return Integer(Float'Floor(Float(weight) / 3.0) - 2.0);
  end fuel_required;

  function fuel_for_modules return Mass is
    total : Mass := 0;
  begin
    for m of modules loop
      total := total + Mass(fuel_required(m));
    end loop;
    return total;
  end fuel_for_modules;

  function recursive_fuel_required(weight : in Mass) return Mass is
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
  end recursive_fuel_required;

  function total_fuel return Mass is
    module_fuel : Mass;
    all_fuel : Mass;
    total : Mass := 0;
  begin
    for m of modules loop
      module_fuel := Mass(fuel_required(m));
      all_fuel := recursive_fuel_required(module_fuel);
      total := total + all_fuel;
    end loop;
    return total;
  end total_fuel;
end day1;
