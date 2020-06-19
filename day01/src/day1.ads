-- AOC, Day 1

package Day1 is
  type Mass is new Natural;

  procedure load_modules(filename : in String);
  function fuel_for_modules return Mass;
  function total_fuel return Mass;

  private
  function fuel_required(weight : in Mass) return Integer;
  function recursive_fuel_required(weight : in Mass) return Mass;
end Day1;
