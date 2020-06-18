-- AOC, Day 1

package Day1 is
  type Mass is new Natural;
  function fuel_for_modules(filename : in String) return Mass;
  function total_fuel(weight : in Mass) return Mass;
end Day1;
