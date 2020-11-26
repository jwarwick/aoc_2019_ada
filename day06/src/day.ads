-- AOC, Day 6
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Day is

  type Orbit_Entry is record
    Left : Unbounded_String := Null_Unbounded_String;
    Right : Unbounded_String := Null_Unbounded_String;
  end record;

  package Orbit_List is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Orbit_Entry);

  type Orbit_Checksum is new Natural;

  function load_orbits(filename : in String) return Orbit_List.Vector;
  -- function load_orbits_from_string(str : in String) return Orbit_List.Vector;
  function orbit_count_checksum(ol : in Orbit_List.Vector) return Orbit_Checksum;
end Day;
