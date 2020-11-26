-- AoC 2019, Day 6
with Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Hash;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-- with GNAT.String_Split;
-- with Ada.Characters.Latin_1;

package body Day is
  package TIO renames Ada.Text_IO;

  package Orbit_Element_Set is new Ada.Containers.Ordered_Sets
    (Element_Type => Unbounded_String);

  type Orbit is record
    Name : Unbounded_String := Null_Unbounded_String;
    Parent : Unbounded_String := Null_Unbounded_String;
    Children : Orbit_Element_Set.Set := Orbit_Element_Set.Empty_Set;
    Depth : Natural := 0;
  end record;

  pragma Warnings (Off, "procedure ""Put"" is not referenced");
  procedure Put(value : in Orbit) is
  pragma Warnings (On, "procedure ""Put"" is not referenced");
  begin
    TIO.Put("Orbit: " & to_string(value.Name) & ", Parent: " & to_string(value.Parent) & ", Depth: " & Natural'IMAGE(value.Depth));
    TIO.New_Line;
    TIO.Put("   Children: ");
    for c of value.Children loop
      TIO.Put(to_string(c) & " ");
    end loop;
  end Put;

  package Orbit_Hashed_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Orbit,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

  procedure parse_line(line : in String; orbits : in out Orbit_List.Vector) is
    idx : constant Natural := index(line, ")");
    left : constant String := line(line'first .. idx-1);
    right : constant String := line(idx+1 .. line'last);
    curr : constant Orbit_Entry := Orbit_Entry'(left => to_unbounded_string(left), right => to_unbounded_string(right));
  begin
    orbits.append(curr);
  end parse_line;

  function load_orbits(filename : in String) return Orbit_List.Vector is
    file : TIO.File_Type;
    orbits : Orbit_List.Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      parse_line(TIO.get_line(file), orbits);
    end loop;
    TIO.close(file);
    return orbits;
  end load_orbits;

  -- function load_orbits_from_string(str : in String) return Orbit_List.Vector is
  --   use GNAT;
  --   use Ada.Characters;
  --   orbits : Orbit_List.Vector := Orbit_List.Empty_Vector;
  --   subs : String_Split.Slice_Set;
  --   seps : constant String := Latin_1.LF & Latin_1.CR;
  -- begin
  --   String_Split.Create(S => subs, From => str, Separators => seps, Mode => String_Split.Multiple);
  --   for i in 1 .. String_Split.Slice_Count(subs) loop
  --     declare
  --       line : constant String := String_Split.Slice(subs, i);
  --     begin
  --       parse_line(line, orbits);
  --     end;
  --   end loop;
  --   return orbits;
  -- end load_orbits_from_string;

  procedure add_orbit(name : in Unbounded_String; parent : in Unbounded_String; orbits : in out Orbit_Hashed_Maps.Map) is
  begin
    orbits.include(to_string(name), Orbit'(
      Name => name,
      Parent => parent,
      Children => Orbit_Element_Set.Empty_Set,
      Depth => 0));
  end add_orbit;

  procedure propogate_depth(name : in String; depth : in Integer; orbits : in out Orbit_Hashed_Maps.Map) is
    curr : Orbit := orbits(name);
  begin
    curr.Depth := depth;
    orbits(name) := curr;
    for c of curr.Children loop
      propogate_depth(to_string(c), depth + 1, orbits);
    end loop;
  end propogate_depth;

  function build_map(ol : in Orbit_List.Vector) return Orbit_Hashed_Maps.Map is
    orbits : Orbit_Hashed_Maps.Map := Orbit_Hashed_Maps.Empty_Map;
  begin
    for curr of ol loop
      if not orbits.contains(to_string(curr.left)) then
        add_orbit(curr.left, Null_Unbounded_String, orbits);
      end if;

      if not orbits.contains(to_string(curr.right)) then
        add_orbit(curr.right, curr.left, orbits);
      end if;

      orbits(to_string(curr.left)).Children.insert(curr.right);
    end loop;

    propogate_depth("COM", 0, orbits);

    return orbits;
  end build_map;

  function orbit_count_checksum(ol : in Orbit_List.Vector) return Orbit_Checksum is
    orbits : constant Orbit_Hashed_Maps.Map := build_map(ol);
    total : Natural := 0;
  begin
    for c in orbits.Iterate loop
      total := total + orbits(c).Depth;
      -- Put(orbits(c));
      -- TIO.New_Line;
    end loop;
    return Orbit_Checksum(total);
  end orbit_count_checksum;

end Day;
