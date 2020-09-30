-- Fuel Management System
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

package FMS is
  procedure load(w1 : in String; w2 : in String);
  procedure load_file(path : String);

  function closest_intersection return Positive;
  function shortest_intersection return Positive;

  private
  type Direction is (Up, Down, Left, Right);
  type Wire_Segment is record
    dir : Direction;
    distance : Positive;
  end record;

  package Wire is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Wire_Segment);
  wire_1 : Wire.Vector;
  wire_2 : Wire.Vector;

  type Position is record
    x : Integer := 0;
    y : Integer := 0;
    dist : Natural := 0;
  end record;
  function hash(p : in Position) return Ada.Containers.Hash_Type;
  function equivalent_positions(left, right: Position) return Boolean;

  package Wire_Points is new Ada.Containers.Hashed_Sets(Element_Type => Position, Hash => hash, Equivalent_Elements => equivalent_positions);
  wire_points_1 : Wire_Points.Set;
  wire_points_2 : Wire_Points.Set;
end FMS;
