-- Fuel Management System
with Ada.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;

package body FMS is
  package TIO renames Ada.Text_IO;

  function to_dir(c : in Character) return Direction is
  begin
    case c is
      when 'U' => return Up;
      when 'D' => return Down;
      when 'L' => return Left;
      when 'R' => return Right;
      when others => raise Constraint_Error;
    end case;
  end to_dir;

  function to_string(d : in Direction) return String is
  begin
    case d is
      when Up => return "U";
      when Down => return "D";
      when Left => return "L";
      when Right => return "R";
    end case;
  end to_string;

  function to_string(p : in Position) return String is
  begin
    return "(" & Integer'IMAGE(p.x) & "," & Integer'IMAGE(p.y) & "): " & Natural'IMAGE(p.dist);
  end to_string;

  function distance(p : in Position) return Natural is
  begin
    return abs(p.x) + abs(p.y);
  end distance;

  function hash(p : in Position) return Ada.Containers.Hash_Type is
  begin
    return Ada.Strings.Hash(p.x'IMAGE & "," & p.y'IMAGE);
  end hash;

  function equivalent_positions(left, right: Position) return Boolean is
  begin
    return (left.x = right.x) and then (left.y = right.y);
  end equivalent_positions;

  -- function "=" (left : in Position; right : in Position) return Boolean is
  -- begin
  --   return (left.x = right.x) and (left.y = right.y);
  -- end "=";

  function to_string(wp : in Wire_Points.Set) return String is
    package Unbounded renames Ada.Strings.Unbounded;
    s : Unbounded.Unbounded_String;
  begin
    for elt of wp loop
      Unbounded.append(s, to_string(elt) & ", ");
    end loop;
    return Unbounded.to_string(s);
  end to_string;

  function to_string(ws : in Wire_Segment) return String is
  begin
    return to_string(ws.dir) & Integer'Image(ws.distance);
  end to_string;

  function to_string(w : in Wire.Vector) return String is
    package Unbounded renames Ada.Strings.Unbounded;
    s : Unbounded.Unbounded_String;
  begin
    for elt of w loop
      Unbounded.append(s, to_string(elt) & ", ");
    end loop;
    return Unbounded.to_string(s);
  end to_string;

  procedure parse_wire(w_str : in String; w : in out Wire.Vector) is
    start : Natural := w_str'First;
    finish : Natural;
    delimiters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.to_set(Sequence => ",");
  begin
    w.clear;
    while start <= w_str'Last loop
      Ada.Strings.Fixed.find_token(Source => w_str(start .. w_str'Last),
                                    Set => delimiters,
                                    Test => Ada.Strings.outside,
                                    First => start, Last => finish);
      if not(finish = 0 and then start = w_str'First) then
        w.append((dir => to_dir(w_str(start)), distance => Integer'Value(w_str(start+1 .. finish))));
      end if;
      start := finish + 1;
    end loop;
  end parse_wire;

  procedure step(pos : in out Position; dir : in Direction) is
  begin
    case dir is
      when Up => pos := (x => pos.x, y => pos.y - 1, dist => pos.dist + 1);
      when Down => pos := (x => pos.x, y => pos.y + 1, dist => pos.dist + 1);
      when Left => pos := (x => pos.x - 1, y => pos.y, dist => pos.dist + 1);
      when Right => pos := (x => pos.x + 1, y => pos.y, dist => pos.dist + 1);
    end case;
  end step;

  procedure expand(pos : in out Position; segment : in Wire_Segment; points : in out Wire_Points.Set) is
  begin
    for i in  1 .. segment.distance loop
      step(pos => pos, dir => segment.dir);
      points.include(pos);
    end loop;
  end expand;

  procedure expand_segments(w : in Wire.Vector; points : in out Wire_Points.Set) is
    start_pos : constant Position := (x => 0, y => 0, dist => 0);
    curr_pos : Position := start_pos;
  begin
    points.clear;
    for segment of w loop
      expand(pos => curr_pos, segment => segment, points => points);
    end loop;
  end expand_segments;

  procedure load(w1 : in String; w2 : in String) is
  begin
    parse_wire(w1, wire_1);
    expand_segments(wire_1, wire_points_1);
    parse_wire(w2, wire_2);
    expand_segments(wire_2, wire_points_2);
  end load;

  procedure load_file(path : String) is
    file : TIO.File_Type;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => path);
    declare
      str1 : constant String := TIO.get_line(file);
      str2 : constant String := TIO.get_line(file);
    begin
      load(w1 => str1, w2 => str2);
    end;
    TIO.close(file);
  end load_file;

  function closest_intersection return Positive is
    use Wire_Points;
    best : Positive := Positive'LAST;
    in_common : constant Wire_Points.Set := wire_points_1 and wire_points_2; 
  begin
    for elt of in_common loop
      declare
        curr : constant Natural := distance(elt);
      begin
        if curr < best then
          best := curr;
        end if;
      end;
    end loop;
    return best;
  end closest_intersection;

  function shortest_intersection return Positive is
    use Wire_Points;
    best : Positive := Positive'LAST;
    in_common : constant Wire_Points.Set := wire_points_1 and wire_points_2; 
  begin
    for elt of in_common loop
      declare
        e1 : constant cursor := find(wire_points_1, elt);
        d1 : constant natural := element(e1).dist;
        e2 : constant cursor := find(wire_points_2, elt);
        d2 : constant natural := element(e2).dist;
        curr : constant Natural := d1 + d2;
      begin
        if curr < best then
          best := curr;
        end if;
      end;
    end loop;
    return best;
  end shortest_intersection;
end FMS;
