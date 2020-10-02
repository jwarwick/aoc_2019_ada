-- IntCode Interpreter
with Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body IntCode is
  package TIO renames Ada.Text_IO;

  procedure append_input(val : Integer) is
  begin
    input_vector.append(val);
  end append_input;

  function take_output return Integer is
    val : constant Integer := output_vector.First_Element;
  begin
    output_vector.Delete_First;
    return val;
  end take_output;

  function integer_hash(i: Integer) return Ada.Containers.Hash_Type is (Ada.Strings.Hash(Integer'Image(i)));

  type Program_Counter is new Natural;
  pc : Program_Counter;
  function to_pc(i : Integer) return Program_Counter is (Program_Counter(i));
  function to_index(p : Program_Counter) return Natural is (Natural(p));

  function val(offset : Integer) return Integer is (memory(memory(offset + to_index(pc))));
  function val_a(offset : Integer := 1) return Integer renames val;
  function val_b(offset : Integer := 2) return Integer renames val;
  function loc_a return Integer is (memory(to_index(pc) + 1));
  function loc_b return Integer is (memory(to_index(pc) + 2));
  function loc_c return Integer is (memory(to_index(pc) + 3));

  procedure increment_pc(s : Integer := 4) is
  begin
    pc := to_pc(to_index(pc) + s);
  end increment_pc;

  procedure load_file(path : String) is
    file : TIO.File_Type;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => path);
    declare
      str : String := TIO.get_line(file);
    begin
      load(str);
    end;
    TIO.close(file);
  end load_file;

  procedure load(s : String) is
    start : Natural := s'First;
    finish : Natural;
    delimiters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.to_set(Sequence => ",");
  begin
    memory.clear;
    while start <= s'Last loop
      Ada.Strings.Fixed.find_token(Source => s(start .. s'Last),
                                    Set => delimiters,
                                    Test => Ada.Strings.outside,
                                    First => start, Last => finish);
      if not(finish = 0 and then start = s'First) then
        memory.append(Integer'Value(s(start .. finish)));
      end if;
      start := finish + 1;
    end loop;
    pc := to_pc(memory.first_index);
  end load;

  procedure eval is
    should_halt : Boolean := False;
    val : Integer;
  begin
    pc := to_pc(memory.first_index);
    while not(should_halt) loop
      val := memory(to_index(pc));
      case OpCode_Map(val) is
        when Add =>
          memory(loc_c) := val_a + val_b;
          increment_pc;
        when Mult =>
          memory(loc_c) := val_a * val_b;
          increment_pc;
        when Input =>
          memory(loc_a) := input_vector.First_Element;
          input_vector.Delete_First;
          increment_pc(2);
        when Output =>
          output_vector.Append(val_a);
          increment_pc(2);
        when Halt =>
          should_halt := True;
      end case;
    end loop;
  end eval;

  procedure poke(addr : Natural; value : Integer) is
  begin
    memory(addr) := value;
  end poke;

  function peek(addr : Natural) return Integer is (memory(addr));

  function dump return String is
    package Unbounded renames Ada.Strings.Unbounded;
    str : Ada.Strings.Unbounded.Unbounded_String;
  begin
    for m of memory loop
      Unbounded.append(str, (Integer'Image(m) & ", "));
    end loop;
    return Unbounded.to_string(str);
  end dump;

begin
  OpCode_Map.insert(1, Add);
  OpCode_Map.insert(2, Mult);
  OpCode_Map.insert(3, Input);
  OpCode_Map.insert(4, Output);
  OpCode_Map.insert(99, Halt);

end IntCode;
