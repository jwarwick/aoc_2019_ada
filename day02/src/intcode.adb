-- IntCode Interpreter
with Ada.Strings.Maps;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body IntCode is
  package TIO renames Ada.Text_IO;

  function integer_hash(i: Integer) return Ada.Containers.Hash_Type is (Ada.Strings.Hash(Integer'Image(i)));

  pc : Memory_Vector.Cursor;

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
    pc := memory.first;
  end load;

  procedure eval is
  begin
    null;
  end eval;

  procedure poke(addr : Natural; value : Integer) is
  begin
    memory(addr) := value;
  end poke;

  function peek(addr : Natural) return Integer is (memory(addr));

  procedure dump is
  begin
    for m of memory loop
      TIO.put(Integer'Image(m) & ", ");
    end loop;
    TIO.new_line;
  end dump;

begin
  OpCode_Map.insert(1, Add);
  OpCode_Map.insert(2, Mult);
  OpCode_Map.insert(99, Halt);

end IntCode;
