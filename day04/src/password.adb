pragma Ada_2012;
with Ada.Strings.Fixed;

package body Password is

  function is_monotonic(str : in String) return Boolean
  is
    curr : Natural := Natural'First;
    last : Natural := Natural'First;
  begin
    for s in str'Range loop
      curr := Natural'Value((1 => str(s)));
      if curr < last then
        return false;
      end if;
      last := curr;
    end loop;
    return true;
  end is_monotonic;

  function has_duplicate(str : in String) return Boolean
  is
    last : Character := str(str'First);
  begin
    for s in str'First+1 .. str'Last loop
      if str(s) = last then
        return true;
      end if;
      last := str(s);
    end loop;
    return false;
  end has_duplicate;

  function part1_count (first : in Natural; last : in Natural) return Natural
  is
    subtype PasswordValue is Natural range first .. last;
    count : Natural := 0;

    function to_string(p : in PasswordValue) return String
    is
    begin
      return Ada.Strings.Fixed.Trim(PasswordValue'IMAGE(p), Ada.Strings.Left);
    end to_string;

  begin
    for p in PasswordValue'Range loop
      declare 
        str : constant String := to_string(p);
      begin
        if has_duplicate(str) and then is_monotonic(str) then
          count := count + 1;
        end if;
      end;
    end loop;
    return count;
  end part1_count;

end Password;
