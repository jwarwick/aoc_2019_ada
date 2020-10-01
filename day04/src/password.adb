pragma Ada_2012;
-- with Ada.Text_IO; use Ada.Text_IO;
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

  function has_standalone_duplicate(str : in String) return Boolean
  is
    n3 : Character := str(str'First);
    n2 : Character := str(str'First+1);
    n1 : Character := str(str'First+2);
  begin
    -- special case of first two digits being the duplicate pair
    if n3 = n2 and then n1 /= n2 then
      return true;
    end if;

    for s in str'First+3 .. str'Last loop
      if n2 = n1 and then str(s) /= n1 and then n3 /= n2 then
        return true;
      end if;
      n3 := n2;
      n2 := n1;
      n1 := str(s);
    end loop;

    -- special case of last two digits being the duplicate pair
    if str(str'Last) = str(str'Last - 1) and then str(str'Last - 1) /= str(str'Last - 2) then
      return true;
    end if;
    return false;
  end has_standalone_duplicate;

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

  function part2_count (first : in Natural; last : in Natural) return Natural
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
        if has_standalone_duplicate(str) and then is_monotonic(str) then
          count := count + 1;
        end if;
      end;
    end loop;
    return count;
  end part2_count;
end Password;
