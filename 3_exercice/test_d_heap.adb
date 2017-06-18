
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Assertions; use Ada.Assertions;
with d_heap;

procedure test_d_heap is

  function bigger (x, y: in Integer) return Boolean is
  begin
    return x > y;
  end bigger;

  function smaller (x, y: in Integer) return Boolean is
  begin
    return x < y;
  end smaller;

  package HP is new d_heap(20, Integer, smaller, bigger, Integer'Image);

  test_heap: HP.heap;
  n: Integer;
begin

  HP.empty(test_heap);

  -- Insert items into heap
  HP.put(test_heap, 100);
  HP.put(test_heap, 200);
  HP.put(test_heap, 50);
  HP.put(test_heap, 1000);
  HP.put(test_heap, 1);

  -- Get from heap
  n:= HP.get_least(test_heap);
  if n /= 1 then
    Assert(false, "Error in first get");
  end if;
  HP.delete_least(test_heap);

  n:= HP.get_least(test_heap);
  if n /= 50 then
    Assert(false, "Error in second get");
  end if;
  HP.delete_least(test_heap);

  n:= HP.get_least(test_heap);
  if n /= 100 then
    Assert(false, "Error in third get");
  end if;
  HP.delete_least(test_heap);

  n:= HP.get_least(test_heap);
  if n /= 200 then
    Assert(false, "Error in fourth get");
  end if;
  HP.delete_least(test_heap);

  n:= HP.get_least(test_heap);
  if n /= 1000 then
    Assert(false, "Error in fifth get");
  end if;
  HP.delete_least(test_heap);

  if HP.is_empty(test_heap) = false then
    Assert(false, "Heap not empty, and it should");
  end if;

end test_d_heap;