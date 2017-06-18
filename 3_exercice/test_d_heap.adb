
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

  HP.put(test_heap, 100);
  HP.put(test_heap, 200);
  HP.put(test_heap, 50);
  HP.put(test_heap, 1000);
  HP.put(test_heap, 1);

  -- Asserts
  n:= HP.get_least(test_heap);
  Assert(n=1, "Error in first get");
  HP.delete_least(test_heap);

  n:= HP.get_least(test_heap);
  Assert(n=50, "Error in second get");
  HP.delete_least(test_heap);

  n:= HP.get_least(test_heap);
  Assert(n = 100, "Error in third get");
  HP.delete_least(test_heap);

  n:= HP.get_least(test_heap);
  Assert(n = 200, "Error in fourth get");
  HP.delete_least(test_heap);

  n:= HP.get_least(test_heap);
  Assert(n = 1000, "Error in fifth get");
  HP.delete_least(test_heap);

  Assert(HP.is_empty(test_heap), "Heap not empty, and it should");

end test_d_heap;