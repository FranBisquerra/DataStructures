generic
  size: positive;
  type item is private;
  with function "<" (x1, x2: in item) return boolean;
  with function ">" (x1, x2: in item) return boolean;
  with function printer(x: in item) return String;

package d_heap is
  type heap is limited private;

  bad_use, space_overflow: exception;

  procedure empty (h: out heap);
  procedure put (h: in out heap; x: in item);
  procedure delete_least (h: in out heap);

  function is_empty (h: in heap) return boolean;
  function get_least (h: in heap) return item;

private

  type mem_space is array(1..size) of item;

  type heap is 
    record
      a: mem_space;
      n: natural;
    end record;

end d_heap;