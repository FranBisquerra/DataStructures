with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body d_heap is

  procedure empty (h: out heap) is
    n: natural renames h.n;
  begin
    n:= 0;
  end empty;

  procedure put (h: in out heap; x: in item) is
    a: mem_space renames h.a;
    n: natural renames h.n;
    i: natural; -- index to a node in the heap
    pi: natural; -- index to the parent of the node i
  begin 
    if n = size then raise space_overflow; end if;
    n:= n + 1; i:= n; pi:= n / 2;
    while pi > 0 and then a(pi) > x loop
      a(i):= a(pi); i:= pi; pi:= i / 2;
    end loop;
    a(i):= x;
  end put;

  procedure delete_least (h: in out heap) is 
    a: mem_space renames h.a;
    n: natural renames h.n;
    x: item;
    i: natural; -- index to a node in the heap
    ci: natural; -- index to the least child the node i
  begin
    if n = 0 then raise bad_use; end if;
    x:= a(n); n:= n - 1;
    i:= 1; ci:= i * 2;
    if ci < n and then a(ci + 1) < a(ci) then ci:= ci + 1; end if;
    while ci <= n and then a(ci) < x loop
      a(i):= a(ci); i:= ci; ci:= i*2;
      if ci < n and then a(ci + 1) < a(ci) then ci:= ci+1; end if;
    end loop;
    a(i):= x;
  end delete_least;

  function is_empty (h: in heap) return boolean is
    n: natural renames h.n;
  begin
    return n = 0;
  end is_empty;

  function get_least (h: in heap) return item is
    a: mem_space renames h.a;
    n: natural renames h.n;
  begin
    if n = 0 then raise bad_use; end if;
    return a(1);
  end get_least;


end d_heap;