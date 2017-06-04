package body d_stack is

  procedure empty(s: out stack) is
    top: pnode renames s.top;
  begin
    top := null;
  end empty;

  function is_empty(s: in stack) return boolean is
    top: pnode renames s.top;
  begin
    return top=null;
  end is_empty;

  function top(s: in stack) return item is
    top: pnode renames s.top;
  begin
    return top.x;
  exception
    when constraint_error => raise bad_use;
  end top;

  procedure push(s: in out stack; x: in item) is
    top: pnode renames s.top;
    r: pnode;
  begin
    r:= new node;
    r.all:= (x, top);
    top:= r;
  exception
    when storage_error => raise space_overflow;
  end push;
 
  procedure pop(s: in out stack) is
    top: pnode renames s.top;
  begin
    top:= top.next;
  exception
    when constraint_error => raise bad_use; -- empty stack
  end pop;

end d_stack;