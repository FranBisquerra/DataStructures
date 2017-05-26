  
package body  d_list is

  procedure empty(l: out list) is
    p: pnode renames l.p;
  begin 
    p:= null;
  end empty;

  function is_empty(l: in list) return boolean is
    p: pnode renames l.p;
  begin
    return p = null;
  end is_empty;

  -- inserts always at the begining of the list
  procedure insert(l: in out list; x: in item) is
    p: pnode renames l.p;
    r: pnode;
  begin
    r:= new node; r.all:= (x, null);
    if p /= null then r.next:= p; end if;
    p:= r;
  end insert;

end d_list;