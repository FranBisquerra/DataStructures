  
package body  d_mapping is

  procedure empty(s: out set) is
    first: pnode renames s.first;
  begin
    first:= null;
  end empty;
  
  procedure put(s: in out set; k: in key; x: in item) is
    first: pnode renames s.first;
    pp, p, q: pnode;
  begin
    pp:= null; p:= first;
    while p/=null and then p.k < k loop pp:= p; p:= p.next; end loop;
    if p/=null and then p.k=k then raise already_exists; end if;
    
    q:= new node; q.all:= (k, x, p);
    if pp=null then first:= q; else pp.next:= q; end if;

    exception
      when storage_error => raise space_overflow;
  end put;

  procedure update(s: in out set; k: in key; x: in item) is
    first: pnode renames s.first;
    p: pnode;
  begin
    p:= first;

    while p/=null and then p.k < k loop p:= p.next; end loop;
    if p=null or else p.k/=k then raise does_not_exist; end if ;

    p.x:= x;
  end update;

  procedure get(s: in set; k: in key; x: out item) is
    first: pnode renames s.first;
    p: pnode;
  begin
    p:= first;

    while p/=null and then p.k < k loop p:= p.next; end loop;
    if p=null or else p.k/=k then raise does_not_exist; end if ;

    x:= p.x;
  end get;

  procedure remove(s: in out set; k: in key) is
    first: pnode renames s.first;
    pp, p: pnode;
  begin
    pp:= null; p:= first;

    -- look for it
    while p/=null and then p.k < k loop pp:= p; p:= p.next; end loop;
    if p=null or else p.k/=k then raise does_not_exist; end if ;

    if pp=null then first:= p.next; -- remove the only item
    else pp.next:= p.next; end if;
  end remove;

  -- ITERATOR
  procedure first(s: in set; it: out iterator) is
    first: pnode renames s.first;
    p: pnode renames it.p;
  begin
    p:= first;
  end first;

  procedure next(s: in set; it: in out iterator) is
    p: pnode renames it.p;
  begin
    p:= p.next;
  exception
    when constraint_error => raise bad_use;
  end next;

  function is_valid(it: iterator) return boolean is
    p: pnode renames it.p;
  begin
    return p/=null;
  end is_valid;

  procedure get(s: in set; it: in iterator; k: out key; x: out item) is
    p: pnode renames it.p;
  begin
    k:= p.k; x:= p.x;
  exception
    when constraint_error => raise bad_use;
  end get; 

end d_mapping;