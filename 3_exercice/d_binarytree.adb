package body d_binarytree is

  procedure empty(t: out tree) is
    root: pnode renames t.root;
  begin
    root:= null;
  end empty;
  
  procedure put(p: in out pnode; k: in key; x: in item) is
  begin
    if p=null then 
      p:= new node;
      p.all:= (k, x, null, null);
    else 
      if k<p.k then put(p.lt, k, x);
      elsif k>p.k then put(p.rt, k, x);
      else raise already_exists;
      end if;
    end if;
  exception
    when storage_error => raise space_overflow;
  end put;

  procedure put(t: in out tree; k: in key; x: in item) is
    root: pnode renames t.root;
  begin
    put(root, k, x);
  end put;

  procedure update(p: in pnode; k: in key; x: in item) is
  begin 
    if p=null then 
      raise does_not_exist;
    else 
      if k<p.k then update(p.lt, k, x);
      elsif k>p.k then update(p.rt, k, x);
      else p.x:= x;
      end if;
    end if;  
  end update;

  procedure update(t: in out tree; k: in key; x: in item) is
    root: pnode renames t.root;
  begin 
    update(root, k, x);
  end update;

-- ITERATOR
  procedure first (t: in tree; it: out iterator) is
    root: pnode renames t.root;
    st: stack renames it.st;
    p: pnode;
  begin

    empty(st);
    if root/= null then
      p:= root;
      while p.lt /=null loop
        push(st, p); 
        p:= p.lt;
      end loop;
      push(st, p);
    end if;
  end first;

  procedure next(t: in tree; it: in out iterator) is
    st: stack renames it.st;
    p: pnode;
  begin

    p:= top(st);
    pop(st);
    if p.rt/=null then
      p:= p.rt;
      while p.lt/=null loop
        push(st, p);
        p:= p.lt;
      end loop;
      push(st, p);
    end if;

  exception
    when d_node_stack.bad_use => raise d_binarytree.bad_use;
  end next;

  function is_valid (it: in iterator) return boolean is
    st: stack renames it.st;
  begin
    return not is_empty(st);
  end is_valid;

  procedure get (t: in tree; it: in iterator; k: out key; x: out item) is
    st: stack renames it.st;
    p: pnode;
  begin
    p:= top(st);
    k:= p.k; x:= p.x;
  exception
    when d_node_stack.bad_use => raise d_binarytree.bad_use;
  end get;

end d_binarytree;
