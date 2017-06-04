generic

  type key is private;
  type item is private;
  with function "<" (k1, k2: in key) return boolean;

package d_mapping is

  type set is limited private;

  already_exists: exception;
  does_not_exist: exception;
  space_overflow: exception;
  bad_use: exception;

  procedure empty (s: out set);
  procedure put (s: in out set; k: in key; x: in item);
  procedure remove(s: in out set; k: in key);
  procedure update(s: in out set; k: in key; x: in item);

  -- ITERATOR
  type iterator is private;
  
  procedure first (s : in set; it : out iterator);
  procedure next (s : in set; it : in out iterator);
  function is_valid ( it : in iterator) return boolean;
  procedure get (s : in set; it : in iterator; k : out key; x: out item);

private

  type node;
  type pnode is access node;
  type node is record
    k: key;
    x: item;
    next: pnode;
  end record;

  type set is record
    first: pnode;
  end record;

  type iterator is record
    p : pnode; -- current item pointer 
  end record;

end d_mapping;