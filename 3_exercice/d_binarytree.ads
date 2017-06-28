with d_stack;

generic
  type key is private;
  type item is private;

  with function "<"(k1,k2: in key) return boolean is <>;
  with function ">"(k1,k2: in key) return boolean is <>;

package d_binarytree is

  type tree is private;

  bad_use, space_overflow, already_exists,does_not_exist: exception;

  procedure empty (t: out tree);
  procedure put (t: in out tree; k: in key; x: in item);
  procedure update(t: in out tree; k: in key; x: in item);
  procedure get (t: in tree; k: in key; x: out item);

  -- ITERATOR
  type iterator is private;

  procedure first (t : in tree; it : out iterator);
  procedure next (t : in tree; it : in out iterator);
  function is_valid (it : in iterator) return boolean;
  procedure get (t : in tree; it : in iterator; k : out key; x: out item);

private

  type node;
  type pnode is access node;

  type node is
    record
      k: key;
      x: item;
      lt, rt: pnode;
  end record;

  type tree is
    record
      root: pnode;
  end record;

  package d_node_stack is new d_stack(pnode);
  use d_node_stack;

  type iterator is 
    record
      st: stack;
  end record;

end d_binarytree;
