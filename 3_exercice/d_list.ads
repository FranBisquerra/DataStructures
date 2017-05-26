generic
  type item is private;
package d_list is

  type list is limited private;

  bad_use: exception;
  space_overflow : exception;

  procedure empty(l: out list);
  function is_empty( l: in list) return boolean;
  procedure insert(l: in out list; x: in item);

private

  type node;
  type pnode is access node;
  type node is record
    x: item;
    next: pnode;
 end record;

 type list is record
  p: pnode;
 end record;

end d_list;