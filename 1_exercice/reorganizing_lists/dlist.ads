generic
	type item is private;
	
	with function "<" (x,y: in item) return boolean is <>;
  	with function "=" (x,y: in item) return boolean is <>;
  	with function ">" (x,y: in item) return boolean is <>;

package dlist is

	type list is limited private;
	type pcell is limited private;

	procedure insert (l: in out list; x: in item);
	procedure dis_order(l: in out list; x: in item);
	function size(l: in list) return Integer;
	function get_item(l: in list; position: in Integer) return item;

private

type cell;
type pcell is access cell;

type list is record
	first: pcell:= null;
end record;

end dlist;
