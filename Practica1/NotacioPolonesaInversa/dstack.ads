generic
	type item is private;
	max: integer := 100;
package dstack is

	type stack is limited private;

	bad_use: exception;
	space_overflow: exception;

	procedure empty (s: out stack);
	procedure push (s: in out stack, x: in item);
	procedure pop (s: in out stack);
	function top (s: in stack) return item;
	function is_empty (s: in stack) return boolean;

private

type index is new integer range 0..max;

type stack is record
	top: index := 0;
end record;

end dstack;
