generic

	type item is private;

package d_stack is

	type stack is private;

	bad_use, space_overflow: exception;

	procedure empty (s: out stack);
	procedure push (s: in out stack; x: in item);
	procedure pop (s: in out stack);
	function top (s: in stack) return item;
	function is_empty (s: in stack) return boolean;

private

	type node;
	type pnode is access node;
	type node is record
		x: item;
		next: pnode;
	end record;

	type stack is record
		top: pnode;
	end record;

end d_stack;
