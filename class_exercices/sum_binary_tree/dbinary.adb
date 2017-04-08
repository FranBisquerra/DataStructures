package body dbinary is

	procedure insert(t: in out tree; x: in Item) is
			free: Integer renames t.free;
		begin
			t.m(free):= x;
			free:= free + 1;
	end insert;	

	function preorder(t: in tree; idx: in rang; sum: in Item; x: in Item) return boolean is

	ok1, ok2 : Boolean;
	s : Item;
	begin

	if idx < free then

		s := sum + t.m(idx);
		put_line(Image(s));
		ok1:= preorder(a, idx*2, s, x);

		ok2:= preorder(a, (idx*2)+1,  s, x);

		return ok1 or ok2;
	else
		return sum = x;
	end if;

	end preorder;

	function is_path_sum(t: in tree; x: in item) return boolean is

	begin
		return preorder(t, rang'First, first_item, x);
	end is_path_sum;

end dbinary;