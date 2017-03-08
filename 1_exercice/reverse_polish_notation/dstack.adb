package body dstack is 

	type block is record
		x: item;
		next: index;
	end record;

	type mem_space is array(index range 1..index'last) of block;

	ms: mem_space;
	free: index;

	procedure prep_mem_space is 
	begin
		for i in index range 1..index'last-1 loop
			ms(i).next:= i + 1;
		end loop;
		ms(index'last):= 0;
		free:= 1;
	end prep_mem_space;

	function get_block return index is
		aux: index;
	begin
		if free = 0 then raise space_overflow; end if;
		aux:= free; 
		free:= ms(free).next; 
		ms(aux).next:= 0; 
		return aux;
	end get_block;

	procedure release_block (aux: in out block) is
	begin 
		ms(aux).next:= free;
		free:= aux; aux:= 0;
	end release_block;

	procedure release_stack (top: in out index) is
		aux: index;
	begin
		if top /= 0 then
			aux:= top;
			while ms(aux).next /= 0 loop aux:= ms(aux).next; end loop;
			ms(aux).next:= free; 
			free:= top;
			top:= 0;
		end if;
	end release_stack;

	procedure empty (s: out stack) is
		top: index renames s.top;
	begin
		release_stack(top);
	end empty;

	function is_empty (s: in stack) return boolean is
		top: index renames s.top;
	begin
		return top = 0;
	end is_empty;

	function top (s: in stack) return item is 
		top: index renames s.top;
	begin
		return ms(top).x;
	exception
		when constraint_error => raise bad_use; 
	end top;

	procedure push (s: in out stack; x: in item) is
		top: index renames s.top;
		aux: index;
	begin
		aux:= get_block;
		ms(aux):= (x, top);
		top:= aux;
	end push;

	procedure pop (s: in out stack) is
		top: index renames s.top;
		aux: index;
	begin
		aux:= top;
		top:= ms(top).next;
		release_block(aux);
	exception 
		when constraint_error => raise bad_use;
	end pop;

	begin
		prep_mem_space;
	end dstack;