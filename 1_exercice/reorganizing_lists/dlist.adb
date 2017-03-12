package body dlist is 

	type cell is record
		x: item;
		next: pcell;
	end record;

	-- Insert an item into a list
	procedure insert(l: in out list; x: in item) is
		p, pp, r: pcell;
		first: pcell renames l.first;
	begin
		pp:= null; 
		p:= first;
		while p /= null loop
			pp:= p; 
			p:= p.next;
		end loop;
		r:= new cell; 
		r.all:= (x, null);
		r.next:= p;
		if pp = null then 
			first:= r;
		else 
			pp.next:= r;
		end if ;
	end insert;

	-- Reorders the list leaving the items with lower value than x at the begining of the list and the ones 
	-- with greater value after it. Keeping the same original order of the numbers.
	procedure dis_order(l: in out list; x: in item) is
		p, p_next, first_min, last_min, first_max, last_max: pcell;
		first: pcell renames l.first;
	begin
		first_min:= null;
		first_max:= null;
		p:= first;
		while p/= null loop
			p_next:= p.next;
			-- Add to the list of lower values
			if p.x < x then 
				if first_min = null then 
					first_min:= p;
					last_min:= p;
				else
					last_min.next:= p;
					last_min:= p;
				end if; 
			end if;
			-- Add to the list of greater values
			if p.x > x then 
				if first_max = null then 
					first_max:= p;
					last_max:= p;
				else
					last_max.next:= p;
					last_max:= p;
				end if;
			end if;
			-- Add to the begining of the list of greater values
			if p.x = x then 
				if first_max = null then 
					first_max:= p;
					last_max:= p;
				else
					p.next:= first_max;
					first_max:= p;
				end if;
			end if;

			p:= p_next;
		end loop;

		-- Edge case when list of lower values is empty raises a constraint error
		if first_min /= null then
			first:= first_min;
			last_min.next:= first_max;
		else 
			first:= first_max;
		end if;
		
		-- Edge case when list of greater values is empty raises a constraint error
		if first_max /= null then
			last_max.next:= null;
		end if;
	end dis_order;

	-- Return number of items in the list
	function size(l: in list) return Integer is
		p: pcell;
		l_size: Integer:= 0;
		first: pcell renames l.first;
	begin
		p:= first;
		while p /= null loop
			l_size:= l_size + 1;
			p:= p.next;
		end loop;
		return l_size;
	end size;

	-- Return item from list based on position
	function get_item(l: in list; position: in Integer) return item is
		p: pcell;
		first: pcell renames l.first;
		i: Integer:= 1;
	begin
		p:= first;
		while (i < position) loop
			p:= p.next;
			i:= i + 1;
		end loop;
	return p.x;
	end get_item;

end dlist;