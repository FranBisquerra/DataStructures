package body d_hashing is

  procedure empty(s: out set) is
    dt: dispersion_table renames s.dt;
    item_number: natural renames s.item_number;
  begin
    for i in 0..dt_size-1 loop
      dt(i).state:= free;
    end loop;
    item_number:= 0;
  end empty;

  procedure put(s: in out set; k: in key; x: in item) is
    dt: dispersion_table renames s.dt;
    item_number: natural renames s.item_number;
    initial_position, current_position: natural; 
    attemps_amount: natural;
  begin
  
    initial_position:= hash(k, max_item_number); 
    current_position:= initial_position;
    attemps_amount:= 0;

    while dt(current_position).state=occupied and then dt(current_position).k/=k loop
      attemps_amount:= attemps_amount+1; 
      current_position:= (initial_position+attemps_amount*attemps_amount) mod dt_size;
    end loop;

    if dt(current_position).state=occupied then raise already_exists; end if ;
    if item_number=max_item_number then raise space_overflow; end if ;

    dt(current_position).state:= occupied; 
    dt(current_position).k:= k; 
    dt(current_position).x:= x;
    item_number:= item_number+1;
  end put;

  procedure get(s: in set; k: in key; x: out item) is
    dt: dispersion_table renames s.dt;
    initial_position, current_position: natural;
    attemps_amount: natural;
  begin
    initial_position:= hash(k, max_item_number);
    current_position:= initial_position; 
    attemps_amount:= 0;

    while dt(current_position).state=occupied and then dt(current_position).k/=k loop
      attemps_amount:= attemps_amount+1; 
      current_position:= (initial_position+attemps_amount*attemps_amount) mod max_item_number;
    end loop;

    if dt(current_position).state=free then raise does_not_exist; end if;
    x:= dt(current_position).x;
  end get;

end d_hashing;