with Ada.Strings.Unbounded.Hash; use Ada.Strings.Unbounded; 

package body d_hashing is

  procedure empty(s: out set) is
    dt: dispersion_table renames s.dt;
    item_number: Ada.Containers.Hash_Type renames s.item_number;
  begin
    for i in 0..dt_size-1 loop
      dt(i).state:= free;
    end loop;
    item_number:= 0;
  end empty;

  procedure put(s: in out set; k: in Unbounded_String; x: in item) is
    dt: dispersion_table renames s.dt;
    item_number: Ada.Containers.Hash_Type renames s.item_number;
    attemps_amount, initial_position, current_position: Ada.Containers.Hash_Type; 
  begin
  
    initial_position:= Hash(k) mod dt_size;
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

  procedure get(s: in set; k: in Unbounded_String; x: out item) is
    dt: dispersion_table renames s.dt;
    attemps_amount, initial_position, current_position: Ada.Containers.Hash_Type;
  begin
    initial_position:= Hash(k) mod dt_size;
    current_position:= initial_position; 
    attemps_amount:= 0;

    while dt(current_position).state=occupied and then dt(current_position).k/=k loop
      attemps_amount:= attemps_amount+1; 
      current_position:= (initial_position+attemps_amount*attemps_amount) mod dt_size;
    end loop;

    if dt(current_position).state=free then raise does_not_exist; end if;
    x:= dt(current_position).x;
  end get;

  procedure update(s: in out set; k: in Unbounded_String; x: in item) is
    dt: dispersion_table renames s.dt;
    attemps_amount, initial_position, current_position: Ada.Containers.Hash_Type;
  begin
    initial_position:= Hash(k) mod dt_size;
    current_position:= initial_position; 
    attemps_amount:= 0;

    while dt(current_position).state=occupied and then dt(current_position).k/=k loop
      attemps_amount:= attemps_amount+1; 
      current_position:= (initial_position+attemps_amount*attemps_amount) mod dt_size;
    end loop;

    if dt(current_position).state=free then raise does_not_exist; end if;
    dt(current_position).x:= x;
  end update;  

end d_hashing;