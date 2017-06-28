with Ada.Containers, Ada.Strings.Unbounded; use Ada.Containers, Ada.Strings.Unbounded; 

generic

  type item is private;
  size: positive; -- prime number

package d_hashing is

  type set is limited private;

  bad_use, space_overflow, already_exists,does_not_exist: exception;

  procedure empty (s: out set);
  procedure put (s: in out set; k: in Unbounded_String; x: in item);
  procedure get(s: in set; k: in Unbounded_String; x: out item);
  procedure update(s: in out set; k: in Unbounded_String; x: in item);

private

  dt_size: constant Ada.Containers.Hash_Type:= Ada.Containers.Hash_Type(size);
  max_item_number: constant Ada.Containers.Hash_Type:= dt_size*8/10;
  subtype hash_index is Ada.Containers.Hash_Type range 0..dt_size-1;

  type cell_state is (free, occupied);

  type cell is record
    k: Unbounded_String;
    x: item;
    state: cell_state;
  end record;

  type dispersion_table is array(hash_index) of cell;

  type set is record
    dt: dispersion_table;
    item_number: Ada.Containers.Hash_Type; -- number of stored items
  end record;

end d_hashing;