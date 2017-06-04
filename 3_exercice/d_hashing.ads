generic

  type key is private;
  type item is private;
  with function "<" (k1, k2: in key) return boolean;
  with function hash (k: in key; b: in positive) return natural;
  size: positive; -- prime number

package d_hashing is

  type set is limited private;

  bad_use, space_overflow, already_exists,does_not_exist: exception;

  procedure empty (s: out set);
  procedure put (s: in out set; k: in key; x: in item);

private

  dt_size: constant natural:= size;
  max_item_number: constant natural:= size*8/10;
  type cell_state is (free, occupied);

  type cell is record
    k: key;
    x: item;
    state: cell_state;
  end record;

  type dispersion_table is array(natural range 0..dt_size-1) of cell;

  type set is record
    dt: dispersion_table;
    item_number: natural; -- number of stored items
  end record;

end d_hashing;