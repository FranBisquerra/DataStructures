generic
   type item is private;
   first_item: Item; -- element neutre del tipus item, si son nombres sera 0
   len: Integer;
   with function "+"(a,b: in Item) return Item;
   with function Image(a: in Item) return String; --per imprimir items

package dbinary is

   type tree is limited private;
   -- no pot haver mes procediments o funcions publiques
   procedure insert(t: in out tree; x: in item);
   function is_path_sum(a: in tree; x: in item) return boolean;

private

   subtype rang is Integer range 1..len;

   type mem is array (rang) of item;

   type tree is record
     m: mem;
     free: rang := rang'First;
  end record;

end dbinary;