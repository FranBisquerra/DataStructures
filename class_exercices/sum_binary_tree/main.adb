with dbinary;
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   package binary is new dbinary(Integer, 0, 200,"+", Integer'Image);
   use binary;
   t : binary.tree;
begin
   insert(t, 3);
   insert(t, 4);
   insert(t, 8);

   put(is_path_sum(t, 11)'Img);
end Main;