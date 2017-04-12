with d_traversal;

generic
   type item is (<>);

   with function "<"(x,y: in item) return boolean is <>;
   with function "="(x,y: in item) return boolean is <>;
   with function ">"(x,y: in item) return boolean is <>;
   with function "+"(x,y: in item) return item is <>;
   with function Image(x: in item) return String;
   with function Succ(x: in item) return item is <>;
   with function Pred(x: in item) return item is <>;
   with package Trav is new d_traversal(<>);

package d_binarytree is

   type tree is limited private;

   bad_use: exception;
   space_overflow: exception;

   procedure empty(t: out tree);
   function is_empty(t: in tree) return boolean;
   procedure graft (t: out tree; lt, rt: in tree; x: in item);
   procedure root(t: in tree; x: out item);
   procedure left(t: in tree; lt: out tree);
   procedure right(t: in tree; rt: out tree);
   procedure inordre(t: in tree);   
   function is_right_tree(t: in tree; r: in Trav.traversal) return boolean;
   function is_bst(t: in tree) return boolean;
   function is_path_sum(t: in tree; x: in item) return boolean;

private

   type node;
   type pnode is access node;

   type node is
      record
         x: item;
         l, r: pnode;
      end record;

   type tree is
      record
         root: pnode;
      end record;

   i: Natural:= 0;   

end d_binarytree;
