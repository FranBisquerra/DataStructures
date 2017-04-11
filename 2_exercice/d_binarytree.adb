with Ada.Text_IO; use Ada.Text_IO;

package body d_binarytree is

   procedure empty(t: out tree) is
      p: pnode renames t.root;
   begin
      p:=null;
   end empty;

   function is_empty(t: in tree) return boolean is
      p: pnode renames t.root;
   begin
      return p=null;
   end is_empty;

   procedure graft(t: out tree; lt, rt: in tree; x: in item) is
      p: pnode renames t.root;
      pl: pnode renames lt.root;
      pr: pnode renames rt.root;
   begin
      p:= new node;
      p.all:= (x, pl, pr);
   exception
      when storage_error => raise space_overflow;
   end graft;

   procedure root(t: in tree; x: out item) is
      p: pnode renames t.root;
   begin
      x:= p.x;
   exception
      when constraint_error => raise bad_use;
   end root;

   procedure left(t: in tree; lt: out tree) is
      p: pnode renames t.root;
      pl: pnode renames lt.root;
   begin
      pl:= p.l;
   exception
      when constraint_error => raise bad_use;
   end left;

   procedure right(t: in tree; rt: out tree) is
      p: pnode renames t.root;
      pr: pnode renames rt.root;
   begin
      pr:= p.r;
   exception
      when constraint_error => raise bad_use;
   end right;

   procedure inordre(t: in tree) is
      p: pnode renames t.root;
      pl: pnode renames t.root.l;
      pr: pnode renames t.root.r;
      tl, tr: tree;
   begin
      if pl/=null then
         left(t, tl);
         inordre(tl);
      end if;
      Put(Image(p.x));
      if pr/=null then
         right(t, tr);
         inordre(tr);
      end if;
   end inordre;

   procedure do_right_tree(t: in tree; r: in Trav.traversal; b: in out boolean) is
      p: pnode renames t.root;
      pl: pnode renames t.root.l;
      pr: pnode renames t.root.r;
      tl, tr: tree;
   begin
      if pl/=null then
         left(t, tl);
         do_right_tree(tl, r, b);
         if not b then return; end if;
      end if;
      Put(i'Img & ": " & Image(p.x) & "[" & Trav.Image(Trav.get(r, i)) & "]");
      if Trav.Image(Trav.get(r, i)) /= Image(p.x) then b:= false; return; end if;
      i:= i + 1;
      if pr/=null then
         right(t, tr);
         do_right_tree(tr, r, b);
         if not b then return; end if;
      end if;
   end do_right_tree;
   
   function do_right_tree(t: in tree; r: in Trav.traversal) return boolean is
      p: pnode renames t.root;
      pl: pnode renames t.root.l;
      pr: pnode renames t.root.r;
      tl, tr: tree;
   begin
      if pl/=null then
         left(t, tl);
         if not do_right_tree(tl, r) then return false; end if;
      end if;
      Put(i'Img & ": " & Image(p.x) & "[" & Trav.Image(Trav.get(r, i)) & "]");
      if Trav.Image(Trav.get(r, i)) /= Image(p.x) then return false; end if;
      i:= i + 1;
      if pr/=null then
         right(t, tr);
         if not do_right_tree(tr, r) then return false; end if;
      end if;
      return true;
   end do_right_tree;
   
   function is_right_tree(t: in tree; r: in Trav.traversal) return boolean is
      right: boolean;
   begin
      i:= 1; -- set the global variable
      return do_right_tree(t, r);
   end is_right_tree;
   
   function do_bst(t: in tree; min: in item; max: in item) return boolean is
      p: pnode renames t.root;
      pl: pnode renames t.root.l;
      pr: pnode renames t.root.r;
      tl, tr: tree;
   begin
      if p.x < min and p.x > max then return false; end if;        
      if pl/=null then
         left(t, tl);
         if not do_bst(tl, min, Pred(max)) then return false; end if;
      end if;
      if pr/=null then
         right(t, tr);
         if not do_bst(tr, Succ(min), max) then return false; end if;
      end if;
      return true;
      end do_bst;
   
   function is_bst(t: in tree) return boolean is
      p: pnode renames t.root;
      bst: boolean;
      min, max: item;
   begin
      min:= p.x; max:= p.x;
      return do_bst(t, min, max);    
   end is_bst;
end d_binarytree;
