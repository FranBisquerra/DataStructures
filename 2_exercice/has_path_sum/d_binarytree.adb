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
      if Trav.Image(Trav.get(r, i)) /= Image(p.x) then return false; end if;
      i:= i + 1;
      if pr/=null then
         right(t, tr);
         if not do_right_tree(tr, r) then return false; end if;
      end if;
      return true;
   end do_right_tree;
   
   function is_right_tree(t: in tree; r: in Trav.traversal) return boolean is
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
      if p.x < min or p.x > max then return false; end if;        
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
      min, max: item;
   begin
      min:= item'First; max:= item'Last;
      return do_bst(t, min, max);    
   end is_bst;

   function do_path_sum(t: in tree; sum: in item; x: in item) return boolean is
      p: pnode renames t.root;
      pl: pnode renames t.root.l;
      pr: pnode renames t.root.r;
      ok: boolean := False;
      s: item;
      lt, rt: tree;
   begin
    if pl = null and pr = null then
      s:= sum + p.x;
      return s = x;
    else   
      s:= sum + p.x;
      if pl /= null then   
          left(t, lt); 
          ok:= do_path_sum(lt, s, x);
        if pr/= null and ok = False then
          right(t, rt);
          ok:= do_path_sum(rt, s, x);
        end if;
      end if;
      return ok;
    end if;
   end do_path_sum;

   function is_path_sum(t: in tree; x: in item) return boolean is
   begin
      return do_path_sum(t, item'First, x);
   end is_path_sum;

end d_binarytree;
