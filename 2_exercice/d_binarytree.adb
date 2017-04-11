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

   procedure inordre_pnode(n: in pnode) is
      p: pnode renames n;
      pl: pnode renames n.l;
      pr: pnode renames n.r;
   begin
      if pl/=null then
         inordre_pnode(pl);
      end if;
      Put(Image(p.x));
      if pr/=null then
         inordre_pnode(pr);
      end if;
   end inordre_pnode;

   procedure inordre(t: in tree) is
      p: pnode renames t.root;
      pl: pnode renames t.root.l;
      pr: pnode renames t.root.r;
   begin
      if pl/=null then
         inordre_pnode(pl);
      end if;
      Put(Image(p.x));
      if pr/=null then
         inordre_pnode(pr);
      end if;
   end inordre;

   procedure inordre_pnode(n: in pnode; r: in out traversal) is
      p: pnode renames n;
      pl: pnode renames n.l;
      pr: pnode renames n.r;
   begin
      if pl/=null then
         inordre_pnode(pl, r);
      end if;
      Put(index'Img&" : " & Image(p.x));
      r(index):=p.x;
      index:= index + 1;
      if pr/=null then
         inordre_pnode(pr, r);
      end if;
   end inordre_pnode;

   procedure inordre(t: in tree; r: in out traversal) is
      p: pnode renames t.root;
      pl: pnode renames t.root.l;
      pr: pnode renames t.root.r;
   begin
      if pl/=null then
         inordre_pnode(pl, r);
      end if;
      Put(index'Img&" : " & Image(p.x));      
      r(index):=p.x;
      index:= index + 1;
      if pr/=null then
         inordre_pnode(pr, r);
      end if;
   end inordre;

   

end d_binarytree;
