with Ada.Text_IO; use Ada.Text_IO;

package body d_traversal is

    function length(tr: traversal) return idx is
    begin
        return tr.amount;
    end length;

    function is_empty(tr: in traversal) return boolean is
    begin
        return length(tr) = 0;
    end is_empty;

    function get(tr: in traversal; index: in idx) return item is
    begin
        return tr.content(index);        
    exception
        when Constraint_Error => raise bad_use;
    end get; 
    
    procedure add(tr: in out traversal; x: in item) is
    begin
        tr.content(tr.amount):= x;
        tr.amount:= tr.amount + 1;
        Put_Line(""&tr.amount'Img & " " & image(x));
    exception
        when Constraint_Error => raise space_overflow;
    end add;
    
    function compare(first_tr, second_tr: in traversal; index: in idx) return boolean is
    begin
        return first_tr.content(index) = second_tr.content(index);
    end compare;

    procedure slice(tr: in traversal; sub_tr: in out traversal; begining, ending: idx) is
        i: idx;
    begin
    Put_Line("");
        if begining >= 0 and ending < length(tr) then 
            i:= begining;
            while i < length(tr) loop
                add(sub_tr, get(tr, i));
                i:= i + 1;
            end loop;
        end if;
    end slice;

end d_traversal;