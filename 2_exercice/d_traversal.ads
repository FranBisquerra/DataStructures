generic
   Max : Positive; -- Maximun lenght of a traversal
   type item is private;

   with function "<"(x,y: in item) return boolean is <>;
   with function "="(x,y: in item) return boolean is <>;
   with function ">"(x,y: in item) return boolean is <>;
   with function Image(x: in item) return String;

package d_traversal is
    Max_Length: constant Positive:= Max;

    subtype idx is Natural range 0..Max_Length;
    type traversal is limited private;

    bad_use: exception;
    space_overflow: exception;
    not_sliceable: exception;

    function length(tr: in traversal) return idx;
    function is_empty(tr: in traversal) return boolean;
    function get(tr: in traversal; index: in idx) return item;
    procedure add(tr: in out traversal; x: in item);
    function compare(first_tr, second_tr: in traversal; index: in idx) return boolean;
    procedure slice(tr: in traversal; sub_tr: in out traversal; begining, ending: idx);

private

    type tr_content is array (idx range 1..idx'Last) of item;
    
    type traversal is 
    record
        content: tr_content;
        amount: idx:= 1;
    end record;

end d_traversal;
