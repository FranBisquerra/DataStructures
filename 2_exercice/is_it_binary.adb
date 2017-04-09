with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with d_binarytree;

procedure is_it_binary is

    package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);

    package char_tree is new d_binarytree(item => Character, Max => 100, Image => Character'Image);
    use char_tree;

    -- Variable declarations
    source: File_Type;
    inorder_traversal: SB.Bounded_String;
    preorder_traversal: SB.Bounded_String;
    root: Natural:= 1;	
    t: tree;

    -- Delete blanks
    procedure delete_blanks(with_blanks: in out SB.Bounded_String) is
        idx: Natural:= 2;
    begin
        while idx < SB.Length(with_blanks) loop
            SB.Delete(with_blanks, idx, idx);
            idx:= idx + 1;
        end loop;
    end delete_blanks;

    -- Returns the root index in String
    function find_root_index(c_root: in Character; sub_inorder: in SB.Bounded_String) return Natural is
        idx: Natural:= 1;
    begin
        while idx <= SB.Length(sub_inorder) loop
            if SB.Element(sub_inorder, idx) = c_root then
                return idx;                
            end if;
            idx:= idx + 1;
        end loop;
        return 0;
    end find_root_index;

     -- Splits the inorder String into two sub arrays based on the root position
    procedure split_inorder(c_root: in Character;  sub_inorder: in SB.Bounded_String; sub_inorder_l, sub_inorder_r: out SB.Bounded_String) is
        root_idx, l_limit, r_limit: Natural;
    begin
        root_idx:= find_root_index(c_root, sub_inorder);
        Put_Line("after: "&c_root & " - " & root_idx'Img);

        l_limit:= root_idx - 1; 
        r_limit:= root_idx + 1; 

        sub_inorder_l:= SB.To_Bounded_String(SB.Slice(sub_inorder, 1, l_limit));
        sub_inorder_r:= SB.To_Bounded_String(SB.Slice(sub_inorder, r_limit, SB.Length(sub_inorder)));

        Put_Line("sub_l: "&SB.To_String(sub_inorder_l));
        Put_Line("sub_r: "&SB.To_String(sub_inorder_r));
        Put_Line("");
    end split_inorder;

    -- Builds the binary tree based on the inorder and preorder traversals
    procedure build_binary_tree(r: in out tree; sub_inorder: in SB.Bounded_String) is 
        sub_inorder_l: SB.Bounded_String;
        sub_inorder_r: SB.Bounded_String;
        t, lt, rt: tree;
        c_root: Character;
    begin
        Put_Line("sub_inorder: "&SB.To_String(sub_inorder));
        c_root:= SB.Element(preorder_traversal, root);
        
        -- keep iterating
        if SB.Length(sub_inorder) > 1 then 
            split_inorder(c_root, sub_inorder, sub_inorder_l, sub_inorder_r);
            if SB.Length(sub_inorder_l) >= 1 then
                root:= root + 1;
                Put_Line("left, root: "&root'Img);
                build_binary_tree(lt, sub_inorder_l);
            end if;
            if SB.Length(sub_inorder_r) >= 1 then
                root:= root + 1;
                Put_Line("right, root: " &root'Img);            
                build_binary_tree(rt, sub_inorder_r);
            end if;
            graft(r, lt, rt, c_root);
            Put_Line("item: " & c_root);
        -- is leaf
        elsif SB.Length(sub_inorder) = 1 then 
            graft(r, lt, rt, c_root);
            Put_Line("leaf: " & c_root);
        end if;
    end build_binary_tree;

    -- String to 'traversal'
    function to_traversal(str: in SB.Bounded_String) return traversal is
        i: idx:= 1;
        j: Natural:= 1;
        tr: traversal;
    begin
    Put_Line("to tr");
        while j <= SB.Length(str) loop
            tr(i):= SB.Element(str, j);
            Put(tr(i));
            i:= i + 1;
            j:= j + 1;
        end loop;
        Put_Line("");
        return tr;
    end to_traversal;

    binary_tree: tree;
begin

    -- Load traversals from file
	Open(source, Mode => In_File, Name => "tree_traversals");
    inorder_traversal:= SB.To_Bounded_String(Get_Line(source));
    delete_blanks(inorder_traversal);
    preorder_traversal:= SB.To_Bounded_String(Get_Line(source));
    delete_blanks(preorder_traversal);
    Close(source);

    build_binary_tree(binary_tree, inorder_traversal);
    inordre(binary_tree);
    if right_tree(binary_tree, to_traversal(inorder_traversal)) then
        Put_Line("Correcte");
    else
        Put_Line("Incorrecte");
    end if;
    
exception
    when End_Error =>
        Put_Line("Please check the tree_traversals file it must have only two lines, being the first one the inorder traversal");
        Put_Line("and the second one the preorder traversal.");
        Close(source);
    when Constraint_Error =>
        Put_Line("String too long for variable.");
    when Storage_Error =>
        Put_Line ("Traversals too long to store.");
end is_it_binary;