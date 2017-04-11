with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with d_binarytree, d_traversal;

procedure is_it_binary is

    package char_tree is new d_binarytree(item => Character, Max => 100, Image => Character'Image);
    use char_tree;

    package CT is new d_traversal(item => Character, Max => 100, Image => Character'image);

    -- Variable declarations
    source: File_Type;
    tr1, tr2, inorder_traversal, preorder_traversal: CT.traversal;
    b, root: Natural:= 1;	
    t: tree;

    -- Returns the root index in traversal
    function find_root_index(c_root: in Character; sub_inorder: in CT.traversal) return Natural is
        i: Natural:= 1;
    begin
        while i <= CT.Length(sub_inorder) loop
            if CT.get(sub_inorder, i) = c_root then
                return i;                
            end if;
            i:= i + 1;
        end loop;
        return 0;
    end find_root_index;

     -- Splits the inorder String into two sub arrays based on the root position
    procedure split_inorder(c_root: in Character;  sub_inorder: in CT.traversal; sub_inorder_l, sub_inorder_r: in out CT.traversal) is
        root_idx: Natural;
    begin
        root_idx:= find_root_index(c_root, sub_inorder);
        CT.slice(sub_inorder, sub_inorder_l, 1, root_idx - 1);
        CT.slice(sub_inorder, sub_inorder_r, root_idx + 1, CT.Length(sub_inorder));

    end split_inorder;

    -- Builds the binary tree based on the inorder and preorder traversals
    procedure build_binary_tree(r: in out tree; sub_inorder: in CT.traversal) is 
        sub_inorder_l: CT.traversal;
        sub_inorder_r: CT.traversal;
        t, lt, rt: tree;
        c_root: Character;
    begin
        c_root:= CT.get(preorder_traversal, root);
        
        -- keep iterating
        if CT.length(sub_inorder) > 1 then 
            split_inorder(c_root, sub_inorder, sub_inorder_l, sub_inorder_r);
            if CT.length(sub_inorder_l) >= 1 then
                root:= root + 1;
                build_binary_tree(lt, sub_inorder_l);
            end if;
            if CT.length(sub_inorder_r) >= 1 then
                root:= root + 1;
                build_binary_tree(rt, sub_inorder_r);
            end if;
            graft(r, lt, rt, c_root);
        -- is leaf
        elsif CT.length(sub_inorder) = 1 then 
            graft(r, lt, rt, c_root);
        end if;
    end build_binary_tree;

    binary_tree: tree;

begin

    -- Load traversals from file
--	Open(source, Mode => In_File, Name => "tree_traversals");
--    inorder_traversal:= SB.To_Bounded_String(Get_Line(source));
--    delete_blanks(inorder_traversal);
--    preorder_traversal:= SB.To_Bounded_String(Get_Line(source));
--    delete_blanks(preorder_traversal);
--    Close(source);

--    build_binary_tree(binary_tree, inorder_traversal);
--    inordre(binary_tree);
    --if right_tree(binary_tree, to_traversal(inorder_traversal)) then
    --    Put_Line("Correcte");
    --else
    --    Put_Line("Incorrecte");
    --end if;

    CT.add(tr1, 'A');
    CT.add(tr1, 'B');
    CT.add(tr1, 'C');
    CT.add(tr1, 'D');
    CT.add(tr1, 'E');

    CT.slice(tr1, tr2, 1, 3);

    while b < CT.length(tr2) loop
        Put(CT.get(tr2, b));
        b:= b + 1;
    end loop;

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