with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with d_binarytree, d_traversal;

procedure is_it_binary is

    -- Exceptions
    not_a_proper_character: exception;

    package CT is new d_traversal(item => Character, Max => 100, Image => Character'image);

    package char_tree is new d_binarytree(item => Character, traversal => CT.traversal, Image => Character'Image);
    use char_tree;

    -- Variable declarations
    source: File_Type;
    inorder_traversal, preorder_traversal: CT.traversal;
    root: Natural:= 1;	
    t, binary_tree : tree;

    -- Delete blanks
    procedure delete_blanks(line: in String; tr: in out CT.traversal) is
        subtype uppercase is Character Range 'A'..'Z';
    begin
        for char of line loop
            if char in uppercase then
                CT.add(tr, char);
            elsif char /= ' ' then
                raise not_a_proper_character;
            end if;
        end loop;
    end delete_blanks;

     -- Splits the inorder String into two sub arrays based on the root position
    procedure split_inorder(c_root: in Character;  sub_inorder: in CT.traversal; sub_inorder_l, sub_inorder_r: in out CT.traversal) is
        root_idx: Natural;
    begin
        root_idx:= CT.index_of(c_root, sub_inorder);
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

begin

    if Argument_Count > 0 then 
        -- Load traversals from file
        Open(source,Mode => In_File, Name => Argument(1));
        delete_blanks(Get_Line(source), inorder_traversal);
        delete_blanks(Get_Line(source), preorder_traversal);        
        Close(source);
        
        build_binary_tree(binary_tree, inorder_traversal);
        inordre(binary_tree);
    else 
        Put_Line("No file name specified, please introduce one...");
    end if;

exception
    when not_a_proper_character => Put_Line("There is at least one character that doesn match the requirements...");

end is_it_binary;