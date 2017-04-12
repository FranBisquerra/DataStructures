with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with d_binarytree, d_traversal;

procedure is_it_binary is

    -- Exceptions
    not_a_proper_character: exception;
    not_proper_inorder_traveral: exception;

    package CT is new d_traversal(item => Character, Max => 100, Image => Character'image);
    
    package char_tree is new d_binarytree(item => Character, Image => Character'Image, 
                                            Succ => Character'Succ, Pred => Character'Pred, Trav => CT);
    use char_tree;

    -- Variable declarations
    source, output: File_Type;
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
        if root_idx = 0 then raise not_proper_inorder_traveral; end if;
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
            Put("tree: "&c_root &" -> ");
            graft(r, lt, rt, c_root);
        -- is leaf
        elsif CT.length(sub_inorder) = 1 then 
            Put("leaf: "&c_root &" -> ");        
            graft(r, lt, rt, c_root);
        end if;
    end build_binary_tree;

begin

    if Argument_Count > 0 then 
        -- Load traversals from file
        Open(File => source,Mode => In_File, Name => Argument(1));
        delete_blanks(Get_Line(source), inorder_traversal);
        delete_blanks(Get_Line(source), preorder_traversal);        
        Close(source);
        
        build_binary_tree(binary_tree, inorder_traversal);
        Put_Line("");
        inordre(binary_tree);
        Put_Line("");

        Open(File => output, Mode => Out_File, Name => "resultats.txt");
        if is_right_tree(binary_tree, inorder_traversal) then 
            Put(output, '1');
            Put_Line("is OK");        
        else
            Put(output, '0');        
            Put_Line("is not OK");        
        end if;
            Set_Line(output, 2);
        if is_bst(binary_tree) then 
            Put(output, '1');
            Put_Line("is BST");        
        else
            Put(output, '0');        
            Put_Line("is not BST");        
        end if;

        Close(output);
    else 
        Put_Line("No file name specified, please introduce one...");
    end if;

exception
    when not_a_proper_character => Put_Line("There is at least one character that doesn match the requirements...");
    when not_proper_inorder_traveral => 
        Put_Line("Something is wrong with the traversals, it is not possible to build the tree. Please check and try again");
        Open(File => output, Mode => Out_File, Name => "resultats.txt");
            Put(output, '0');
            Set_Line(output, 2);
            Put(output, '0');        
            Close(output);
end is_it_binary;