with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with d_binarytree, d_traversal;

procedure has_path_sum is

    -- Exceptions
    not_a_proper_number: exception;
    not_proper_inorder_traveral: exception;

    package IT is new d_traversal(item => Integer, Max => 100, Image => Integer'Image);
    
    package integer_tree is new d_binarytree(item => Integer, Image => Integer'Image, 
                                            Succ => Integer'Succ, Pred => Integer'Pred, Trav => IT);
    use integer_tree;

    -- Variable declarations
    source, output: File_Type;
    inorder_traversal, preorder_traversal: IT.traversal;
    root: Natural:= 1;	
    t, binary_tree : tree;
    read_item: Integer;    

     -- Splits the inorder String into two sub arrays based on the root position
    procedure split_inorder(i_root: in Integer;  sub_inorder: in IT.traversal; sub_inorder_l, sub_inorder_r: in out IT.traversal) is
        root_idx: Natural;
    begin
        root_idx:= IT.index_of(i_root, sub_inorder);
        if root_idx = 0 then raise not_proper_inorder_traveral; end if;
        IT.slice(sub_inorder, sub_inorder_l, 1, root_idx - 1);
        IT.slice(sub_inorder, sub_inorder_r, root_idx + 1, IT.Length(sub_inorder));
    end split_inorder;

    -- Builds the binary tree based on the inorder and preorder traversals
    procedure build_binary_tree(r: in out tree; sub_inorder: in IT.traversal) is 
        sub_inorder_l: IT.traversal;
        sub_inorder_r: IT.traversal;
        t, lt, rt: tree;
        i_root: Integer;
    begin
        i_root:= IT.get(preorder_traversal, root);
        if IT.length(sub_inorder) > 1 then 
            split_inorder(i_root, sub_inorder, sub_inorder_l, sub_inorder_r);
            if IT.length(sub_inorder_l) >= 1 then
                root:= root + 1;
                build_binary_tree(lt, sub_inorder_l);
            end if;
            if IT.length(sub_inorder_r) >= 1 then
                root:= root + 1;
                build_binary_tree(rt, sub_inorder_r);
            end if;
            Put("tree: "&i_root'Img &" -> ");
            graft(r, lt, rt, i_root);
        -- is leaf
        elsif IT.length(sub_inorder) = 1 then 
            Put("leaf: "&i_root'Img &" -> ");        
            graft(r, lt, rt, i_root);
        end if;
    end build_binary_tree;

    count: Integer;
begin

    if Argument_Count > 0 then 
        -- Load traversals from file
        Open(File => source,Mode => In_File, Name => Argument(1));
        Set_line(source, 1);
        While not End_Of_Line(source) loop
            Get(source, read_item);
            IT.add(inorder_traversal, read_item);
        end loop;
        Set_line(source, 2);
        While not End_Of_Line(source) loop
            Get(source, read_item);
            IT.add(preorder_traversal, read_item);
        end loop;
        Close(source);
        
        build_binary_tree(binary_tree, inorder_traversal);
        Put_Line("");
        inordre(binary_tree);
        Put_Line("");

        Open(File => output, Mode => Out_File, Name => "resultats.txt");
        Close(output);
    else 
        Put_Line("No file name specified, please introduce one...");
    end if;

exception
    when not_a_proper_number => Put_Line("There is at least one number that doesn match the requirements...");
    when not_proper_inorder_traveral => 
        Put_Line("Something is wrong with the traversals, it is not possible to build the tree. Please check and try again");
        Open(File => output, Mode => Out_File, Name => "resultats.txt");
            Put(output, '0');
            Set_Line(output, 2);
            Put(output, '0');        
            Close(output);
end has_path_sum;