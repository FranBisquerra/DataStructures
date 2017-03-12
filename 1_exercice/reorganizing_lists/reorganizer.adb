with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with dlist;

procedure reorganizer is 

	package integer_list is new dlist (Item => Integer); use integer_list;

	-- Variable declarations
	read_item: Integer;
	number_list: list;
	source: File_Type;
	l_size: Integer:= 0;
	l_item: Integer;
begin 
	-- Load info from file
	Open(source, Mode => In_File, Name => "llista_init");
	while not End_Of_File(source) loop
		Get(source, read_item);
		insert(number_list, read_item);
	end loop;
	Close(source);

	dis_order(number_list, Integer'Value(Argument(1)));

	-- Print list
	l_size:= size(number_list);
	if l_size = 0 then
		Put_Line("No elements in file");
	else
	Put("Disordered list: ");
		for i in 1..l_size loop

			l_item:= get_item(number_list, i);
			Put("" & l_item'Img);
		end loop;
	end if;

end reorganizer;