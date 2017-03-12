with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with dstack;

procedure calculator is 

type TOperator is ('+', '-', 'x', '/');

begin

	for i in 1..Argument_Count loop

		Put_Line(Argument(i));

	end loop; 

end calculator;