with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with dstack;

procedure calculator is 

package integer_stack is new dstack (Item => Integer); use integer_stack;

-- Operators
SUM: constant Character:= '+'; 
SUB: constant Character:= '-';
DIV: constant Character:= '/';
MUL: constant Character:= 'x';
POW: constant Character:= '^';

-- Exceptions
not_valid_operator: exception;

-- Variable declarations
operators: constant array (1..5) of Character:= (SUM, SUB, DIV, MUL, POW);
number_stack: stack;
first_operand: Integer;
second_operand: Integer;
result: Integer:= 0;

-- Checks if the character is a valid operator
function is_operator(operator: in Character) return boolean is
begin
	for i in 1..operators'last loop
		if operator = operators(i) then return true; end if;
	end loop;
	return false;
end is_operator;

-- Calculus
function operate(first_operand: in Integer; second_operand: in Integer; operator: in Character) return Integer is
begin
	case operator is
		when SUM => return first_operand + second_operand;
		when SUB => return first_operand - second_operand;
		when DIV => return first_operand / second_operand;
		when MUL => return first_operand * second_operand;
		when POW => return first_operand ** second_operand;
		when others => raise not_valid_operator;
	end case;
end operate;

begin
	empty(number_stack);

	for arg in 1..Argument_Count loop
		if is_operator(Argument(arg)(1)) then
			second_operand:= top_pop(number_stack);
			first_operand:=  top_pop(number_stack);
			result:= operate(first_operand, second_operand, Argument(arg)(1));
			push(number_stack, result);
		else
			push(number_stack, Integer'Value(Argument(arg)));
		end if;
	end loop; 

	Put_Line("The result is " & Integer'Image(result));

exception
	when bad_use => 
		Put_Line("The introduced operation is not a valid one, please check and try again.");
	when Constraint_Error =>
	 	Put_Line("One of the operators is not a valid one, remember that the available operations are: +, -, *, /, ^");
	when space_overflow =>
	 	Put_Line("The program stack ran out of memory...");
end calculator;