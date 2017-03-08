with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure word_count is

	subtype letter is Character range 'a'..'z';
	subtype capital_letter is Character range 'A'..'Z';

	Centinell : constant Character := '.';

	counter: Natural := 0;
	char: Character := ' ';
	last_char: Character := ' ';
	done: Boolean := False;

begin 

	put_line("Introduce a sentence, we will count the amount of words in it for you ;).");

	while not done loop	
		
		get_immediate(char);
		put(char);

		if char = Centinell then 
			done := True;
			if last_char in letter or last_char in capital_letter then 
				counter := counter + 1;
			end if;
		elsif char not in letter or char not in capital_letter then
			if last_char in letter or last_char in capital_letter then 
				counter := counter + 1;
			end if;
		end if;

		last_char := char;

	end loop;

	put_line("The amount of words is " & Integer'Image(counter));
end word_count;