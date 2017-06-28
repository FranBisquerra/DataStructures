with Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with d_clinic;
with d_heap;
with pparaula_rd; use pparaula_rd;

procedure veterinary is
  
  -- Clinic package
  package CL is new d_clinic (100, 1000); use CL;

  clinic: CL.clinic;
  current_cycle: CL.cycle:= 0;
  visit_reason: CL.visit_reason;
  random_seed: Integer;
  auto_exec: Boolean;
  key: Character;
  continue: Boolean:= false;
  cycles_to_execute: Integer;

  -- Types and subtypes
  subtype probability_range is Integer range 1 .. 100;
  subtype probability is Integer range probability_range'First .. probability_range'Last;
  P: probability;

  -- Other packages instantiation
  package RN is new Ada.Numerics.Discrete_Random(probability_range);
  package SU renames Ada.Strings.Unbounded;
  G: RN.Generator;
  
  -- Read file
  origin_file: OrigenParaules(f_directe);
  pet: tparaula;
  pets_number: Integer;
  pets_readed: Integer:= 1;

  -- Exceptions
  Invalid_Arguments_Number: exception;

  -- Function and procedures
  function check_arguments(count: in Integer) return Boolean is
  value: Integer;
  begin
    -- Check parameters and return the type execution
    if count = 1 then
        value:= Integer'Value(Argument(1));
        return false;
    elsif count = 2 then
      value:= Integer'Value(Argument(1));
      value:= Integer'Value(Argument(2));
      return true;
    else
      raise Invalid_Arguments_Number;
    end if;
  end check_arguments;

  procedure read_pet is
  begin
      get(origin_file, pet, pets_readed);
      Put(pet);
      pets_readed:= pets_readed + 1;
  end read_pet;

  function generate_random_reason return CL.visit_reason is
  reason: CL.visit_reason;
  begin
    P:= RN.Random(G);
    case P is
      when 1..25 => reason:= CL.visit_reason'(CHECK);
      when 26..50 => reason:= CL.visit_reason'(EMERGENCY);
      when 51..75 => reason:= CL.visit_reason'(CURE);
      when others => reason:= CL.visit_reason'(SURGERY);
    end case;
    return reason;
  end generate_random_reason;

  procedure print_menu is
  begin
    -- Show options
    Put_Line("Press 'c' to continue execution.");
    Put_Line("Press 'w' to show pets in waitting room.");
    Put_Line("Press 'h' to show history pet.");
    Put_Line("Press 'l' to list by visit reason.");
    Put_Line("Press 'e' to finish the program.");
    Put("Select an option: ");
  end print_menu;

  procedure show_waitting_room is
  begin
  Put_Line(SU.To_String(CL.retrieve_wait_room_pets(clinic)));
  Ada.Text_IO.New_Line;
  end show_waitting_room;

  procedure pet_history is
    str_pet_name: String(1..100) := (others => ' ');
    last: Integer;
  begin
    -- Remove intro
    Get_Immediate(key);
    Put("Write pet name: ");
    Get_Line(str_pet_name, last);

    Put_Line(SU.To_String(CL.retrieve_pet_historic(clinic, SU.Trim(SU.To_Unbounded_String(str_pet_name), Both))));
    Ada.Text_IO.New_Line;
  end pet_history;

  procedure list_visit_reason is
    reason: CL.visit_reason;
    str_reason: String(1..100) := (others => ' ');
    str_reason_trim: SU.Unbounded_String;
    last: Integer;
  begin
    -- Remove intro
    Get_Immediate(key);
    Put("Write reason type: ");
    Get_Line(str_reason, last); 
    str_reason_trim:= SU.Trim(SU.To_Unbounded_String(str_reason), Both);

    if SU.To_String(str_reason_trim) = "CHECK" then reason:= CL.visit_reason'(CHECK);
    elsif SU.To_String(str_reason_trim) = "EMERGENCY" then reason:= CL.visit_reason'(EMERGENCY);
    elsif SU.To_String(str_reason_trim) ="CURE" then reason:= CL.visit_reason'(CURE);
    elsif SU.To_String(str_reason_trim) ="SURGERY" then reason:= CL.visit_reason'(SURGERY);
    else reason:= CL.visit_reason'(NONE);
    end if;

    if reason /= CL.visit_reason'(NONE) then
      Put_Line(SU.To_String(CL.retrieve_visits_by_type(clinic, reason)));
    else
      Put_Line("Invalid reason.");
    end if;
    Ada.Text_IO.New_Line;
  end list_visit_reason;

begin

  Put_Line("Running veterinary process...");

  auto_exec:= check_arguments(Argument_Count);
  random_seed:= Integer'Value(Argument(1));
  if auto_exec then
    cycles_to_execute:= Integer'Value(Argument(2));
  end if;

  CL.init_clinic(clinic, random_seed);
  RN.Reset(G, random_seed);

  -- Init read file
  open(origen => origin_file, nom => "pets.data");
  pets_number:= Size(origin_file);
  Put_Line("El nombre de mascotes és " & Size(origin_file)'img);

  if auto_exec then
    -- Automatic mode
    Ada.Text_IO.New_Line;
    Put_Line("+ Automatic mode");
    while current_cycle < cycles_to_execute loop
      -- Read pet
      if pets_readed <= pets_number then
        Ada.Text_IO.New_Line;
        read_pet;
        visit_reason:= generate_random_reason;
        Put_Line(visit_reason'Img);
        -- Put pet
        CL.put(SU.To_Unbounded_String(toString(pet)), visit_reason, current_cycle, clinic);
      end if;
      -- Advance cycle
      CL.advance_cycle(clinic, current_cycle);
      current_cycle:= current_cycle + 1;
    end loop;
  else 
    -- Manual mode
    Ada.Text_IO.New_Line;
    Put_Line("+ Manual mode");
    while true loop
      Ada.Text_IO.New_Line;
      print_menu;
      while not continue loop
        Get_Immediate(key);
        if key /= ASCII.CR and key /= ASCII.LF then
          Ada.Text_IO.New_Line;
          case key is
            when 'C'|'c' => continue:= true;
            when 'W'|'w' => Put_Line("Show waitting room"); show_waitting_room;
            when 'H'|'h' => Put_Line("Show pet history"); pet_history;
            when 'L'|'l' => Put_Line("List visit reason"); list_visit_reason;
            when 'E'|'e' => Put_Line("Execution aborted..."); return;
            when others => Put_Line("Invalid option, please select one option."); Ada.Text_IO.New_Line;
          end case;
          if not continue then print_menu; end if;
        end if;
      end loop;
      -- Read pet
      if pets_readed <= pets_number then
        read_pet;
        -- Put pet
        CL.put(SU.To_Unbounded_String(toString(pet)), visit_reason, current_cycle, clinic);
      end if;
      -- Advance cycle
      CL.advance_cycle(clinic, current_cycle);
      -- Return to main loop
      continue:= false;
    end loop;
  end if;

  exception
    when Invalid_Arguments_Number => Put_Line("Invalid arguments number. " &
      "For 'manual mode' introduce <seed> and " &
      "for 'automatic mode' introduce <seed> <cycles_number>");

end veterinary;