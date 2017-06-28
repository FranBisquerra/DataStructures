with Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded;
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
  Invalid_Arguments_Number, Not_A_Number: exception;

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
begin

  Put_Line("Running veterinary process...");
  New_Line;

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
    New_Line;
    Put_Line("+ Automatic mode");
    while current_cycle < cycles_to_execute loop
      New_Line;
      -- Read pet
      if pets_readed <= pets_number then
        read_pet;
      end if;
      visit_reason:= generate_random_reason;
      Put_Line(visit_reason'Img);
      -- Put pet
      -- Advance cycle
      current_cycle:= current_cycle + 1;
    end loop;
  else 
    -- Manual mode
    New_Line;
    Put_Line("+ Manual mode");
    while true loop
      New_Line;
      print_menu;
      while not continue loop
        Get_Immediate(key);
        if key /= ASCII.CR and key /= ASCII.LF then
          New_Line;
          case key is
            when 'C'|'c' => continue:= true;
            when 'W'|'w' => Put_Line("Show waitting room"); --show_waitting_room;
            when 'H'|'h' => Put_Line("Show pet history"); --pet_history;
            when 'L'|'l' => Put_Line("List visit reason"); --list_visit_reason;
            when 'E'|'e' => Put_Line("Execution aborted..."); return;
            when others => Put_Line("Invalid option, please select one option."); New_Line;
          end case;
          if not continue then print_menu; end if;
        end if;
      end loop;
      -- Read pet
      if pets_readed <= pets_number then
        read_pet;
      end if;
      -- Put pet
      -- Advance cycle
      -- Return to main loop
      continue:= false;
    end loop;
  end if;

  exception
    when Not_A_Number => Put_Line("The input parameter/s must be numeric.");
    when Invalid_Arguments_Number => Put_Line("Invalid arguments number. " &
      "For 'manual mode' introduce <seed> and " &
      "for 'automatic mode' introduce <seed> <cycles_number>");

end veterinary;