with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with d_clinic;

procedure test_d_clinic is

  package SU renames Ada.Strings.Unbounded;

  NEW_LINE : constant Character := ASCII.LF;

  package CL is new d_clinic (100, 1000);
  use CL;

  clinic: CL.clinic;

  cycles_amount: Natural:= 25;
  test_cycles: Natural:= 0;
  current_cycle: CL.cycle:= 0;
  random_seed: Integer;

  type pet is
    record
      name: SU.Unbounded_String;
      reason: CL.visit_reason;
  end record;

  animals_amount: Natural:= 10;
  type pets_array is array (Natural range 1..animals_amount) of pet;
  pets: pets_array;
  pet_idx: Natural:= 1;

  wr_string: SU.Unbounded_String:= SU.To_Unbounded_String("");
  ph_string: SU.Unbounded_String:= SU.To_Unbounded_String("");
  hs_string: SU.Unbounded_String:= SU.To_Unbounded_String("");
begin

  random_seed:= 2;
  CL.init_clinic(clinic, random_seed);

  -- Mock pets
  pets(1):= (SU.To_Unbounded_String("Pep"), CL.visit_reason'(CHECK));
  pets(2):= (SU.To_Unbounded_String("Maria"), CL.visit_reason'(CHECK));
  pets(3):= (SU.To_Unbounded_String("Lluc"), CL.visit_reason'(CURE));
  pets(4):= (SU.To_Unbounded_String("Xesc"), CL.visit_reason'(SURGERY));
  pets(5):= (SU.To_Unbounded_String("Biel"), CL.visit_reason'(CHECK));
  pets(6):= (SU.To_Unbounded_String("Mateu"), CL.visit_reason'(CURE));
  pets(7):= (SU.To_Unbounded_String("Tomeu"), CL.visit_reason'(EMERGENCY));
  pets(8):= (SU.To_Unbounded_String("Ramon"), CL.visit_reason'(SURGERY));
  pets(9):= (SU.To_Unbounded_String("Paquita"), CL.visit_reason'(EMERGENCY));
  pets(10):= (SU.To_Unbounded_String("Maria"), CL.visit_reason'(CURE));

  ph_string:= CL.retrieve_pet_historic(clinic, SU.To_Unbounded_String("Lluc"));
  Put_Line("****H0 => " & SU.To_String(ph_string));

  while test_cycles <= cycles_amount loop

    current_cycle:= test_cycles;

    if pet_idx <= animals_amount then
      Put_Line("*** Put new pet ***");
      CL.put(pets(pet_idx).name, pets(pet_idx).reason, current_cycle, clinic);
      ph_string:= CL.retrieve_pet_historic(clinic, SU.To_Unbounded_String("Pep"));
      Put_Line("****H1 => " & SU.To_String(ph_string));
      Put_Line("*** Done put new pet ***");

      pet_idx:= pet_idx + 1;
    end if;

    Put_Line("*** Advance one cycle ***");
    Put_Line("       ");
    CL.advance_cycle(clinic, current_cycle);
    Put_Line("*** Done advance one cycle ***");
    Put_Line("       ");

    test_cycles:= test_cycles + 1;

    wr_string:= CL.retrieve_wait_room_pets(clinic);
    Put_Line("In while => " & SU.To_String(wr_string));

  end loop;

    ph_string:= CL.retrieve_pet_historic(clinic, SU.To_Unbounded_String("Maria"));
    Put_Line("****H2 => " & SU.To_String(ph_string));

    ph_string:= CL.retrieve_pet_historic(clinic, SU.To_Unbounded_String("Tomeu"));
    Put_Line("****H3 => " & SU.To_String(ph_string));

    hs_string:= retrieve_visits_by_type(clinic, CL.visit_reason'(SURGERY));
    Put_Line("****VR1 => " & SU.To_String(hs_string));

    hs_string:= retrieve_visits_by_type(clinic, CL.visit_reason'(CHECK));
    Put_Line("****VR2 => " & SU.To_String(hs_string));

    hs_string:= retrieve_visits_by_type(clinic, CL.visit_reason'(NONE));
    Put_Line("****VR3 => " & SU.To_String(hs_string));
end test_d_clinic;