with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with d_clinic;

procedure test_d_clinic is

  package SU renames Ada.Strings.Unbounded;

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

  animals_amount: Natural:= 4;
  type pets_array is array (Natural range 1..animals_amount) of pet;
  pets: pets_array;
  pet_idx: Natural:= 1;
begin

  random_seed:= 2;
  CL.init_clinic(clinic, random_seed);

  -- Mock pets
  pets(1):= (SU.To_Unbounded_String("Pep"), CL.visit_reason'(CHECK));
  pets(2):= (SU.To_Unbounded_String("Maria"), CL.visit_reason'(EMERGENCY));
  pets(3):= (SU.To_Unbounded_String("Lluc"), CL.visit_reason'(CURE));
  pets(4):= (SU.To_Unbounded_String("Xesc"), CL.visit_reason'(SURGERY));

  while test_cycles <= cycles_amount loop

    current_cycle:= test_cycles;

    if pet_idx <= animals_amount then
      Put_Line("*** Put new pet ***");
      CL.put(pets(pet_idx).name, pets(pet_idx).reason, current_cycle, clinic);
      Put_Line("*** Done put new pet ***");

      pet_idx:= pet_idx + 1;
    end if;

    Put_Line("*** Advance one cycle ***");
    Put_Line("       ");
    CL.advance_cycle(clinic, current_cycle);
    Put_Line("*** Done advance one cycle ***");
    Put_Line("       ");

    test_cycles:= test_cycles + 1;
  end loop; 
end test_d_clinic;