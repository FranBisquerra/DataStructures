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

  animals_amount: Natural:= 4;

  type names_arr is array (Natural range 1..animals_amount) of SU.Unbounded_String;

  names: names_arr;

  test_cycles: Natural:= 0;

  pet_name: SU.Unbounded_String;
  reason: CL.visit_reason;
  current_cycle: CL.cycle:= 0;

begin

  CL.init_clinic(clinic);

  -- Mock names
  names(1):= SU.To_Unbounded_String("Pep");
  names(2):= SU.To_Unbounded_String("Maria");
  names(3):= SU.To_Unbounded_String("Lluc");
  names(4):= SU.To_Unbounded_String("Xesc");

  while test_cycles < animals_amount loop

    pet_name:= names(test_cycles + 1);
    reason:= CL.visit_reason'(CHECK);
    current_cycle:= test_cycles;

    Put_Line("*** Put new pet ***");
    CL.put(pet_name, reason, current_cycle, clinic);
    Put_Line("*** Done put new pet ***");
    Put_Line("*** Advance one cycle ***");
    CL.advance_cycle(clinic, current_cycle);
    Put_Line("*** Done advance one cycle ***");
    Put_Line("       ");
    test_cycles:= test_cycles + 1;
  end loop;

end test_d_clinic;