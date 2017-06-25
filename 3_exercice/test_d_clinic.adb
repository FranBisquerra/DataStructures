with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with d_clinic;

procedure test_d_clinic is

  package SU renames Ada.Strings.Unbounded;

  package CL is new d_clinic (100, 5, 1000);
  use CL;

  clinic: CL.clinic;
  pet_name: SU.Unbounded_String;
  reason: CL.visit_reason;
  current_cycle: CL.cycle;

begin
 pet_name:= SU.To_Unbounded_String("Pep");
 reason:= CL.visit_reason'(CHECK);
 current_cycle:= 1;

 CL.put(pet_name, reason, current_cycle, clinic);

 Put(" ");



end test_d_clinic;