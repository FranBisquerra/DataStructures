with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded;
--with Ada.Containers;

--with d_heap, d_mapping, d_hashing, d_binarytree, d_stack;

procedure veterinary is
  
  -- Types and subtypes
  subtype probability_range is Integer range 1 .. 100;
  subtype probability is Integer range probability_range'First .. probability_range'Last;
  type visit_reason is (CHECK, CURE, SURGERY, EMERGENCY);

  -- Package instantiations
  package RN is new Ada.Numerics.Discrete_Random (probability_range);
  package SU renames Ada.Strings.Unbounded;
  --package HS renames Ada.Containers;

  -- Random number variables
  G: RN.Generator;
  prob: probability;

  -- Hashing tests
  pet_name: SU.Unbounded_String:= SU.To_Unbounded_String("Boss");
  --pet_hash: natural;

  -- Items definitions
  type box is 
    record
      is_opened: boolean:= false;
      is_free: boolean:= true;
      pet_name: SU.Unbounded_String;
      pet_left_cicles: natural;
      pet_reason: visit_reason;
  end record;

  type support_list_item is
    record
      pet_name: SU.Unbounded_String;
      visits_amount: positive;
  end support_list_item;

begin
  RN.Reset(G);
  Put_Line("Main program");
  
  -- inicialitzar consultes.



end veterinary;