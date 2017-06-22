with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded;

with d_heap; use heap;
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
  Generator: RN.Generator;
  prob: probability;

  -- Hashing tests
  pet_name: SU.Unbounded_String:= SU.To_Unbounded_String("Boss");
  --pet_hash: natural;

  -- Items definitions
  type history is
    record
      reason: visit_reason;
      start_cycle: Integer; 
  end record;

  type queue is
    record
      pet_name: SU.Unbounded_String;
      waitting_cycles: Natural;
      reason: visit_reason;
  end record;

  type box is 
    record
      is_opened: boolean:= false;
      is_free: boolean:= true;
      pet_name: SU.Unbounded_String;
      left_cicles: natural;
      reason: visit_reason;
    end record;

  type visit is
    record
      amount: Natural;
  end record;

  -- Structures definitions
  histories:;  
  waitting_room: heap;
  boxes:;
  visits_history:;

  -- Environment initalization 
  probability_new_box: constant:= 10;
  probability_delete_box: constant:= 10; 

  box_number: Integer:= 1;
  cycle: Integer:= 0;

begin
  RN.Reset(G);
  Put_Line("Main program");

  -- Initialize boxes
  -- Read pet

  -- There are free boxes?





end veterinary;