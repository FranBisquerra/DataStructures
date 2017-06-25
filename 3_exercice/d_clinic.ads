with d_heap, d_mapping, d_hashing, d_binarytree;
with Ada.Containers, Ada.Strings.Unbounded, Ada.Strings.Hash; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

generic
  waiting_room_size: Natural;
  historic_amount: Positive;

package d_clinic is
  type clinic is limited private;

  type visit_reason is (CHECK, CURE, SURGERY, EMERGENCY);
  subtype cycle is Natural;

  -- clinic exceptions: exception;

  -- package types declarations
  package SU renames Ada.Strings.Unbounded;

  procedure init_clinic(c: in out clinic);
  procedure put(name: in SU.Unbounded_String; reason: in visit_reason; current_cycle: in cycle; c: in out clinic);
  procedure advance_cycle(c: in out clinic; current_cycle: in cycle);

  --function retrieve_wait_room_pets(c: in clinic) return SU.Unbounded_String;
  --function retrieve_pet_historic(name: in SU.Unbounded_String) return SU.Unbounded_String;
  --function retrieve_visits_by_type(visit_type: in visit_reason) return SU.Unbounded_String;

private

  -- Random numbers definitions
  subtype probability_range is Integer range 1 .. 100;
  subtype probability is Integer range probability_range'First .. probability_range'Last;
  package RN is new Ada.Numerics.Discrete_Random (probability_range);
  Generator: RN.Generator;
  
  -- Box definitions
  type box_item is 
    record
      is_opened: boolean:= false;
      is_free: boolean:= true;
      name: SU.Unbounded_String;
      left_cicles: cycle;
      reason: visit_reason;
    end record;
  
  -- Waiting room definitions
  type waiting_room_item is
    record
      name: SU.Unbounded_String;
      waiting_cycles: cycle;
      reason: visit_reason;
  end record;

  function wr_bigger (x, y: in waiting_room_item) return Boolean;
  function wr_smaller (x, y: in waiting_room_item) return Boolean;
  function wr_image (x: in waiting_room_item) return String;

  -- Historic definitions
  type historic_item is
    record
      reason: visit_reason;
      start_cycle: cycle; 
  end record;

  -- Waiting room data structure (Inverted comparator to force heap of maximums)
  package WR is new d_heap(waiting_room_size, waiting_room_item, wr_bigger, wr_smaller, wr_image);
  -- History room data structure
  package H is new d_hashing(historic_item, historic_amount);
  -- Boxes data structure
  package BOX is new d_mapping(Natural, box_item, "<");
  -- Helpers sets data structure
  package HS is new d_binarytree(SU.Unbounded_String, Natural, SU."<", SU.">");
  type hs_array is array (visit_reason) of HS.tree;

  subtype opened_boxes_amount is Natural range 1..5;

  type clinic is
    record
    waiting_room: WR.heap;
    boxes: BOX.set;
    opened_boxes: opened_boxes_amount:= 1;
    historic: H.set;
    helper_sets: hs_array;
  end record;

end d_clinic;