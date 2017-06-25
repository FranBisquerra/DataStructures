package body d_clinic is 

  -- Waiting room item functions
  function wr_bigger (x, y: in waiting_room_item) return Boolean is
  begin
    return x.waiting_cycles > y.waiting_cycles;
  end wr_bigger;

  function wr_smaller (x, y: in waiting_room_item) return Boolean is
  begin
    return x.waiting_cycles < y.waiting_cycles;
  end wr_smaller;

  function wr_image (x: in waiting_room_item) return String is
  begin 
    return SU.To_String(x.name);
  end wr_image;

  -- Helper functions
  function calculate_wait_cycles(reason: in visit_reason) return cycle is
    initial_cycle_amount: cycle;
  begin
    case reason is
      when visit_reason'(CHECK) => initial_cycle_amount:= 0;
      when visit_reason'(CURE) => initial_cycle_amount:= 1;
      when visit_reason'(SURGERY) => initial_cycle_amount:= 2;
      when visit_reason'(EMERGENCY) => initial_cycle_amount:= 3;
    end case; 
    return initial_cycle_amount;
  end calculate_wait_cycles;

  procedure put(name: in SU.Unbounded_String; reason: in visit_reason; cycle_in: in cycle; c: in out clinic) is
    WR_pet: waiting_room_item;
    waiting_room: WR.heap renames c.waiting_room;
    current_cycle: cycle renames c.current_cycle;
  begin
    WR_pet.name:= name;
    WR_pet.reason:= reason;
    WR_pet.waiting_cycles:= calculate_wait_cycles(reason);

    WR.put(waiting_room, WR_pet);

    --Put_Line("name: " & SU.To_String(WR_after_pet.name) & 
    --          ", cycle: " & WR_after_pet.waiting_cycles'Img &
    --          ", reason: " & WR_after_pet.reason'Img);
  end put;

end d_clinic;