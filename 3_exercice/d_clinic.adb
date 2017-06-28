package body d_clinic is 

  -- Begin waiting room item functions
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
  -- End waiting room item functions

  -- Begin helper functions
  function calculate_wait_cycles(reason: in visit_reason) return cycle is
    initial_cycle_amount: cycle;
  begin
    case reason is
      when visit_reason'(CHECK) => initial_cycle_amount:= 0;
      when visit_reason'(CURE) => initial_cycle_amount:= 1;
      when visit_reason'(SURGERY) => initial_cycle_amount:= 2;
      when visit_reason'(EMERGENCY) => initial_cycle_amount:= 3;
      when visit_reason'(NONE) => raise NO_VISIT_REASON;
    end case; 
    return initial_cycle_amount;
  end calculate_wait_cycles;

  function calculate_box_cycles(reason: in visit_reason) return cycle is
    initial_cycle_amount: cycle;
  begin
    case reason is
      when visit_reason'(CHECK) => initial_cycle_amount:= 3;
      when visit_reason'(CURE) => initial_cycle_amount:= 5;
      when visit_reason'(SURGERY) => initial_cycle_amount:= 8;
      when visit_reason'(EMERGENCY) => initial_cycle_amount:= 4;
      when visit_reason'(NONE) => raise NO_VISIT_REASON;
    end case; 
    return initial_cycle_amount;
  end calculate_box_cycles;

  function not_registered(c: in clinic; name: in SU.Unbounded_String) return Boolean is
    retrieved: HL.set;
  begin
    H.get(c.historic, name, retrieved);
    return true;
  exception
    when H.DOES_NOT_EXIST => return true;
  end not_registered;
  -- End helper functions

  -- Begin Printing functions
  function box_to_string(box_to_print: in box_item) return String is
  begin 
    return "is_opened: " & box_to_print.is_opened'Img & 
            ", is_free: " & box_to_print.is_free'Img &
            ", name: " & SU.To_String(box_to_print.name) & 
            ", left_cycles: " & box_to_print.left_cycles'Img &
            ", reason: " & box_to_print.reason'Img;
  end box_to_string;

  function wr_item_to_string(printable: waiting_room_item) return String is
    begin
      return "name: " & SU.To_String(printable.name) & 
             ", waited_cycles: " & printable.waiting_cycles'Img &
             ", reason: " & printable.reason'Img;
  end wr_item_to_string;
  --End printing functions

  procedure init_clinic(c: in out clinic; random_seed: in Integer) is
    support_box_item, printable: box_item;
    box_amount: Natural:= 5;
    counter: Natural:= 1;
    it: BOX.iterator;
    printable_key: Natural;
    helper_tree: HS.tree;
  begin

    -- Reset random generator
    RN.Reset(Generator, random_seed);
    
    -- Initializing boxes
    while counter < box_amount loop
      BOX.put(c.boxes, counter, support_box_item);      
      counter:= counter + 1;
    end loop;
    support_box_item.is_opened:= true;
    BOX.put(c.boxes, counter, support_box_item);
    
    -- Initializing helper sets
    HS.empty(helper_tree);
    for v_reason in visit_reason'Range loop
      if v_reason /= visit_reason'(NONE) then
        c.helper_sets(v_reason):= helper_tree;
      end if;  
    end loop;

    -- TO DELETE
    Put_Line("-----------------------------------------------");
    BOX.first(c.boxes, it);
    while BOX.is_valid(it) loop
      BOX.get(c.boxes, it, printable_key, printable);
      Put_Line("box: " & printable_key'Img & ", " & box_to_string(printable));
      BOX.next(c.boxes, it);
    end loop;  
    Put_Line("-----------------------------------------------"); 
    Put_Line("");   
  end init_clinic;

  -- Add 1 cycle to all items in waiting room
  procedure manage_waiting_room(waiting_room: in out WR.heap; current_cycle: in cycle) is
    support_waiting_room: WR.heap;
    support_item: waiting_room_item;
  begin
    WR.empty(support_waiting_room);

    while WR.is_empty(waiting_room) /= true loop
      support_item:= WR.get_least(waiting_room);
      WR.delete_least(waiting_room);

      Put_Line("Before update current_cycle("& current_cycle'Img &"): " & " " & wr_item_to_string(support_item));

      support_item.waiting_cycles:= support_item.waiting_cycles + 1;

      Put_Line("After update current_cycle("& current_cycle'Img &"): " & " " & wr_item_to_string(support_item));
                
      Put_Line("-----------------------------------------------");

      WR.put(support_waiting_room, support_item);
    end loop;

    waiting_room:= support_waiting_room;
  end manage_waiting_room;

  function can_close_box(c: in clinic) return Boolean is
    prob: probability;
    can_close: Boolean:= false;
  begin
    if c.opened_boxes > 1 then
      prob:= RN.Random(Generator);
      if prob <= 10 then 
        can_close:= true;
      end if;
    end if;
    return can_close;
  end can_close_box;

  procedure close_box (c: in out clinic; box_to_close: in out box_item) is
  begin
    box_to_close.is_opened:= false;
    box_to_close.is_free:= true;
    c.opened_boxes:= c.opened_boxes - 1;
  end close_box;

  function can_open_box(c: in clinic) return Boolean is
    prob: probability;
    can_open: Boolean:= false;
  begin
    if c.opened_boxes < 5 then
      prob:= RN.Random(Generator);
      if prob <= 10 then 
        can_open:= true;
      end if;
    end if;
    return can_open;
  end can_open_box;

  procedure open_available_box(c: in out clinic) is
    box_to_open: box_item;
    it: BOX.iterator;
    box_to_open_key: Natural;
    already_opened: Boolean:= false;
  begin 
    BOX.first(c.boxes, it);
    while BOX.is_valid(it) and then already_opened = false loop
      BOX.get(c.boxes, it, box_to_open_key, box_to_open);

      if box_to_open.is_opened = false then
        Put_Line("Opening box: " & box_to_open_key'Img & ", " & box_to_string(box_to_open));
        box_to_open.is_opened:= true;
        BOX.update(c.boxes, box_to_open_key, box_to_open);
        already_opened:= true;
        c.opened_boxes:= c.opened_boxes + 1;
      end if;

      BOX.next(c.boxes, it);
    end loop;  
  end open_available_box;

  procedure enter_box(c: in out clinic; wr_item: in waiting_room_item) is
    box_to_enter: box_item;
    it: BOX.iterator;
    box_to_enter_key: Natural;
    entered: Boolean:= false;
  begin 

    BOX.first(c.boxes, it);
    while BOX.is_valid(it) and then entered = false loop
      BOX.get(c.boxes, it, box_to_enter_key, box_to_enter);

      if box_to_enter.is_opened and then box_to_enter.is_free then
        box_to_enter:= (true, false, wr_item.name, calculate_box_cycles(wr_item.reason), wr_item.reason);
        entered:= true;
        
        Put_Line("/++++++++++++++++++++++++++++++++++++++++++++++++/");
        Put_Line("Box_to_enter: " & box_to_enter_key'Img & ", " & box_to_string(box_to_enter));
        Put_Line("/++++++++++++++++++++++++++++++++++++++++++++++++/");
        Put_Line(" ");

        BOX.update(c.boxes, box_to_enter_key, box_to_enter);
      end if;

      BOX.next(c.boxes, it);
    end loop;  
  end enter_box;

  function are_free_boxes(c: in clinic) return Boolean is
    free_box: box_item;
    it: BOX.iterator;
    free_box_key: Natural;
    found: Boolean:= false;
  begin 
    BOX.first(c.boxes, it);
    while BOX.is_valid(it) and then found = false loop
      BOX.get(c.boxes, it, free_box_key, free_box);

      if free_box.is_opened and then free_box.is_free then
        found:= true;
      end if;

      BOX.next(c.boxes, it);
    end loop; 
    return found;
  end are_free_boxes;

  procedure manage_boxes(c: in out clinic; current_cycle: in cycle) is
    box_to_update: box_item;
    it: BOX.iterator;
    box_to_update_key: Natural;
    update_box: Boolean:= false;
  begin
    -- Substract a cycle to occupied boxes
    BOX.first(c.boxes, it);
      while BOX.is_valid(it) loop
        BOX.get(c.boxes, it, box_to_update_key, box_to_update);

        Put_Line("Im at box: " & box_to_update_key'Img & ", " & box_to_string(box_to_update));

        if box_to_update.is_opened = true then

          Put_Line("+ Box " & box_to_update_key'Img &" is opened");

          if box_to_update.is_free = false then 
            update_box:= true;
            Put_Line("+ Box " & box_to_update_key'Img &" is opened and not free");

            box_to_update.left_cycles:= box_to_update.left_cycles - 1;

            if box_to_update.left_cycles = 0 then
              Put_Line("+ Box " & box_to_update_key'Img &" has no cycles left");
              -- begin save historic
              Put_Line("***** Proceed to save historic at cycle: "&current_cycle'Img&" *****");

              -- end save historic  
              if can_close_box(c) = true then
                Put_Line("+ Box " & box_to_update_key'Img &" can be closed");
                close_box(c, box_to_update);
                Put_Line("+ Box " & box_to_update_key'Img &" closed");
              else
                Put_Line("+ Box " & box_to_update_key'Img &" can not be closed");
                box_to_update.is_free:= true;
                Put_Line("+ Box " & box_to_update_key'Img &" freed");
              end if;
            end if;
          else 
            Put_Line("+ Box " & box_to_update_key'Img &" is opened and free");
            if can_close_box(c) = true then
              update_box:= true;
              Put_Line("+ Box " & box_to_update_key'Img &" can be closed");
              close_box(c, box_to_update);
              Put_Line("+ Box " & box_to_update_key'Img &" closed");
            end if;
          end if;

          if update_box then 
            BOX.update(c.boxes, box_to_update_key, box_to_update);
          end if;

        end if;
        update_box:= false;
        BOX.next(c.boxes, it);
      end loop;  

      Put_Line("    ");
    -- Create boxes if posible
    if can_open_box(c) = true then
      open_available_box(c);
    end if;
    end manage_boxes;

  procedure put(name: in SU.Unbounded_String; reason: in visit_reason; current_cycle: in cycle; c: in out clinic) is
    WR_pet: waiting_room_item;
    waiting_room: WR.heap renames c.waiting_room;
    HL_pet: HL.set;
  begin
    -- register it if it doesnt exist
    --if not_registered(c, name) then
      --H.put(c.historic, name, HL_pet); 
    --end if;
    -- pass to the waiting room
    WR_pet:= (name, calculate_wait_cycles(reason), reason);
    WR.put(waiting_room, WR_pet);
  end put;

  procedure advance_cycle(c: in out clinic; current_cycle: in cycle) is
    pet_from_wr: waiting_room_item;
    are_animals_left: Boolean:= true;
  begin 

    manage_waiting_room(c.waiting_room, current_cycle);    
    manage_boxes(c, current_cycle);
    
    if are_animals_left and then are_free_boxes(c) then
      if WR.is_empty(c.waiting_room) then 
        are_animals_left:= false;
      else
        pet_from_wr:= WR.get_least(c.waiting_room);
        WR.delete_least(c.waiting_room);
        enter_box(c, pet_from_wr);
      end if;
    end if;
  end advance_cycle;

end d_clinic;