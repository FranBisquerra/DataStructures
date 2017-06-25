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
    end case; 
    return initial_cycle_amount;
  end calculate_wait_cycles;
  -- End helper functions

  procedure init_clinic(c: in out clinic) is
    support_box_item, printable: box_item;
    box_amount: Natural:= 5;
    counter: Natural:= 1;
    it: BOX.iterator;
    key: Natural;
    helper_tree: HS.tree;
  begin
    -- Initializing boxes
    while counter < box_amount loop
      BOX.put(c.boxes, counter, support_box_item);      
      counter:= counter + 1;
    end loop;
    support_box_item.is_opened:= true;
    BOX.put(c.boxes, 0, support_box_item);
    
    -- Initializing helper sets
    HS.empty(helper_tree);
    for v_reason in visit_reason'Range loop
      c.helper_sets(v_reason):= helper_tree;
    end loop;

    -- TO DELETE
    Put_Line("-----------------------------------------------");
    BOX.first(c.boxes, it);
    while BOX.is_valid(it) loop
      BOX.get(c.boxes, it, key, printable);
      Put_Line("Boxes at init: " &
              "is_opened: " & printable.is_opened'Img & 
              ", is_free: " & printable.is_free'Img &
              ", name: " & SU.To_String(printable.name) & 
              ", left_cicles: " & printable.left_cicles'Img);
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

      Put_Line("Before update cycle("& current_cycle'Img &"): " &
                "name: " & SU.To_String(support_item.name) & 
                ", cycle: " & support_item.waiting_cycles'Img &
                ", reason: " & support_item.reason'Img);

      support_item.waiting_cycles:= support_item.waiting_cycles + 1;

      Put_Line("After update cycle("& current_cycle'Img &"): " &
                "name: " & SU.To_String(support_item.name) & 
                ", cycle: " & support_item.waiting_cycles'Img &
                ", reason: " & support_item.reason'Img);
            
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
      RN.Reset(Generator);
      prob:= RN.Random(Generator);
      if prob <= 10 then 
        can_close:= true;
      end if;
    end if;
    return can_close;
  end can_close_box;

  function can_open_box(c: in clinic) return Boolean is
    prob: probability;
    can_open: Boolean:= false;
  begin
    if c.opened_boxes < 5 then
      RN.Reset(Generator);
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
        box_to_open.is_opened:= true;
        BOX.update(c.boxes, box_to_open_key, box_to_open);
        already_opened:= true;
        c.opened_boxes:= c.opened_boxes + 1;
      end if;

      BOX.next(c.boxes, it);
    end loop;  
  end open_available_box;

  procedure manage_boxes(c: in out clinic; current_cycle: in cycle) is
    box_to_update: box_item;
    it: BOX.iterator;
    box_to_update_key: Natural;
  begin
    -- Substract a cycle to occupied boxes
    BOX.first(c.boxes, it);
      while BOX.is_valid(it) loop
        BOX.get(c.boxes, it, box_to_update_key, box_to_update);
        Put_Line("Im here");
        if box_to_update.is_opened = true then
        Put_Line("Im here inside opened");
          if box_to_update.is_free = false then 
            Put_Line("Im here inside free");
            box_to_update.left_cicles:= box_to_update.left_cicles - 1;

            if box_to_update.left_cicles = 0 then
              -- save historic
              if can_close_box(c) = true then
                box_to_update.is_opened:= false;
                c.opened_boxes:= c.opened_boxes - 1;
              else
                box_to_update.is_free:= true;
              end if;
            end if;
          else 
            if can_close_box(c) = true then
              box_to_update.is_opened:= false;
              c.opened_boxes:= c.opened_boxes - 1;
            end if;
          end if;

          BOX.update(c.boxes, box_to_update_key, box_to_update);
        end if;
        
       
        BOX.next(c.boxes, it);
      end loop;  

    -- Create boxes if posible
    if can_open_box(c) = true then
      open_available_box(c);
    end if;
    end manage_boxes;

  procedure put(name: in SU.Unbounded_String; reason: in visit_reason; current_cycle: in cycle; c: in out clinic) is
    WR_pet: waiting_room_item;
    waiting_room: WR.heap renames c.waiting_room;
  begin
    WR_pet.name:= name;
    WR_pet.reason:= reason;
    WR_pet.waiting_cycles:= calculate_wait_cycles(reason);

    WR.put(waiting_room, WR_pet);

  end put;

  procedure advance_cycle(c: in out clinic; current_cycle: in cycle) is
  begin 

    manage_waiting_room(c.waiting_room, current_cycle);    
    manage_boxes(c, current_cycle);

  end advance_cycle;

end d_clinic;