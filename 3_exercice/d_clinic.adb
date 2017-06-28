package body d_clinic is

  -- BEGIN FILE FUNCTIONS
  procedure write_to_file(printable: in String) is
  begin
    -- to file
    Open(File => output, Mode => Append_File, Name => "results.results");
    Put_line(output, printable);
    Close(output);
  end write_to_file;
  -- END FILE FUNCTIONS

  -- BEGIN PRINT FUNCTION
  -- *** Box item to string ***
  function box_to_string(box_to_print: in box_item) return String is
  begin 
    return "is_opened: " & box_to_print.is_opened'Img & 
            ", is_free: " & box_to_print.is_free'Img &
            ", name: " & SU.To_String(box_to_print.name) & 
            ", left_cycles: " & box_to_print.left_cycles'Img &
            ", enter_cycle: " & box_to_print.enter_cycle'Img &
            ", reason: " & box_to_print.reason'Img;
  end box_to_string;

  -- *** Waiting room item to string ***
  function wr_item_to_string(printable: waiting_room_item) return String is
    begin
      return NEW_LINE & "name: " & SU.To_String(printable.name) & 
             ", enter_cycle: " & printable.enter_cycle'Img &
             ", waiting_cycles: " & printable.waiting_cycles'Img &
             ", reason: " & printable.reason'Img;
  end wr_item_to_string;

  -- *** Historic item to string ***
  function h_item_to_string(printable: historic_item) return String is
    begin
      return NEW_LINE & "start_cycle: " & printable.start_cycle'Img & 
             ", reason: " & printable.reason'Img;
  end h_item_to_string;

  -- *** Waiting room item to string ***
  procedure wr_to_u_string(waiting_room: in out WR.heap; wr_u_string: out SU.Unbounded_String) is
    support_waiting_room: WR.heap;
    support_item: waiting_room_item;
  begin
    WR.empty(support_waiting_room);

    while WR.is_empty(waiting_room) /= true loop
      support_item:= WR.get_least(waiting_room);
      WR.delete_least(waiting_room);

      SU.Append(wr_u_string, SU.To_Unbounded_String(wr_item_to_string(support_item)));

      WR.put(support_waiting_room, support_item);
    end loop;

    waiting_room:= support_waiting_room;
    
  end wr_to_u_string;
  --END PRINT FUNCTION 

  -- BEGIN WAITING ROOM FUNCTIONS
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
  -- END WAITING ROOM FUNCTIONS

  -- BEGIN HELPER FUNCTIONS
  -- *** Wait cycles by reason ***
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

  -- *** Box cycles by reason ***
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

  -- *** Pet not registered ***
  function not_registered(c: in clinic; name: in SU.Unbounded_String) return Boolean is
    retrieved: historic_list;
  begin
    H.get(c.historic, name, retrieved);
    return false;
  exception
    when H.DOES_NOT_EXIST => return true;
  end not_registered;

  -- *** Pet not in helper structures ***
  function pet_not_in_helper(t: HS.tree; name: SU.Unbounded_String) return Boolean is
    retrieved: Natural;
  begin
    HS.get(t, name, retrieved);
    return false;
  exception
    when HS.DOES_NOT_EXIST => return true;
  end pet_not_in_helper;

  -- *** Adds a cycle to waiting room pets ***
  procedure manage_waiting_room(waiting_room: in out WR.heap; current_cycle: in cycle) is
    support_waiting_room: WR.heap;
    support_item: waiting_room_item;
  begin
    WR.empty(support_waiting_room);

    while WR.is_empty(waiting_room) /= true loop
      support_item:= WR.get_least(waiting_room);
      WR.delete_least(waiting_room);
      support_item.waiting_cycles:= support_item.waiting_cycles + 1;
      WR.put(support_waiting_room, support_item);
    end loop;

    waiting_room:= support_waiting_room;
  end manage_waiting_room;

  -- *** Check if pet is already in waiting room ***
  procedure is_in_waiting_room(waiting_room: in out WR.heap; name: in SU.Unbounded_String) is
    support_waiting_room: WR.heap;
    support_item: waiting_room_item;
  begin
    WR.empty(support_waiting_room);

    while WR.is_empty(waiting_room) /= true loop
      support_item:= WR.get_least(waiting_room);
      WR.delete_least(waiting_room);
      
      if name = support_item.name then
        raise PET_ALREADY_IN_WAITING_ROOM;
      end if;

      WR.put(support_waiting_room, support_item);
    end loop;

    waiting_room:= support_waiting_room;
  end is_in_waiting_room;

  -- *** True if box is can be closed ***
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

  -- *** Close box ***
  procedure close_box (c: in out clinic; box_to_close: in out box_item) is
  begin
    box_to_close.is_opened:= false;
    box_to_close.is_free:= true;
    c.opened_boxes:= c.opened_boxes - 1;
  end close_box;

  -- *** True if box can be opened ***
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

  -- *** Opens the first available box ***
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

        write_to_file("Opening box: " & box_to_open_key'Img & ", " & box_to_string(box_to_open));

        box_to_open.is_opened:= true;
        BOX.update(c.boxes, box_to_open_key, box_to_open);
        already_opened:= true;
        c.opened_boxes:= c.opened_boxes + 1;
      end if;

      BOX.next(c.boxes, it);
    end loop;  
  end open_available_box;

  -- *** Checks if pet is already in box ***
  procedure is_pet_in_box(c: in out clinic; name: SU.Unbounded_String) is
    support_box: box_item;
    it: BOX.iterator;
    support_box_key: Natural;
  begin 
    BOX.first(c.boxes, it);
    while BOX.is_valid(it) loop
      BOX.get(c.boxes, it, support_box_key, support_box);
      
      if name = support_box.name then 
        raise PET_ALREADY_IN_BOX;
      end if;

      BOX.next(c.boxes, it);
    end loop;  
  end is_pet_in_box;


  -- *** Enters the first open and available box ***
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
        box_to_enter:= (true, false, wr_item.name, calculate_box_cycles(wr_item.reason), wr_item.enter_cycle, wr_item.reason);
        entered:= true;
        
        write_to_file("/++++++++++++++++++++++++++++++++++++++++++++++++/" & NEW_LINE &
                      "Entering box: " & box_to_enter_key'Img & ", " & box_to_string(box_to_enter) & NEW_LINE &
                      "/++++++++++++++++++++++++++++++++++++++++++++++++/" & NEW_LINE);

        BOX.update(c.boxes, box_to_enter_key, box_to_enter);
      end if;

      BOX.next(c.boxes, it);
    end loop;  
  end enter_box;

  -- *** True if there are available boxes ***
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

  -- *** Saves the pet historic ***
  procedure save_historic(c: in out clinic; current_cycle: in cycle; box_to_save: in box_item) is 
    helper_tree: HS.tree;
    pet_historic: historic_list;
    h_item: historic_item;
    init_cycle: cycle;
    amount_visits: Natural:= 0;
  begin
    -- update pet historic
    write_to_file("get from historic: "& SU.To_String(box_to_save.name));
    H.get(c.historic, box_to_save.name, pet_historic);

    write_to_file("current_cycle: " &current_cycle'Img& " enter_cycle" &box_to_save.enter_cycle'Img);
    init_cycle:= current_cycle - (box_to_save.enter_cycle - 1);
    h_item:= (box_to_save.reason, init_cycle);

    HL.put(pet_historic.h_list, pet_historic.amount, h_item);
    pet_historic.amount:= pet_historic.amount + 1;
    write_to_file("update historic: "& SU.To_String(box_to_save.name));
    H.update(c.historic, box_to_save.name, pet_historic);

    -- update helpers list
    helper_tree:= c.helper_sets(box_to_save.reason);

    if pet_not_in_helper(helper_tree, box_to_save.name) then
      write_to_file("pet not in helper lists: "& SU.To_String(box_to_save.name));
      HS.put(helper_tree, box_to_save.name, 1);      
    else
      write_to_file("pet in helper lists: "& SU.To_String(box_to_save.name));
      HS.get(helper_tree, box_to_save.name, amount_visits);
      amount_visits:= amount_visits + 1;
      HS.update(helper_tree, box_to_save.name, amount_visits);
    end if;

    c.helper_sets(box_to_save.reason):= helper_tree; 

  end save_historic;
  -- END HELPER FUNCTIONS

  -- *** Initializes the clinic ***
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

    write_to_file("INITIALIZING" & NEW_LINE & "-----------------------------------------------");
    BOX.first(c.boxes, it);
    while BOX.is_valid(it) loop
      BOX.get(c.boxes, it, printable_key, printable);
      write_to_file("box: " & printable_key'Img & ", " & box_to_string(printable));
      BOX.next(c.boxes, it);
    end loop;  
    write_to_file("-----------------------------------------------");   
  end init_clinic;

  -- *** Manages the boxes behaviour ***
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

        write_to_file("+ Checking box: " & box_to_update_key'Img & ", " & box_to_string(box_to_update));

        if box_to_update.is_opened = true then

          write_to_file("+ Box " & box_to_update_key'Img &" is opened");

          if box_to_update.is_free = false then 
            update_box:= true;
            write_to_file("+ Box " & box_to_update_key'Img &" is opened and not free");

            box_to_update.left_cycles:= box_to_update.left_cycles - 1;

            if box_to_update.left_cycles = 0 then
              write_to_file("+ Box " & box_to_update_key'Img &" has no cycles left");

              write_to_file("+ Saving historic at cycle: "&current_cycle'Img);
              save_historic(c, current_cycle, box_to_update);

              if can_close_box(c) = true then
                write_to_file("+ Box " & box_to_update_key'Img &" can be closed");
                close_box(c, box_to_update);
                write_to_file("+ Box " & box_to_update_key'Img &" closed");
              else
                write_to_file("+ Box " & box_to_update_key'Img &" can not be closed");
                box_to_update.is_free:= true;
                write_to_file("+ Box " & box_to_update_key'Img &" freed");
              end if;
            end if;
          else 
            write_to_file("+ Box " & box_to_update_key'Img &" is opened and free");
            if can_close_box(c) = true then
              update_box:= true;
              write_to_file("+ Box " & box_to_update_key'Img &" can be closed");
              close_box(c, box_to_update);
              write_to_file("+ Box " & box_to_update_key'Img &" closed");
            end if;
          end if;

          if update_box then 
            BOX.update(c.boxes, box_to_update_key, box_to_update);
          end if;

        end if;
        update_box:= false;
        BOX.next(c.boxes, it);
      end loop;  
    -- Create boxes if posible
    if can_open_box(c) = true then
      open_available_box(c);
    end if;
    end manage_boxes;

  -- *** Puts a pet into the clinic ***
  procedure put(name: in SU.Unbounded_String; reason: in visit_reason; current_cycle: in cycle; c: in out clinic) is
    WR_pet: waiting_room_item;
    waiting_room: WR.heap renames c.waiting_room;
    HL_pet: historic_list;
  begin
    -- register it if it doesnt exist

    if not_registered(c, name) then
      write_to_file("+ Registering pet: " & SU.To_String(name));
      H.put(c.historic, name, HL_pet); 
    end if;
    -- pass to the waiting room
    is_in_waiting_room(c.waiting_room, name);
    WR_pet:= (current_cycle, name, calculate_wait_cycles(reason), reason);
    WR.put(waiting_room, WR_pet);
  end put;

  -- *** Advances the clinic a cycle ***
  procedure advance_cycle(c: in out clinic; current_cycle: in cycle) is
    pet_from_wr: waiting_room_item;
    are_animals_left: Boolean:= true;
  begin 
    write_to_file("CURRENT CYCLE [" & current_cycle'Img & " ]");
    manage_waiting_room(c.waiting_room, current_cycle);    
    manage_boxes(c, current_cycle);
    
    if are_animals_left and then are_free_boxes(c) then
      if WR.is_empty(c.waiting_room) then 
        are_animals_left:= false;
      else
        pet_from_wr:= WR.get_least(c.waiting_room);
        WR.delete_least(c.waiting_room);
        is_pet_in_box(c, pet_from_wr.name);
        enter_box(c, pet_from_wr);
      end if;
    end if;
  end advance_cycle;

  -- *** Retrieves the pets from the waiting room ***
  function retrieve_wait_room_pets(c: in out clinic) return SU.Unbounded_String is
    wr_u_string: SU.Unbounded_String;
  begin
    if WR.is_empty(c.waiting_room) = false then
      wr_to_u_string(c.waiting_room, wr_u_string);
    else
      wr_u_string:= SU.To_Unbounded_String("No pets in waiting room!");
    end if;

    return wr_u_string;
  end retrieve_wait_room_pets;

  -- *** Retrieves pet historic by name ***
  function retrieve_pet_historic(c: in clinic; name: in SU.Unbounded_String) return SU.Unbounded_String is
    ph_u_string: SU.Unbounded_String;
    retrieved: historic_list;
    printable: historic_item;
    it: HL.iterator;
    pet_key: Integer;
  begin
    if not_registered(c, name) then
      ph_u_string:= SU.To_Unbounded_String("Pet is not registered...");
    else
      H.get(c.historic, name, retrieved);
      HL.first(retrieved.h_list, it);
      if HL.is_valid(it) then
        ph_u_string:= SU.To_Unbounded_String("Pet historic for ");
        SU.Append(ph_u_string, name);
        SU.Append(ph_u_string, " => ");
        while HL.is_valid(it) loop
          HL.get(retrieved.h_list, it, pet_key, printable);
          SU.Append(ph_u_string, h_item_to_string(printable));
          HL.next(retrieved.h_list, it);
        end loop;
      else 
        ph_u_string:= SU.To_Unbounded_String("No historic for this pet yet...");
      end if;
    end if;
    return ph_u_string;
  end retrieve_pet_historic;

  -- *** Retrieves visits log by reason ***
  function retrieve_visits_by_type(c: in clinic; visit_type: in visit_reason) return SU.Unbounded_String is
    support_tree: HS.tree;
    it: HS.iterator;
    pet_key: SU.Unbounded_String;
    pet_times: Natural;
    hs_u_string, name_times_string: SU.Unbounded_String;
  begin
    support_tree:= c.helper_sets(visit_type);
    HS.first(support_tree, it);
    if HS.is_valid(it) then
      hs_u_string:= SU.To_Unbounded_String("Historic for ");
      SU.Append(hs_u_string, visit_type'Img);
      SU.Append(hs_u_string, " reason => ");
      while HS.is_valid(it) loop
        HS.get(support_tree, it, pet_key, pet_times);

        SU.Append(name_times_string, NEW_LINE);
        SU.Append(name_times_string, pet_key);
        SU.Append(name_times_string, ": ");
        SU.Append(name_times_string, pet_times'Img);

        SU.Append(hs_u_string, name_times_string);

        name_times_string:= SU.To_Unbounded_String("");  

        HS.next(support_tree, it);
      end loop;
    else
      hs_u_string:= SU.To_Unbounded_String("No pets registered because of the given reason...");
    end if;
    return hs_u_string;
  end retrieve_visits_by_type;

end d_clinic;