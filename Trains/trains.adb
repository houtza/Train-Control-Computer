-- Written by John McCormick, May 2002
-- Rewritten by John McCormick, May 2008
-- Modifed by Andrew Berns, October 2017
with Blocks;
with Display;
with DoubleTalk;      use DoubleTalk.Phrase_Strings;
with Halls;
with Turnouts;
with Sound;
with Ada.Exceptions;  use Ada.Exceptions;

pragma Elaborate_All (Blocks);
pragma Elaborate_All (Display);
pragma Elaborate_All (DoubleTalk);

package body Trains is

   use type Cabs.Cab_ID;
   use type Layout.Block_ID;
   use type Layout.Hall_ID;
   use type Layout.Turnout_Limb;

   -----------------------------------------------------------------------------
   procedure Speak (Phrase : in String) is
      -- Call DoubleTalk to speak Phrase
      pragma Inline (Speak);
   begin
      DoubleTalk.Speak (Phrase => To_Bounded_String (Phrase),
                        Voice  => DoubleTalk.Paul);
   end Speak;

   ----------------------------------------------------------------------------
   -- Basic train information not needing mutex protection
   ----------------------------------------------------------------------------
   type Hall_Rec is                  -- For tracking recently triggered Hall
      record                         -- sensors.  Needed when reversing a
         Valid : Boolean := False;   -- stopped train that has one or both of
         Hall  : Layout.Hall_ID;     -- its magnets over a Hall sensor.
      end record;
   type Train_End      is (Front, Rear);
   type Hall_Rec_Array is array (Train_End) of Hall_Rec;

   type Train_Info_Rec is
      record
         Name         : Locomotives.Loco_String;
         Cab          : Cabs.Control_Cab_ID;
         Direction    : Direction_Type          := Forward;
         Min_Throttle : Cabs.Percent;
         Throttle     : Cabs.Percent            := 0;
         Recent_Halls : Hall_Rec_Array;
      end record;
   type Train_Info_Array is array (Train_ID) of Train_Info_Rec;

   Train_Info : Train_Info_Array;

   -- The number of trains running is equal to the ID of the last train
   -- initialized (see body of procedure Initialize).
   Num_Trains_Running : Request_ID := 0;

   -----------------------------------------------------------------------------
   -- Train/Cab assignment (used to translate a cab_id to a train_id
   -----------------------------------------------------------------------------
   type Train_Array is array (Cabs.Control_Cab_ID) of Train_ID;
   Train_Powered_By : Train_Array;


   -----------------------------------------------------------------------------
   -- Mutex for each train's halted state data is provided by a protected object
   -----------------------------------------------------------------------------

   protected type Stop_Status is
      -- Maintains information on the halted states of a train

      ------------------------------------------
      function Stopped return Boolean;
      -- True if Train is stopped for any reason
      -------------------------------------------
      function Stop_Reasons return Stop_Rec;
      -- Returns all the information on the stopped state of Train
      -------------------------------------------
      procedure Dispatcher_Stop (Train : in Train_ID);
      procedure Dispatcher_Go;
      -- Dispatcher Stop and Go
      -------------------------------------------
      procedure Lost_Caboose_Stop (Train : in Train_ID);
      procedure Lost_Caboose_Go;
      -- Lost Caboose Stop and Go
      -------------------------------------------
      procedure Stop (Train   : in Train_ID;
                      Turnout : in Layout.Turnout_ID);
      procedure Go   (Turnout : in Layout.Turnout_ID);
      -- Turnout Stop and Go
      -------------------------------------------
      procedure Stop (Train : in Train_ID;
                      Block : in Layout.Block_ID);
      procedure Go   (Block : in Layout.Block_ID);
      -- Block Stop and Go
      -------------------------------------------
      procedure Clear_All;
      procedure Clear_All_Set_Dispatcher;
      -- Clear all stop reasons but set a dispatcher stop
      -------------------------------------------
      entry Wait_For_Stop (Why : out Stop_Rec);
      -- For task that restarts the train

   private
      Why_Stopped : Stop_Rec;
   end Stop_Status;

   -----------------------------------------------------------------------------

   protected body Stop_Status is
      -------------------------------------------
      function Stopped return Boolean is
         Result : Boolean;
      begin
         Result := False;
         -- Check all of the possible stop reaons to determine if we
         -- are stopped for any reason
         for Index in Stop_Set'Range loop
            Result := Result or Why_Stopped.Reasons (Index);
         end loop;
         return Result;
      end Stopped;

      -------------------------------------------
      function Stop_Reasons return Stop_Rec is
      begin
         return Why_Stopped;
      end Stop_Reasons;

      -------------------------------------------
      procedure Dispatcher_Stop (Train : in Train_ID) is
      begin
         Cabs.Set_Limit (Cab   => Train_Info (Train).Cab,
                         Value => 0);
         Why_Stopped.Reasons (Dispatcher_Request) := True;
      end Dispatcher_Stop;

      -------------------------------------------
      procedure Dispatcher_Go is
      begin
         -- Don't remove dispatcher stop if the caboose is still lost
         if not Why_Stopped.Reasons (Lost_Caboose) then
            Why_Stopped.Reasons (Dispatcher_Request) := False;
         end if;
      end Dispatcher_Go;

      -------------------------------------------
      procedure Lost_Caboose_Stop (Train : in Train_ID) is
      begin
         Cabs.Set_Limit (Cab   => Train_Info (Train).Cab,
                         Value => 0);
         Why_Stopped.Reasons (Dispatcher_Request) := True;
         Why_Stopped.Reasons (Lost_Caboose)       := True;
      end Lost_Caboose_Stop;

      -------------------------------------------
      procedure Lost_Caboose_Go is
      begin
         Why_Stopped.Reasons (Lost_Caboose) := False;
      end Lost_Caboose_Go;

      -------------------------------------------
      procedure Stop (Train   : in Train_ID;
                      Turnout : in Layout.Turnout_ID) is
      begin
         Cabs.Set_Limit (Cab   => Train_Info (Train).Cab,
                         Value => 0);
         Why_Stopped.Reasons (Turnout_Failure) := True;
         Why_Stopped.Turnouts (Turnout) := True;
      end Stop;

      -------------------------------------------
      procedure Go (Turnout : in Layout.Turnout_ID) is
         Others_Failed : Boolean;
      begin
         -- Remove Turnout from the set of failed turnouts
         Why_Stopped.Turnouts (Turnout) := False;

         -- See if we are waiting for any other failed turnouts
         Others_Failed := False;
         for Index in Layout.Turnout_ID loop
            Others_Failed := Others_Failed or Why_Stopped.Turnouts (Index);
         end loop;
         Why_Stopped.Reasons (Turnout_Failure) := Others_Failed;
      end Go;

      -------------------------------------------
      procedure Stop (Train : in Train_ID;
                      Block : in Layout.Block_ID) is
      begin
         Cabs.Set_Limit (Cab   => Train_Info (Train).Cab,
                         Value => 0);
         Why_Stopped.Reasons (Reservation_Failure) := True;
         Why_Stopped.Block := Block;
      end Stop;

      -------------------------------------------
      procedure Go (Block : in Layout.Block_ID) is
      begin
         if Why_Stopped.Block = Block then
            Why_Stopped.Reasons (Reservation_Failure) := False;
         end if;
      end Go;

      -------------------------------------------
      procedure Clear_All is
      begin
         Why_Stopped.Reasons  := (others => False);
         Why_Stopped.Turnouts := (others => False);
      end Clear_All;

      -------------------------------------------
      procedure Clear_All_Set_Dispatcher is
      begin
         Why_Stopped.Reasons  := (others => False);
         Why_Stopped.Reasons (Dispatcher_Request) := True;
         Why_Stopped.Turnouts := (others => False);
      end Clear_All_Set_Dispatcher;

      -------------------------------------------
      entry Wait_For_Stop (Why : out Stop_Rec)
        when Stopped is
         -- Entry used to block stop handler task until the train stops
      begin
         Why := Why_Stopped;
      end Wait_For_Stop;

   end Stop_Status;

   -----------------------------------------------------------------------------
   -- The stop status for each of the trains is stored in this
   -- array of protected stop status objects
   type Stop_Status_Array is array (Train_ID) of Stop_Status;
   Status : Stop_Status_Array;

   -----------------------------------------------------------------------------
   -- Local procedures for next block reservations
   -- Called by the block list protected operations
   -----------------------------------------------------------------------------

   procedure Reserve_Next_Block (Train         : in Train_ID;
                                 Current_Block : in Layout.Block_ID;
                                 Polarity      : in Layout.Block_Polarity) is
      -- Attempt to reserve the block ahead of the train.
      --    Current_Block is the block under the front of the train.
      --    Polarity      is the polarity of Current_Block.
      Next_Block  : Layout.Block_ID;         -- The block after Current_Block
      Termination : Layout.Termination_Type; -- What terminates Current_Block
      Success     : Boolean;                 -- of reservation attempt
      Turnout     : Layout.Turnout_ID;       -- Turnout that ends Current_Block
      Direction   : Layout.Turn_Choice;      -- Direction of Turnout
   begin
      -- What is at the end of the current block?
      Termination := Layout.Terminated_By (Block     => Current_Block,
                                           Direction => Polarity);
      -- Determine the Next_Block
      case Termination is
         when Layout.A_Deadend =>
            return;    -- Nothing to do for a dead end
         when Layout.A_Block =>
            -- Can get the next block right off the map (Layout)
            Next_Block := Layout.Next_Block (Block     => Current_Block,
                                             Direction => Polarity);
         when Layout.A_Turnout =>
            -- This case requires to find out which of two blocks is next
            -- What is the ID of the turnout at the end of Current_Block
            Turnout := Layout.Adjacent_Turnout (Block     => Current_Block,
                                                Direction => Polarity);
            -- To what direction is Turnout set?
            Direction := Turnouts.Direction_Of (Turnout);
            -- Now we can determine Next_Block
            Next_Block := Layout.Next_Block (Turnout => Turnout,
                                             Limb    => Direction);
      end case;

      -- Attempt to reserve the next block
      Blocks.Reserve (Train   => Train,
                      Block   => Next_Block,
                      Success => Success);
      if not Success then
         -- Stop the train because of reservation failure
         Status (Train).Stop (Train => Train,
                              Block => Next_Block);
         Speak ("Waiting for block " & Layout.Block_ID'Image (Next_Block));
      end if;
   end Reserve_Next_Block;

   --------------------------------------------------------------------------
   procedure Release_Next_Block (Train         : in Train_ID;
                                 Current_Block : in Layout.Block_ID;
                                 Polarity      : in Layout.Block_Polarity) is
      -- Release the reservation on the block ahead of the train.
      -- Called when the train is changing directions.
      --    Current_Block is the block under the front of the train.
      --    Polarity      is the polarity of Current_Block.
      Next_Block  : Layout.Block_ID;         -- The block after Current_Block
      Termination : Layout.Termination_Type; -- What terminates Current_Block
      Turnout     : Layout.Turnout_ID;       -- Turnout that ends Current_Block
      Direction   : Layout.Turn_Choice;      -- Direction of Turnout
   begin
      -- What is at the end of the current block?
      Termination := Layout.Terminated_By (Block     => Current_Block,
                                           Direction => Polarity);
      -- Determine the Next_Block
      case Termination is
         when Layout.A_Deadend =>
            return;    -- Nothing to do for a dead end
         when Layout.A_Block =>
            -- Can get the next block right off the map (Layout)
            Next_Block := Layout.Next_Block (Block     => Current_Block,
                                             Direction => Polarity);
         when Layout.A_Turnout =>
            -- This case requires to find out which of two blocks is next
            -- What is the ID of the turnout at the end of Current_Block
            Turnout := Layout.Adjacent_Turnout (Block     => Current_Block,
                                                Direction => Polarity);
            -- To what direction is Turnout set?
            Direction := Turnouts.Direction_Of (Turnout);
            -- Now we can determine Next_Block
            Next_Block := Layout.Next_Block (Turnout => Turnout,
                                             Limb    => Direction);
      end case;

      -- Release the next block (no problem if it isn't reserved by this train)
      Blocks.Release (Train => Train,
                      Block => Next_Block);
      -- Signal train in case it is waiting for this block
      Status (Train).Go (Block => Next_Block);

   end Release_Next_Block;

   -----------------------------------------------------------------------------
   -- Mutex for each train's block list is provided by a protected object
   -----------------------------------------------------------------------------

   protected type Protected_Block_List is
      -- Maintains a list of blocks under the train and makes
      -- cab assignments and reservations for them with a
      -- guarantee of mutual exclusive access to the list.
      --
      -- The list is in order from rear to front of train.
      -- After train intialization, this list should never be empty.

      -------------------------------------------
      procedure Set_ID (My_Train : in Train_ID);
      -- Give the protected object the ID of the train whose block
      -- list it is maintaining.
      -- This procedure should only be called one (during package
      -- elaboration).

      -------------------------------------------
      function Full return Boolean;
      -- Returns True if the block list can take no more entries.
      -- Used to see if a train has "grown too long", an indication
      -- that the caboose has been lost.
      -------------------------------------------
      function Front return Layout.Block_ID;
      -- Returns the block under the front of the train
      -------------------------------------------
      function Powered_Blocks return Layout.Block_Array;
      -- Returns an array of blocks powered for this train.
      -- Used for displaying train status.
      -------------------------------------------
      procedure Clear;
      -- Assigns the null cab to all blocks in the list
      -- Clears the list of blocks
      -------------------------------------------
      procedure Add (Block : in Layout.Block_ID);
      -- Add Block to the front of the list
      -- No powering or reservations
      -------------------------------------------
      procedure Add (Block    : in Layout.Block_ID;
                     Cab      : in Cabs.Cab_ID;
                     Polarity : in Layout.Block_Polarity);
      -- Add Block to front of the list
      -- Power the block
      -- Attempt to reserve the block that follows Block

      -------------------------------------------
      procedure Delete (Train : in Train_ID;
                        Block : out Layout.Block_ID);
      -- Returns the block now behind the train
      -- The null cab is assigned to that block
      -- The reservation is released on that block

      -------------------------------------------
      procedure Try;
      -- Try to reserve the next block ahead of the train.
      -- Called while waiting for a block to be released by another train.
      -------------------------------------------
      procedure Reverse_Order (Train : in Train_ID);
      -- Reverse the order of the blocks in the list.  Also reverses the
      -- polarity of the block on the layout, releases the reservation on the
      -- block originally in front of the train and attempts to reserve the
      -- block behind the train.
      -- This procedure is called when changing the direction of a train.
      -------------------------------------------
      procedure  Change (Direction : in  Layout.Turn_Choice;
                         No_Need   : out Boolean;
                         Turnout   : out Layout.Turnout_ID;
                         Adjacent  : out Boolean;
                         Success   : out Boolean);

      -- Change the reservations to reflect a turnout change.
      -- No_Need  is True if turnout is already in Direction
      -- Turnout  is the turnout ahead of the train that is to be changed
      -- Adjacent is True if Turnout is adjacent to the front block of the train
      -- Success  is True if the attempt to make a new reservation succeeded.

   private

      Train : Train_ID;  -- The train for whom the list of blocks is maintained

      -- A varying length ordered list of blocks with the rear of the
      -- train in at index 1 and the front of the train at index Size
      Size       : Natural := 0;
      The_Blocks : Layout.Block_Array (1 .. Max_Length_Running + 1);

   end Protected_Block_List;

   -----------------------------------------------------------------------------
   protected body Protected_Block_List is

      -------------------------------------------
      procedure Set_ID (My_Train : in Train_ID) is
      begin
         Train := My_Train;
      end Set_ID;

      -------------------------------------------
      function Full return Boolean is
      begin
         return Size = The_Blocks'Last;
      end Full;

      -------------------------------------------
      function Front return Layout.Block_ID is
      begin
         return The_Blocks (Size);
      end Front;

      -------------------------------------------
      function Powered_Blocks return Layout.Block_Array is
      begin
         return The_Blocks (1 .. Size);
      end Powered_Blocks;

      -------------------------------------------
      procedure Clear is
      begin
         for Index in 1 .. Size loop
            Blocks.Assign_Cab (Block    => The_Blocks (Index),
                               Cab      => Cabs.Null_Cab,
                               Polarity => Layout.Normal);
         end loop;
         Size := 0;
      end Clear;

      -------------------------------------------
      procedure Add (Block : in Layout.Block_ID) is
      begin
         -- Add the block to the list
         Size := Size + 1;
         The_Blocks (Size) := Block;
      end Add;

      -------------------------------------------
      procedure Add (Block    : in Layout.Block_ID;
                     Cab      : in Cabs.Cab_ID;
                     Polarity : in Layout.Block_Polarity) is
      begin
         -- Power the block
         Blocks.Assign_Cab (Block, Cab, Polarity);

         -- Add the block to the list
         Size := Size + 1;
         The_Blocks (Size) := Block;

         -- Attempt to reserve the next block
         Reserve_Next_Block (Train         => Train_Powered_By (Cab),
                             Current_Block => Block,
                             Polarity      => Polarity);
      end Add;

      -------------------------------------------
      procedure Try is
         Stop_Status : Stop_Rec;
         Success     : Boolean;      -- True if train can get the block
      begin
         Stop_Status := Status (Train).Stop_Reasons;
         if Stop_Status.Reasons (Reservation_Failure) then
            -- Try to reserve the block we are waiting for
            Blocks.Reserve (Train   => Train,
                            Block   => Stop_Status.Block,
                            Success => Success);
            if Success then
               Status (Train).Go (Block => Stop_Status.Block);
            end if;
         end if;
      end Try;

      -------------------------------------------
      procedure Delete (Train : in Train_ID;
                        Block : out Layout.Block_ID) is
      begin
         Block := The_Blocks (1);  -- The block at the rear of the train

         -- Assign the null cab to the block
         Blocks.Assign_Cab (Block    => Block,
                            Cab      => Cabs.Null_Cab,
                            Polarity => Layout.Normal);

         -- Release the block reservation
         Blocks.Release (Train, Block);

         -- Remove the block from the list
         The_Blocks (1 .. Size - 1) := The_Blocks (2 .. Size);
         Size := Size - 1;
      end Delete;

      -------------------------------------------
      procedure Reverse_Order (Train : in Train_ID) is
         Temp : Layout.Block_ID;   -- For swapping array values
         -- Block powering information
         Cab       : Cabs.Cab_ID;
         Direction : Layout.Block_Polarity;
      begin
         -- Get the polarity of the front block
         Blocks.Cab_Assigned (Block    => The_Blocks (Size),
                              Cab      => Cab,
                              Polarity => Direction);
         -- Release the reservation on the block following the front block
         Release_Next_Block (Train         => Train,
                             Current_Block => The_Blocks (Size),
                             Polarity      => Direction);

         -- Reverse the blocks in the list
         for Index in 1 .. Size / 2 loop
            Temp := The_Blocks (Index);
            The_Blocks (Index) := The_Blocks (Size - Index + 1);
            The_Blocks (Size - Index + 1) := Temp;
         end loop;

         -- Change the polarity of the blocks
         for Index in 1 .. Size loop
            Blocks.Cab_Assigned (Block    => The_Blocks (Index),
                                 Cab      => Cab,
                                 Polarity => Direction);
            -- Reverse the direction
            Direction := Layout.Opposite (Direction);
            Blocks.Assign_Cab (Block    => The_Blocks (Index),
                               Cab      => Cab,
                               Polarity => Direction);
         end loop;

         -- Attempt to reserve the block ahead of the new front of train
         -- Note: The Direction was obtained in the last iteration of the
         --       polarity changing loop above.
         Reserve_Next_Block (Train         => Train,
                             Current_Block => The_Blocks (Size),
                             Polarity      => Direction);
      end Reverse_Order;

      -------------------------------------------
      procedure Change (Direction : in  Layout.Turn_Choice;
                        No_Need   : out Boolean;
                        Turnout   : out Layout.Turnout_ID;
                        Adjacent  : out Boolean;
                        Success   : out Boolean) is
         -- The block under the front of the train and its cab and polarity
         Front_Block : Layout.Block_ID;
         Cab         : Cabs.Cab_ID;
         Polarity    : Layout.Block_Polarity;

         New_Block   : Layout.Block_ID;  -- The new block to reserve
         Old_Block   : Layout.Block_ID;  -- The old block to release

      begin
         -- Get the block under the front of the train
         Front_Block := The_Blocks (Size);
         Blocks.Cab_Assigned (Block   => Front_Block,
                              Cab      => Cab,
                              Polarity => Polarity);
         -- Get the next choice turnout from the map (Layout)
         Turnout := Layout.Choice_Turnout (Block     => Front_Block,
                                           Direction => Polarity);

         -- Is the turnout already in the requested direction?
         if Direction = Turnouts.Direction_Of (Turnout) then
            No_Need := True;
         else
            -- We have to change the turnout
            No_Need  := False;
            -- Is this turnout adjacent to the front block
            Adjacent := Front_Block =
              Layout.Next_Block (Turnout, Layout.Common);

            if Adjacent then
               -- Reserve the block in the new direction
               New_Block := Layout.Next_Block (Turnout => Turnout,
                                               Limb    => Direction);
               Blocks.Reserve (Train   => Train,
                               Block   => New_Block,
                               Success => Success);
               if not Success then
                  -- Stop the train because of reservation failure
                  Status (Train).Stop (Train => Train,
                                       Block => New_Block);
                  Speak ("Waiting for block " &
                           Layout.Block_ID'Image (New_Block));
               end if;
               -- Free the block in the old direction
               Old_Block := Layout.Next_Block
                 (Turnout => Turnout,
                  Limb    => Layout.Opposite (Direction));
               Blocks.Release (Train => Train,
                               Block => Old_Block);
               -- If the train was waiting on the block in the old direction,
               -- let it go ahead
               Status (Train).Go (Block => Old_Block);
            end if;
         end if;
      end Change;
   end Protected_Block_List;


   -- A block list for each train is stored in this array of
   -- protected block list objects
   type Block_List_Array is array (Train_ID) of Protected_Block_List;
   Block_List : Block_List_Array;


   ----------------------------------------------------------------------------
   -- Local procedures and functions
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Change_Force_Turnout (Train     : in Train_ID;
                                   Block     : in Layout.Block_ID;
                                   Direction : in Layout.Block_Polarity) is
      use Layout;
      -- Checks for and changes a force turnout on the Direction end of Block
      --
      -- Preconditions  : none
      --
      -- Postconditions : Any force turnout on the Direction end of Block is
      --                  set to the correct direction

      Turnout          : Turnout_ID;  -- Force turnout at end of Block
      Turn_Direction   : Turn_Choice; -- The required direction
      Choice_Turnout   : Turnout_ID;  -- Choice turnout at end of Block
      Choice_Direction : Turn_Choice; -- Choice_Turnout's current direction
   begin
      -- First check for a direct force turnout
      if Layout.Has_Force_Turnout (Block, Direction) then
         -- The the ID and required direction of the force turnout
         Layout.Get_Force_Turnout (Block              => Block,
                                   Direction          => Direction,
                                   Turnout            => Turnout,
                                   Required_Direction => Turn_Direction);
         Turnouts.Set (Train, Turnout, Turn_Direction);

         -- Second, check for an indirect force turnout - the second turnout
         -- of a joint turnout
      elsif Layout.Terminated_By (Block, Direction) = Layout.A_Turnout then
         Choice_Turnout   := Layout.Adjacent_Turnout (Block, Direction);
         Choice_Direction := Turnouts.Direction_Of (Choice_Turnout);
         -- Is the choice turnout at the end of Block part of a joint pair?
         if Layout.Has_Joint (Choice_Turnout, Choice_Direction) then
            Turnout := Layout.Joint_Turnout (Choice_Turnout, Choice_Direction);
            Turnouts.Set (Train, Turnout, Choice_Direction);
         end if;
      end if;
   end Change_Force_Turnout;

   ----------------------------------------------------------------------------
   procedure Process_New_Block (Block             : in Layout.Block_ID;
                                Hall              : in Layout.Hall_ID;
                                Cab               : in Cabs.Cab_ID;
                                Adjacent_Block    : in Layout.Block_ID;
                                Adjacent_Polarity : in Layout.Block_Polarity) is

      -- This procedure takes care of everything that needs to be done when the
      -- front of a train passes over a Hall sensor.  This includes
      --    1.  Powering the new block of track
      --    2.  Reserving the next block of track
      --    3.  Setting any force turnout to its required position
      --    4.  Stopping a train because of a lost caboose

      Train    : Train_ID;
      Polarity : Layout.Block_Polarity;
   begin
      -- Determine the train from the cab assignment
      Train := Train_Powered_By (Cab);

      -- Determine the polarity needed for the new block from
      -- the polarity of the block adjacent to it.
      if Layout.Is_Reversing (Hall) then
         Polarity := Layout.Opposite (Adjacent_Polarity);
      else
         Polarity := Adjacent_Polarity;
      end if;

      -- Power the block, add it to the block list, and
      -- try to reserve the next block
      Block_List (Train).Add (Block, Cab, Polarity);

      -- Update the most recent front Hall sensor triggering
      Train_Info (Train).Recent_Halls (Front) := (Valid => True,
                                                  Hall  => Hall);
      -- Check for lost caboose
      if Block_List (Train).Full then
         Status (Train).Lost_Caboose_Stop (Train);
         Speak ("Train "& Train_ID'Image (Train) & " lost its caboose");
      end if;

      if not Status (Train).Stopped then
         -- Change any force turnout at the end of the newly powered block
         Change_Force_Turnout (Train, Block, Polarity);
      end if;

      -- Display the current list of blocks for this train
      Display.Put (Train  => Train,
                   Blocks => Block_List (Train).Powered_Blocks);
   exception
      when The_Error : others =>
         Display.Put_Error ("Exception " & Exception_Name (The_Error) & " has" &
                              " occurred in Trains.Process_New_Block");
         Speak ("An exception has occurred in Process New Block");
   end Process_New_Block;

   ----------------------------------------------------------------------------
   task type Stop_Handler is
      entry Set_ID (My_Train : Train_ID);
   end Stop_Handler;

   ----------------------------------------------------------------------------

   task body Stop_Handler is
      Train       : Train_ID;
      Period      : constant Duration := 1.0;  -- Seconds between checks
      Why_Stopped : Stop_Rec;         -- All info on why train is stopped
      -- Block number, polarity, and cab that powers the block under train front
      Front_Block    : Layout.Block_ID;
      Front_Polarity : Layout.Block_Polarity;
      Cab            : Cabs.Cab_ID;
   begin
      accept Set_ID (My_Train : Train_ID) do
         Train := My_Train;
      end Set_ID;
      -- Clear all halts for this train so this task will block on Wait_For_Stop
      Status (Train).Clear_All;

      loop -- forever

         -- Task is blocked until Train stops
         Status (Train).Wait_For_Stop (Why_Stopped);

         loop  -- until train is able to go
            -- Display why train is stopped
            Display.Put (Train  => Train,
                         Status => Why_Stopped);
            -- Wait a bit for recovery
            delay Period;
            -- Refresh why we stopped (something can change while we delayed)
            Why_Stopped := Status (Train).Stop_Reasons;

            -- Are we waiting for a block?
            if Why_Stopped.Reasons (Reservation_Failure) then
               -- Try to reserve the block
               Block_List (Train).Try;
            end if;

            -- Check for movement of lost caboose
            if not Block_List (Train).Full then
               Status (Train).Lost_Caboose_Go;
            end if;

            -- Exit when train is no longer stopped
            exit when not Status (Train).Stopped;
         end loop;

         -- Train is able to go, first set any force turnouts
         Front_Block := Block_List (Train).Front;     -- Block under train front
         Blocks.Cab_Assigned (Block    => Front_Block, -- Get polarity
                              Cab      => Cab,
                              Polarity => Front_Polarity);
         Change_Force_Turnout (Train, Front_Block, Front_Polarity);
         -- Now release the speed limit
         Cabs.Set_Limit (Cab   => Train_Info (Train).Cab,
                         Value => 100);
         -- Update the display
         Display.Put (Train  => Train,
                      Status => Status (Train).Stop_Reasons);

      end loop;
   exception
      when The_Error : others =>
         Display.Put_Error ("Exception " & Exception_Name (The_Error) & " has" &
                              " occurred in Trains.Stop_Handler task");
         Speak ("A stop handler task has died");
   end Stop_Handler;

   -- Create a task for each train
   type Stop_Handler_Array is array (Train_ID) of Stop_Handler;
   Stopped_Train_Task : Stop_Handler_Array;


   ----------------------------------------------------------------------------
   -- Bodies of package subprograms
   ----------------------------------------------------------------------------

   procedure Update_Location (Hall : in Layout.Hall_ID) is

      -- Blocks adjacent to Hall
      Block_A : Layout.Block_ID;
      Block_B : Layout.Block_ID;
      -- Cabs assigned to blocks adjacent to Hall
      Cab_A : Cabs.Cab_ID;
      Cab_B : Cabs.Cab_ID;
      -- Polarities of the the Cabs assigned to blocks adjacent to Hall
      Polarity_A : Layout.Block_Polarity;
      Polarity_B : Layout.Block_Polarity;

   begin
      -- Get the blocks adjacent to Hall
      Layout.Get_Adjacent_Blocks (Hall, Block_A, Block_B);
      -- Get the cabs and polarities assigned to the blocks adjacent to Hall
      Blocks.Cab_Assigned (Block    => Block_A,
                           Cab      => Cab_A,
                           Polarity => Polarity_A);
      Blocks.Cab_Assigned (Block    => Block_B,
                           Cab      => Cab_B,
                           Polarity => Polarity_B);

      if Cab_A = Cabs.Null_Cab and Cab_B = Cabs.Null_Cab then
         -- Invalid Hall trigger
         Speak ("Hall " & Layout.Hall_ID'Image (Hall) &
                  " was triggered with no train near by");


         --------------------------------------------------------------------------
         -- If only one block is powered, this Hall is under the front of
         -- the train.
         --------------------------------------------------------------------------

      elsif Cab_A = Cabs.Null_Cab then
         -- Power Block_A, reserve the next block, and change any force turnout
         Process_New_Block (Block             => Block_A,
                            Hall              => Hall,
                            Cab               => Cab_B,
                            Adjacent_Block    => Block_B,
                            Adjacent_Polarity => Polarity_B);

      elsif Cab_B = Cabs.Null_Cab then
         -- Power Block_B, reserve the next block, and change any force turnout
         Process_New_Block (Block             => Block_B,
                            Hall              => Hall,
                            Cab               => Cab_A,
                            Adjacent_Block    => Block_A,
                            Adjacent_Polarity => Polarity_A);


      else
         --------------------------------------------------------------------------
         -- Both blocks are powered by the same cab.
         -- This Hall is under the rear of the train.
         --------------------------------------------------------------------------

         declare
            Train : Train_ID;        -- The train that triggered Hall
            Block : Layout.Block_ID; -- Block removed from rear of list
         begin
            -- Determine the train from the cab powering Block_A and Block_B
            Train := Train_Powered_By (Cab_A);

            -- Remove the block from the protected block list
            -- (releasing reservation and connecting null cab)
            Block_List (Train).Delete (Train, Block);

            -- Update the most recent rear Hall sensor triggering
            Train_Info (Train).Recent_Halls (Rear) := (Valid => True,
                                                       Hall  => Hall);

            -- Display the current list of blocks for this train
            Display.Put (Train  => Train,
                         Blocks => Block_List (Train).Powered_Blocks);
         end;  -- declare block
      end if;

   exception
      when The_Error : others =>
         Display.Put_Error ("Exception " & Exception_Name (The_Error) & " has" &
                              " occurred in Train.Operations.Update_Location");
         Speak ("An exception has occurred in Update Location");

   end Update_Location;

   ----------------------------------------------------------------------------
   procedure Turn (Train     : in Train_ID;
                   Direction : in Layout.Turn_Choice) is

      Turnout       : Layout.Turnout_ID;  -- The turnout to change
      No_Need       : Boolean; -- True if turnout is already in Direction
      Adjacent      : Boolean; -- True if Turnout adjacent to Train's lead block
      Success       : Boolean; -- True if reservation changes succeeded
      Joint_Turnout : Layout.Turnout_ID;  -- Possible joint turnout to change
      Common_Limb   : Layout.Block_ID;    -- The common limb of the turnout

      -- The logic to change a turnout is split between this procedure and the
      -- protected Change procedure because the reservation logic must be done in
      -- the protected object (to ensure the changes are atomic) but the actual
      -- turnout changing cannot be done there since it involves a possibly
      -- blocking rendezvous.

   begin
      -- Get the Turnout to change, whether it needs to be changed, whether it
      -- is adjacent to the train's front block, and whether or not the
      -- necessary change in reservations were successfully made.
      Block_List (Train).Change (Direction => Direction,
                                 No_Need   => No_Need,
                                 Turnout   => Turnout,
                                 Adjacent  => Adjacent,
                                 Success   => Success);

      if No_Need then
         return;  -- nothing to do
      end if;

      -- Is this choice turnout on the end of the front block?
      if Adjacent then
         -- Change the turnout
         Turnouts.Set (Train, Turnout, Direction);
         -- We need to change a joint turnout only if the reservation change was
         -- succcessful and Turnout has a joint turnout in Direction
         if Success and Layout.Has_Joint (Turnout, Direction) then
            Joint_Turnout := Layout.Joint_Turnout (Turnout, Direction);
            Turnouts.Set (Train, Joint_Turnout, Direction);
         end if;

      else -- Choice turnout is not on a block reserved by this train
         -- Try to reserve the block connected to the turnout's common limb
         Common_Limb := Layout.Next_Block (Turnout, Layout.Common);
         Blocks.Reserve (Train   => Train,
                         Block   => Common_Limb,
                         Success => Success);
         if Success then
            -- We can change the turnout
            Turnouts.Set (Requestor => Train,
                          Turnout   => Turnout,
                          Direction => Direction);
            Blocks.Release (Train   => Train,
                            Block   => Common_Limb);
         end if;
      end if;
   end Turn;

   ----------------------------------------------------------------------------
   procedure Set_Direction (Train     : in Train_ID;
                            Direction : in Direction_Type) is
      -- Recent Hall triggerings for this train
      Recent_Halls : Hall_Rec_Array;
      -- Data needed to change possible force turnout
      Front_Block    : Layout.Block_ID;       -- The block under front of train
      Front_Polarity : Layout.Block_Polarity; -- Current polarity of Front_Block
      Cab            : Cabs.Cab_ID;           -- Cab assigned to Front_Block
   begin
      -- Ignore request if train is already traveling in Direction
      if Direction /= Train_Info (Train).Direction then
         -- Reverse block polarities and the order of blocks in
         -- the protected list and change reservations
         Block_List (Train).Reverse_Order (Train);


         -- Take care of the rare case where a stopped train has one or both of
         -- its magnets over a Hall sensor

         -- Copy the recent Hall triggerings
         Recent_Halls := Train_Info (Train).Recent_Halls;
         -- Invalidate the train's recent Hall data so we won't reuse it
         Train_Info (Train).Recent_Halls (Front).Valid := False;
         Train_Info (Train).Recent_Halls (Rear).Valid := False;
         -- See if a magnet is still over a recently triggered Front Hall sensor
         if Recent_Halls (Front).Valid and then
           Halls.Is_Triggered (Recent_Halls (Front).Hall) then
            -- Update block powering and reservations as
            -- though the Hall was just triggered
            Update_Location (Recent_Halls (Front).Hall);
         end if;
         -- See if a magnet is still over a recently triggered Rear Hall sensor
         -- making sure not to process the same Hall twice (should it be both
         -- the most recently triggered Front and Rear)
         if (Recent_Halls (Rear).Hall /= Recent_Halls (Front).Hall and
               Recent_Halls (Rear).Valid) and then
           Halls.Is_Triggered (Recent_Halls (Rear).Hall) then
            -- Update block powering and reservations as
            -- though the Hall was just triggered
            Update_Location (Recent_Halls (Rear).Hall);
         end if;



         -- Handle possible force turnout
         if not Status (Train).Stopped then
            Front_Block := Block_List (Train).Front;
            -- Get the polarity of Front_Block
            Blocks.Cab_Assigned (Block    => Front_Block,
                                 Cab      => Cab,
                                 Polarity => Front_Polarity);
            Change_Force_Turnout (Train, Front_Block, Front_Polarity);
         end if;

         -- Display the current list of blocks for this train
         Display.Put (Train  => Train,
                      Blocks => Block_List (Train).Powered_Blocks);
         Train_Info (Train).Direction := Direction;
         -- Update the display with the new direction
         Display.Put (Train, Direction);
      end if;
   end Set_Direction;
   
   ----------------------------------------------------------------------------
   procedure Set_Throttle (Train : in Train_ID;
                           Value : in Cabs.Percent) is
      -- For slopes of line segments in throttle transformation
      type Line_Slope is delta 0.01 range 0.0 .. 100.0;
      Slope        : Line_Slope;
      Throttle     : Cabs.Percent;
      Min_Throttle : Cabs.Percent;
   begin
      -- Transform the throttle value into a throttle setting using
      -- the minimum throttle value for this train
      Min_Throttle := Train_Info (Train).Min_Throttle;
      if Value <= 10 then
         Slope    := Line_Slope (Min_Throttle) / 10;
         Throttle := Cabs.Percent (Value * Slope);
      else
         Slope    := (100.0 - Line_Slope (Min_Throttle)) / 90;
         Throttle := Min_Throttle + Cabs.Percent ((Value - 10) * Slope);
      end if;

      Cabs.Set (Cab   => Train_Info (Train).Cab,
                Value => Throttle);

      -- Only display Value if it differs from the last value
      if abs (Value - Train_Info (Train).Throttle) >= 1 then
         Display.Put (Train    => Train,
                      Throttle => Display.Percent (Value));
         Train_Info (Train).Throttle := Value;
      end if;
   end Set_Throttle;


   ----------------------------------------------------------------------------
   procedure Stop (Train : in Train_ID) is
   begin
      Status (Train).Dispatcher_Stop (Train);
      Speak ("Emergency stop train " & Train_ID'Image (Train));
   end Stop;


   ----------------------------------------------------------------------------
   procedure Stop (Train   : in Request_ID;
                   Turnout : in Layout.Turnout_ID) is
   begin
      if Train /= Dispatcher then
         Status (Train).Stop (Train, Turnout);
      else
         for Train in 1 .. Num_Trains_Running loop
            Status (Train).Stop (Train, Turnout);
         end loop;
      end if;
      Speak ("Failure turn out " & Layout.Turnout_ID'Image (Turnout));
   end Stop;


   ----------------------------------------------------------------------------
   procedure Start (Train  : in Train_ID) is
   begin
      Speak ("Starting train " & Train_ID'Image (Train));
      Status (Train).Dispatcher_Go;
   end Start;


   ----------------------------------------------------------------------------
   procedure Start (Turnout : in Layout.Turnout_ID) is
   begin
      for Train in 1 .. Num_Trains_Running loop
         Status (Train).Go (Turnout);
      end loop;
   end Start;


   ----------------------------------------------------------------------------
   procedure Initialize_Train (Train          : in  Train_ID;
                               Loco           : in  Locomotives.Loco_Rec;
                               Blocks_Under   : in  Layout.Search.Block_List;
                               Turnouts_Under : in  Layout.Search.Turnout_List;
                               Cab            : in  Cabs.Cab_ID) is
   begin
      -- Don't allow the train to move yet
      Cabs.Set_Limit (Cab   => Cab,
                      Value => 0);

      -- Save the Cab assigned to this train (two way look up)
      Train_Powered_By (Cab) := Train;
      Train_Info (Train).Cab  := Cab;

      -- Save the locomotive information
      Train_Info (Train).Name := Loco.Name;
      Train_Info (Train).Min_Throttle := Loco.Minimum_Throttle;
      -- Initialize the current direction
      Train_Info (Train).Direction := Forward;
      -- Initialize the current throttle setting
      Train_Info (Train).Throttle := 0;

      -- Clear all reasons for stopping the train
      -- except for dispatcher halt
      Status (Train).Clear_All_Set_Dispatcher;

      -- Process this train's list of blocks
      Block_List (Train).Clear;
      for Index in 1 .. Blocks_Under.Size loop
         -- Add the block to the train's list of blocks
         Block_List (Train).Add (Blocks_Under.Items (Index).Block);
         -- Power the block
         Blocks.Assign_Cab (Block    => Blocks_Under.Items (Index).Block,
                            Cab      => Cab,
                            Polarity => Blocks_Under.Items (Index).Direction);
      end loop;

      -- Attempt to reserve the next block in front of the train
      Reserve_Next_Block
        (Train         => Train,
         Current_Block => Blocks_Under.Items (Blocks_Under.Size).Block,
         Polarity      => Blocks_Under.Items (Blocks_Under.Size).Direction);

      -- Set the Turnouts under the train
      for Index in 1 .. Turnouts_Under.Size loop
         Turnouts.Set (Requestor => Train,
                       Turnout   => Turnouts_Under.Items (Index).Turnout,
                       Direction => Turnouts_Under.Items (Index).Direction);
      end loop;

      -- We assumed that the trains are initialized in ascending order by
      -- train ID.  So the last value tells us the number of trains.
      Num_Trains_Running := Train;

   end Initialize_Train;


   ----------------------------------------------------------------------------
   -- Sound operations for a train

   function Sound_Unit_Of (Train : in Train_ID) return Sound.Installed_Range is
      -- Convert a train number to a Dallee sound unit number
      pragma Inline (Sound_Unit_Of);
   begin
      -- Simple type conversion from Train_ID to Dallee_ID
      return Sound.Installed_Range (Train);
   end Sound_Unit_Of;

   procedure Bell_Off (Train : in Train_ID) is
   begin
      Sound.Bell_Off (Unit => Sound_Unit_Of (Train));
   end Bell_Off;

   procedure Bell_On (Train : in Train_ID) is
   begin
      Sound.Bell_On (Unit => Sound_Unit_Of (Train));
   end Bell_On;

   procedure Sound_Horn (Train : in Train_ID) is
   begin
      Sound.Sound_Horn (Unit   => Sound_Unit_Of (Train),
                        Signal => Sound.Approach_Highway);
   end Sound_Horn;


   ----------------------------------------------------------------------------
   -- The following functions return information on a train.

   function Loco_Name (Train : in Train_ID) return Locomotives.Loco_String is
   begin
      return Train_Info (Train).Name;
   end Loco_Name;

   function Why_Stopped (Train : in Train_ID) return Stop_Rec is
   begin
      return Status (Train).Stop_Reasons;
   end Why_Stopped;

   function Powered_Blocks (Train : in Train_ID) return Layout.Block_Array is
   begin
      return Block_List (Train).Powered_Blocks;
   end Powered_Blocks;

   function Direction (Train : in Train_ID) return Direction_Type is
   begin
      return Train_Info (Train).Direction;
   end Direction;


begin
   -- Set the ID of each of the train block lists and tasks
   for Train in Train_ID loop
      Block_List (Train).Set_ID (Train);
      Stopped_Train_Task (Train).Set_ID (Train);
   end loop;
end Trains;
