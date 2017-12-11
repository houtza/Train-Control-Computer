with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Console_Management;   use Console_Management;

with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Exceptions;

with Blocks;
with Cabs;
with Command;              use Command;
with Display;
with DoubleTalk;           use DoubleTalk;  use DoubleTalk.Phrase_Strings;
with Engineers;
with Engineers.Operations;
with Halls;
with Hand_Controllers;
with Layout;
with Layout.Search;
with Locomotives;          use Locomotives;
with Trains;
with Trains.Operations;
with Turnouts;

procedure Dispatcher is

   -- The main program for the train project

   -- Written by John McCormick, June 2002
   -- Rewritten by John McCormick, June 2008

   use type Layout.Block_ID;

   -- The range of trains we can run
   subtype Train_Count is Integer range 1 .. Trains.Max_Trains;

   -- Input for single character user response
   type Response_Letter is (Q, Y, N, R);
   subtype Yes_No_Quit     is Response_Letter range Q .. N;
   subtype Normal_Reversed is Response_Letter range N .. R;
   package Response_IO is new Ada.Text_IO.Enumeration_IO (Response_Letter);


   -- States and substates
   type State_Type is (Run, Setup, Start, Shutdown,
                       Location, Reserve, Confirm);
   subtype Setup_State   is State_Type range Start .. Confirm;
   subtype Run_State     is State_Type range Run .. Shutdown;
   subtype Pre_Run_State is State_Type range Setup .. Shutdown;

   -----------------------------------------------------------------------------
   procedure Speak (Phrase : in String) is
      -- Call DoubleTalk to speak Phrase
      pragma Inline (Speak);
   begin
      DoubleTalk.Speak (Phrase => To_Bounded_String (Phrase),
                        Voice  => Paul);
   end Speak;


   -----------------------------------------------------------------------------
   procedure Display_Setup_Title (Train : in Train_Count;
                                  Info  : in String) is
   -- Display the train number and information on the top of the screen

      Centered : String (1 .. 80);
   begin
      Clear_Screen;

      Set_Cursor (Row => 0, Column => 31);
      Set_Blink;
      Put ("Set up of Train #");
      Put (Item => Train, Width => 1);
      Cancel_Blink;

      New_Line;
      Ada.Strings.Fixed.Move (Source => Info,
                              Target => Centered,
                              Justify => Ada.Strings.Center);
      Set_Cursor (Row => 1, Column => 0);
      Put (Centered);
      New_Line;
   end Display_Setup_Title;


   -----------------------------------------------------------------------------
   procedure Get_Choice (Choice : out Yes_No_Quit) is
   -- Get a valid response from the user
   begin
      loop
         begin
            Response_IO.Get (Choice);
            exit;
         exception
            when Constraint_Error =>
               null;    -- Error message below
            when Data_Error =>
               Skip_Line;
         end;
         Set_Text_Color (LightRed);
         Put_Line ("Invalid choice.  Please enter Y, N, or Q");
         Set_Text_Color (White);
      end loop;
   end Get_Choice;


   ------------------------------------------------------------------------
   procedure Get_Block (Error_Prompt : in  String;
                        Block        : out Layout.Block_ID) is
   -- Get a valid block ID from the user
      Value : Integer;  -- Input value
   begin
      Validity_Loop :
      loop
         begin
            Get (Value);
            Block := Layout.Block_ID (Value);  -- Cast Integer to Block_ID
            exit Validity_Loop;
         exception
            when Data_Error =>
               Skip_Line;
            when Constraint_Error =>
               null;
         end;
         Set_Text_Color (LightRed);
         Put_Line ("Invalid entry.  " & Error_Prompt);
         Set_Text_Color (White);
      end loop Validity_Loop;
   end Get_Block;


   ------------------------------------------------------------------------
   procedure Get_Direction (Direction : out Layout.Block_Polarity) is
   -- Get a valid block direction from the user

      Letter : Normal_Reversed;
   begin
      Validity_Loop :
      loop
         New_Line;
         Set_Text_Color (LightMagenta);
         Put_Line ("What is the direction of the locomotive on the block?");
         Put_Line ("   N = Normal");
         Put_Line ("   R = Reversed");
         Set_Text_Color (White);
         begin
            Response_IO.Get (Letter);
            exit Validity_Loop;
         exception
            when Constraint_Error =>
               null;  -- Error message below
            when Data_Error =>
               Skip_Line;
         end;
         Set_Text_Color (LightRed);
         Put_Line ("Invalid direction.  Please enter n or r.");
         Set_Text_Color (White);
      end loop Validity_Loop;
      if Letter = N then
         Direction := Layout.Normal;
      else
         Direction := Layout.Reversed;
      end if;
   end Get_Direction;


   ------------------------------------------------------------------------
   procedure Get_Train_Location (Train      : in  Train_Count;
                                 Train_Name : in  String;
                                 Blocks     : out Layout.Search.Block_List;
                                 Turnouts   : out Layout.Search.Turnout_List;
                                 Next_State : out Setup_State) is
      -- Get train location information from the user
      --
      -- Preconditions  : None
      --
      -- Postconditions : Next_State is Reserve and
      --                  Blocks is a list of blocks beneath the train and
      --                  Turnouts is a list of turnouts beneath the train and
      --                     or
      --                  Next_State is Location, Start, or Shutdown and
      --                  Blocks is an empty list and
      --                  Turnouts is an empty list

      -- Location information from user
      Loco_Block    : Layout.Block_ID;
      Caboose_Block : Layout.Block_ID;
      Direction     : Layout.Block_Polarity;

      Success  : Boolean;     -- of layout search
      Response : Yes_No_Quit;

   begin
      -- Get the blocks of the locomotive and caboose from the user

      Clear_Screen;
      Display_Setup_Title (Train, Train_Name);
      New_Line;
      Set_Text_Color (LightMagenta);
      Put ("On which block is the locomotive pulling train ");
      Put (Item => Train, Width => 1);
      Put_Line (" located?");
      Set_Text_Color (White);
      Get_Block (Error_Prompt => "Enter block that the locomotive is on.",
                 Block        => Loco_Block);
      New_Line;
      Set_Text_Color (LightMagenta);
      Put ("On which block is the caboose for train ");
      Put (Item => Train, Width => 1);
      Put_Line (" located?");
      Set_Text_Color (White);
      Get_Block (Error_Prompt => "Enter block that the caboose is on.",
                 Block        => Caboose_Block);

      -- Use the information from the user to fill in the lists

      if Loco_Block = Caboose_Block then
         -- When the train is on a single block, we need to know in
         -- which direction the locomotive is facing
         Get_Direction (Direction);

         -- Just one block beneath the train
         Blocks.Size      := 1;
         Blocks.Items (1) := (Loco_Block, Direction);

         -- No turnouts beneath the train
         Turnouts.Size := 0;

         Next_State := Reserve;

      else
         Layout.Search.Blocks_Beneath (Loco     => Loco_Block,
                                       Caboose  => Caboose_Block,
                                       Blocks   => Blocks,
                                       Turnouts => Turnouts,
                                       Success  => Success);
         if Success then
            Next_State := Reserve;

         else
            Blocks.Size   := 0;
            Turnouts.Size := 0;

            New_Line;
            Set_Text_Color (LightRed);
            Put_Line ("   The maximum number of blocks beneath a train is 3.");
            Put_Line ("   There are more than 3 blocks beneath your train.");
            New_Line;
            Set_Text_Color (LightMagenta);
            Put_Line ("Would you like to reenter the information?");
            Put_Line
              ("   Y = yes, I wish to reenter this train's location.");
            Put_Line
              ("   N = no,  I wish to restart set up from the beginning.");
            Put_Line
              ("   Q = no,  I wish to terminate this operating session.");
            Set_Text_Color (White);

            Get_Choice (Response);
            case Response is
               when Y =>
                  Next_State := Location;  -- We'll try again
               when N =>
                  Next_State := Start;     -- Start from the beginning
               when Q =>
                  Next_State := Shutdown;  -- Shut down the system
            end case;
         end if;
      end if;

   end Get_Train_Location;


   -----------------------------------------------------------------------------
   procedure Reserve_Blocks (Train      : in  Train_Count;
                             My_Blocks  : in  Layout.Search.Block_List;
                             Next_State : out Setup_State) is
      -- Reserve all of the Blocks under the Train
      --
      -- Preconditions  : None
      --
      -- Postconditions : Next_State is Confirm and
      --                  All blocks in My_Blocks are reserved by train
      --                     or
      --                  Next_State is Location, Start, or Shutdown and
      --                  No blocks are reserved for Train

      Index    : Positive;         -- for block list
      My_ID    : Trains.Train_ID;
      Success  : Boolean;          -- of reservation attempt
      Response : Yes_No_Quit;
   begin
      My_ID := Trains.Train_ID (Train);

      -- Clear any reservations already held by this train
      Blocks.Clear_Reservations (Train => My_ID);

      -- Attempt to reserve all of the blocks in the list Blocks
      -- Each iteration, attempt to make one reservation
      Index := 1;
      Reserve_Loop :
      loop
         Blocks.Reserve (Block   => My_Blocks.Items (Index).Block,
                         Train   => My_ID,
                         Success => Success);
         exit Reserve_Loop when not Success or Index = My_Blocks.Size;
         Index := Index + 1;
      end loop Reserve_Loop;

      if Success then
         Next_State := Confirm;

      else
         Blocks.Clear_Reservations (Train => Trains.Train_ID (Train));
         New_Line;
         Set_Text_Color (LightRed);
         Put ("The location information for Train ");
         Put (Item => Train, Width => 1);
         Put_Line (" is not valid because");
         Put_Line ("it conflicts with a train you entered earlier");
         New_Line;
         Set_Text_Color (LightMagenta);
         Put_Line ("Would you like to reenter the information?");
         Put_Line ("   Y = yes, I wish to reenter this train's location.");
         Put_Line ("   N = no,  I wish to restart set up from the beginning.");
         Put_Line ("   Q = no,  I wish to terminate this operating session.");
         Set_Text_Color (White);

         Get_Choice (Response);
         case Response is
            when Y =>
               Next_State := Location;  -- We'll try again
            when N =>
               Next_State := Start;     -- Start from the beginning
            when Q =>
               Next_State := Shutdown;  -- Shut down the system
         end case;
      end if;
   end Reserve_Blocks;


   ------------------------------------------------------------------------
   procedure Display_Turnouts is
   -- Get and display the status of all turnouts
      Status : Turnouts.Status_Rec;
   begin
      for Turnout in Layout.Turnout_ID loop
         Status := Turnouts.Status (Turnout);
         Display.Put  (Turnout   => Turnout,
                       Direction => Status.Current,
                       Moving    => Status.Moving);
      end loop;
   end Display_Turnouts;

   ------------------------------------------------------------------------
   procedure Display_Trains (Num_Trains : in Train_Count) is
   -- Get and display the status of all trains
   begin
      for Train in 1 .. Trains.Train_ID (Num_Trains) loop
         Display.Put (Train => Train,
                      Name  => Trains.Operations.Loco_Name (Train));
         Display.Put (Train     => Train,
                      Direction => Trains.Operations.Direction (Train));
         Display.Put (Train    => Train,
                      Throttle => 0);
         Display.Put (Train  => Train,
                      Status => Trains.Operations.Why_Stopped (Train));
         Display.Put (Train  => Train,
                      Blocks => Trains.Operations.Powered_Blocks (Train));
      end loop;
   end Display_Trains;

   ------------------------------------------------------------------------
   procedure Enable_Session (Num_Trains : in Train_Count) is
      -- Enables an operating session
      --
      -- Preconditions  :  Num_Trains have been intialized

      Controller : Hand_Controllers.Hand_Controller_ID;
      Engineer   : Engineers.Engineer_ID;
   begin

      -- Setup the display
      Clear_Screen;
      Display.Enable;
      Display_Turnouts;
      Display_Trains (Num_Trains);
      delay 2.0;

      -- Enable the engineer tasks for all the trains
      for Train in 1 .. Num_Trains loop
         -- Convert a train number into a controller letter A=1, B=2, C=3
         Controller := Hand_Controllers.Hand_Controller_ID'Val (Train - 1);
         Engineer   := Engineers.Engineer_ID (Train);
         Engineers.Operations.Enable (Engineer   => Engineer,
                                      Train      => Trains.Train_ID (Train),
                                      Controller => Controller);
      end loop;

      -- Set up the turnout callbacks
      Turnouts.Set_Failure_Callback  (To => Trains.Operations.Stop'Access);
      Turnouts.Set_Recovery_Callback (To => Trains.Operations.Start'Access);
      Turnouts.Set_Change_Callback   (To => Display.Put'Access);

      -- Enable Hall sensors
      Halls.Initialize;
      Halls.Enable (Trains.Operations.Update_Location'Access);
   end Enable_Session;

   ------------------------------------------------------------------------
   procedure Disable_Session (Num_Trains : in  Train_Count) is
   -- Halt the trains
   -- Disable the Engineer tasks, Hall senors, and display
   begin
      -- Stop all trains
      for Train in 1 .. Trains.Train_ID (Num_Trains) loop
         Trains.Operations.Stop (Train);
      end loop;

      -- Disable the display
      Display.Disable;

      -- Disable Hall sensors
      Halls.Disable;

      -- Disable Engineer tasks
      for Engineer in 1 .. Engineers.Engineer_ID (Num_Trains) loop
         Engineers.Operations.Disable (Engineer);
      end loop;

   end Disable_Session;

   ----------------------------------------------------------------------------
   procedure Get_New_Locomotive (Train : in  Train_Count;
                                 Loco  : out Locomotives.Loco_Rec) is
   -- Gets the information for a locomotive not listed in package Locomotives

      Name   : String (1 .. 20);
      Model  : String (1 .. 13);
      Number : String (1 .. 4);
      Length : Natural;         -- String Length
   begin
      Display_Setup_Title (Train => Train,
                           Info  => "Locomotive Information");
      Set_Text_Color (LightMagenta);

      Put_Line ("What is the road name of the locomotive?");
      Set_Text_Color (White);
      Get_Line (Item => Name,
                Last => Length);
      if Length = Name'Last then
         Skip_Line;  -- Need to advance reading marker when string is filled
      else
         -- Pad with blanks
         Name (Length + 1 .. Name'Last) := (others => ' ');
      end if;

      Set_Text_Color (LightMagenta);
      Put_Line ("What model is the locomotive?");
      Set_Text_Color (White);
      Get_Line (Item => Model,
                Last => Length);
      if Length = Model'Last then
         Skip_Line;  -- Need to advance reading marker when string is filled
      else
         -- Pad with blanks
         Model (Length + 1 .. Model'Last) := (others => ' ');
      end if;

      Set_Text_Color (LightMagenta);
      Put_Line ("What is the number on the locomotive?");
      Set_Text_Color (White);
      Get_Line (Item => Number,
                Last => Length);
      if Length = Number'Last then
         Skip_Line;  -- Need to advance reading marker when string is filled
      else
         -- Pad with blanks
         Number (Length + 1 .. Number'Last) := (others => ' ');
      end if;

      Loco.Name := Name & ' ' & Model & " #" & Number;

      Throttle_Validation_Loop :
      loop
         Set_Text_Color (LightMagenta);
         Put_Line ("Enter the percent throttle at which this locomotive " &
                   "just begins to move.");
         Put_Line ("   (If you are not sure of this value, enter 20)");
         Set_Text_Color (White);
         begin
            Get (Loco.Minimum_Throttle);
            exit Throttle_Validation_Loop;
         exception
            when Constraint_Error =>
               null;   -- Error message below
            when Data_Error =>
               Skip_Line;
         end;
         Set_Text_Color (LightRed);
         Put_Line ("You must enter number between 1 and 100");
         Set_Text_Color (White);
         New_Line;
      end loop Throttle_Validation_Loop;
   end Get_New_Locomotive;


   ----------------------------------------------------------------------------
   procedure Choose_Locomotive (Train : in  Train_Count;
                                Loco  : out Locomotives.Loco_Rec) is
      -- Obtains the information for a locomotive
      --
      -- Preconditions  : None
      --
      -- Postconditions : Loco data returned

      subtype Choice_Range is Positive range 1 .. Available_Locos'Last + 1;
      Choice : Choice_Range;

   begin
      Display_Setup_Title (Train => Train,
                           Info  => "");
      Set_Text_Color (LightMagenta);
      Put_Line ("The following locomotive choices are available");
      New_Line;

      -- Display available choices plus "Other"
      Put_Line ("      Road Name            Model        Number");
      Set_Text_Color (White);
      for Index in Available_Locos'Range loop
         Put (Item  => Index,
              Width => 3);
         Put ("   ");
         Put_Line (Available_Locos (Index).Name);
      end loop;
      Put (Item  => Available_Locos'Last + 1,
           Width => 3);
      Put_Line ("   Other");
      New_Line;

      -- Get a valid locomotive index number
      -- Each iteration, process one number
      Index_Validation_Loop :
      loop
         Set_Text_Color (LightMagenta);
         Put ("Enter the line number from the above table of the " &
              "locomotive pulling Train #");
         Put (Item  => Train,
              Width => 1);
         Set_Text_Color (White);
         New_Line;
         begin
            Get (Choice);
            Skip_Line;
            exit Index_Validation_Loop;
         exception
            when Constraint_Error =>
               null;    -- Error message below
            when Data_Error =>
               Skip_Line;
         end;
         Set_Text_Color (LightRed);
         Put ("You must enter number between 1 and ");
         Put (Item  => Available_Locos'Last + 1,
              Width => 1);
         New_Line;
         Set_Text_Color (White);
      end loop Index_Validation_Loop;

      if Choice <= Available_Locos'Last then
         Loco := Available_Locos (Choice);
      else
         Get_New_Locomotive (Train, Loco);
      end if;
   end Choose_Locomotive;


   ----------------------------------------------------------------------------
   procedure Confirm_Train_Info (Train    : in  Train_Count;
                                 Loco     : in  Locomotives.Loco_Rec;
                                 Blocks   : in  Layout.Search.Block_List;
                                 Response : out Response_Letter) is
      -- Confirm the train information with the user
      --
      -- Preconditions  : None
      --
      -- Postconditions : Response indicates user confirmation of data


   begin
      Display_Setup_Title (Train => Train,
                           Info  => "Confirmation of Train Information");
      New_Line (2);
      Put (Loco.Name);
      New_Line (2);
      Put ("Locomotive on block  ");
      Put (Item  => Positive (Blocks.Items (Blocks.Size).Block),
           Width => 3);
      New_Line;
      Put ("Caboose    on block  ");
      Put (Item  => Positive (Blocks.Items (1).Block),
           Width => 3);
      New_Line (2);
      Put ("Train occupies blocks");
      for Index in 1 .. Blocks.Size loop
         Put (Item  => Positive (Blocks.Items (Index).Block),
              Width => 3);
         if Index /= Blocks.Size then
            Put (',');
         end if;
      end loop;
      New_Line (2);

      Set_Text_Color (LightMagenta);
      Put_Line ("Is this information correct?");
      Put_Line
        ("   Y = yes, the information for this train is correct.");
      Put_Line
        ("   N = no,  I wish to enter different information for this train.");
      Put_Line
        ("   R = no,  I wish to restart setting up from the beginning.");
      Put_Line
        ("   Q = no,  I wish to quit and shut down the system");
      Set_Text_Color (White);

      Validation_Loop :
      loop
         begin
            Response_IO.Get (Response);
            exit Validation_Loop;
         exception
            when Data_Error =>
               Skip_Line;
               Set_Text_Color (LightRed);
               Put_Line ("Please enter Y, N, R, or Q");
               Set_Text_Color (White);
         end;
      end loop Validation_Loop;

   end Confirm_Train_Info;


   -----------------------------------------------------------------------------
   procedure Set_Up_One_Train (Train      : in  Train_Count;
                               Next_State : out Pre_Run_State) is
      -- Set up one train using information from the user
      --    Blocks beneath are reserved and powered
      --    Turnouts beneath are set correctly
      --    Train data structures initialized
      --
      -- Preconditions  : none
      --
      -- Postconditions : Next_State is Setup and Train is initialzed
      --                    or
      --                  Next_State is Start or Shutdown and
      --                  Train is not initialized

      My_Loco     : Locomotives.Loco_Rec;
      My_Blocks   : Layout.Search.Block_List (Max_Size => 3);
      My_Turnouts : Layout.Search.Turnout_List (Max_Size => 12);
      My_Cab      : Cabs.Control_Cab_ID;
      My_ID       : Trains.Train_ID;

      Response : Response_Letter;  -- For user confirmation

      State : Setup_State;  -- Curent substate within state Setup


   begin
      State := Location;

      -- Set up one train
      -- Each iteration, process one set up state
      Train_Loop :
      loop
         case State is

            when Location => -- Get the location of the train

               -- Get the information on the locomotive pulling this train
               Choose_Locomotive (Train, My_Loco);

               -- Determine the blocks and turnouts beneath the train
               Get_Train_Location (Train      => Train,
                                   Train_Name => My_Loco.Name,
                                   Blocks     => My_Blocks,
                                   Turnouts   => My_Turnouts,
                                   Next_State => State);

            when Reserve =>
               -- We have a valid location for this train.
               -- Now try to reserve its blocks.
               Reserve_Blocks (Train      => Train,
                               My_Blocks  => My_Blocks,
                               Next_State => State);

            when Confirm =>
               -- Confirm the train information with the user
               Confirm_Train_Info (Train, My_Loco, My_Blocks, Response);

               case Response is
                  when Y =>
                     -- Use the acquired data to complete the initialization
                     --    Connect the blocks beneath to the train's cab.
                     --    Set the turnouts beneath the train
                     --    Initialize train data structures

                     -- Cab number is the same as train number
                     My_Cab := Cabs.Control_Cab_ID (Train);
                     My_ID  := Trains.Train_ID (Train);

                     Trains.Operations.Initialize_Train
                       (Train          => My_ID,
                        Loco           => My_Loco,
                        Blocks_Under   => My_Blocks,
                        Turnouts_Under => My_Turnouts,
                        Cab            => My_Cab);

                     Next_State := Setup;
                     return;

                  when N =>
                     -- Clear the reservations made for this train
                     Blocks.Clear_Reservations (Train => My_ID);
                     State := Location;

                  when Q =>
                     -- Clear the reservations made for all trains
                     Blocks.Clear_All_Reservations;
                     Next_State := Shutdown;
                     return;

                  when R =>
                     -- Clear the reservations made for all trains
                     Blocks.Clear_All_Reservations;
                     Next_State := Start;
                     return;
               end case;


            when Start | Shutdown =>
               -- Clear the reservations made for all trains
               Blocks.Clear_All_Reservations;
               Next_State := State;
               return;   -- without initializing Train

         end case;
      end loop Train_Loop;

   end Set_Up_One_Train;


   ----------------------------------------------------------------------------
   procedure Set_Up_Trains (Num_Trains : in  Train_Count;
                            Next_State : out Run_State) is

      -- Set up the trains
      --
      -- Preconditions  : none
      --
      -- Postconditions : Next_State is Run and
      --                  all trains are initialzed
      --                     or
      --                  Next_State is Start or Shutdown and
      --                  no trains are initialized

   begin
      -- Set up all the trains for this operating session
      -- Each iteration, set up one train
      for Train in 1 .. Num_Trains loop
            Set_Up_One_Train (Train, Next_State);

         -- Did the user want to abandon setting up the trains?
         if Next_State /= Setup then
            return;  -- with state from Set_Up_One_Train
         end if;

      end loop;
      Next_State := Run;  -- all trains were successfully set up
   end Set_Up_Trains;


   ----------------------------------------------------------------------------
   procedure Get_Number_Of_Trains (Num_Trains : out Train_Count) is
      -- Gets the number of trains for the operating session
   begin
      Clear_Screen;
      Set_Text_Color (LightMagenta);
      Set_Cursor (Row => 8, Column => 27);
      Put_Line ("Flying Scottsman Software");
      New_Line (3);
      Put_Line ("Set up 1, 2, or 3 trains");
      New_Line;
      Put_Line ("There must be at least one unoccupied block between " &
                "each pair of trains");
      New_Line (3);

      -- Get the number of trains to run in this session
      -- Each iteration, process one input value
      Validation_Loop :
      loop
         Set_Text_Color (LightMagenta);
         Put_Line ("How many trains do you wish to run (1, 2, or 3)?");
         Set_Text_Color (White);
         begin
            Get (Num_Trains);
            Skip_Line;
            exit Validation_Loop;
         exception
            when Constraint_Error =>
               null;   -- Error message below
            when Data_Error =>
               Skip_Line;
         end;
         Set_Text_Color (LightRed);
         Put_Line ("You must enter either 1, 2, or 3");
         New_Line;
         Set_Text_Color (White);
      end loop Validation_Loop;

   end Get_Number_Of_Trains;


   ----------------------------------------------------------------------------
   procedure Process_Commands (Num_Trains : in  Train_Count;
                               State      : out Run_State) is
      -- Process keyboard commands for a session
      --
      -- Preconditions  : All trains are initialized (ready to run)
      --
      -- Postconditions : Operating session is complete
      --                  State is either Start (to start a new session) or
      --                  Shutdown (to terminate the program)

      The_Command : Command.Command_Rec;
   begin
      -- Set up keyboard input
      Set_Raw_Mode;
      Disable_Echo;

      State := Run;
      -- Each iteration, process one keyboard command
      Command_Loop :
      loop
         Command.Get (The_Command);

         -- Process the command
         case The_Command.Which is
            when Stop_All =>
               for Train in 1 .. Trains.Train_ID (Num_Trains) loop
                  Trains.Operations.Stop (Train);
               end loop;
            when Stop =>
               if Positive (The_Command.Train) <= Num_Trains then
                  Trains.Operations.Stop (The_Command.Train);
               else
                  Speak ("Invalid command");
               end if;

            when Go =>
               if Positive (The_Command.Train) <= Num_Trains then
                  Trains.Operations.Start (The_Command.Train);
               else
                  Speak ("Invalid command");
               end if;

            when Right =>
               Turnouts.Set (Requestor => Trains.Dispatcher,
                             Turnout   => The_Command.Turnout,
                             Direction => Layout.Right);
            when Left =>
               Turnouts.Set (Requestor => Trains.Dispatcher,
                             Turnout   => The_Command.Turnout,
                             Direction => Layout.Left);
            when Free =>
               Blocks.Release (Train => Trains.Dispatcher,
                               Block => The_Command.Block);
            when Restart =>
               State := Start;

            when Quit =>
               State := Shutdown;

            when Error =>
               Speak ("Invalid command");
         end case;

         exit Command_Loop when State /= Run;
      end loop Command_Loop;

      -- Return keyboard input to normal
      Set_Cooked_Mode;
      Enable_Echo;
      Clear_Screen;
   end Process_Commands;


-------------------------------------------------------------------------------

   State      : Run_State;   -- The state of the system
   Num_Trains : Train_Count;  -- Number of trains to run in a session

begin
   State := Start;

   -- Conduct zero or more operating sessions
   -- Each iteration, process one stage of a session
   Session_Loop :
   loop
      case State is
         when Start =>
            Speak ("Ready to begin a new operating session");

            -- Connect all blocks to the null cab
            for Block in Layout.Block_ID loop
               Blocks.Assign_Cab (Block    => Block,
                                  Cab      => Cabs.Null_Cab,
                                  Polarity => Layout.Normal);
            end loop;
            -- Clear all the reservations
            Blocks.Clear_All_Reservations;

            -- Turn off all turnout callbacks
            Turnouts.Set_Failure_Callback  (To => null);
            Turnouts.Set_Recovery_Callback (To => null);
            Turnouts.Set_Change_Callback   (To => null);

            Get_Number_Of_Trains (Num_Trains);
            State := Setup;

         when Setup =>
            Set_Up_Trains (Num_Trains, State);

         when Run =>
            Enable_Session (Num_Trains);
            Process_Commands (Num_Trains, State);
            Disable_Session (Num_Trains);
         when Shutdown =>
            exit Session_Loop;
      end case;

   end loop Session_Loop;

   Set_Cursor (Row => 8, Column => 0);
   Put_Line ("Shutting down the system, please wait");
   New_Line;
   Put_Line ("   Moving all turnouts to their default positions");
   New_Line (3);
   Turnouts.Shut_Down;
   Speak ("The system has been shut down");
   Put_Line ("The system has been shut down");
   Put_Line ("Please turn off the power");

exception
   when The_Error : others =>
      Put_Line ("An exception has occurred in the dispatcher program");
      Put_Line (Ada.Exceptions.Exception_Name (The_Error));
      Speak ("An exception has occurred in the dispatcher program");
end Dispatcher;
