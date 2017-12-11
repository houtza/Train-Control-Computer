package Trains.Operations is
   ----------------------------------------------------------------------------
   -- Updates the position of a train based on the triggering of a Hall sensor
   --
   -- Preconditions  : Initialize_Train was previously called for Train
   --
   -- Postconditions : The location of the train that triggered Hall is updated
   --                  Message is sent to the Display
   procedure Update_Location (Hall : Layout.Hall_ID);

   ----------------------------------------------------------------------------
   -- Turn the train by changing the next choice turnout ahead of it
   --
   -- Preconditions  : Initialize_Train was previously called for Train
   --
   -- Postconditions : If the next choice turnout for the train is available
   --                     its direction is set to Direction.
   --                     Message is sent to the Display
   --                  Else
   --                     no turnout is changed
   procedure Turn (Train     : in Train_ID;
                   Direction : in Layout.Turn_Choice);

   ----------------------------------------------------------------------------
   -- Set the direction that the train travels
   --
   -- Preconditions  : Initialize_Train was previously called for Train
   --                  Train is not moving
   --
   -- Postconditions : Train's direction is set to Direction
   --                  Message is sent to the Display
   procedure Set_Direction (Train     : in Train_ID;
                            Direction : in Direction_Type);

   ----------------------------------------------------------------------------
   -- Set Train's throttle using the minimum throttle value for this train's
   -- locomotive.
   --    Values between 0% and 10% are transformed into throttle settings
   --       between 0% and the minimum thottle value for this locomotive.
   --    Values between 10% and 100% are transformed into throttle settings
   --       between the minimum throttle for this locomotive and 100%.
   --
   -- Preconditions  : Initialize_Train was previously called for Train
   --
   -- Postconditions : Train's throttle is set to the transformed Value
   --                  Value is sent to the Display
   procedure Set_Throttle (Train : in Train_ID;
                           Value : in Cabs.Percent);

   ----------------------------------------------------------------------------
   -- Called by Dispatcher or engineer to stop Train
   --
   -- Preconditions  : Initialize_Train was previously called for Train
   --
   -- Postconditions : Train is stopped
   --                  Message is sent to the Display
   procedure Stop (Train : in Train_ID);

   ----------------------------------------------------------------------------
   -- Called by a failed turnout to stop Train
   --
   -- Preconditions  : Initialize_Train was previously called for Train
   --
   -- Postconditions : Train is stopped (all trains are stopped
   --                                    if Train is Dispatcher)
   --                  Message is sent to the Display
   procedure Stop (Train   : in Request_ID;
                   Turnout : in Layout.Turnout_ID);

   ----------------------------------------------------------------------------
   -- Starts Train that was stopped by the dispatcher
   --
   -- Preconditions  : Initialize_Train was previously called for Train
   --
   -- Postconditions : If Train was stopped by a Dispatcher_Request, it is
   --                  restarted.  Otherwise, does nothing.
   --                  Message is sent to the Display
   procedure Start (Train : in Train_ID);

   ----------------------------------------------------------------------------
   -- Starts trains that were stopped by failure of Turnout
   --
   -- Preconditions  : Initialize_Train was previously called for all trains
   --
   -- Postconditions : Each trains stopped by failed Turnout is restarted
   --                  Message is sent to the Display
   procedure Start (Turnout : in Layout.Turnout_ID);


   ----------------------------------------------------------------------------
   -- Initialize a train
   --
   -- Assumptions    : There is at least one empty block between this train
   --                  and any other train.
   --                  Trains are intialized in ascending order by Train_ID
   --
   -- Preconditions  : The blocks in the list Blocks are reserved by Train.
   --
   -- Postconditions : Train is initialized and ready to run.
   --                  a)  Turnouts beneath the train are in the
   --                      correct position.
   --                  b)  Blocks beneath Train are powered by Cab
   procedure Initialize_Train (Train          : in  Train_ID;
                               Loco           : in  Locomotives.Loco_Rec;
                               Blocks_Under   : in  Layout.Search.Block_List;
                               Turnouts_Under : in  Layout.Search.Turnout_List;
                               Cab            : in  Cabs.Cab_ID);

   ----------------------------------------------------------------------------
   -- Sound operations for a train
   --
   -- Preconditions : Initialize_Train was previously Called for Train
   procedure Bell_Off (Train : in Train_ID);
   procedure Bell_On  (Train : in Train_ID);
   procedure Sound_Horn (Train : in Train_ID);

   ----------------------------------------------------------------------------
   -- The following functions return information on a train.
   --
   -- Preconditions : Initialize_Train was previously Called for Train

   function Loco_Name (Train : in Train_ID) return Locomotives.Loco_String;

   function Why_Stopped (Train : in Train_ID) return Stop_Rec;

   -- Note the blocks returned are in order from rear to front of train
   function Powered_Blocks (Train : in Train_ID) return Layout.Block_Array;


   function Direction (Train : in Train_ID) return Direction_Type;
end Trains.Operations;
