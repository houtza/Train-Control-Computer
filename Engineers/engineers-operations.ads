package Engineers.Operations is
   -----------------------------------------------------------------------------
   -- Assigns a train and a hand controller to an engineer
   --
   -- Preconditions  : Engineer is not currently enabled
   --                  Train is not currently assigned to an engineer
   --                  Controller is not currently assigned to an engineer
   --
   -- Postconditions : The Engineer is assigned Train and Controller
   --                  The engineer's skill level is Novice (we will not
   --                     implement any other skill levels)
   procedure Enable (Engineer   : in Engineer_ID;
                     Train      : in Trains.Train_ID;
                     Controller : in Hand_Controllers.Hand_Controller_ID);

   -----------------------------------------------------------------------------
   -- Removes all resources assigned to the Engineer which ceases to command
   -- the Train.
   --
   -- Preconditions  : The train assigned to Engineer is stopped
   --
   -- Postconditions : No train or hand controller is assigned to engineer.
   --                  The Engineer no longer takes input from Controller and
   --                  no longer commands a train.
   procedure Disable (Engineer : in Engineer_ID);
end Engineers.Operations;
