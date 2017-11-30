-- This package contains the types and operations for the engineers
--
-- Written by John McCormick, June 2002
-- Modified by Andrew Berns, October 2017
with Trains;
with Ada.Real_Time;
with Hand_Controllers;

package Engineers is
   Max_Engineers : constant := Trains.Max_Trains;

   -- ID zero is for Dispatcher
   type Engineer_Range is range 0 .. Max_Engineers;
   subtype Engineer_ID is Engineer_Range range 1 .. Max_Engineers;

   -- Number of seconds throttle must be off to say that a train is stopped
   Stop_Duration : constant Ada.Real_Time.Time_Span :=
                     Ada.Real_Time.To_Time_Span (3.0);

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

end Engineers;
