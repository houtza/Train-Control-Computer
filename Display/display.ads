-- This package supplies routines that objects call to display
-- information on the Dispather Screen.
--
-- For each train
--   Name
--   Direction of travel  (Forward or Backward)
--   Throttle Setting
--   Track blocks occupied
--   Status
--
-- For each turnout
--   Number
--   Direction (in or moving toward)
--   State (blinking indicates moving)
--
-- All calls are non blocking.
--
-- Written by John W. McCormick, May 2002

with Engineers;
with Layout;
with Locomotives;
with Trains;

package Display is

   subtype Percent is Integer range 0 .. 100;

   ----------------------------------------------------------------------------
   -- These two procedures control whether calls to the various Put procedures
   -- in this package simply discards the information or displays it on the
   -- Dispatcher Screen.  Initiallially the Display is disabled.

   -- Enables the Put procedures to display the information on the Screen.
   -- Headings, labels, and current status of all Trains and Turnouts are
   -- displayed on the screen before information from calls to Put.
   procedure Enable;

   -- Disables the Put procedures.  All information from calls to Put is
   -- discarded.
   procedure Disable;

   ----------------------------------------------------------------------------
   -- Display the Direction of the given Turnout
   -- If Moving, then the information displayed blinks
   procedure Put (Turnout   : in Layout.Turnout_ID;
                  Direction : in Layout.Turn_Choice;
                  Moving    : in Boolean);

   ----------------------------------------------------------------------------
   -- Display the Name of the given train
   procedure Put (Train : in Trains.Train_ID;
                  Name  : in Locomotives.Loco_String);

   ----------------------------------------------------------------------------
   -- Display the direction in which the train is running
   procedure Put (Train     : in Trains.Train_ID;
                  Direction : in Trains.Direction_Type);

   ----------------------------------------------------------------------------
   -- Display Train's throttle setting
   procedure Put (Train    : in Trains.Train_ID;
                  Throttle : in Percent);

   ----------------------------------------------------------------------------
   -- Display the Status of the given train on its three status lines
   procedure Put (Train  : in Trains.Train_ID;
                  Status : in Trains.Stop_Rec);

   ----------------------------------------------------------------------------
   -- Display the list of blocks that the given train is on
   --
   -- Precondition  : Blocks'Length <= 10
   procedure Put (Train  : in Trains.Train_ID;
                  Blocks : in Layout.Block_Array);

   ----------------------------------------------------------------------------
   -- Display an error message at the bottom of the screen.  Meant for
   -- general system failure messages.  Errors are numbered from 0 to 255 and
   -- the string is truncated to 75 characters.  Older error messages are
   -- replaced by newer error messages.
   procedure Put_Error (Error_Message : in String);

end Display;
