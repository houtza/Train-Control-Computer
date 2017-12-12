with Layout;
with Trains;

package Turnout_Test_IO is

   -- This package supplies library level procedures that are used as callbacks
   -- during the testing of the turnout package.  Each procedure simply
   -- displays its parameters.

   procedure Display_Failure (Requestor : in Trains.Request_ID;
                              Turnout   : in Layout.Turnout_ID);

   procedure Display_Recover (Turnout : in Layout.Turnout_ID);

   procedure Display_Change (Turnout   : in Layout.Turnout_ID;
                             Direction : in Layout.Turn_Choice;
                             Moving    : in Boolean);

end Turnout_Test_IO;
