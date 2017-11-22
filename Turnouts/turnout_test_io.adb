with Ada.Text_IO;
package body Turnout_Test_IO is

   -----------------------------------------------------------------------------
   procedure Display_Failure (Requestor : in Trains.Request_ID;
                              Turnout   : in Layout.Turnout_ID) is
   begin
      Ada.Text_IO.Put_Line ("Turnout failure:      Turnout"  &
                            Layout.Turnout_ID'Image (Turnout)&
                            "  Train "                       &
                            Trains.Request_ID'Image (Requestor));
   end Display_Failure;

   -----------------------------------------------------------------------------
   procedure Display_Recover (Turnout : in Layout.Turnout_ID) is
   begin
      Ada.Text_IO.Put_Line ("Turnout recovery:     Turnout"  &
                            Layout.Turnout_ID'Image (Turnout));
   end Display_Recover;

   -----------------------------------------------------------------------------
   procedure Display_Change (Turnout   : in Layout.Turnout_ID;
                             Direction : in Layout.Turn_Choice;
                             Moving    : in Boolean) is
   begin
      Ada.Text_IO.Put ("Turnout " &
                       Layout.Turnout_ID'Image (Turnout) &
                       " is ");
      if Moving then
         Ada.Text_IO.Put ("moving ");
      end if;

      Ada.Text_IO.Put_Line (Layout.Turn_Choice'Image (Direction));
   end Display_Change;

end Turnout_Test_IO;
