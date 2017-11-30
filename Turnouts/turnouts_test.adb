with Layout; use Layout;
with Turnouts;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Turnout_Test_IO;
with DoubleTalk; use DoubleTalk; use DoubleTalk.Phrase_Strings;
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
procedure Turnouts_Test is

-- A simple test program for the turnout package

   Turnout : Integer;
   Char    : Character;

begin
   Speak (Phrase => To_Bounded_String ("Beginning turn out tests"),
          Voice  => Paul);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Turnout test program");
   Ada.Text_IO.New_Line;

   -- Set up the callback routines

   Turnouts.Set_Failure_Callback
     (To => Turnout_Test_IO.Display_Failure'Access);
   Turnouts.Set_Recovery_Callback
     (To => Turnout_Test_IO.Display_Recover'Access);
   Turnouts.Set_Change_Callback (To => Turnout_Test_IO.Display_Change'Access);

   delay 5.0;

   -- Go through all of the turnouts and change to right then left
   for Turnout in Layout.Turnout_ID range 1 .. 24 loop
      for Direction in reverse Layout.Turn_Choice loop
         Speak (Phrase => To_Bounded_String ("Changing turn out number  " &
                Turnout_ID'Image (Turnout) & " to " &
                Turn_Choice'Image (Direction)),
                Voice  => Paul);
         Turnouts.Set (Requestor => 0,
                       Turnout   => Turnout,
                       Direction => Direction);
         delay 5.0;
      end loop;
   end loop;

   Speak (Phrase => To_Bounded_String ("Beginning manual tests"),
          Voice  => Paul);

   loop
      Ada.Text_IO.Put_Line ("Enter Turnout number and direction (L or R)");
      begin
         Ada.Integer_Text_IO.Get (Turnout);
         Ada.Text_IO.Get (Char);

         if Char = 'l' or Char = 'L' then
            Turnouts.Set (Requestor => 0,
                          Turnout   => Layout.Turnout_ID (Turnout),
                          Direction => Layout.Left);
         elsif Char = 'r' or Char = 'R' then
            Turnouts.Set (Requestor => 0,
                          Turnout   => Layout.Turnout_ID (Turnout),
                          Direction => Layout.Right);
         else
            Ada.Text_IO.Put_Line ("Invalid polarity)");
         end if;
      exception
         when Ada.Text_IO.Data_Error =>
            Ada.Text_IO.Put_Line ("Data Error raised");
            Ada.Text_IO.Skip_Line;
         when others =>
            Ada.Text_IO.Put_Line ("Some exception raised");
      end;
      Ada.Text_IO.New_Line;

   end loop;
end Turnouts_Test;
