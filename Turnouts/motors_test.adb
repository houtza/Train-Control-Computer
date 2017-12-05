with Motors;
with Layout;
with Ada.Text_IO;

procedure Motors_Test is

begin
   for Turnout in Layout.Turnout_ID loop
      Ada.Text_IO.Put_Line  (Item => "Press Enter to test turnout #"
                             & Layout.Turnout_ID'Image (Turnout));
      Ada.Text_IO.Skip_Line;

      Ada.Text_IO.Put_Line ("Setting " & Layout.Turnout_ID'Image (Turnout));

      for Position in reverse Layout.Turn_Choice loop
         Ada.Text_IO.Put_Line ("Setting to "
                               & Layout.Turn_Choice'Image (Position));

         Motors.Set (Motor     => Turnout,
                     Direction => Position);

         for Iteration in 0 .. 1 loop
            delay 1.5;

            Ada.Text_IO.Put_Line(Item => "In position: "
                                 & Boolean'Image (Motors.In_Position
                                   (Motor => Turnout)));
         end loop;
      end loop;
   end loop;


   -- Setting two turnouts at the same time
   Ada.Text_IO.Put_Line ("Setting " & Layout.Turnout_ID'Image (2));
   Ada.Text_IO.Put_Line ("Setting " & Layout.Turnout_ID'Image (3));

   Motors.Set (Motor     => 2,
               Direction => Layout.Right);

   Motors.Set (Motor     => 3,
               Direction => Layout.Right);

   delay 1.5;

   Ada.Text_IO.Put_Line(Item => "In position: "
                        & Boolean'Image (Motors.In_Position (Motor => 2)));

   Ada.Text_IO.Put_Line(Item => "In position: "
                        & Boolean'Image (Motors.In_Position (Motor => 3)));

   delay 1.5;

   Ada.Text_IO.Put_Line(Item => "In position: "
                        & Boolean'Image (Motors.In_Position (Motor => 2)));

   Ada.Text_IO.Put_Line(Item => "In position: "
                        & Boolean'Image (Motors.In_Position (Motor => 3)));
end Motors_Test;
