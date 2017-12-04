with Motors;
with Layout;
with Ada.Text_IO;

procedure Motors_Test is
begin
   for Turnout in Layout.Turnout_ID loop
      Ada.Text_IO.Put_Line ("Setting " & Layout.Turnout_ID'Image (Turnout));
      for Position in reverse Layout.Turn_Choice loop
         Ada.Text_IO.Put_Line ("Setting to "
                               & Layout.Turn_Choice'Image (Position));
         Motors.Set (Motor     => Turnout,
                     Direction => Position);

         for Iteration in 0 .. 1 loop
            delay 1.5;
            Ada.Text_IO.Put_Line ("In position: "
                                  & Boolean'Image (
                Motors.In_Position (Motor => Turnout)));
         end loop;
      end loop;
   end loop;
end Motors_Test;
