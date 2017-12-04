with Ada.Text_IO;
with Hand_Controllers;

procedure Hand_Controller_Test is
   use type Hand_Controllers.Controller_ID;
   use type Hand_Controllers.Button_State_Type;
   use type Hand_Controllers.Direction_Switch_Type;
   use type Hand_Controllers.Turn_Switch_Type;
   use type Hand_Controllers.Throttle_Level;

   -- Local Variables
   Red_Button : Hand_Controllers.Button_State_Type;
   Black_Button : Hand_Controllers.Button_State_Type;
   Direction_Switch: Hand_Controllers.Direction_Switch_Type;
   Turn_Switch: Hand_Controllers.Turn_Switch_Type;
   Controller_Throttle: Hand_Controllers.Throttle_Level;
begin
   loop
      Ada.Text_IO.Put_Line  (Item => "Press Enter to display all three hand controllers data");
      Ada.Text_IO.Skip_Line;

      -- Loop through all 3 hand controllers
      for Controller in Hand_Controllers.Controller_ID loop
         Ada.Text_IO.Put_Line (Item => "---------- Controller " & Hand_Controllers.Controller_ID'Image(Controller)  & " ----------");


         -- Get data for controller
         Hand_Controllers.Get_Digital_State(ID               => Controller,
                                            Red_Button       => Red_Button,
                                            Black_Button     => Black_Button,
                                            Direction_Switch => Direction_Switch,
                                            Turn_Switch      => Turn_Switch);

         Hand_Controllers.Get_Analog_State(ID       => Controller,
                                           Throttle => Controller_Throttle);

         -- Print out states
         Ada.Text_IO.Put_Line (Item => "Red button state: " & Hand_Controllers.Button_State_Type'Image (Red_Button));

         Ada.Text_IO.Put_Line (Item => "Black button state: " & Hand_Controllers.Button_State_Type'Image (Black_Button));

         Ada.Text_IO.Put_Line (Item => "Direction switch state: " & Hand_Controllers.Direction_Switch_Type'Image (Direction_Switch));

         Ada.Text_IO.Put_Line (Item => "Turn switch state: " &  Hand_Controllers.Turn_Switch_Type'Image (Turn_Switch));

         Ada.Text_IO.Put_Line (Item => "Throttle Level: " & Hand_Controllers.Throttle_Level'Image (Controller_Throttle));

         Ada.Text_IO.Put_Line (Item => "----------------------------------");
      end loop;

   end loop;
end Hand_Controller_Test;
