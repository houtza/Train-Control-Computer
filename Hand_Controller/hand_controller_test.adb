with Ada.Text_IO;
with Hand_Controllers;

procedure Hand_Controller_Test is
   use type Hand_Controllers.Controller_ID;
   use type Hand_Controllers.Button_State_Type;
   use type Hand_Controllers.Direction_Switch_Type;
   use type Hand_Controllers.Turn_Switch;
   use type Hand_Controllers.Throttle_Level;

   -- Local Variables
   Red_Button : Hand_Controllers.Button_State_Type;
   Black_Button : Hand_Controllers.Button_State_Type;
   Direction_Switch: Hand_Controllers.Direction_Switch_Type;
   Turn_Switch: Hand_Controllers.Turn_Switch;
   Throttle_Level: Hand_Controllers.Throttle_Level;
   Controller: Hand_Controllers.Controller_ID;
begin
   loop
      Ada.Text_IO.Put_Line ("Press Enter to display all three hand controllers data");
      Ada.Text_IO.Skip_Line;

      -- Loop through all 3 hand controllers
      for I in Integer range 1...3 loop
         if I = 1 then
            Controller = Hand_Controllers.A;
            Ada.Text_IO.Put_Line("---------- Controller A ----------");
         end if;

         if I = 2 then
            Controller = Hand_Controllers.B;
            Ada.Text_IO.Put_Line("---------- Controller B ----------");
         end if;

         if I = 3 then
            Controller = Hand_Controllers.C;
            Ada.Text_IO.Put_Line("---------- Controller C ----------");
         end if;

         -- Get data for controller
         Hand_Controllers.Get_Digital_State(ID              => Controller,
                                            red_button      => Red_Button,
                                            black_button    => Black_Button,
                                            dirction_switch => Direction_Switch,
                                            turn_switch     => Turn_Switch);

         Hand_Controllers.Get_Analog_State(ID                 => Controller,
                                           Controller_Voltage => Throttle_Level);

         Ada.Text_IO.Put("Red button state: ");
         if Red_Button = Hand_Controllers.Up then
            Ada.Text_IO.Put_Line("Up");
         else
            Ada.Text_IO.Put_Line("Down");
         end if;

         Ada.Text_IO.Put("Black button state: ");
         if Black_Button = Hand_Controllers.Up then
            Ada.Text_IO.Put_Line("Up");
         else
            Ada.Text_IO.Put_Line("Down");
         end if;

         Ada.Text_IO.Put("Direction switch state: ");
         if Direction_Switch = Hand_Controllers.Backward then
            Ada.Text_IO.Put_Line("Backward");
         else
            Ada.Text_IO.Put_Line("Forward");
         end if;

         Ada.Text_IO.Put("Turn switch state: ");
         if Direction_Switch = Hand_Controllers.Left then
            Ada.Text_IO.Put_Line("Left");
         else
            if Direction_Switch = Hand_Controllers.Right then
               Ada.Text_IO.Put_Line("Right");
            else
               Ada.Text_IO.Put_Line("Centered");
            end if;
         end if;

         Ada.Text_IO.Put("Throttle Level: ");
         Ada.Text_IO.Put_Line(Integer'Image(Controller_Voltage));
         Ada.Text_IO.Put_Line("----------------------------------");
      end loop;

   end loop;
end Hand_Controller_Test;
