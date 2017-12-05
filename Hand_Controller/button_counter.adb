with Hand_Controllers;
with Ada.Text_IO;
with Ada.Real_Time;

procedure Button_Counter is
   use type Hand_Controllers.Controller_ID;
   use type Hand_Controllers.Button_State_Type;
   use type Hand_Controllers.Direction_Switch_Type;
   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;

   -- Local Variables
   Previous_Red_Button: Hand_Controllers.Button_State_Type := Hand_Controllers.Up;
   Previous_Black_Button: Hand_Controllers.Button_State_Type := Hand_Controllers.Up;
   Black_Button_Count: Integer := 0;
   Last_Press_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   -- Variables to pass into Get_Digital_State()
   Red_Button : Hand_Controllers.Button_State_Type;
   Black_Button : Hand_Controllers.Button_State_Type;
   Direction_Switch: Hand_Controllers.Direction_Switch_Type;
   Turn_Switch: Hand_Controllers.Turn_Switch_Type;

begin
   loop
      Hand_Controllers.Get_Digital_State(ID               => Hand_Controllers.B,
                                         Red_Button       => Red_Button,
                                         Black_Button     => Black_Button,
                                         Direction_Switch => Direction_Switch,
                                         Turn_Switch      => Turn_Switch);

      -- Check if the red button has been pressed.
      if Previous_Red_Button = Hand_Controllers.Up
        and Red_Button = Hand_Controllers.Down
        and (Ada.Real_Time.Clock - Last_Press_Time) > Ada.Real_Time.Milliseconds (200) then
         Ada.Text_IO.Put_Line (Item => "Black Button Press Count: "
                               & Integer'Image (Black_Button_Count));
         Black_Button_Count := 0;

         -- Adjust last pressed time
         Last_Press_Time := Ada.Real_Time.Clock;
      end if;

      -- Check if the black button has been pressed. Increment Black_Button_Count
      if Previous_Black_Button = Hand_Controllers.Up
        and Black_Button = Hand_Controllers.Down
        and (Ada.Real_Time.Clock - Last_Press_Time) > Ada.Real_Time.Milliseconds (200) then
         Black_Button_Count := Black_Button_Count + 1;

         -- Adjust last pressed time
         Last_Press_Time := Ada.Real_Time.Clock;
      end if;

      -- Adjust previous button variables
      Previous_Red_Button := Red_Button;
      Previous_Black_Button := Black_Button;
   end loop;
end Button_Counter;
