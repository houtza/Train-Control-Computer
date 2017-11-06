with Hand_Controllers;
with Ada.Text_IO;

procedure Button_Counter is
   use type Hand_Controllers.Controller_ID;
   use type Hand_Controllers.Button_State_Type;

   -- Local Variables
   Hand_Controller: Hand_Controllers;
   Previous_Red_Button: Button_State_Type := Up;
   Previous_Black_Button: Button_State_Type := Up;
   Black_Button_Count: Integer := 0;
begin
   loop
      -- Variables to pass into Get_Digital_State()
      Controller_ID_Var : Controller_ID := A;
      Red_Button : Button_State_Type;
      Black_Button : Button_State_Type;
      Direction_Switch: Direction_Switch_Type;
      Turn_Switch: Direction_Switch_Type;

      Hand_Controller.Get_Digital_State(ID              => Controller_ID_Var,
                                        red_button      => Red_Button,
                                        black_button    => Black_Button,
                                        dirction_switch => Direction_Switch,
                                        turn_switch     => Turn_Switch);

      -- Check if the red button has been pressed.
      if Previous_Red_Button = Up and Red_Button = Down then
         Ada.Text_IO.Put_Line (Item => "Black Button Press Count: " & Integer'Image (Black_Button_Count));
         Black_Button_Count := 0;
      end if;

      -- Check if the black button has been pressed. Increment Black_Button_Count
      if Previous_Black_Button = Up and Black_Button = Down then
         Black_Button_Count := Black_Button_Count + 1;
      end if;

      -- Adjust previous button vars
      Previous_Red_Button := Red_Button;
      Previous_Black_Button := Black_Button;

      --delay (might need to be adjusted)
      delay 0.25;
   end loop;
end Button_Counter;
