package Hand_Controllers is
	
	type Controller_ID is (A,B,C);
	type Button_State_Type is (Down,Up);
	type Direction_Switch_Type is (Backward,Forward);
	type Turn_Switch is (Left, Right, Centered);
    type Throttle_Level is range 0 .. 100;
      
	
	
	
	
	procedure Get_Digital_State(ID : in Controller_ID; 
	                     red_button: out Button_state_Type;
	                   black_button: out Button_state_Type; 
			        dirction_switch: out Direction_Switch_Type;
			            turn_switch: out Direction_Switch_Type);
					  
					  
	procedure Get_Analog_State(ID : in Controller_ID; Controller_Voltage : out Analog_Voltage);
	                         

							 
							 
private 
	for Controller_ID use (A => 16#24C#, B =>16#24D#, C=>16#24E#);
	
	for Button_State_Type use(Down=>0, Up=>1);
	for Direction_Switch_Type use (Backward=> 0, Forward=>1);
	for Turn_Switch use (Left=> 2#01#, Right=>2#10#,Centered=>2#11#);

end Hand_Controllers;
