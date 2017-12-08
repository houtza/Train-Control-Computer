with Port_IO;	
with Ada.Unchecked_Conversion;
with Analog_Digital_Converter;
	
package body Hand_Controller.Hand_Controllers is
   procedure Get_Analog_State(ID : in Controller_ID; Controller_Voltage : out Throttle_Level) is
   begin
      Controller_Voltage := Analog_Digital_Converter.Convert(Channel_Type(ID), Controller_Voltage) * 20;
   end Get_Analog_State;				 
end Hand_Controllers;
