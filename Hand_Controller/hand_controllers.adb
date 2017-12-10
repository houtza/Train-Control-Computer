with Port_IO;	
with Ada.Unchecked_Conversion;
with Analog_Digital_Converter;
	
package body Hand_Controllers is
   procedure Get_Analog_State(ID : in Controller_ID; Controller_Voltage : out Throttle_Level) is
      Voltage : Analog_Digital_Converter.Voltage_Range;
   begin
      Analog_Digital_Converter.Convert(Channel_Type (Controller_ID'Pos (ID)), Voltage);
      Controller_Voltage := Throttle_Level (Float(Voltage) * 20.0);
   end Get_Analog_State;				 
end Hand_Controllers;