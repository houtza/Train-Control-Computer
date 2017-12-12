with Port_IO;
with Ada.Unchecked_Conversion;
with ADC;

use type ADC.Channel_Range;
use type ADC.Voltage_Range;
package body Hand_Controllers is
   type HC_Record is
      record
         Red : Button_State_Type;
         Black : Button_State_Type;
         Direct_Switch : Direction_Switch_Type;
         Turn_Switch : Turn_Switch_Type;
      end record;

   for HC_Record use
      record
         Red at 0 range 0 .. 0;
         Black at 0 range 1 .. 1;
         Direct_Switch at 0 range 2 .. 2;
         Turn_Switch at 0 range 3 .. 4;
      end record;

   for HC_Record'Size use 8; -- what?

   function Byte_To_HC_Record is
     new Ada.Unchecked_Conversion(Source => Port_IO.Byte,
                                  Target => HC_Record);

   HC_Addresses : array (Controller_ID) of Port_IO.Address_Range :=
     (16#24C#, 16#24D#, 16#24E#);

   procedure Get_Digital_State(ID : in Controller_ID;
                               Red_Button : out Button_State_Type;
                               Black_Button : out Button_State_Type;
                               Direction_Switch : out Direction_Switch_Type;
                               Turn_Switch : out Turn_Switch_Type) is
      Controller_Status : HC_Record;
   begin
      Controller_Status := Byte_To_HC_Record (Port_IO.In_Byte
                                              ( HC_Addresses (ID) )); -- what?

      Red_Button := Controller_Status.Red;
      Black_Button := Controller_Status.Black;
      Direction_Switch := Controller_Status.Direct_Switch;
      Turn_Switch := Controller_Status.Turn_Switch;
   end Get_Digital_State;

   procedure Get_Analog_State(ID : in Controller_ID;
                              Throttle : out Throttle_Level) is
      Controller_Voltage : ADC.Voltage_Range;
   begin
      ADC.Get(ADC.Channel_Range (Controller_ID'Pos (ID)), Controller_Voltage);
      if Controller_Voltage < 0.0 then
         Throttle := 0;
      else
         Throttle := Throttle_Level (Float(Controller_Voltage) * 20.0);
      end if;
   end Get_Analog_State;
end Hand_Controllers;



