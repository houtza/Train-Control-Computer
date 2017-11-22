with Port_IO;
with Ada.Unchecked_Conversion;

package body Hand_Controller is
    type HC_Record is
        record 
            Red : Button_State_Type;
            Black : Button_State_Type;
            Direct_Switch : Direction_Switch_Type;
            Turn_Switch : Direction_Switch_Type;
        end record; 

    for HC_Record use
        record
            Red at 0 range 0 .. 0;
            Black at 1 range 1 .. 1;
            Direct_Switch at 2 range 2 .. 2;
            Turn_Switch at 3 range 3 .. 4;
        end record;

    for HC_Record'Size use 8;

    type Eight_Bit_Value is mod 2**8;

    function Byte_To_HC_Record is new Ada.Unchecked_Conversion(Source => Port_IO.Byte,
            Target => HC_Record);

    procedure Get_Digital_State(ID : in Controller_ID;
                                red_button : out Button_State_Type;
                                black_button : out Button_State_Type;
                                dirction_switch : out Direction_Switch_Type;
                                turn_switch : out Direction_Switch_Type) is
    Parameters_In : HC_Record;
    New_Num : Eight_Bit_Value;
    begin 
        Parameters_In.Red := red_button;
        Parameters_In.Black := black_button;
        Parameters_In.Direct_Switch := dirction_switch;
        Parameters_In.Turn_Switch := turn_switch;

        New_Num := Byte_To_HC_Record(Parameters_In);
        Ada.Text_IO.Put_Line(Item => Eight_Bit_Value'Image(New_Num));
    end Get_Digital_State;


    procedure Get_Analog_State(ID : in Controller_ID; Controller_Voltage : out Throttle_Level) is
    begin
        Controller_Voltage := 0;
    end Get_Analog_State;				 
end Hand_Controllers;            
    
