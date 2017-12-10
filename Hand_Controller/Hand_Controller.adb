with Port_IO;
with Ada.Unchecked_Conversion;

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
            Black at 1 range 1 .. 1;
            Direct_Switch at 2 range 2 .. 2;
            Turn_Switch at 3 range 3 .. 4;
        end record;

        for HC_Record'Size use 8;

            function Byte_To_HC_Record is new Ada.Unchecked_Conversion(Source => Port_IO.Byte,
                    Target => HC_Record);

    procedure Get_Digital_State(ID : in Controller_ID;
                                Red_Button : out Button_State_Type;
                                Black_Button : out Button_State_Type;
                                Dirction_Switch : out Direction_Switch_Type;
                                T_Switch : out Turn_Switch_Type) is
        Controller_Status : HC_Record;     
    begin 
        Controller_Status := Byte_To_HC_Record( 
            Port_IO.In_Byte ( Port_IO.Address_Range(ID) ) );

        Red_Button := Controller_Status.Red;
        Black_Button := Controller_Status.Black;
        Dirction_Switch := Controller_Status.Direct_Switch;
        T_Switch := Controller_Status.Turn_Switch;
    end Get_Digital_State;

    procedure Get_Analog_State(ID : in Controller_ID; 
                               Controller_Voltage : out Throttle_Level) is
    begin
        Controller_Voltage := 0;
    end Get_Analog_State;				 
end Hand_Controllers;            
    
