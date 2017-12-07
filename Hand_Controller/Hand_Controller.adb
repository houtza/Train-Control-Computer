with Port_IO;
with Ada.Unchecked_Conversion;

package body Hand_Controller is
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

    for HC_Record use
        function
            function Byte_To_HC_Record is new Ada.Unchecked_Conversion(Source => Port_IO.In_Byte,
                    Target => HC_Record);
        end function;

    procedure Get_Digital_State(ID : in Controller_ID;
                                Red_Button : out Button_State_Type;
                                Black_Button : out Button_State_Type;
                                Dirction_Switch : out Direction_Switch_Type;
                                Turn_Switch : out Turn_Switch_Type) is
    Parameters_In : Controller_ID.ID;
    New_Num : HC_Record;
    begin 
        New_Num := Byte_To_HC_Record(Parameters_In);
    end Get_Digital_State;


    procedure Get_Analog_State(ID : in Controller_ID; Controller_Voltage : out Throttle_Level) is
    begin
        Controller_Voltage := 0;
    end Get_Analog_State;				 
end Hand_Controllers;            
    
