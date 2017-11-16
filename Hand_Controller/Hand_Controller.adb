with Ada.Port_IO;
with Ada.Unchecked_Conversion;
with Hand_Controllers;


procedure Digital_Reads is
    baseAddress : constant := 16#260#;
    
    type Controller_ID is (A,B,C);
    for Controller_ID use (A => 16#24C#, B => 16#24D#, C => 16#24E#);

    for Controller_ID'Size use 16;

    type Sixteen_Bit_Value is mod 2**16;    
    
    function Hand_Controller_Status_Record is new Ada.Unchecked_Conversion
    (Source => Controller_ID, 
    Target => Sixteen_Bit_Value);

    procedure Convert_Num (New_A : in A;
                           New_B : in B;
                           New_C in C );
        
        Parameter_As_Record : Controller_ID;
        Some_Num : Sixteen_Bit_Value;

        begin
            Parameter_As_Record.A := New_A;
            Parameter_As_Record.B = New_B;
            Parameter_As_Record.C = New_C;

            Some_Num := Hand_Controller_Status_Record (Parameter_As_Record);
            Ada.Text_IO.Put_Line(Item => Sixteen_Bit_Value'Image (Some_Num));
        end Convert_Num;
    end Digital_Reads; 


    
