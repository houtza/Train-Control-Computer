with Port_IO;
with Ada.Unchecked_Conversion;
package body Dallee is
    type Settings_Array is array (0 .. 7) of Dallee.Setting;
    for Settings_Array'Component_Size use 1;
    for Settings_Array'Size use 8;

    Base_Address : constant Port_IO.Address_Range := 16#263#;
    Current_Settings : Settings_Array := ( others => off);

    function Settings_To_Byte is new Ada.Unchecked_Conversion ( Source => Settings_Array,
                                                                Target => Port_IO.Byte);
    procedure Set_Horn( Unit : in Dallee.Dallee_ID;
                        To : in Dallee.Setting) is
    begin
        Current_Settings((Integer(Unit)-1)*2) := To;
        Port_IO.Out_Byte( Address => Base_Address,
                          Data => Settings_To_Byte(Current_Settings));
    end Set_Horn;

    procedure Set_Bell (Unit : in Dallee.Dallee_ID;
                        To : in Dallee.Setting) is
    begin
        Current_Settings (Integer(Unit)*2-1) := To;
    end Set_Bell;

begin
    Port_IO.Out_Byte(Base_Address, 0);
end Dallee;
