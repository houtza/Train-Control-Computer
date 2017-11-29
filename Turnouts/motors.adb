with Motors;
with Port_IO;
with Ada.Unchecked_Conversion;
with Interfaces;
package body Motors is
   use type Layout.Turnout_ID;
   use type Layout.Turn_Choice;
   use type Port_IO.Address_Range;
   use type Port_IO.Byte;
   use type Interfaces.Unsigned_16;
   
   -- Array of turn_choice's to represent a byte in memory
   type Turnout_Choice_Array is array (0 .. 7) of Layout.Turn_Choice;
   for Turnout_Choice_Array'Component_Size use 1;
   for Turnout_Choice_Array'Size use 8;
   
   -- Array of Boolean's to represent a byte in memory
   type Bit_Array is array (0 .. 7) of Boolean;
   for Bit_Array'Component_Size use 1;
   for Bit_Array'Size use 8;
   
   -- Conversion functions
   function To_Turnout_Choice_Array is new Ada.Unchecked_Conversion(Source => Port_IO.Byte,
                                                                    Target => Turnout_Choice_Array);
   
   function To_Byte is new Ada.Unchecked_Conversion(Source => Turnout_Choice_Array,
                                                    Target => Port_IO.Byte);
   
   function To_Bit_Array is new Ada.Unchecked_Conversion(Source => Port_IO.Byte,
                                                         Target => Bit_Array);
   
   procedure Set (Motor     : in Layout.Turnout_ID;
                  Direction : in Layout.Turn_Choice) is
      
      -- Local Variables
      Board_Base_Address  : Interfaces.Unsigned_16;
      Turnout_Address: Port_IO.Address_Range;
      Bit: Integer;
      
   begin
      if Integer(Motor) > 24 then
         Board_Base_Address := 16#228#;
      else
         Board_Base_Address := 16#220#;
      end if;
      
      -- Determine port address for the given turnout
      if Integer(Motor) < 9 then
         Turnout_Address := Port_IO.Address_Range(Board_Base_Address + 4);-- Read port A high. Use read byte?
      elsif Integer(Motor) < 17 then
         Turnout_Address := Port_IO.Address_Range(Board_Base_Address + 6); -- Read port C high. Use read byte?
      else
         Turnout_Address := Port_IO.Address_Range(Board_Base_Address + 1); -- Set port B low. Use read byte?
      end if;
      
      -- Determine bit
      Bit := (Integer(Motor) - 1) mod 8;
      
     
      -- write the desird position to the specified bit. Do this by converting 
      -- ReadByte return value into a BitArray and flipping index Bit.
      -- Should Left = 1 or Right = 1? Does it matter?
      
      
      -- write the byte back to where it came from. Use write byte?
   
   end Set;
   
   
   -- Protected object that handles the reading and writing of Bytes.
   protected type Read_Write_Manager is
      procedure Set_Turnout_Position_At_Address_Location(Address   : in Port_IO.Address_Range;
                                                         Bit_Index : in Integer;
                                                         Direction : in Layout.Turn_Choice);
   end Read_Write_Manager;

   protected body Read_Write_Manager is
      procedure Set_Turnout_Position_At_Address_Location(Address   : in Port_IO.Address_Range;
                                                         Bit_Index : in Integer;
                                                         Direction : in Layout.Turn_Choice) is
         -- Local Variables
         Byte_Returned: Port_IO.Byte;
         Converted_Turnout_Choice_Array: Turnout_Choice_Array;
      begin
         -- Read Byte, set direction at bit index, write back out
         Byte_Returned := Port_IO.In_Byte(Address => Address);
         Converted_Turnout_Choice_Array := To_Turnout_Choice_Array(Byte_Returned);
         Converted_Turnout_Choice_Array(Bit_Index) := Direction;
         Port_IO.Out_Byte(Address => Address,
                          Data    => To_Byte(Converted_Turnout_Choice_Array));
      end Set_Turnout_Position_At_Address_Location;
      
   end Read_Write_Manager;
   
   -- Do Next.
   function In_Position (Motor : in Layout.Turnout_ID) return Boolean is
   begin
      return True;
   end In_Position;
   
   
   
   

end Motors;
