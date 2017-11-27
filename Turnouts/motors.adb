with Motors;
with Ada.Unchecked_Conversion;
package body Motors is
   
   procedure Set (Motor     : in Layout.Turnout_ID;
                  Direction : in Layout.Turn_Choice) is
      
      use type Layout.Turnout_ID;
      
      type Bit_Array is array (0 .. Integer'Size - 1) of Boolean;
      for Bit_Array'Component_Size use 1;
      for Bit_Array'Size use Integer'Size;
      
      Base_Address  : constant := 16#260#; -- Change. IDK what the base is
      Bit: Integer;
      
      function To_Bit_Array is new Ada.Unchecked_Conversion(Source => Integer,
                                                            Target => Bit_Array);
   
   begin
      -- Read in current byte for the turnout
      if Integer(Motor) < 9 then
         null; -- Read port A high. Use read byte?
      elsif Integer(Motor) < 17 then
         null; -- Read port C high. Use read byte?
      else
         null; -- Set port B low. Use read byte?
      end if;
      
      -- set bit = 
      Bit := (Integer(Motor) - 1) mod 8;
     
      -- write the desird position to the specified bit. Do this by converting 
      -- ReadByte return value into a BitArray and flipping index Bit.
      -- Should Left = 1 or Right = 1? Does it matter?
      
      
      -- write the byte back to where it came from. Use write byte?
   
   end Set;
   
   
   -- Do Next.
   function In_Position (Motor : in Layout.Turnout_ID) return Boolean is
   begin
      return True;
   end In_Position;
   
   

end Motors;
