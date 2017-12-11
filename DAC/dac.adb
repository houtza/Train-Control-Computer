with Port_IO;

package body DAC is 
   use type Port_IO.Address_Range;
   procedure Set(Channel : in Channel_Range;
                 Value   : in Voltage_Range) is
      
      word : Port_IO.Word;
      
   begin 
              
      word :=Port_IO.Word ((Float(Value) + 5.0) * 409.5)
      Port_IO.Out_Word(Address=>16#240#+ (Integer(Channel)*2),Data=>word);
      
   end Set;
begin
   for Channel in Channel_Range is 
      Set(Channel, 2047.5);
      
   end loop;
   
end DAC