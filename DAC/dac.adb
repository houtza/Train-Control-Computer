with Port_IO;

package body DAC is
   use type Port_IO.Address_Range;

   procedure Set(Channel : in Channel_Range;
                 Value   : in Voltage_Range) is

      Word : Port_IO.Word;

   begin

      Word := Port_IO.Word ((Float(Value) + 5.0) * 409.5);
      -- changed to take in adress range
      Port_IO.Out_Word(Address => 16#240# + Port_IO.Address_Range (Channel) * 2,
                       Data    => Word);

   end Set;
begin
   for Channel in Channel_Range loop
      --Set(Channel, Voltage_Range(0)); -- what to do here?
      Port_IO.Out_Word (Address => 16#240# + Port_IO.Address_Range (Channel) * 2,
                        Data => 2048);
   end loop;

end DAC;
