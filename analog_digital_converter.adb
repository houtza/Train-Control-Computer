with Port_IO;
use type Port_IO.Address_Range;
use type Port_IO.Byte;
use type Port_IO.Word;
package body Analog_Digital_Converter is

   -- Semaphore object since we need to delay within.
   protected Semaphore is
      procedure Release;
      entry Wait;
   private
      In_Use: Boolean := False;
   end Semaphore;

   protected body Semaphore is

      procedure Release is
      begin
         In_Use := False;
      end Release;

      entry Wait when not In_Use is
      begin
         In_Use := True;
      end Wait;
   end Semaphore;

   procedure Convert(Channel : in Channel_Type;
                     Value   : out Voltage_Range) is

      -- Local Variables
      Base_Address  : constant := 16#260#;
      Final_Voltage: Port_IO.Word;
   begin
      Semaphore.Wait;

      -- Have to do twice to avoid glitchyness
      For_Loop :
      for I in 1 .. 2 loop

         -- write channel to Base + 2
         Port_IO.Out_Byte(Address => Port_IO.Address_Range(Base_Address + 2),
                          Data    => Port_IO.Byte(Channel));

         -- write junk data to Base + 1 to start conversion
         Port_IO.Out_Word(Address => Port_IO.Address_Range(Base_Address),
                          Data    => Port_IO.Word(Channel));

         loop
            -- Check if the status register has been set to 0 (conversion complete)
            exit when Port_IO.In_Byte(Address =>  Port_IO.Address_Range(Base_Address + 2)) < 128;
            delay 0.02;
         end loop;

         -- Set Final Value
         Final_Voltage := Port_IO.In_Word(Base_Address) / 16;
         Value := Voltage_Range ((Float (Final_Voltage) / 409.5) + 5.0);
      end loop For_Loop;

      Semaphore.Release;
   end Convert;

end Analog_Digital_Converter;