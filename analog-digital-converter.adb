-- Semaphore object since we need to delay within.
protected type Semaphore is
   procedure Release;
   entry Wait;
private:
   In_Use: Boolean := False;
end Semaphore;

protected body Semaphore is

   procedure Release is
   begin
      In_Use := False;
   end Release

   entry Wait when not In_Use is
   begin
      In_Use := True;
   end Wait;
end Semaphore;

with Port_IO;
procedure Convert(Channel : in Channel_Type;
                  Value   : out Voltage_Range) is

   -- Local Variables
   Base_Adress  : constant := 16#260#;
   Final_Voltage: Port_IO.Word;
begin
   Semaphore.Wait;

   -- Have to do twice to avoid glitchyness
   For_Loop :
   for I in Integer range 1 .. 2 loop

      -- write channel to Base + 2
      Port_IO.Out_Byte(Address => Base_Adress + 2,
                       Data    => Channel);

      -- write junk data to Base + 1 to start conversion
      Port_IO.Out_Word(Address => Base,
                       Data    => 1);

      loop
         -- Check if the status register has been set to 0 (conversion complete)
         exit when Port_IO.In_Byte(Address => Base + 2) < 128;
         delay 0.02;
      end loop

      -- Set Final Value
      Final_Voltage := Port_IO.In_Word(Base_Adress);
      Value := ((Final_Voltage/4095)*10) - 5;
   end loop For_Loop;

   Semaphore.Release;
end Convert;
