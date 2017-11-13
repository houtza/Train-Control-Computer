with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with DAC;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
procedure DAC_Test is

   package Volt_IO is new Ada.Text_IO.Fixed_IO (DAC.Voltage_Range);

-- Written by John W. McCormick March 2002

   Value   : DAC.Voltage_Range;
   Channel : Integer;

begin
   Ada.Text_IO.Put_Line ("Test of Digital to Analog device driver");
   Ada.Text_IO.New_Line;
   loop
      Ada.Text_IO.Put_Line ("Enter Channel and Value");
      begin
         Ada.Integer_Text_IO.Get (Channel);
         Volt_IO.Get (Value);
         DAC.Set (Channel => DAC.Channel_Range (Channel),
                  Value   => Value);
      exception
         when Ada.Text_IO.Data_Error =>
            Ada.Text_IO.Put_Line ("Data error");
            Ada.Text_IO.Skip_Line;
         when others =>
            Ada.Text_IO.Put_Line ("Error other than Data error");
      end;
   end loop;
end DAC_Test;
