-- Written by John W. McCormick March 2002

with DAC;
use type DAC.Voltage_Range;

package body Cabs is

   -- Each cab has a current power setting and an upper limit on that setting
   type Cab_Rec is
      record
         Power_Level : Percent := 0;
         Limit       : Percent := 0;
      end record;

   type Cab_Array is array (Control_Cab_ID) of Cab_Rec;

   type Real_Percent is delta 0.01 range 0.0 .. 100.0;

   -- The actual cab settings
   The_Cabs : Cab_Array;

   ----------------------------------------------------------------------------
   procedure Set (Cab   : in Control_Cab_ID;
                  Value : in Percent) is
      Fraction : Real_Percent;
   begin
      -- Determine the power setting
      if Value <= The_Cabs (Cab).Limit then
         The_Cabs (Cab).Power_Level := Value;
      else
         The_Cabs (Cab).Power_Level := The_Cabs (Cab).Limit;
      end if;

      -- Change the power setting
      Fraction := Real_Percent (The_Cabs (Cab).Power_Level) / 100;
      DAC.Set (Channel => DAC.Channel_Range (Cab - 1),
               Value   => DAC.Voltage_Range (Fraction *
                                             DAC.Voltage_Range'Last));
   end Set;

   ----------------------------------------------------------------------------
   procedure Get (Cab   : in  Cab_ID;
                  Value : out Percent) is
   begin
      if Cab = Null_Cab or Cab = Aux_Null_Cab then
         Value := 0;
      else
         Value := The_Cabs (Cab).Power_Level;
      end if;
   end Get;

   ----------------------------------------------------------------------------
   procedure Set_Limit (Cab   : in Control_Cab_ID;
                        Value : in Percent) is
      Fraction : Real_Percent;
   begin
      The_Cabs (Cab).Limit := Value;
      if The_Cabs (Cab).Power_Level > Value then
         The_Cabs (Cab).Power_Level := Value;

         -- Change the power setting
         Fraction := Real_Percent (The_Cabs (Cab).Power_Level) / 100;
         DAC.Set (Channel => DAC.Channel_Range (Cab - 1),
                  Value   => DAC.Voltage_Range (Fraction *
                                             DAC.Voltage_Range'Last));
      end if;
   end Set_Limit;

   ----------------------------------------------------------------------------
   procedure Get_Limit (Cab   : in  Cab_ID;
                        Value : out Percent) is
   begin
      if Cab = Null_Cab or Cab = Aux_Null_Cab then
         Value := 0;
      else
         Value := The_Cabs (Cab).Limit;
      end if;
   end Get_Limit;

begin
   -- Initialize the D to A converter for each cab
   for Cab in The_Cabs'Range loop
      -- Set the power level of the cab to zero
      DAC.Set (Channel => DAC.Channel_Range (Cab - 1),
               Value   => 0.0);
   end loop;

end Cabs;
