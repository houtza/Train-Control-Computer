-- This package provides access to a multi-channel analog to digital converter

-- Written by John W. McCormick March 2002
-- Modified by Andrew Berns, October 2016
-- Modified by Marcus Devens, Nov 2017
package ADC is

   -- There are 8 channels on the board
   type Channel_Range is range 0 .. 7;

   -- The ouput range of the analog to digital converters
   type Voltage_Range is delta 2.0 ** (-12) range -5.0 .. 5.0;

   -----------------------------------------------------------------------------
   -- Sets the given Channel to the given value
   --
   -- Preconditions  : none
   --
   -- Postconditions : The Value is set to voltage on given channel
   --
   procedure Get (Channel : in Channel_Range;
                  Value   : out Voltage_Range);

end ADC;
