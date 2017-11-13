-- This package provides access to a multi-channel digital to analog converter

-- Written by John W. McCormick March 2002
-- Modified by Andrew Berns, October 2016
package DAC is

   -- There are 6 channels on the board
   type Channel_Range is range 0 .. 5;

   -- The ouput range of the digital to analog converters
   type Voltage_Range is delta 2.0 ** (-12) range -5.0 .. 5.0;

   -----------------------------------------------------------------------------
   -- Sets the given Channel to the given value
   --
   -- Preconditions  : none
   --
   -- Postconditions : The Channel is set to Value
   -- 
   procedure Set (Channel : in Channel_Range;
                  Value   : in Voltage_Range);

end DAC;
