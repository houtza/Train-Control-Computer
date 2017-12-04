-- This package provides access to the four Dallee Sound Units throough
-- the interface board built by John McCormick
--
-- Written by John McCormick, March 2011

package Dallee is

   -- There are four Dallee units
   type Dallee_ID is range 1 .. 4;

   -- We turn sounds off or on
   type Setting  is (Off, On);

   -----------------------------------------------------------------------------
   -- Turn a horn off or on
   --
   -- Preconditions  : None
   -- Postconditions : The horn on the specified Dallee unit is turned off or on
   procedure Set_Horn (Unit : in Dallee_ID;
                       To   : in Setting);

   -----------------------------------------------------------------------------
   -- Turn a bell off or on
   --
   -- Preconditions  : None
   -- Postconditions : The bell on the specified Dallee unit is turned off or on
   procedure Set_Bell (Unit : in Dallee_ID;
                       To   : in Setting);

end Dallee;
