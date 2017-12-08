-- This package contains the types and operations for the engineers
--
-- Written by John McCormick, June 2002
-- Modified by Andrew Berns, October 2017
with Trains;
with Ada.Real_Time;
with Hand_Controllers;

package Engineers is
   Max_Engineers : constant := Trains.Max_Trains;

   -- ID zero is for Dispatcher
   type Engineer_Range is range 0 .. Max_Engineers;
   subtype Engineer_ID is Engineer_Range range 1 .. Max_Engineers;

   -- Number of seconds throttle must be off to say that a train is stopped
   Stop_Duration : constant Ada.Real_Time.Time_Span :=
                     Ada.Real_Time.To_Time_Span (3.0);

end Engineers;
