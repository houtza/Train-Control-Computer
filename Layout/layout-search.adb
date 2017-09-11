package body Layout.Search is
   ----------------------------------------------------------------------------
   --- You might find it helpful to create some local procedures ...
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- Consider using this as a local recursive search procedure ...
   ----------------------------------------------------------------------------
   procedure Search (Start     : in     Block_ID;       -- Rear of train
                     Finish    : in     Block_ID;       -- Front of train
                     Direction : in     Block_Polarity; -- Search direction
                     Blocks    : in out Block_List;     -- Under train
                     Turnouts  : in out Turnout_List;   -- Under train
                     Success   :        out Boolean) is

      -- Local variables will go here
   begin
      null;
   end Search;

   ----------------------------------------------------------------------------
   procedure Blocks_Beneath (Loco     : in  Block_ID;
                             Caboose  : in  Block_ID;
                             Blocks   : out Block_List;
                             Turnouts : out Turnout_List;
                             Success  : out Boolean) is
   begin
      null;
      -- First, try searching in the normal direction

      -- Next, try searching in the reverse direction

   end Blocks_Beneath;

end Layout.Search;
