-- Author: Dr. John McCormick
--
-- This child package provides types and a procedure to determine the
-- blocks and the turnouts beneath a train given the location of its
-- locomotive and caboose.
package Layout.Search is
   pragma Pure (Layout.Search);

   ----------------------------------------------------------------------------
   -- Types for a list of blocks (All details are public)
   ----------------------------------------------------------------------------
   type Block_Rec is   -- Elements in the list of blocks
      record
         Block     : Block_ID;
         Direction : Block_Polarity;
      end record;
   type Block_Array is array (Positive range <>) of Block_Rec;

   ----------------------------------------------------------------------------
   -- The value of the record discriminant Max_Size determines the maximum
   -- number of blocks in the list.  This discriminant is set when a variable
   -- of type Block_List is declared
   ----------------------------------------------------------------------------
   type Block_List (Max_Size : Positive) is
      record
         Size : Natural := 0;
         Items : Block_Array (1 .. Max_Size);
      end record;

   ----------------------------------------------------------------------------
   -- Types for a list of turnouts (all details are public)
   ----------------------------------------------------------------------------
   type Turnout_Rec is   -- Elements in the list of turnouts
      record
         Turnout   : Turnout_ID;
         Direction : Turn_Choice;
      end record;
   type Turnout_Array is array (Positive range <>) of Turnout_Rec;

   ----------------------------------------------------------------------------
   -- The value of the record discriminant Max_Size determines the maximum
   -- number of turnouts in the list.  This discriminant is set when a variable
   -- of type Turnout_List is declared
   ----------------------------------------------------------------------------
   type Turnout_List (Max_Size : Positive) is
      record
         Size : Natural := 0;
         Items : Turnout_Array (1 .. Max_Size);
      end record;


   ----------------------------------------------------------------------------
   -- Search the layout for the blocks and turnouts beneath a train
   --
   --
   -- Preconditions  : Loco /= Caboose
   --
   -- Postconditions :  For this operation to be successful, there is a unique
   --                   path from the caboose to the locomotive that contains
   --                   at most Blocks.Max_Size blocks.
   --
   --                  If Success then
   --                     Blocks is a list of blocks under the train and the
   --                        direction each block should be powered to move
   --                        the train in the forward direction.
   --                        The blocks in this list are in order from
   --                        Caboose to Loco.
   --                     Turnouts is a list of turnouts under the train and
   --                        the direction each turnout should be set.
   --                        The turnouts in this list are in no order.
   --                  Else
   --                      We were unable to locate the train.  Based on the
   --                      input, there are more blocks or turnouts under the
   --                      train than will fit on the lists.
   --                      Blocks and Turnouts are undefined.
   procedure Blocks_Beneath (Loco     : in  Block_ID;
                             Caboose  : in  Block_ID;
                             Blocks   : out Block_List;
                             Turnouts : out Turnout_List;
                             Success  : out Boolean);

      ----------------------------------------------------------------------------
   -- Consider using this as a local recursive search procedure ...
   ----------------------------------------------------------------------------
   procedure Search (Start     : in     Block_ID;       -- Rear of train
                     Finish    : in     Block_ID;       -- Front of train
                     Direction : in     Block_Polarity; -- Search direction
                     Blocks    : in out Block_List;     -- Under train
                     Turnouts  : in out Turnout_List;   -- Under train
                     Success   :        out Boolean);


end Layout.Search;
