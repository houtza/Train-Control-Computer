------------------------------------------------------------------------------
-- Written by John McCormick, 2009
-- Modified by Andrew Berns, August 2017
--
-- This procedure tests the layout search procedure using the inputs and
-- expected outputs given in the array 'Test_Cases'.  Test cases should be
-- added to this array to make the test results more meaningful.
------------------------------------------------------------------------------
with Ada.Text_IO;
with Layout.Search;

procedure Search_Test is
   use type Layout.Block_ID;

   package Block_IO     is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Turnout_IO   is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Polarity_IO  is new Ada.Text_IO.Enumeration_IO (Layout.Block_Polarity);
   package Direction_IO is new Ada.Text_IO.Enumeration_IO (Layout.Turnout_Limb);

   Blocks   : Layout.Search.Block_List   (Max_Size => 3);
   Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

   Loco    : Layout.Block_ID;
   Caboose : Layout.Block_ID;
   OK      : Boolean;

begin
   loop
      Ada.Text_IO.Put_Line ("Enter Locomotive and Caboose blocks");
      Ada.Text_IO.Put_Line ("(same block to exit program");
      Block_IO.Get (Loco);
      Block_IO.Get (Caboose);
      exit when Loco = Caboose;
      Ada.Text_IO.New_Line;
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      if OK then
         Ada.Text_IO.Put_Line ("List of blocks (and polarities) under your train");
         for Index in 1 .. Blocks.Size loop
            Block_IO.Put (Blocks.Items (Index).Block);
            Ada.Text_IO.Put (' ');
            Polarity_IO.Put (Blocks.Items (Index).Direction);
            Ada.Text_IO.New_Line;
         end loop;
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("List of turnouts (and directions) under your train");
         for Index in 1 .. Turnouts.Size loop
            Turnout_IO.Put (Turnouts.Items (Index).Turnout);
            Ada.Text_IO.Put (' ');
            Direction_IO.Put (Turnouts.Items (Index).Direction);
            Ada.Text_IO.New_Line;
         end loop;
      else
         Ada.Text_IO.Put_Line ("Search failed");
      end if;
      Ada.Text_IO.New_Line (2);
   end loop;

end Search_Test;
