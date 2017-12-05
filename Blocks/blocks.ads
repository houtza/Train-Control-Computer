-- This package provides operations for allocation and powering
-- of Blocks of Track

-- Written by John W. McCormick, March 2002
with Layout;
with Cabs;
with Trains;

package Blocks is

   pragma Elaborate_Body (Blocks);

   ----------------------------------------------------------------------------
   -- Reserve a block a track
   --
   -- Preconditions  : none
   --
   -- Postconditions : Success is True and Block and its cross blocks are
   --                  allocated to Train
   --                             or
   --                  Success is False (because Block and/or one of its
   --                  cross blocks are allocated to another train) and the
   --                  reservations on the Block and its cross blocks are
   --                  unchanged.
   procedure Reserve (Train   : in  Trains.Train_ID;
                      Block   : in  Layout.Block_ID;
                      Success : out Boolean);

   ----------------------------------------------------------------------------
   -- Release a block a track
   --
   -- Preconditions  : none
   --
   -- Postconditions : If Block is reserved by Train, Block and its cross
   --                     blocks are released.
   --                  If Train is Trains.Dispatcher, Block is released.
   --                  Otherwise Block and its cross blocks are not changed.
   procedure Release (Train : in  Trains.Request_ID;
                      Block : in  Layout.Block_ID);

   ----------------------------------------------------------------------------
   -- Release all blocks reserved by this train
   --
   -- Preconditions  : none
   --
   -- Postconditions : Train has no blocks reserved
   procedure Clear_Reservations (Train : in Trains.Train_ID);

   ----------------------------------------------------------------------------
   -- Clear all reservations of all blocks
   --
   -- Preconditions  : none
   --
   -- Postconditions : No block is reserved by a train
   procedure Clear_All_Reservations;

   ----------------------------------------------------------------------------
   -- Returns who reserved Block
   --
   -- Preconditions  : none
   --
   -- Postconditions : The ID of the train who owns the block is returned
   --                  If no train owns the block, the dispatachers ID (zero)
   --                  is returned.
   function Reserved_By (Block : in Layout.Block_ID) return Trains.Request_ID;

   ----------------------------------------------------------------------------
   -- Assign a cab to power a block
   --
   -- Preconditions  : none
   --
   -- Postconditions : Block is powered by Cab with the given Polarity
   procedure Assign_Cab (Block    : in Layout.Block_ID;
                         Cab      : in Cabs.Cab_ID;
                         Polarity : in Layout.Block_Polarity);

   ----------------------------------------------------------------------------
   -- Returns the cab and polarity assigned to power a block
   --
   -- Preconditions  : none
   --
   -- Postconditions : The cab assigned to a block and the block's polarity
   --                  are returned
   procedure Cab_Assigned (Block    : in  Layout.Block_ID;
                           Cab      : out Cabs.Cab_ID;
                           Polarity : out Layout.Block_Polarity);

end Blocks;
