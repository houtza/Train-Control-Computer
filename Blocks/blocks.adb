-- Written by John W. McCormick, March 2002
with Port_IO;
with Ada.Unchecked_Conversion;

package body Blocks is

   use type Trains.Train_ID;
   use type Layout.Block_ID;
   use type Port_IO.Address_Range;

   ----------------------------------------------------------------------------
   -- Types and Protected Object for Reservations
   ----------------------------------------------------------------------------

   -- Each block reservation contains the train that made the reservation and
   -- a count of how many times the train reserved it.  The count allows a
   -- train to "loop back" on a cross block.
   type Reservation_Rec is
      record
         By    : Trains.Request_ID := Trains.Dispatcher;
         Count : Natural           := 0;
      end record;

   -- There is a block reservation record for each block
   type Reservation_Array is array (Layout.Block_ID) of Reservation_Rec;

   -------------------------------------------------------------------------
   -- We must ensure mutual exclusive access to the reservation list.
   -- We use the following protected object to accomplish this.

   protected Reservation_List is
      procedure Reserve (Train   : in  Trains.Train_ID;
                         Block   : in  Layout.Block_ID;
                         Success : out Boolean);
      procedure Release (Train : in  Trains.Request_ID;
                         Block : in  Layout.Block_ID);
      procedure Clear (Train : in  Trains.Request_ID);
      procedure Clear_All;
      function Reserved_By (Block : in  Layout.Block_ID)
                            return Trains.Request_ID;
   private
      The_Reservations : Reservation_Array;
   end Reservation_List;

   -----------------------------------------------------------------------------
   protected body Reservation_List is

      procedure Reserve (Train   : in  Trains.Train_ID;
                         Block   : in  Layout.Block_ID;
                         Success : out Boolean) is
         -- A list of blocks that cross over Block
         Cross_Blocks    : Layout.Block_List (Layout.Max_Cross_Blocks);
         One_Cross_Block : Layout.Block_ID;  -- One block from the list
      begin
         -- Can we reserve the specified block?
         if The_Reservations (Block).By = Trains.Dispatcher or
           The_Reservations (Block).By = Train             then
            Success := True;  -- Initialize, for cross block scan

            -- Can we reserve the cross blocks?
            -- First, get a list of cross blocks
            Layout.Find_Cross_Blocks (Block, Cross_Blocks);
            -- Second, check the reservation of all the cross blocks
            for Index in 1 .. Cross_Blocks.Size loop
               One_Cross_Block := Cross_Blocks.Blocks (Index);
               if The_Reservations (One_Cross_Block).By /= Trains.Dispatcher
                 and The_Reservations (One_Cross_Block).By /= Train then
                  -- Found a cross block that is not available
                  Success := False;
                  return;
               end if;
            end loop;

            -- If we reach here, the block and all the cross blocks
            -- Are Available

            -- Reserve Block
            The_Reservations (Block).By    := Train;
            The_Reservations (Block).Count := The_Reservations (Block).Count
              + 1;
            -- Reserve all of the cross blocks
            for Index in 1 .. Cross_Blocks.Size loop
               One_Cross_Block := Cross_Blocks.Blocks (Index);
               The_Reservations (One_Cross_Block).By    := Train;
               The_Reservations (One_Cross_Block).Count :=
                 The_Reservations (One_Cross_Block).Count + 1;
            end loop;

         else -- Block is already reserved
            Success := False;
         end if;
      end Reserve;
      ---------------------------------
      procedure Release (Train : in  Trains.Request_ID;
                         Block : in  Layout.Block_ID) is
         -- A list of blocks that cross over Block
         Cross_Blocks    : Layout.Block_List (Layout.Max_Cross_Blocks);
         One_Cross_Block : Layout.Block_ID;  -- One block from the list
      begin
         -- If the Block is not reserved by a train, nothing to releases
         if The_Reservations (Block).By = Trains.Dispatcher then
            The_Reservations (Block).Count := 0; -- ensure consistent count

            -- If Train is the dispatcher, release the block
         elsif Train = Trains.Dispatcher then
            The_Reservations (Block).By    := Trains.Dispatcher;
            The_Reservations (Block).Count := 0;

            -- Does Train have the right to release this block?
         elsif The_Reservations (Block).By = Train then
            -- Release one reservation on the block
            The_Reservations (Block).Count := The_Reservations (Block).Count
                                              - 1;
            if The_Reservations (Block).Count = 0 then
               The_Reservations (Block).By := Trains.Dispatcher;
            end if;
            -- Release one reservation on each cross block
            Layout.Find_Cross_Blocks (Block, Cross_Blocks);
            for Index in 1 .. Cross_Blocks.Size loop
               One_Cross_Block := Cross_Blocks.Blocks (Index);
               The_Reservations (One_Cross_Block).Count :=
                 The_Reservations (One_Cross_Block).Count - 1;
               if The_Reservations (One_Cross_Block).Count = 0 then
                  The_Reservations (One_Cross_Block).By  := Trains.Dispatcher;
               end if;
            end loop;
         end if;
      end Release;
      ---------------------------------
      procedure Clear (Train : in Trains.Request_ID) is
      begin
         for Block in The_Reservations'Range loop
            if The_Reservations (Block).By = Train then
               The_Reservations (Block).Count := 0;
               The_Reservations (Block).By    := Trains.Dispatcher;
            end if;
         end loop;
      end Clear;

      ---------------------------------
      procedure Clear_All is
      begin
         for Block in The_Reservations'Range loop
            The_Reservations (Block).Count := 0;
            The_Reservations (Block).By    := Trains.Dispatcher;
         end loop;
      end Clear_All;

      ---------------------------------
      function Reserved_By (Block : in  Layout.Block_ID)
                            return Trains.Request_ID is
      begin
         return The_Reservations (Block).By;
      end Reserved_By;
   end Reservation_List;


   ----------------------------------------------------------------------------
   -- Types and Protected Object for Block Powering
   ----------------------------------------------------------------------------

   type Power_Rec is
      record
         Cab      : Cabs.Cab_ID;
         Polarity : Layout.Block_Polarity;
      end record;
   for Power_Rec use
      record
         Cab      at 0 range 0 .. 2;
         Polarity at 0 range 3 .. 3;
      end record;
   for Power_Rec'Size use 4;

   type Nibble_Type is (Low, High);
   pragma Unreferenced (Low, High);

   type Power_Rec_Array is array (Nibble_Type) of Power_Rec;
   for Power_Rec_Array'Component_Size use 4;
   for Power_Rec_Array'Size use 8;

   --------------------------------------------------------
   function To_Byte is new Ada.Unchecked_Conversion
     (Source => Power_Rec_Array,
      Target => Port_IO.Byte);
   --------------------------------------------------------
   function To_Power_Rec_Array is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Power_Rec_Array);

   ----------------------------------------------------------------------------
   -- We must ensure mutual exclusive access to each digital output port.
   -- We use the following protected object for all ports to accomplish this.

   protected Power_Ports is
      procedure Change (Address  : in Port_IO.Address_Range;
                        Nibble   : in Nibble_Type;
                        Cab      : in Cabs.Cab_ID;
                        Polarity : in Layout.Block_Polarity);
      -- Change the Nibble half of the byte at Address to the
      --Given Cab and Polarity

      function Value_Of (Address : in Port_IO.Address_Range;
                         Nibble  : in Nibble_Type)          return Power_Rec;
      -- Return Nibble half of the byte at Address
   end Power_Ports;

   -----------------------------
   protected body Power_Ports is
      procedure Change (Address  : in Port_IO.Address_Range;
                        Nibble   : in Nibble_Type;
                        Cab      : in Cabs.Cab_ID;
                        Polarity : in Layout.Block_Polarity) is
         Byte_Value : Power_Rec_Array; -- Value of the byte as two Power Recs
      begin
         -- Get the current value of the byte at Address
         Byte_Value := To_Power_Rec_Array (Port_IO.In_Byte (Address));
         -- Change the desired nibble
         Byte_Value (Nibble) := (Cab, Polarity);
         -- Put the updated value back to the Port
         Port_IO.Out_Byte (Address => Address,
                           Data    => To_Byte (Byte_Value));
      end Change;

      function Value_Of (Address : in Port_IO.Address_Range;
                         Nibble  : in Nibble_Type)          return Power_Rec is
         Byte_Value : Power_Rec_Array; -- Value of the byte as two Power Recs
      begin
         -- Get the current value of the byte at Address
         Byte_Value := To_Power_Rec_Array (Port_IO.In_Byte (Address));
         -- Return the desired nibble
         return Byte_Value (Nibble);
      end Value_Of;
   end Power_Ports;

   ----------------------------------------------------------------------------
   -- Data for mapping of Block number to a digital I/O address

   Blocks_Per_Board : constant := 12;

   -- The 82C55 base addresses for the block interface boards
   subtype Board_ID is Integer range
     0 .. Integer (Layout.Block_ID'Last - 1) / Blocks_Per_Board;

   type Board_Address_Array is array (Board_ID) of Port_IO.Address_Range;

   Base : constant Board_Address_Array := (16#200#, 16#208#, 16#210#, 16#218#);

   -- The 6 82C55 ports connected to a single block interface board
   type Port_ID is (A_Low, B_Low, C_Low, A_High, B_High, C_High);
   pragma Unreferenced (A_Low, B_Low, C_Low, B_High, C_High);



   ----------------------------------------------------------------------------
   -- Exported operations
   ----------------------------------------------------------------------------
   procedure Reserve (Train   : in  Trains.Train_ID;
                      Block   : in  Layout.Block_ID;
                      Success : out Boolean) is
   begin
      Reservation_List.Reserve (Train, Block, Success);
   end Reserve;

   ----------------------------------------------------------------------------
   procedure Release (Train : in  Trains.Request_ID;
                      Block : in  Layout.Block_ID) is
   begin
      Reservation_List.Release (Train, Block);
   end Release;

   ----------------------------------------------------------------------------
   procedure Clear_Reservations (Train : in Trains.Train_ID) is
   begin
      Reservation_List.Clear (Train);
   end Clear_Reservations;

   ----------------------------------------------------------------------------
   procedure Clear_All_Reservations is
   begin
      Reservation_List.Clear_All;
   end Clear_All_Reservations;

   ----------------------------------------------------------------------------
   function Reserved_By (Block : in Layout.Block_ID) return Trains.Request_ID is
   begin
      return Reservation_List.Reserved_By (Block);
   end Reserved_By;

   ----------------------------------------------------------------------------
   procedure Assign_Cab (Block    : in Layout.Block_ID;
                         Cab      : in Cabs.Cab_ID;
                         Polarity : in Layout.Block_Polarity) is
      Board   : Board_ID;    -- The block interface board controlling Block
      Port    : Port_ID;     -- The 82C555 port controlling Block
      Address : Port_IO.Address_Range;  -- Address of the byte controlling Block
      Nibble  : Nibble_Type; -- The nibble in the byte controlling Block

   begin
      -- Calculate the address of the I/O port to which the block is connected
      Board := Board_ID ((Block - 1) / Blocks_Per_Board);
      Port  := Port_ID'Val (((Block - 1) mod Blocks_Per_Board) / 2);
      Address := Base (Board) + Port_ID'Pos (Port);
      if Port >= A_High then
         Address := Address + 1;  -- Skip over the control port located between
      end if;                     --   the low and high data ports

      -- Calculate the nibble of the I/O Port to which the block is connected
      Nibble := Nibble_Type'Val (Block rem 2);  -- Even blocks are low nibbles

      -- Change the nibble in the I/O port to which the block is connected
      Power_Ports.Change (Address  => Address,
                          Nibble   => Nibble,
                          Cab      => Cab,
                          Polarity => Polarity);
   end Assign_Cab;

   ----------------------------------------------------------------------------
   procedure Cab_Assigned (Block    : in  Layout.Block_ID;
                           Cab      : out Cabs.Cab_ID;
                           Polarity : out Layout.Block_Polarity) is
      Board   : Board_ID;    -- The block interface board controlling Block
      Port    : Port_ID;     -- The 82C555 port controlling Block
      Address : Port_IO.Address_Range;  -- Address of the byte controlling Block
      Nibble  : Nibble_Type; -- The nibble in the byte controlling Block
      Result  : Power_Rec;   -- The Cab and Polarity desired

   begin
      -- Calculate the address of the I/O port to which the block is connected
      Board := Board_ID ((Block - 1) / Blocks_Per_Board);
      Port  := Port_ID'Val (((Block - 1) mod Blocks_Per_Board) / 2);
      Address := Base (Board) + Port_ID'Pos (Port);
      if Port >= A_High then
         Address := Address + 1;  -- Skip over the control port located between
      end if;                     --   the low and high data ports

      -- Calculate the nibble of the I/O Port to which the block is connected
      Nibble := Nibble_Type'Val (Block rem 2);  -- Even blocks are low nibbles

      -- Get the value of the nibble
      Result   := Power_Ports.Value_Of (Address, Nibble);
      -- Return the results
      Cab      := Result.Cab;
      Polarity := Result.Polarity;
   end Cab_Assigned;


   -----------------------------------------------------------------------------
   -- Bit pattern to send to control port to set ports A, B, & C to output
   All_Output_Ports : constant := 16#80#;

begin
   declare
      Address : Port_IO.Address_Range;
   begin
      -- Configure the digital I/O hardware
      for Board in Board_ID loop
         -- Set all three ports on both 82C55 chips as output

         -- Low chip control port
         Port_IO.Out_Byte (Address => Base (Board) + 3,
                           Data    => All_Output_Ports);
         -- High chip control port
         Port_IO.Out_Byte (Address => Base (Board) + 7,
                           Data    => All_Output_Ports);


         -- Set all ports to zero (all blocks normal direction, null cab)
         for Port in Port_ID loop
            Address := Base (Board) + Port_ID'Pos (Port);
            if Port >= A_High then
               Address := Address + 1;  -- Skip over the control port located
            end if;                     --   between the low and high data ports
            Port_IO.Out_Byte (Address, 0);
         end loop;
      end loop;
   end;
end Blocks;
