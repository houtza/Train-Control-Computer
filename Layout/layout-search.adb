package body Layout.Search is
   ----------------------------------------------------------------------------
   --- Handle the case that the next turnout is a force turnout ...
   ----------------------------------------------------------------------------
   procedure Handle_Force_Turnout (Start     : in     Block_ID;       -- Rear of train
                                   Finish    : in     Block_ID;       -- Front of train
                                   Direction : in     Block_Polarity; -- Search direction
                                   Blocks    : in out Block_List;     -- Under train
                                   Turnouts  : in out Turnout_List;   -- Under train
                                   Success   : out Boolean) is

      -- Local variables
      Start_Block: Block_Rec := (Block => Start, Direction => Direction);

      Next_Block_ID: Block_ID := Next_Block(Block  => Start,
                                            Direction => Direction);

      Next_Hall_Reversing: Boolean
        := Is_Reversing(Next_Hall(Block   => Start,
                                  Direction => Direction));

      Forced_Turnout_ID: Turnout_ID;
      Forced_Turnout: Turnout_Rec;
      Forced_Turnout_Choice: Turn_Choice;
      Search_Direction: Block_Polarity := Direction;
   begin
      Get_Force_Turnout(Block              => Start,
                        Direction          => Direction,
                        Turnout            => Forced_Turnout_ID,
                        Required_Direction => Forced_Turnout_Choice);

      -- Add Force turnout to list.
      Forced_Turnout := (Turnout   => Forced_Turnout_ID,
                         Direction => Forced_Turnout_Choice);
      Turnouts.Size := Turnouts.Size + 1;
      Turnouts.Items(Turnouts.Size) := Forced_Turnout;

      if Next_Hall_Reversing then
         Search_Direction := Opposite(Direction => Direction);
      end if;

      Search(Start     => Next_Block_ID,
             Finish    => Finish,
             Direction => Search_Direction,
             Blocks    => Blocks,
             Turnouts  => Turnouts,
             Success   => Success);
      if not Success then
         -- Finish block not found on fource turnout limb, decrement blocks
         -- and turnouts size.
         Blocks.Size := Blocks.Size - 1;
         Turnouts.Size := Turnouts.Size -1;
         Success := False;
      end if;
   end Handle_Force_Turnout;


   ----------------------------------------------------------------------------
   --- Handle the case that a block follows the start block ...
   ----------------------------------------------------------------------------
   procedure Handle_Block_Terminator (Start     : in     Block_ID;       -- Rear of train
                                      Finish    : in     Block_ID;       -- Front of train
                                      Direction : in     Block_Polarity; -- Search direction
                                      Blocks    : in out Block_List;     -- Under train
                                      Turnouts  : in out Turnout_List;   -- Under train
                                      Success   : out Boolean) is

      -- Local variables
      Start_Block: Block_Rec := (Block => Start, Direction => Direction);

      Next_Block_ID: Block_ID := Next_Block(Block  => Start,
                                            Direction => Direction);
      Next_Hall_Reversing: Boolean
        := Is_Reversing(Next_Hall(Block   => Start,
                                  Direction => Direction));

      Search_Direction: Block_Polarity := Direction;


   begin
      -- Add start block to list
      Blocks.Size := Blocks.Size + 1;
      Blocks.Items(Blocks.Size) := Start_Block;

      if Has_Force_Turnout(Block     => Start,
                           Direction => Direction) then

         -- If force turnout is next, call handle function
         Handle_Force_Turnout(Start     => Start,
                              Finish    => Finish,
                              Direction => Direction,
                              Blocks    => Blocks,
                              Turnouts  => Turnouts,
                              Success   => Success);
      else
         -- Call search with the next block as your start
          if Next_Hall_Reversing then
            Search_Direction := Opposite(Direction => Direction);
         end if;

         Search(Start     => Next_Block_ID,
                Finish    => Finish,
                Direction => Search_Direction,
                Blocks    => Blocks,
                Turnouts  => Turnouts,
                Success   => Success);

         if not Success then
            -- Finish block not found, decrement blocks size
            Blocks.Size := Blocks.Size - 1;
            Success := False;
         end if;
      end if;
   end Handle_Block_Terminator;


   ----------------------------------------------------------------------------
   --- Search a given limb of a choice turnout (Takes in the turnout choice)...
   ----------------------------------------------------------------------------
   procedure Search_Limb_Of_Turnout (Start            : in     Block_ID;       -- Rear of train
                                     Finish           : in     Block_ID;       -- Front of train
                                     Direction        : in     Block_Polarity; -- Search direction
                                     Blocks           : in out Block_List;     -- Under train
                                     Turnouts         : in out Turnout_List;   -- Under train
                                     Success          : out Boolean;
                                     Turnout_Choice   : in Turn_Choice;
                                     Choice_Turnout_ID: in Turnout_ID) is

      -- Local variables
      Start_Block: Block_Rec := (Block => Start, Direction => Direction);
      Next_Block_ID: Block_ID;
      Next_Hall_Reversing: Boolean;
      Choice_Turnout: Turnout_Rec;
      Joint_Turnout_ID: Turnout_ID;
      Joint_Turnout_Obj: Turnout_Rec;
      Search_Direction: Block_Polarity := Direction;

   begin
      if(Has_Joint(Turnout   =>  Choice_Turnout_ID,
                   Direction => Turnout_Choice)) then

         Joint_Turnout_ID := Joint_Turnout(Turnout   => Choice_Turnout_ID,
                                           Direction => Turnout_Choice);

         Joint_Turnout_Obj := (Turnout => Joint_Turnout_ID,
                               Direction => Turnout_Choice);
         Choice_Turnout := (Turnout => Choice_Turnout_ID,
                            Direction => Turnout_Choice);

         -- Add both turnouts to list. (It's a joint turnout so there's two)
         Turnouts.Size := Turnouts.Size + 2;
         Turnouts.Items(Turnouts.Size - 1) := Joint_Turnout_Obj;
         Turnouts.Items(Turnouts.Size) := Choice_Turnout;


         -- Need to add the start block to the list
         Blocks.Size := Blocks.Size + 1;
         Blocks.Items(Blocks.Size) := Start_Block;

         Next_Block_ID := Next_Block(Turnout => Choice_Turnout_ID,
                                     Limb    => Turnout_Choice);

         Next_Hall_Reversing
           := Is_Reversing(Next_Hall(Turnout   => Choice_Turnout_ID,
                                     Direction => Turnout_Choice));

         -- Search with the block that follows the joint turnout for the given limb
          if Next_Hall_Reversing then
            Search_Direction := Opposite(Direction => Direction);
         end if;

         Search(Start     => Next_Block_ID,
                Finish    => Finish,
                Direction => Search_Direction,
                Blocks    => Blocks,
                Turnouts  => Turnouts,
                Success   => Success);

         if not Success then
            -- Search failed after searching joint turnout limb.
            -- Decrement blocks and turnouts size.
            Blocks.Size := Blocks.Size - 1;
            Turnouts.Size := Turnouts.Size -2;
            Success := False;
         end if;
      else
         -- Add choice turnout to the list
         Choice_Turnout := (Turnout => Choice_Turnout_ID,
                            Direction => Turnout_Choice);
         Turnouts.Size := Turnouts.Size + 1;
         Turnouts.Items(Turnouts.Size) := Choice_Turnout;

         -- Need to add the start block to the list
         Blocks.Size := Blocks.Size + 1;
         Blocks.Items(Blocks.Size) := Start_Block;

         Next_Block_ID := Next_Block(Turnout => Choice_Turnout_ID,
                                     Limb    => Turnout_Choice);

         Next_Hall_Reversing
           := Is_Reversing(Next_Hall(Turnout   => Choice_Turnout_ID,
                                     Direction => Turnout_Choice));

         -- Search with the block that follows the choice turnout for the given limb
          if Next_Hall_Reversing then
            Search_Direction := Opposite(Direction => Direction);
         end if;

         Search(Start     => Next_Block_ID,
                Finish    => Finish,
                Direction => Search_Direction,
                Blocks    => Blocks,
                Turnouts  => Turnouts,
                Success   => Success);

         if not Success then
            -- Didn't find Finish on this limb of turnout. Derement size.
            Blocks.Size := Blocks.Size - 1;
            Turnouts.Size := Turnouts.Size -1;
            Success := False;
         end if;
      end if;

   end Search_Limb_Of_Turnout;


   ----------------------------------------------------------------------------
   --- Handle the case that the next turnout is a choice turnout ...
   ----------------------------------------------------------------------------
   procedure Handle_Choice_Turnout (Start    : in     Block_ID;       -- Rear of train
                                    Finish    : in     Block_ID;       -- Front of train
                                    Direction : in     Block_Polarity; -- Search direction
                                    Blocks    : in out Block_List;     -- Under train
                                    Turnouts  : in out Turnout_List;   -- Under train
                                    Success   : out Boolean) is

      -- Local variables
      Choice_Turnout_ID: Turnout_ID := Choice_Turnout(Block     => Start,
                                                      Direction => Direction);
   begin
      -- Search along the left limb of the choice turnout
      Search_Limb_Of_Turnout(Start             => Start,
                             Finish            => Finish,
                             Direction         => Direction,
                             Blocks            => Blocks,
                             Turnouts          => Turnouts,
                             Success           => Success,
                             Turnout_Choice    => Left,
                             Choice_Turnout_ID => Choice_Turnout_ID);

      -- Search along the right limb of the choice turnout if we didn't
      -- find the Finish block on the left limb.
      if not Success then
         Search_Limb_Of_Turnout(Start             => Start,
                                Finish            => Finish,
                                Direction         => Direction,
                                Blocks            => Blocks,
                                Turnouts          => Turnouts,
                                Success           => Success,
                                Turnout_Choice    => Right,
                                Choice_Turnout_ID => Choice_Turnout_ID);
      end if;
   end Handle_Choice_Turnout;


   ----------------------------------------------------------------------------
   -- local recursive search procedure
   ----------------------------------------------------------------------------
   procedure Search (Start     : in     Block_ID;       -- Rear of train
                     Finish    : in     Block_ID;       -- Front of train
                     Direction : in     Block_Polarity; -- Search direction
                     Blocks    : in out Block_List;     -- Under train
                     Turnouts  : in out Turnout_List;   -- Under train
                     Success   : out Boolean) is

      -- Local variables
      Start_Block: Block_Rec := (Block => Start, Direction => Direction);

      Terminating_Element: Termination_Type
        := Terminated_By(Block     => Start,
                         Direction => Direction);
   begin
      if Blocks.Size = Blocks.Max_Size then
         -- We've exceeded the max block size. Finish block not found.
         Success := False;
      elsif Start = Finish then
         -- Success case. We've found what we were looking for.
         Blocks.Size := Blocks.Size + 1;
         Blocks.Items(Blocks.Size) := Start_Block;
         Success := True;
      else
         -- Need to check what's next after the start block
         if Terminating_Element = A_Block then
            -- If It's a block, call helper method for block.
            Handle_Block_Terminator(Start     => Start,
                                    Finish    => Finish,
                                    Direction => Direction,
                                    Blocks    => Blocks,
                                    Turnouts  => Turnouts,
                                    Success   => Success);

         elsif Terminating_Element = A_Turnout then
            -- a choice turnout is next, call handle function
            -- if the turnouts list isn't full
            if Turnouts.Size = Turnouts.Max_Size then
               Success := False;
            else
               Handle_Choice_Turnout(Start     => Start,
                                     Finish    => Finish,
                                     Direction => Direction,
                                     Blocks    => Blocks,
                                     Turnouts  => Turnouts,
                                     Success   => Success);
            end if;
         else
            -- Handle dead end terminator
            Success := False;
         end if;
      end if;
   end Search;

   ----------------------------------------------------------------------------
   procedure Blocks_Beneath (Loco     : in  Block_ID;
                             Caboose  : in  Block_ID;
                             Blocks   : out Block_List;
                             Turnouts : out Turnout_List;
                             Success  : out Boolean) is
   begin
      -- Reset list if they had previous values.
      -- In case of repeat calls to this method
      Turnouts.Size := 0;
      Blocks.Size := 0;

      -- Search normal direction
      Search(Start => Caboose,
             Finish => Loco,
             Direction => Normal,
             Blocks => Blocks,
             Turnouts => Turnouts,
             Success => Success);

      -- If not successfull search, then try the reversed direction
      if not Success then
         Search(Start => Caboose,
                Finish => Loco,
                Direction => Reversed,
                Blocks => Blocks,
                Turnouts => Turnouts,
                Success => Success);
      end if;
   end Blocks_Beneath;

end Layout.Search;
