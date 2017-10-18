package body Layout.Search is
   ----------------------------------------------------------------------------
   --- Handle the case that the next turnout is a force turnout ...
   ----------------------------------------------------------------------------
   procedure HandleForceTurnout (Start     : in     Block_ID;       -- Rear of train
                                 Finish    : in     Block_ID;       -- Front of train
                                 Direction : in     Block_Polarity; -- Search direction
                                 Blocks    : in out Block_List;     -- Under train
                                 Turnouts  : in out Turnout_List;   -- Under train
                                 Success   : out Boolean) is

      -- Variables --
      StartBlock: Block_Rec := (Block => Start, Direction => Direction);
      NextBlock: Block_ID;
      Next_Hall_Reversing: Boolean;
      Forced_Turnout_ID: Turnout_ID;
      Forced_Turnout: Turnout_Rec;
      Forced_Turnout_Choice: Turn_Choice;


   begin
      Get_Force_Turnout(Block              => Start,
                        Direction          => Direction,
                        Turnout            => Forced_Turnout_ID,
                        Required_Direction => Forced_Turnout_Choice);

      -- add Force turnout to list.
      Forced_Turnout := (Turnout   => Forced_Turnout_ID,
                         Direction => Forced_Turnout_Choice);
      Turnouts.Size := Turnouts.Size + 1;
      Turnouts.Items(Turnouts.Size) := Forced_Turnout;



      -- Call search with block that comes after the forced turnout.
      NextBlock := Next_Block(Block  => Start,
                                        Direction => Direction);

      Next_Hall_Reversing := Is_Reversing(Next_Hall(Block   => Start,
                                                    Direction => Direction));
      Search(Start     => NextBlock,
             Finish    => Finish,
             Direction => (if Next_Hall_Reversing then Opposite(Direction => Direction) else Direction),
             Blocks    => Blocks,
             Turnouts  => Turnouts,
             Success   => Success);

      if not Success then
         Blocks.Size := Blocks.Size - 1;
         Turnouts.Size := Turnouts.Size -1;
         Success := False;
      end if;
   end HandleForceTurnout;



   ----------------------------------------------------------------------------
   --- Handle the case that a block follows the start block ...
   ----------------------------------------------------------------------------
   procedure HandleBlockTerminator (Start     : in     Block_ID;       -- Rear of train
                                    Finish    : in     Block_ID;       -- Front of train
                                    Direction : in     Block_Polarity; -- Search direction
                                    Blocks    : in out Block_List;     -- Under train
                                    Turnouts  : in out Turnout_List;   -- Under train
                                    Success   : out Boolean) is

      -- Variables --

      StartBlock: Block_Rec := (Block => Start, Direction => Direction);
      NextBlock: Block_ID := Next_Block(Block  => Start,
                                        Direction => Direction);
      Next_Hall_Reversing: Boolean := Is_Reversing(Next_Hall(Block   => Start,
                                                             Direction => Direction));


   begin
      Blocks.Size := Blocks.Size + 1;
      Blocks.Items(Blocks.Size) := StartBlock;

      if Has_Force_Turnout(Block     => Start,
                           Direction => Direction) then

         Ada.Text_IO.Put_Line ("DEBUG: Force Turnout Decected for block - "); ----------------------------------------------------------
         Ada.Text_IO.New_Line;

         -- if force turnout is next, call handle function
         HandleForceTurnout(Start     => Start,
                            Finish    => Finish,
                            Direction => Direction,
                            Blocks    => Blocks,
                            Turnouts  => Turnouts,
                            Success   => Success);

      else
         -- Call search once more, but with the next block as your start
         Search(Start     => NextBlock,
                Finish    => Finish,
                Direction => (if Next_Hall_Reversing then Opposite(Direction => Direction) else Direction),
                Blocks    => Blocks,
                Turnouts  => Turnouts,
                Success   => Success);

         if not Success then
            Ada.Text_IO.Put_Line ("DEBUG: HandleBlockTerminator - Setting to False after search of next block"); ----------------------------------------------------------
            Ada.Text_IO.New_Line;
            Blocks.Size := Blocks.Size - 1;
            Success := False;
         end if;
      end if;
   end HandleBlockTerminator;

   ----------------------------------------------------------------------------
   --- Search the Left Limb of a choice turnout ...
   ----------------------------------------------------------------------------
   procedure SearchLimbOfTurnout (Start            : in     Block_ID;       -- Rear of train
                                  Finish           : in     Block_ID;       -- Front of train
                                  Direction        : in     Block_Polarity; -- Search direction
                                  Blocks           : in out Block_List;     -- Under train
                                  Turnouts         : in out Turnout_List;   -- Under train
                                  Success          : out Boolean;
                                  Turnout_Choice   : in Turn_Choice;
                                  Choice_Turnout_ID: in Turnout_ID) is

      -- Variables --
      StartBlock: Block_Rec := (Block => Start, Direction => Direction);
      NextBlock: Block_ID;
      Next_Hall_Reversing: Boolean;
      Choice_Turnout: Turnout_Rec;
      Joint_Turnout_ID: Turnout_ID;
      Joint_Turnout_Obj: Turnout_Rec;

   begin
      if(Has_Joint(Turnout   =>  Choice_Turnout_ID,
                   Direction => Turnout_Choice)) then

         -- add joint turnout  && choice turnout to the list
         Joint_Turnout_ID := Joint_Turnout(Turnout   => Choice_Turnout_ID,
                                           Direction => Turnout_Choice);

         Ada.Text_IO.Put_Line ("DEBUG: SearchLimbOfTurnout - Joint Turnout detected fam"); ----------------------------------------------------------
         Ada.Text_IO.New_Line;

         Joint_Turnout_Obj := (Turnout => Joint_Turnout_ID, Direction => Turnout_Choice);
         Choice_Turnout := (Turnout => Choice_Turnout_ID, Direction => Turnout_Choice);
         Turnouts.Size := Turnouts.Size + 2;
         Turnouts.Items(Turnouts.Size - 1) := Joint_Turnout_Obj;

         Ada.Text_IO.Put_Line ("DEBUG: SearchLimbOfTurnout - adding second turnout to list for joint turnout"); ----------------------------------------------------------
         Ada.Text_IO.New_Line;
         Turnouts.Items(Turnouts.Size) := Choice_Turnout;


         -- need to add the start block to the list
         Blocks.Size := Blocks.Size + 1;
         Blocks.Items(Blocks.Size) := StartBlock;

         NextBlock := Next_Block(Turnout => Choice_Turnout_ID,
                                 Limb    => Turnout_Choice);

         Next_Hall_Reversing := Is_Reversing(Next_Hall(Turnout   => Choice_Turnout_ID,
                                                       Direction => Turnout_Choice));

         -- search with the block that follows the joint turnout for the given limb
         Search(Start     => NextBlock,
                Finish    => Finish,
                Direction => (if Next_Hall_Reversing then Opposite(Direction => Direction) else Direction),
                Blocks    => Blocks,
                Turnouts  => Turnouts,
                Success   => Success);

         if not Success then
            Blocks.Size := Blocks.Size - 1;
            Turnouts.Size := Turnouts.Size -2;
            Success := False;
         end if;

      else
         Ada.Text_IO.Put_Line ("DEBUG: SearchLimbOfTurnout - not a joint turnout"); ----------------------------------------------------------
         Ada.Text_IO.New_Line;
          -- add choice turnout to the list
         Choice_Turnout := (Turnout => Choice_Turnout_ID, Direction => Turnout_Choice);
         Turnouts.Size := Turnouts.Size + 1;
         Turnouts.Items(Turnouts.Size) := Choice_Turnout;


         -- need to add the start block to the list
         Blocks.Size := Blocks.Size + 1;
         Blocks.Items(Blocks.Size) := StartBlock;

         NextBlock := Next_Block(Turnout => Choice_Turnout_ID,
                                 Limb    => Turnout_Choice);

         Next_Hall_Reversing := Is_Reversing(Next_Hall(Turnout   => Choice_Turnout_ID,
                                                       Direction => Turnout_Choice));

         -- search with the block that follows the choice turnout for the given limb
         Search(Start     => NextBlock,
                Finish    => Finish,
                Direction => (if Next_Hall_Reversing then Opposite(Direction => Direction) else Direction),
                Blocks    => Blocks,
                Turnouts  => Turnouts,
                Success   => Success);

         if not Success then
            Ada.Text_IO.Put_Line ("DEBUG: SearchLimbOfTurnout - not sucess"); ----------------------------------------------------------
            Ada.Text_IO.New_Line;
            Blocks.Size := Blocks.Size - 1;
            Turnouts.Size := Turnouts.Size -1;
            Success := False;
         end if;
      end if;

   end SearchLimbOfTurnout;


   ----------------------------------------------------------------------------
   --- Handle the case that the next turnout is a choice turnout ...
   ----------------------------------------------------------------------------
   procedure HandleChoiceTurnout (Start    : in     Block_ID;       -- Rear of train
                                 Finish    : in     Block_ID;       -- Front of train
                                 Direction : in     Block_Polarity; -- Search direction
                                 Blocks    : in out Block_List;     -- Under train
                                 Turnouts  : in out Turnout_List;   -- Under train
                                 Success   : out Boolean) is

      -- Variables --
      Choice_Turnout_ID: Turnout_ID;
   begin
      Choice_Turnout_ID := Choice_Turnout(Block     => Start,
                                          Direction => Normal);

      Ada.Text_IO.Put_Line ("DEBUG: HandleChoiceTurnout - searching left limb"); ----------------------------------------------------------
      Ada.Text_IO.New_Line;
      -- search along the left limb of the choice turnout
      SearchLimbOfTurnout(Start             => Start,
                          Finish            => Finish,
                          Direction         => Direction,
                          Blocks            => Blocks,
                          Turnouts          => Turnouts,
                          Success           => Success,
                          Turnout_Choice    => Left,
                          Choice_Turnout_ID => Choice_Turnout_ID);

      -- search along the right limb of the choice turnout
      if not Success then
         Ada.Text_IO.Put_Line ("DEBUG: HandleChoiceTurnout - searching right limb"); ----------------------------------------------------------
         Ada.Text_IO.New_Line;
         SearchLimbOfTurnout(Start             => Start,
                             Finish            => Finish,
                             Direction         => Direction,
                             Blocks            => Blocks,
                             Turnouts          => Turnouts,
                             Success           => Success,
                             Turnout_Choice    => Right,
                             Choice_Turnout_ID => Choice_Turnout_ID);
      end if;
   end HandleChoiceTurnout;


   ----------------------------------------------------------------------------
   --- Handle the case that a turnout follows the start block ...
   ----------------------------------------------------------------------------
   procedure HandleTurnoutTerminator (Start     : in     Block_ID;       -- Rear of train
                                      Finish    : in     Block_ID;       -- Front of train
                                      Direction : in     Block_Polarity; -- Search direction
                                      Blocks    : in out Block_List;     -- Under train
                                      Turnouts  : in out Turnout_List;   -- Under train
                                      Success   :        out Boolean) is

      -- Variables --
   begin
      if Turnouts.Size = Turnouts.Max_Size then
         Success := False;
      else
            Ada.Text_IO.Put_Line ("DEBUG: Choice Terminator Detected"); ----------------------------------------------------------
            Ada.Text_IO.New_Line;

            -- a choice turnout is next, call handle function
            HandleChoiceTurnout(Start     => Start,
                                Finish    => Finish,
                                Direction => Direction,
                                Blocks    => Blocks,
                                Turnouts  => Turnouts,
                                Success   => Success);
      end if;
   end HandleTurnoutTerminator;


   ----------------------------------------------------------------------------
   -- Consider using this as a local recursive search procedure ...
   ----------------------------------------------------------------------------
   procedure Search (Start     : in     Block_ID;       -- Rear of train
                     Finish    : in     Block_ID;       -- Front of train
                     Direction : in     Block_Polarity; -- Search direction
                     Blocks    : in out Block_List;     -- Under train
                     Turnouts  : in out Turnout_List;   -- Under train
                     Success   : out Boolean) is

      -- Local variables will go here
      StartBlock: Block_Rec := (Block => Start, Direction => Direction);
      Terminating_Element: Termination_Type := Terminated_By(Block     => Start,
                                                             Direction => Direction);
   begin
      if Blocks.Size = Blocks.Max_Size then
         -- We've exceeded the max block size. Train not found.
         Ada.Text_IO.Put_Line ("DEBUG: Blocks Size is max. Setting Success to false"); ----------------------------------------------------------
         Ada.Text_IO.New_Line;
         Success := False;
      elsif Start = Finish then
         Ada.Text_IO.Put_Line ("DEBUG: Start = Finish. Success!"); ----------------------------------------------------------
         Ada.Text_IO.New_Line;
         -- Success case. We've found what we were looking for
         Blocks.Size := Blocks.Size + 1;
         Blocks.Items(Blocks.Size) := StartBlock;
         Success := True;
      else
         -- Need to check what's next after the start block
         if Terminating_Element = A_Block then
            Ada.Text_IO.Put_Line ("DEBUG: Block Terminator Detected"); ----------------------------------------------------------
            Ada.Text_IO.New_Line;

           -- If It's a block, call helper method for block.
            HandleBlockTerminator(Start     => Start,
                                  Finish    => Finish,
                                  Direction => Direction,
                                  Blocks    => Blocks,
                                  Turnouts  => Turnouts,
                                  Success   => Success);

         elsif Terminating_Element = A_Turnout then
            Ada.Text_IO.Put_Line ("DEBUG: Turnout Terminator Detected"); ----------------------------------------------------------
            Ada.Text_IO.New_Line;

            -- If It's a turnout, call helper method for turnout.
            HandleTurnoutTerminator(Start     => Start,
                                    Finish    => Finish,
                                    Direction => Direction,
                                    Blocks    => Blocks,
                                    Turnouts  => Turnouts,
                                    Success   => Success);
         else
            --Handle dead end terminator
             Ada.Text_IO.Put_Line ("DEBUG: Dead End Terminator Detected"); ----------------------------------------------------------
             Ada.Text_IO.New_Line;
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
      Ada.Text_IO.Put_Line ("DEBUG: Searching Normal Direction "); ----------------------------------------------------------
      Ada.Text_IO.New_Line;

      --Reset list if they had previous values.
      --In case of repeat calls to this method
      Turnouts.Size := 0;
      Blocks.Size := 0;

      Search(Start => Caboose,
             Finish => Loco,
             Direction => Normal,
             Blocks => Blocks,
             Turnouts => Turnouts,
             Success => Success);

      if not Success then
         Ada.Text_IO.Put_Line ("DEBUG: Searching Reversed Direction "); ----------------------------------------------------------
         Ada.Text_IO.New_Line;

         Search(Start => Caboose,
                Finish => Loco,
                Direction => Reversed,
                Blocks => Blocks,
                Turnouts => Turnouts,
                Success => Success);
      end if;

   end Blocks_Beneath;

end Layout.Search;
