------------------------------------------------------------------------------
-- Written by John McCormick, March 2001
-- Major modifications, April 2008
--
-- Reformatted by Andrew Berns, August 2017
------------------------------------------------------------------------------
package body Layout is

   ----------------------------------------------------------------------------
   -- The following types are for the package variable The_Layout which
   -- contains most of the information needed to describe any railroad layout
   ----------------------------------------------------------------------------
   type Hall_Block_Rec is
      record
         Hall  : Hall_ID;
         Block : Block_ID;
      end record;

   type Hall_Block_Array is array (Turn_Choice) of Hall_Block_Rec;

   type Terminator_Rec (Which : Termination_Type := A_Turnout) is
      record
         case Which is
            when A_Turnout =>
               Turnout     : Turnout_ID;
               Next_Blocks : Hall_Block_Array;
            when A_Block =>
               Hall  : Hall_ID;
               Block : Block_ID;
            when A_Deadend =>
               null;
         end case;
      end record;

   ----------------------------------------------------------------------------
   -- This constant array contains most of the description of the layout
   -- Change this constant to describe your layout.
   ----------------------------------------------------------------------------
   type Layout_Array is array (Block_ID, Block_Polarity) of Terminator_Rec;
   The_Layout : constant Layout_Array :=
          (1 => (Normal   => (A_Block, 10,  2),
                 Reversed => (A_Block,  1, 11)),
           2 => (Normal   => (A_Turnout, 6, (Left  => (23,  3),
                                             Right => (22, 17))),
                 Reversed => (A_Turnout, 3, (Left  => (9, 13),
                                             Right => (10,  1)))),
           3 => (Normal   => (A_Block, 38, 4),
                 Reversed => (A_Block, 23, 2)),
           4 => (Normal   => (A_Block, 45, 5),
                 Reversed => (A_Turnout, 18, (Left  => (39, 18),
                                              Right => (38,  3)))),
           5 => (Normal   => (A_Block, 49, 6),
                 Reversed => (A_Block, 45, 4)),
           6 => (Normal   => (A_Block, 50, 7),
                 Reversed => (A_Turnout, 22, (Left  => (48, 22),
                                              Right => (49,  5)))),
           7 => (Normal   => (A_Block, 32, 8),
                 Reversed => (A_Turnout, 23, (Left  => (50, 6),
                                              Right => (51, 40)))),
           8 => (Normal   => (A_Turnout, 12, (Left  => (29, 9),
                                              Right => (28, 27))),
                 Reversed => (A_Turnout, 10, (Left  => (31, 39),
                                              Right => (32,  7)))),
           9 => (Normal   => (A_Block, 19, 10),
                 Reversed => (A_Block, 29,  8)),
          10 => (Normal   => (A_Turnout, 15, (Left  => (6, 11),
                                              Right => (7, 12))),
                 Reversed => (A_Turnout, 13, (Left  => (19, 9),
                                              Right => (20, 29)))),
          11 => (Normal   => (A_Block, 1, 1),
                 Reversed => (A_Block, 6, 10)),
          12 => (Normal   => (A_Block, 3, 31),
                 Reversed => (A_Block, 7, 10)),
          13 => (Normal   => (A_Turnout, 2, (Left  => (9,  2),
                                             Right => (8, 14))),
                 Reversed => (A_Block,  2, 31)),
          14 => (Normal   => (A_Block, 11, 15),
                 Reversed => (A_Block,  8, 13)),
          15 => (Normal   => (A_Turnout, 5, (Left  => (15, 16),
                                             Right => (16, 28))),
                 Reversed => (A_Turnout, 4, (Left  => (12, 26),
                                             Right => (11, 14)))),
          16 => (Normal   => (A_Block, 21, 17),
                 Reversed => (A_Block, 15, 15)),
          17 => (Normal   => (A_Block, 33, 18),
                 Reversed => (A_Turnout,  7, (Left  => (21, 16),
                                              Right => (22,  2)))),
          18 => (Normal   => (A_Turnout, 17, (Left  => (39,  4),
                                              Right => (40, 19))),
                 Reversed => (A_Turnout,  8, (Left  => (34, 30),
                                              Right => (33, 17)))),
          19 => (Normal   => (A_Block, 41, 20),
                 Reversed => (A_Block, 40, 18)),
          20 => (Normal   => (A_Block, 43, 21),
                 Reversed => (A_Turnout, 19, (Left  => (42, 35),
                                              Right => (41, 19)))),
          21 => (Normal   => (A_Turnout, 21, (Left  => (47, 22),
                                              Right => (46, 23))),
                 Reversed => (A_Turnout, 20, (Left  => (44, 39),
                                              Right => (43, 20)))),
          22 => (Normal   => (A_Block, 48,  6),
                 Reversed => (A_Block, 47, 21)),
          23 => (Normal   => (A_Block, 37, 24),
                 Reversed => (A_Block, 46, 21)),
          24 => (Normal   => (A_Turnout, 11, (Left  => (27, 25),
                                              Right => (26, 28))),
                 Reversed => (A_Block, 37, 23)),
          25 => (Normal   => (A_Block, 17, 26),
                 Reversed => (A_Block, 27, 24)),
          26 => (Normal   => (A_Block, 12,  15),
                 Reversed => (A_Turnout, 14, (Left  => (17, 25),
                                              Right => (18, 27)))),
          27 => (Normal   => (A_Block, 18, 26),
                 Reversed => (A_Block, 28,  8)),
          28 => (Normal   => (A_Block, 26, 24),
                 Reversed => (A_Block, 16, 15)),
          29 => (Normal   => (A_Block, 20, 10),
                 Reversed => (A_Block, 30, 30)),
          30 => (Normal   => (A_Block, 34, 18),
                 Reversed => (A_Block, 30, 29)),
          31 => (Normal   => (A_Turnout,  1, (Left  => (2, 13),
                                              Right => (3, 12))),
                 Reversed => (A_Turnout, 16, (Left  => (4, 36),
                                              Right => (5, 32)))),
          32 => (Normal   => (A_Block, 14, 33),
                 Reversed => (A_Block, 5, 31)),
          33 => (Normal   => (A_Block, 24, 34),
                 Reversed => (A_Block, 14, 32)),
          34 => (Normal   => (A_Block, 36, 35),
                 Reversed => (A_Block, 24, 33)),
          35 => (Normal   => (A_Block, 42, 20),
                 Reversed => (A_Turnout, 9, (Left  => (36, 34),
                                             Right => (35, 38)))),
          36 => (Normal   => (A_Block, 13, 37),
                 Reversed => (A_Block,  4, 31)),
          37 => (Normal   => (A_Block, 25, 38),
                 Reversed => (A_Block, 13, 36)),
          38 => (Normal   => (A_Block, 35, 35),
                 Reversed => (A_Block, 25, 37)),
          39 => (Normal   => (A_Block, 44, 21),
                 Reversed => (A_Block, 31,  8)),
          40 => (Normal   => (A_Block, 51,  7),
                 Reversed => (Which => A_Deadend)));


   ----------------------------------------------------------------------------
   -- The following declarations describe the cross blocks in the layout.
   -- Change the constant to fit your layout.
   ----------------------------------------------------------------------------
   subtype Cross_Block_List is Block_List (Max_Size => Max_Cross_Blocks);
   type    Cross_Block_Array is array (Block_ID) of Cross_Block_List;
   Cross_Blocks : constant Cross_Block_Array :=
     (8      => (Max_Size => Max_Cross_Blocks, Size => 1, Blocks => (1 => 29)),
      29     => (Max_Size => Max_Cross_Blocks, Size => 1, Blocks => (1 => 8)),
      23     => (Max_Size => Max_Cross_Blocks, Size => 1, Blocks => (1 => 39)),
      39     => (Max_Size => Max_Cross_Blocks, Size => 1, Blocks => (1 => 23)),
      24     => (Max_Size => Max_Cross_Blocks, Size => 1, Blocks => (1 => 30)),
      30     => (Max_Size => Max_Cross_Blocks, Size => 1, Blocks => (1 => 24)),
      others => (Max_Size => Max_Cross_Blocks,
                 Size     => 0,
                 Blocks   => (1 => Block_ID'Last)));


   ----------------------------------------------------------------------------
   -- Each Hall sensor separates two (not necessarily distinct) blocks.  The
   -- following array stores the two blocks for each Hall sensor.
   ----------------------------------------------------------------------------
   type Two_Block_Array is array (1 .. 2) of Block_ID;
   type Adjacent_Block_Array is array (Hall_ID) of Two_Block_Array;
   Adjacent_Blocks : constant Adjacent_Block_Array :=
                       (1  => (1, 11),    2  => (13, 31),   3  => (12, 31),
                        4  => (31, 36),   5  => (31, 32),   6  => (10, 11),
                        7  => (10, 12),   8  => (13, 14),   9  => (2, 13),
                        10 => (1, 2),     11 => (14, 15),   12 => (15, 26),
                        13 => (36, 37),   14 => (32, 33),   15 => (15, 16),
                        16 => (15, 28),   17 => (25, 26),   18 => (26, 27),
                        19 => (9, 10),    20 => (10, 29),   21 => (16, 17),
                        22 => (2, 17),    23 => (2, 3),     24 => (33, 34),
                        25 => (37, 38),   26 => (24, 28),   27 => (24, 25),
                        28 => (8, 27),    29 => (8, 9),     30 => (29, 30),
                        31 => (8, 39),    32 => (7, 8),     33 => (17, 18),
                        34 => (18, 30),   35 => (35, 38),   36 => (34, 35),
                        37 => (23, 24),   38 => (3, 4),     39 => (4, 18),
                        40 => (18, 19),   41 => (19, 20),   42 => (20, 35),
                        43 => (20, 21),   44 => (21, 39),   45 => (4, 5),
                        46 => (21, 23),   47 => (21, 22),   48 => (6, 22),
                        49 => (5, 6),     50 => (6, 7),     51 => (7, 40));


   ----------------------------------------------------------------------------
   -- Each turnout has three limbs each connected to a different
   -- block.  The following array stores the three block IDs
   -- associated with each turnout on the layout.
   -- The type Turnout_Limb defined is spec is (Left, Right, Common);
   ----------------------------------------------------------------------------
   type Turnout_Limb_Array is array (Turnout_Limb) of Block_ID;
   type Turnout_Array      is array (Turnout_ID)   of Turnout_Limb_Array;
   The_Turnouts : constant Turnout_Array :=
                    (1  => (13, 12, 31),   2  => (2, 14, 13),
                     3  => (13, 1, 2),     4  => (26, 14, 15),
                     5  => (16, 28, 15),   6  => (3, 17, 2),
                     7  => (16, 2, 17),    8  => (30, 17, 18),
                     9  => (34, 38, 35),   10 => (39, 7, 8),
                     11 => (25, 28, 24),   12 => (9, 27, 8),
                     13 => (9, 29, 10),    14 => (25, 27, 26),
                     15 => (11, 12, 10),   16 => (36, 32, 31),
                     17 => (4, 19, 18),    18 => (18, 3, 4),
                     19 => (35, 19, 20),   20 => (39, 20, 21),
                     21 => (22, 23, 21),   22 => (22, 5, 6),
                     23 => (6, 40, 7),     24 => (40, 40, 40),
                     25 => (40, 40, 40),   26 => (40, 40, 40));


   ----------------------------------------------------------------------------
   -- A reversing node is a block boundary in which the polarity of the
   -- adjoining blocks must be opposite in order for a train to cross the
   -- Boundary.  The Following array tells whether a particular boundary
   -- (Specified By Its Hall Sensor) is a reversing node.
   ----------------------------------------------------------------------------
   type Reversing_Boundary_Array is array (Hall_ID) of Boolean;
   Reversing : constant Reversing_Boundary_Array :=
                 (3 | 4 | 5 | 26 | 30 | 31 => True,
                  others                   => False);


   ----------------------------------------------------------------------------
   -- Joint turnouts are pairs of turnouts connected in such a way that changing
   -- one in to particular directione requires the simultaneous change of its
   -- its partner to the same directions.  The following declarations provide
   -- information on the joint turnouts in the layout.
   ----------------------------------------------------------------------------
   type Joint_Turnout_Rec (Has_Joint : Boolean := False) is
      record
         case Has_Joint is
            when True =>
               Turnout   : Turnout_ID;
               Direction : Turn_Choice;
            when False =>
               null;
         end case;
      end record;

   type Joint_Turnout_Array is array (Turnout_ID) of Joint_Turnout_Rec;
   Joint_Turnouts : constant Joint_Turnout_Array :=
                      (2  => (True, 3, Left),
                       3  => (True, 2, Left),
                       6  => (True, 7, Right),
                       7  => (True, 6, Right),
                       17 => (True, 18, Left),
                       18 => (True, 17, Left),
                       others => (Has_Joint => False));


   ----------------------------------------------------------------------------
   -- Choice turnouts are those turnouts that an engineer may change.  The
   -- following declarations provide easy access to the next choice turnout
   -- for the two directions of each block.
   -- The type Block_Polarity in the spec is (Normal, Reversed)
   ----------------------------------------------------------------------------
   type Choice_Turnout_Array is array (Block_ID, Block_Polarity) of Turnout_ID;
   Choice_Turnouts : constant Choice_Turnout_Array :=
                       (1  => (6, 13),    2  => (6, 3),
                        3  => (12, 3),    4  => (12, 18),
                        5  => (12, 18),   6  => (12, 22),
                        7  => (12, 23),   8  => (12, 10),
                        9  => (15, 10),   10 => (15, 13),
                        11 => (6, 13),    12 => (16, 13),
                        13 => (2, 16),    14 => (5, 16),
                        15 => (5, 4),     16 => (17, 4),
                        17 => (17, 7),    18 => (17, 8),
                        19 => (21, 8),    20 => (21, 19),
                        21 => (21, 20),   22 => (12, 20),
                        23 => (11, 20),   24 => (11, 20),
                        25 => (5, 20),    26 => (5, 14),
                        27 => (5, 10),    28 => (20, 4),
                        29 => (15, 17),   30 => (17, 15),
                        31 => (1, 16),    32 => (21, 1),
                        33 => (21, 1),    34 => (21, 1),
                        35 => (21, 9),    36 => (21, 1),
                        37 => (21, 1),    38 => (21, 1),
                        39 => (21, 12),   40 => (12, 26));  -- 26 is garbage


   ----------------------------------------------------------------------------
   -- Functions and procedures defined in the package specification
   ----------------------------------------------------------------------------
   function Opposite (Direction : in Block_Polarity) return Block_Polarity is
   begin
      if Direction = Normal then
         return Reversed;
      else
         return Normal;
      end if;
   end Opposite;

   ----------------------------------------------------------------------------
   function Opposite (Direction : in Turn_Choice) return Turn_Choice is
   begin
      if Direction = Left then
         return Right;
      else
         return Left;
      end if;
   end Opposite;

   ----------------------------------------------------------------------------
   function Terminated_By (Block     : in Block_ID;
                           Direction : in Block_Polarity)
                                                      return Termination_Type is
   begin
      return The_Layout (Block, Direction).Which;
   end Terminated_By;

   ----------------------------------------------------------------------------
   function Adjacent_Turnout (Block     : in Block_ID;
                              Direction : in Block_Polarity)
                                                            return Turnout_ID is
   begin
      if The_Layout (Block, Direction).Which = A_Turnout then
         -- Get the value from the array
         return The_Layout (Block, Direction).Turnout;
      else
         raise Layout_Error;
      end if;
   end Adjacent_Turnout;

   ----------------------------------------------------------------------------
   function Choice_Turnout (Block     : in Block_ID;
                            Direction : in Block_Polarity) return Turnout_ID is
   begin
      if The_Layout (Block, Direction).Which /= A_Deadend then
         -- Get the value from the array
         return Choice_Turnouts (Block, Direction);
      else
         raise Layout_Error;
      end if;
   end Choice_Turnout;

   ----------------------------------------------------------------------------
   procedure Get_Adjacent_Blocks (Hall    : in  Hall_ID;
                                  Block_A : out Block_ID;
                                  Block_B : out Block_ID) is
   begin
      Block_A := Adjacent_Blocks (Hall)(1);
      Block_B := Adjacent_Blocks (Hall)(2);
   end Get_Adjacent_Blocks;

   ----------------------------------------------------------------------------
   function Next_Block (Block     : in Block_ID;
                        Direction : in Block_Polarity) return Block_ID is
   begin
      if The_Layout (Block, Direction).Which = A_Block then
         -- Get the value from the array
         return The_Layout (Block, Direction).Block;
      else
         raise Layout_Error;
      end if;
   end Next_Block;

   ----------------------------------------------------------------------------
   function Next_Block (Turnout : in Turnout_ID;
                        Limb    : in Turnout_Limb) return Block_ID is
   begin
      return The_Turnouts (Turnout)(Limb);
   end Next_Block;

   ----------------------------------------------------------------------------
   function Next_Hall (Block     : in Block_ID;
                       Direction : in Block_Polarity) return Hall_ID is
   begin
      if The_Layout (Block, Direction).Which = A_Block then
         -- Get the value from the array
         return The_Layout (Block, Direction).Hall;
      else
         raise Layout_Error;
      end if;
   end Next_Hall;

   ----------------------------------------------------------------------------
   function Next_Hall (Turnout   : in Turnout_ID;
                       Direction : in Turn_Choice) return Hall_ID is

      Common_Block : Block_ID;  -- The block on the common limb of Turnout

   begin
      Common_Block := The_Turnouts (Turnout)(Common);
      -- Need to find which end of the common block the given Turnout is on
      if The_Layout (Common_Block, Normal).Which   = A_Turnout and then
         The_Layout (Common_Block, Normal).Turnout = Turnout then
         return
           The_Layout (Common_Block, Normal).Next_Blocks (Direction).Hall;
      else
         return
           The_Layout (Common_Block, Reversed).Next_Blocks (Direction).Hall;
      end if;
   end Next_Hall;

   ----------------------------------------------------------------------------
   function Is_Reversing (Hall : in Hall_ID) return Boolean is
   begin
      return Reversing (Hall);  -- Return value in array
   end Is_Reversing;

   ----------------------------------------------------------------------------
   function Has_Force_Turnout (Block     : in Block_ID;
                               Direction : in Block_Polarity) return Boolean is
      -- Information on the block that follows Block in the given Direction
      Next_Block     : Block_ID;
      Next_Direction : Block_Polarity;

      The_Hall       : Hall_ID;    -- The hall between Block and Next_Block

   begin
      if The_Layout (Block, Direction).Which = A_Block then
         -- Get information on the next block and separating Hall
         Next_Block := The_Layout (Block, Direction).Block;
         The_Hall   := The_Layout (Block, Direction).Hall;

         -- Determine the direction of the next block that returns to Block
         if Is_Reversing (The_Hall) then
            Next_Direction := Direction;
         else
            Next_Direction := Opposite (Direction);
         end if;

         -- See if there is a turnout connecting Next_Block to Block
         if The_Layout (Next_Block, Next_Direction).Which = A_Turnout then
            return True;
         end if;
      end if;
      return False;
   end Has_Force_Turnout;

   ----------------------------------------------------------------------------
   procedure Get_Force_Turnout (Block              : in  Block_ID;
                                Direction          : in  Block_Polarity;
                                Turnout            : out Turnout_ID;
                                Required_Direction : out Turn_Choice) is
      -- Information on the block that follows Block in the given Direction
      Next_Block     : Block_ID;
      Next_Direction : Block_Polarity;

      The_Hall       : Hall_ID;        -- The hall between Block and Next_Block

   begin
      if The_Layout (Block, Direction).Which = A_Block then
         -- Get information on the next block and separating Hall
         Next_Block := The_Layout (Block, Direction).Block;
         The_Hall   := The_Layout (Block, Direction).Hall;

         -- Determine the direction of the next block that returns to Block
         if Is_Reversing (The_Hall) then
            Next_Direction := Direction;
         else
            Next_Direction := Opposite (Direction);
         end if;

         declare
            -- Shortened notation for the record we need from array The_Layout
            Terminator  : Terminator_Rec renames The_Layout (Next_Block,
                                                             Next_Direction);
         begin
            -- Determine which turnout connects Next_Block to Block
            if Terminator.Which = A_Turnout then
               Turnout := Terminator.Turnout;
               if Terminator.Next_Blocks (Right).Block = Block then
                  Required_Direction := Right;
               else
                  Required_Direction := Left;
               end if;
               return;  -- Normal return
            end if;
         end;
      end if;

      -- If we haven't returned there wasn't a force turnout
         raise Layout_Error;
   end Get_Force_Turnout;

   ----------------------------------------------------------------------------
   function Has_Joint (Turnout   : in Turnout_ID;
                       Direction : in Turn_Choice) return Boolean is
   begin
      return Joint_Turnouts (Turnout).Has_Joint and then
             Joint_Turnouts (Turnout).Direction = Direction;
   end Has_Joint;

   ----------------------------------------------------------------------------
   function Joint_Turnout (Turnout   : in  Turnout_ID;
                           Direction : in  Turn_Choice) return Turnout_ID is
   begin
      if Joint_Turnouts (Turnout).Has_Joint and then
         Joint_Turnouts (Turnout).Direction = Direction then
         return Joint_Turnouts (Turnout).Turnout;
      else
         raise Layout_Error;
      end if;
   end Joint_Turnout;

   ----------------------------------------------------------------------------
   procedure Find_Cross_Blocks (Block        : in  Block_ID;
                                Cross_Blocks : out Block_List) is
   begin
      -- Copy the list for Block from the constant array of cross block lists.
      Cross_Blocks := Layout.Cross_Blocks (Block);
   end Find_Cross_Blocks;

end Layout;
