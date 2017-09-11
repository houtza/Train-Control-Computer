-- Written by John McCormick, March 2001, reformatted February 2008
-- Reformatted September 2016 by Andrew Berns

-- This package provides a static description of the railroad layout which
-- includes block of track, turnouts, and Hall sensors.  It is static in
-- the sense that it does not depend on the state of any objects (such as
-- turnouts) in the layout.

-- This package can be used with any layout by changing the following
-- types and constants in this package:
--
--    In the Specification (this file):
--        Max_Cross_Blocks
--        Block_ID
--        Hall_ID
--        Turnout_ID
--
--    In the Body, the constants
--        The_Layout
--        Cross_Blocks
--        Adjacent Blocks
--        Is_Reversing
--        Joint_Turnouts
--        Choice_Turnouts
package Layout is

   pragma Pure (Layout);

   -- The maximum number of blocks that cross over any single block
   Max_Cross_Blocks : constant := 1;

   -- Each object in the layout is identified by a number.
   type Block_ID   is range 1 .. 40;
   type Hall_ID    is range 1 .. 51;
   type Turnout_ID is range 1 .. 26;


   -- Each turnout has three limbs, two of which are choices
   type    Turnout_Limb is (Left, Right, Common);
   subtype Turn_Choice  is Turnout_Limb range Left .. Right;

   --Each block is powered in one of two directions
   type Block_Polarity is (Normal, Reversed);

   -- Each block is terminated by one of three terminators
   type Termination_Type is (A_Turnout, A_Block, A_Deadend);

   -- For variable length lists of block IDs
   type Block_Array is array (Positive range <>) of Block_ID;
   type Block_List (Max_Size : Positive) is
      record
         Size   : Natural := 0;
         Blocks : Block_Array (1 .. Max_Size);
      end record;

   ----------------------------------------------------------------------------
   -- The LAYOUT_ERROR exception is raised when a operation is in conflict
   -- with the physical railroad layout.  While this conflict might be due
   -- to an incorrectly specified layout in this package it is more likely
   -- that the parameters passed to an operation are the source of the conflict.
   ----------------------------------------------------------------------------
   Layout_Error : exception;


   ----------------------------------------------------------------------------
   -- Returns the opposite of the block direction given
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the opposite direction of the given polarity
   --                  (Normal -> Reversed, Reversed -> Normal)
   --
   -- Exceptions     : none
   function Opposite (Direction : in Block_Polarity) return Block_Polarity;


   ----------------------------------------------------------------------------
   -- Returns the opposite of the choice turnout limb given
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the opposite direction of the given turnout limb
   --                  (Left -> Right, Right -> Left)
   --
   -- Exceptions     : none
   function Opposite (Direction : in Turn_Choice) return Turn_Choice;


   ----------------------------------------------------------------------------
   -- Returns the feature that terminates the Block in the given Direction
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the type of the terminator in the specified
   --                  Direction
   --
   -- Exceptions     : none
   function Terminated_By (Block     : in Block_ID;
                           Direction : in Block_Polarity)
                                                        return Termination_Type;


   ----------------------------------------------------------------------------
   -- Returns the ID of the turnout that terminates Block in the given Direction
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the ID of the turnout that terminates Block
   --                  in the given Direction
   --
   -- Exceptions     : LAYOUT_ERROR raised if Block is terminated in the given
   --                                Direction by a Hall or deadend
   function Adjacent_Turnout (Block     : in Block_ID;
                              Direction : in Block_Polarity) return Turnout_ID;


   ----------------------------------------------------------------------------
   -- Returns the ID of the next turnout with a direction choice
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the ID of the next turnout with a
   --                  Direction Choice.
   --
   -- Exceptions     : LAYOUT_ERROR raised if there is no choice turnout because
   --                               the given Block and Direction specify a
   --                               path that leads to a deadend without finding
   --                               a choice turnout.
   function Choice_Turnout (Block     : in Block_ID;
                            Direction : in Block_Polarity) return Turnout_ID;


   ----------------------------------------------------------------------------
   -- Returns the IDs of the two blocks separate by the Ggiven Hall sensor
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the ID of the two blocks that are
   --                  Adjacent To Hall
   --
   -- Exceptions     : none
   procedure Get_Adjacent_Blocks (Hall    : in  Hall_ID;
                                  Block_A : out Block_ID;
                                  Block_B : out Block_ID);


   ----------------------------------------------------------------------------
   -- Returns the ID of the next block
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the ID of the block that is adjacent to Block in
   --                  the given Direction.
   --
   -- Exceptions     : LAYOUT_ERROR raised if the next block cannot be
   --                               dertermined because
   --                                  a) It is a deadend
   --                                  b) Because there is a turnout: there
   --                                     are two possible blocks
   function Next_Block (Block     : in Block_ID;
                        Direction : in Block_Polarity) return Block_ID;


   ----------------------------------------------------------------------------
   -- Returns the ID of the block attached to the given Turnout limb
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the ID of the block that is adjacent to
   --                  given Limb of the given Turnout.
   --
   -- Exceptions     : none
   function Next_Block (Turnout : in Turnout_ID;
                        Limb    : in Turnout_Limb) return Block_ID;


   ----------------------------------------------------------------------------
   -- Returns the ID of the next Hall sensor
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the ID of the Hall sensor at the end of Block in
   --                  the given Direction.
   --
   -- Exceptions     : LAYOUT_ERROR raised if the next Hall cannot be
   --                               dertermined because
   --                                  a) It is a deadend
   --                                  b) Because there is a turnout: there
   --                                     are two possible Hall sensors
   function Next_Hall (Block     : in Block_ID;
                       Direction : in Block_Polarity) return Hall_ID;


   ----------------------------------------------------------------------------
   -- Returns the ID of the next Hall sensor at the given limb of the Turnout
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the ID of the Hall sensor at the right or left
   --                  limb of the Turnout.
   --
   -- Exceptions     : none
   function Next_Hall (Turnout   : in Turnout_ID;
                       Direction : in Turn_Choice) return Hall_ID;


   ----------------------------------------------------------------------------
   -- Returns True if Hall is on a reversing boundary and false otherwise
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns True if the given Hall sensor marks a reversing
   --                  boundary.
   --
   -- Exceptions     : none
   function Is_Reversing (Hall : Hall_ID) return Boolean;


   ----------------------------------------------------------------------------
   -- Returns True if Block has a force turnout on the Direction end
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns True exactly when the given Block has a force
   --                  turnout on the specified end
   --
   -- Exceptions     : none
   function Has_Force_Turnout (Block     : in Block_ID;
                               Direction : in Block_Polarity) return Boolean;


   ----------------------------------------------------------------------------
 -- Returns information on the force turnout at the Direction end of Block
 --
 -- Preconditions  : none
 --
 -- Postconditions : Turnout is the force turnout at the direction end of
 --                     block
 --                  Required_Direction is the direction that Turnout must
 --                     be set for the train to leave Block
 --
 -- Exceptions     : LAYOUT_ERROR raised if Block does not have a force
 --                               turnout in the given Direction
   procedure Get_Force_Turnout (Block              : in  Block_ID;
                                Direction          : in  Block_Polarity;
                                Turnout            : out Turnout_ID;
                                Required_Direction : out Turn_Choice);


   ----------------------------------------------------------------------------
   -- Returns True if Turnout has a joint turnout in the given Direction
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns True exactly when the given Turnout has a joint
   --                  turnout on the given limb (Left or Right)
   --
   -- Exceptions     : none
   function Has_Joint (Turnout   : in Turnout_ID;
                       Direction : in Turn_Choice) return Boolean;


   ----------------------------------------------------------------------------
   -- Returns the ID of the joint turnout for Turnout
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns the ID of the joint turnout for
   --                  Turnout in Direction
   --
   -- Exceptions     : LAYOUT_ERROR raised if Turnout does not have a joint
   --                               turnoutin the given direction
   function Joint_Turnout (Turnout   : in  Turnout_ID;
                           Direction : in  Turn_Choice) return Turnout_ID;


   ----------------------------------------------------------------------------
   -- Returns a list of blocks that cross over Block
   --
   -- Preconditions  : none
   --
   -- Postconditions : Cross_Blocks is a (possibly empty) list of
   --                  all blocks that cross Block
   --
   -- Exceptions     : none
   procedure Find_Cross_Blocks (Block        : in  Block_ID;
                                Cross_Blocks : out Block_List);

end Layout;
