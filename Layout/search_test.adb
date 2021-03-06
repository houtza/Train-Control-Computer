with Ada.Text_IO;
with Layout.Search;

procedure Search_Test is

    Test_Failed : exception; -- used in the test cases

   procedure Test_Two_Sequential_Blocks_Succeed_With_Normal_Direction is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 1;
      Caboose : Layout.Block_ID := 11;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works, and is correct");
      Ada.Text_IO.Put_Line ("CABOOSE: 11");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 01");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(11, Normal), (1, Normal)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: Null ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 0
        and then Blocks.Items(1).Block = 11
        and then Blocks.Items(1).Direction = Layout.Normal
        and then Blocks.Items(2).Block = 1
        and then Blocks.Items(2).Direction = Layout.Normal then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;


      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_With_Normal_Direction;

   procedure Test_Two_Sequential_Blocks_Succeed_With_Reversed_Direction is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 11;
      Caboose : Layout.Block_ID := 1;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works, and is correct (with reversed direction");
      Ada.Text_IO.Put_Line ("CABOOSE: 1");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 11");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(1, Reversed), (11, Reversed)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: Null ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 0
        and then Blocks.Items(1).Block = 1
        and then Blocks.Items(1).Direction = Layout.Reversed
        and then Blocks.Items(2).Block = 11
        and then Blocks.Items(2).Direction = Layout.Reversed then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;


      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_With_Reversed_Direction;

   procedure Test_Two_Sequential_Blocks_Succeed_While_Over_Forced_Turnout_Left_Limb is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 15;
      Caboose : Layout.Block_ID := 26;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works, and is correct when on top of a left limb forced turnout");
      Ada.Text_IO.Put_Line ("CABOOSE: 26");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 15");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(26, Normal), (15 Normal)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: {(4, Left)} ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 1
        and then Blocks.Items(1).Block = 26
        and then Blocks.Items(1).Direction = Layout.Normal
        and then Blocks.Items(2).Block = 15
        and then Blocks.Items(2).Direction = Layout.Normal
        and then Turnouts.Items(1).Turnout = 4
        and then Turnouts.Items(1).Direction = Layout.Left
      then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;


      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_While_Over_Forced_Turnout_Left_Limb;

   procedure Test_Two_Sequential_Blocks_Succeed_While_Over_Forced_Turnout_Right_Limb is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 8;
      Caboose : Layout.Block_ID := 7;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works, and is correct when on top of a right limb forced turnout");
      Ada.Text_IO.Put_Line ("CABOOSE: 7");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 8");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(7, Normal), (8 Normal)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: {(10, Right)} ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 1
        and then Blocks.Items(1).Block = 7
        and then Blocks.Items(1).Direction = Layout.Normal
        and then Blocks.Items(2).Block = 8
        and then Blocks.Items(2).Direction = Layout.Normal
        and then Turnouts.Items(1).Turnout = 10
        and then Turnouts.Items(1).Direction = Layout.Right
      then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;


      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_While_Over_Forced_Turnout_Right_Limb;

   procedure Test_Two_Sequential_Blocks_Succeed_While_Over_Forced_Turnout_Reversed_Direction is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 31;
      Caboose : Layout.Block_ID := 32;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works, and is correct when on top of a forced turnout with a reversing hall");
      Ada.Text_IO.Put_Line ("CABOOSE: 32");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 31");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(32, Reversed), (31, Normal)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: {(16, Right)} ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 1
        and then Blocks.Items(1).Block = 32
        and then Blocks.Items(1).Direction = Layout.Reversed
        and then Blocks.Items(2).Block = 31
        and then Blocks.Items(2).Direction = Layout.Normal
        and then Turnouts.Items(1).Turnout = 16
        and then Turnouts.Items(1).Direction = Layout.Right
      then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;


      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_While_Over_Forced_Turnout_Reversed_Direction;

   procedure Test_Two_Sequential_Blocks_Succeed_While_Over_Choice_Turnout_Left_Limb is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 9;
      Caboose : Layout.Block_ID := 8;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works, and is correct when on top of a left limb choice turnout");
      Ada.Text_IO.Put_Line ("CABOOSE: 8");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 9");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(8, Normal), (9 Normal)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: {(12, Left)} ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 1
        and then Blocks.Items(1).Block = 8
        and then Blocks.Items(1).Direction = Layout.Normal
        and then Blocks.Items(2).Block = 9
        and then Blocks.Items(2).Direction = Layout.Normal
        and then Turnouts.Items(1).Turnout = 12
        and then Turnouts.Items(1).Direction = Layout.Left
      then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;


      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_While_Over_Choice_Turnout_Left_Limb;

   procedure Test_Two_Sequential_Blocks_Succeed_While_Over_Choice_Turnout_Right_Limb is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 27;
      Caboose : Layout.Block_ID := 8;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works, and is correct when on top of a left limb choice turnout");
      Ada.Text_IO.Put_Line ("CABOOSE: 8");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 27");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(8, Normal), (27 Normal)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: {(12, Left)} ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 1
        and then Blocks.Items(1).Block = 8
        and then Blocks.Items(1).Direction = Layout.Normal
        and then Blocks.Items(2).Block = 27
        and then Blocks.Items(2).Direction = Layout.Normal
        and then Turnouts.Items(1).Turnout = 12
        and then Turnouts.Items(1).Direction = Layout.Right
      then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;


      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_While_Over_Choice_Turnout_Right_Limb;

   procedure Test_Two_Sequential_Blocks_Succeed_While_Over_Choice_Turnout_Reversed_Direction is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 32;
      Caboose : Layout.Block_ID := 31;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works, and is correct when on top of a choice turnout with a reversing hall");
      Ada.Text_IO.Put_Line ("CABOOSE: 32");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 31");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(31, Reversed), (32, Normal)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: {(16, Right)} ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 1
        and then Blocks.Items(1).Block = 31
        and then Blocks.Items(1).Direction = Layout.Reversed
        and then Blocks.Items(2).Block = 32
        and then Blocks.Items(2).Direction = Layout.Normal
        and then Turnouts.Items(1).Turnout = 16
        and then Turnouts.Items(1).Direction = Layout.Right
      then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;


      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_While_Over_Choice_Turnout_Reversed_Direction;

   procedure Test_Two_Sequential_Blocks_Succeed_While_Over_Joint_Turnout_Normal_Direction is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 2;
      Caboose : Layout.Block_ID := 13;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works in the normal direction, and is correct when on top of a joint turnout");
      Ada.Text_IO.Put_Line ("CABOOSE: 13");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 2");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(13, Normal), (2 Normal)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: {(2, Left), (3, Left)} ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 2
        and then Blocks.Items(1).Block = 13
        and then Blocks.Items(1).Direction = Layout.Normal
        and then Blocks.Items(2).Block = 2
        and then Blocks.Items(2).Direction = Layout.Normal
        and then Turnouts.Items(1).Turnout = 3
        and then Turnouts.Items(1).Direction = Layout.Left
        and then Turnouts.Items(2).Turnout = 2
        and then Turnouts.Items(2).Direction = Layout.Left
      then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;

      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_While_Over_Joint_Turnout_Normal_Direction;

   procedure Test_Two_Sequential_Blocks_Succeed_While_Over_Joint_Turnout_Reversed_Direction is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 13;
      Caboose : Layout.Block_ID := 2;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on two sequential blocks works in the reversed direction, and is correct when on top of a joint turnout");
      Ada.Text_IO.Put_Line ("CABOOSE: 2");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 13");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(2, Reversed), (13 Reversed)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: {(3, Left), (2, Left)} ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 2
        and then Turnouts.Size = 2
        and then Blocks.Items(1).Block = 2
        and then Blocks.Items(1).Direction = Layout.Reversed
        and then Blocks.Items(2).Block = 13
        and then Blocks.Items(2).Direction = Layout.Reversed
        and then Turnouts.Items(1).Turnout = 2
        and then Turnouts.Items(1).Direction = Layout.Left
        and then Turnouts.Items(2).Turnout = 3
        and then Turnouts.Items(2).Direction = Layout.Left
      then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;

      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Two_Sequential_Blocks_Succeed_While_Over_Joint_Turnout_Reversed_Direction;

   procedure Test_Search_Succeeds_When_Multiple_Turnouts_Are_Detected is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 5);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 7);

      Loco    : Layout.Block_ID := 11;
      Caboose : Layout.Block_ID := 7;
      OK      : Boolean;
      Test_Pass: Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train on top of multiple turnouts still succeeds");
      Ada.Text_IO.Put_Line ("CABOOSE: 6");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 11");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: {(7, Normal), (8 Normal), (9 Normal), (10 Normal), (11 Normal)} ");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: {(10, Right), (12, Left), (13, Left, (15, Left)} ");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Pass ");

      -- Check that the test passes
      if Blocks.Size = 5
        and then Turnouts.Size = 4
        and then Blocks.Items(1).Block = 7
        and then Blocks.Items(1).Direction = Layout.Normal
        and then Blocks.Items(2).Block = 8
        and then Blocks.Items(2).Direction = Layout.Normal
        and then Blocks.Items(3).Block = 9
        and then Blocks.Items(3).Direction = Layout.Normal
        and then Blocks.Items(4).Block = 10
        and then Blocks.Items(4).Direction = Layout.Normal
        and then Blocks.Items(5).Block = 11
        and then Blocks.Items(5).Direction = Layout.Normal
        and then Turnouts.Items(1).Turnout = 10
        and then Turnouts.Items(1).Direction = Layout.Right
        and then Turnouts.Items(2).Turnout = 12
        and then Turnouts.Items(2).Direction = Layout.Left
        and then Turnouts.Items(3).Turnout = 13
        and then Turnouts.Items(3).Direction = Layout.Left
        and then Turnouts.Items(4).Turnout = 15
        and then Turnouts.Items(4).Direction = Layout.Left
      then
         Test_Pass := True;
      else
         Test_Pass := False;
      end if;

      if not Test_Pass then
         raise Test_Failed;
      end if;
   end Test_Search_Succeeds_When_Multiple_Turnouts_Are_Detected;

   procedure Test_Search_Fails_When_Dead_End_Or_Max_Blocks_Detected is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 1);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 10);

      Loco    : Layout.Block_ID := 11;
      Caboose : Layout.Block_ID := 40;
      OK      : Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train fails when deadend found or max_blocks limit reached in search");
      Ada.Text_IO.Put_Line ("CABOOSE: 40");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 11");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: Null");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: Null");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Fail ");

      -- Check that the test passes
      if OK then
         raise Test_Failed;
      end if;
   end Test_Search_Fails_When_Dead_End_Or_Max_Blocks_Detected;

   procedure Test_Search_Fails_When_Max_Turnouts_Detected is
      use type Layout.Block_ID;
      use type Layout.Block_Polarity;
      use type Layout.Turnout_ID;
      use type Layout.Turn_Choice;

      Blocks   : Layout.Search.Block_List   (Max_Size => 3);
      Turnouts : Layout.Search.Turnout_List (Max_Size => 1);

      Loco    : Layout.Block_ID := 7;
      Caboose : Layout.Block_ID := 9;
      OK      : Boolean;
   begin
      Layout.Search.Blocks_Beneath (Loco, Caboose, Blocks, Turnouts, OK);
      Ada.Text_IO.Put_Line ("---------------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Testing that a search for a train fails when max_turnouts limit reached in search");
      Ada.Text_IO.Put_Line ("CABOOSE: 9");
      Ada.Text_IO.Put_Line ("LOCOMOTIVE: 7");
      Ada.Text_IO.Put_Line("EXPECTED BLOCKS: Null");
      Ada.Text_IO.Put_Line("EXPECTED TURNOUTS: Null");
      Ada.Text_IO.Put_Line("EXPECTED SEARCH PASS/FAIL: Fail ");

      -- Check that the test passes
      if OK then
         raise Test_Failed;
      end if;
   end Test_Search_Fails_When_Max_Turnouts_Detected;

   use type Layout.Block_ID;
   package Block_IO     is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Turnout_IO   is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Polarity_IO  is new Ada.Text_IO.Enumeration_IO (Layout.Block_Polarity);
   package Direction_IO is new Ada.Text_IO.Enumeration_IO (Layout.Turnout_Limb);

begin
   -- reversed and normal tests
   Test_Two_Sequential_Blocks_Succeed_With_Normal_Direction;
   Test_Two_Sequential_Blocks_Succeed_With_Reversed_Direction;

   --forced turnout tests
   Test_Two_Sequential_Blocks_Succeed_While_Over_Forced_Turnout_Left_Limb;
   Test_Two_Sequential_Blocks_Succeed_While_Over_Forced_Turnout_Right_Limb;
   Test_Two_Sequential_Blocks_Succeed_While_Over_Forced_Turnout_Reversed_Direction;

   --choice turnout tests
   Test_Two_Sequential_Blocks_Succeed_While_Over_Choice_Turnout_Left_Limb;
   Test_Two_Sequential_Blocks_Succeed_While_Over_Choice_Turnout_Right_Limb;
   Test_Two_Sequential_Blocks_Succeed_While_Over_Choice_Turnout_Reversed_Direction;

   -- joint turnout tests
   Test_Two_Sequential_Blocks_Succeed_While_Over_Joint_Turnout_Normal_Direction;
   Test_Two_Sequential_Blocks_Succeed_While_Over_Joint_Turnout_Reversed_Direction;

   -- multiple turnouts test
   Test_Search_Succeeds_When_Multiple_Turnouts_Are_Detected;

   -- failing search tests
   Test_Search_Fails_When_Dead_End_Or_Max_Blocks_Detected;
   Test_Search_Fails_When_Max_Turnouts_Detected;
end Search_Test;
