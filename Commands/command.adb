-- Written by John McCormick
with Ada.Text_IO;
with Ada.Characters.Handling;

package body Command is

   use Ada.Characters.Handling;
   use Engineers;
   use Trains;

   type State_Type is (Start, Got_One, Got_Two, Simple, Compound);

   ----------------------------------------------------------------------------
   procedure Get (Command : out Command_Rec) is
      -- Each command consists of one, two, or three characters
      First  : Character;
      Second : Character;
      Third  : Character;

      -- The number that proceeds a command
      Number : Natural;

      -- The current state of the command processor
      State : State_Type := Start;

   begin
      State_Loop :
      loop
         case State is
            when Start =>
               Ada.Text_IO.Get (First);
               if Is_Digit (First) then
                  State := Got_One;
               else
                  First := To_Lower (First);
                  State := Simple;
               end if;

            when Simple =>
               -- Fill in the out parameter Command
               if First = ' ' then
                  Command := (Which => Stop_All);
               elsif First = 'r' then
                  Command := (Which => Restart);
               elsif First = 'q' then
                  Command := (Which => Quit);
               else
                  Command := (Which => Error);
               end if;
               exit State_Loop;  -- All done

            when Got_One =>
               Ada.Text_IO.Get (Second);
               if Is_Digit (Second) then
                  Number := Positive'Value (First & Second);
                  State := Got_Two;
               else
                  Third := To_Lower (Second);
                  Number := Positive'Value (' ' & First);
                  State := Compound;
               end if;

            when Got_Two =>
               Ada.Text_IO.Get (Third);
               Third := To_Lower (Third);
               State   := Compound;

            when Compound =>
               -- Fill in the out parameter Command
               Excepton_Block :
               begin -- exception block
                  if Third = 's' then
                     Command := (Which => Stop,
                                 Train => Train_ID (Number));
                  elsif Third = 'g' then
                     Command := (Which => Go,
                                 Train => Train_ID (Number));
                  elsif Third = 'l' then
                     Command := (Which   => Left,
                                 Turnout => Layout.Turnout_ID (Number));
                  elsif Third = 'r' then
                     Command := (Which   => Right,
                                 Turnout => Layout.Turnout_ID (Number));
                  elsif Third = 'f' then
                     Command := (Which    => Free,
                                 Block    => Layout.Block_ID (Number));
                  else
                     Command := (Which => Error);
                  end if;

               exception
                  -- Raised when converting out of range Number to an ID type
                  -- (e.g. converting 31 into a Turnout_ID)
                  when Constraint_Error =>
                     Command := (Which => Error);

               end Excepton_Block;

               exit State_Loop;  -- All done
         end case;

      end loop State_Loop;
   end Get;

end Command;
