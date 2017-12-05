package body Sound is

   -- Horn blast durations
   Short : constant Duration := 0.8;
   Long  : constant Duration := 3.2;
   Pause : constant Duration := 0.6;

   -- Horn blast durations for one signal are maintained in this array type
   Max_Blasts : constant := 4;  -- Maximum number of horn blasts in a signal
   type Duration_Array is array (1 .. Max_Blasts) of Duration;

   -- Horn blast durations for all signals are maintained in this array type
   type Signal_Array is array (Horn_Signal) of Duration_Array;

   -- The signal data
   Signal_Duration  : constant Signal_Array :=
          (Stop              => (Short, others => 0.0),
           Start             => (Long, Long, others => 0.0),
           Approach_Highway  => (Long, Long, Short, Long, others => 0.0),
           Approach_Crossing => (Long, Long, Short, others => 0.0),
           Warning           => (Long, Short, others => 0.0));



   ----------------------------------------------------------------------------
   -- Ensure mutual exclusive use of the Dallee Sound Unit interface board
   protected Sound_Unit is
      -- Operations that mirror Dallee operations
      procedure Set_Horn (Unit : in Dallee.Dallee_ID;
                          To   : in Dallee.Setting);
      procedure Set_Bell (Unit : in Dallee.Dallee_ID;
                          To   : in Dallee.Setting);
   end Sound_Unit;
   ----------------------------------------------------------------------------
   protected body Sound_Unit is
      -- Simply call the Dallee operation

      procedure Set_Horn (Unit : in Dallee.Dallee_ID;
                          To   : in Dallee.Setting) is
      begin
         Dallee.Set_Horn (Unit, To);
      end Set_Horn;

      procedure Set_Bell (Unit : in Dallee.Dallee_ID;
                          To   : in Dallee.Setting) is
      begin
         Dallee.Set_Bell (Unit, To);
      end Set_Bell;

   end Sound_Unit;


   ----------------------------------------------------------------------------
   task type Horn_Task  is
      entry Initialize (Unit : in Installed_Range);
      entry Blow (The_Signal : in Horn_Signal);
   end Horn_Task;
   ----------------------------------------------------------------------------
   task body Horn_Task is

      My_Unit : Installed_Range;  -- The sound unit controlled by this task
      Signal  : Horn_Signal;      -- The signal being blown

   begin
      -- Get the Dallee sound unit that this task will control
      accept Initialize (Unit : in Installed_Range) do
         My_Unit := Unit;
      end Initialize;

      -- Handle all horn signal requests
      -- Each iteration, handle one signal request
      Signal_Loop :
      loop
         -- Wait to get a signal to blow
         accept Blow (The_Signal : in Horn_Signal) do
            Signal := The_Signal;
         end Blow;

         -- Blow the given signal
         -- Each iteration, blow one blast
         Blast_Loop :
         for Blast_Count in 1 .. Max_Blasts loop
            -- Short circuit loop if signal has fewer than Max_Blasts
            exit Blast_Loop when Signal_Duration (Signal)(Blast_Count) = 0.0;

            -- Turn on the horn
            Sound_Unit.Set_Horn (Unit  => My_Unit,
                                 To    => Dallee.On);
            -- Leave horn on for the correct amount of time
            delay Signal_Duration (Signal)(Blast_Count);
            -- Turn off the horn
            Sound_Unit.Set_horn (Unit => My_Unit,
                                 To   => Dallee.Off);
            -- Delay before sounding the next blast
            delay Pause;
         end loop Blast_Loop;
      end loop Signal_Loop;
   end Horn_Task;


   ----------------------------------------------------------------------------
   -- The array of Horn tasks
   type Horn_Task_Array is array (Installed_Range) of Horn_Task;
   Horns : Horn_Task_Array;

   ----------------------------------------------------------------------------
   procedure Sound_Horn (Unit   : in Installed_Range;
                         Signal : in Horn_Signal) is
   begin
      -- Give the horn task a signal to blow
      select
         Horns (Unit).Blow (Signal);
      else
         null;     -- Ignore request if the Horn task is busy
      end select;
   end Sound_Horn;

   ----------------------------------------------------------------------------
   procedure Bell_On (Unit : in Installed_Range) is
   begin
      Sound_Unit.Set_Bell (Unit => Unit,
                           To   => Dallee.On);
   end Bell_On;

   ----------------------------------------------------------------------------
   procedure Bell_Off (Unit : in Installed_Range) is
   begin
      Sound_Unit.Set_Bell (Unit => Unit,
                           To   => Dallee.Off);
   end Bell_Off;

begin
   -- Give each task a Dallee unit to control
   for Index in Installed_Range loop
      Horns (Index).Initialize (Unit => Index);
   end loop;
end Sound;
