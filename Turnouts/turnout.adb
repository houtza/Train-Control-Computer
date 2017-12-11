with Motors;
with Ada.Real_Time;

package body Turnouts is
   use type Layout.Turn_Choice;
   use type Ada.Real_Time.Time;

   Failure_Callback : Failure_Ptr;
   Recovery_Callback : Recover_Ptr;
   Change_Callback : Change_Ptr;
   
   protected type Protected_Status_Type is
      procedure Set_Status ( Stat : in Status_Rec);
      function Get_Status return Status_Rec;
   private
      Status : Status_Rec := (Layout.Left, Layout.Left, False);
   end Protected_Status_Type;
   
   protected body Protected_Status_Type is
      procedure Set_Status ( Stat : in Status_Rec ) is
      begin
         Status := Stat;
      end Set_Status;
      Function Get_Status return Status_Rec is
      begin
         return Status;
      end Get_Status;
   end Protected_Status_Type;
   
   Statuses : array ( Layout.Turnout_ID ) of Protected_Status_Type;
   
   task type Turnout_Task is
      entry Set (Req : in Trains.Request_ID;
                 Turn_ID : in Layout.Turnout_ID;
                 Dir : in Layout.Turn_Choice);
   end Turnout_Task;
   
   task body Turnout_Task is
      Requestor : Trains.Request_ID;
      Turnout : Layout.Turnout_ID;
      Direction : Layout.Turn_Choice;
      Status : Status_Rec := (Desired => Layout.Left,
                              Current => Layout.Left,
                              Moving  => False);
      Check_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Failed : Boolean := False;
      Current_Recovery_Direction : Layout.Turn_Choice := Layout.Left;
      
   begin
      loop
         select
            accept Set (Req: in Trains.Request_ID;
                        Turn_ID : in Layout.Turnout_ID;
                        Dir : in Layout.Turn_Choice) do
               
               Requestor := Req;
               Turnout := Turn_ID;
               Direction := Dir;
            end set;
                 
            if Direction /= Status.Desired then
               Status.Moving := True;
               Status.Desired := Direction;
               Motors.Set (Turnout, Direction);
               Change_Callback ( Turnout, Status.Desired, Status.Moving );
               Check_Time := Ada.Real_Time.Clock + Time_Limit;
            end if;
         or
            When Status.Moving =>
               delay until Check_Time;
               if Motors.In_Position ( Turnout ) and ( not Failed or else Current_Recovery_Direction = Status.Desired) then
                  if Failed then
                     Failed := False;
                     Recovery_Callback ( Turnout );
                  end if;
                  Status.Moving := False;
                  Status.Current := Status.Desired;
                  
               elsif not Failed then
                  Failed := True;
                  Current_Recovery_Direction := Status.Desired;
                  Failure_CallBack ( Requestor, Turnout );
               end if;
               if Failed then
                  Current_Recovery_Direction := Layout.Opposite ( Current_Recovery_Direction );
                  Motors.Set ( Turnout, Current_Recovery_Direction );
                  Check_Time := Ada.Real_Time.Clock + Time_Limit;
               end if;
         end select;   
         Statuses ( Turnout ).Set_Status (Status);
      end loop;
            
            
   end Turnout_Task;
   
   Turnout_Monitors : array (Layout.Turnout_ID) of Turnout_Task;
   
   --------------------------
   -- Set_Failure_Callback --
   --------------------------
   procedure Set_Failure_Callback (To : in Failure_Ptr) is
   begin
      Failure_Callback := To;
   end Set_Failure_Callback;

   ---------------------------
   -- Set_Recovery_Callback --
   ---------------------------
   procedure Set_Recovery_Callback (To : in Recover_Ptr) is
   begin
      Recovery_Callback := To;
   end Set_Recovery_Callback;

   -------------------------
   -- Set_Change_Callback --
   -------------------------
   procedure Set_Change_Callback (To : in Change_Ptr) is
   begin
      Change_Callback := To;
   end Set_Change_Callback;

   ---------
   -- Set --
   ---------
   procedure Set
     (Requestor : in Trains.Request_ID;
      Turnout   : in Layout.Turnout_ID;
      Direction : in Layout.Turn_Choice)
   is
   begin
      Turnout_Monitors (Turnout).Set (Req     => Requestor,
                                      Turn_ID => Turnout,
                                      Dir     => Direction);
   end Set;

   ------------
   -- Status --
   ------------

   function Status (Turnout : in  Layout.Turnout_ID) return Status_Rec is
   begin
      return Statuses (Turnout).Get_Status;
   end Status;

   ------------------
   -- Direction_Of --
   ------------------

   function Direction_Of
     (Turnout : in Layout.Turnout_ID)
         return Layout.Turn_Choice
   is
   begin
      return Statuses (Turnout).Get_Status.Desired;
   end Direction_Of;

   ---------------
   -- Shut_Down --
   ---------------

   procedure Shut_Down is
      Change_Count : Integer := 0;
   begin
      for Turnout in Layout.Turnout_ID loop
         if Direction_Of ( Turnout ) /= Layout.Left then
            Motors.Set ( Turnout, Layout.Left);
            Change_Count := Change_Count + 1;
            if Change_Count = 5 then
               delay 3.0;
               Change_Count := 0;
            end if;
         end if;
      end loop;
      if Change_Count > 0 then
         delay 3.0;
      end if;
         
                   
   end Shut_Down;

end Turnouts;
