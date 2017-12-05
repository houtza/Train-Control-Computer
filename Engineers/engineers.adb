package body Engineers is
   -- HINT: You will find it helpful to create an 'Engineer' task type which
   -- includes a rendezvous for enabling and disabling, as well as a selective
   -- accept that reads from the hand controller after a fixed polling rate.
   -- The engineer can then pass on any changes in state to the Train it
   -- controls.
   
   -----------------------------------------------------------------------------
   procedure Enable (Engineer   : in Engineer_ID;
                     Train      : in Trains.Train_ID;
                     Controller : in Hand_Controllers.Hand_Controller_ID) is
   begin
      null;
   end Enable;

   -----------------------------------------------------------------------------
   procedure Disable (Engineer : in Engineer_ID) is
   begin
      null;
   end Disable;

end Engineers;
