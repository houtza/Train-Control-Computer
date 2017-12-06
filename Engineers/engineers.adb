with Trains;
with Ada.Real_Time;
with Hand_Controllers;

package body Engineers is
    task Engineer;

    task Engineer is 
        entry Do_Enable;
        entry Do_Disable;
    end Engineer;

    task body Engineer is
    begin 
      
        delay 5.0;
        
        select
            accept Do_Enable()do 
                Enable();
            end Do_Enable;
        or 
            accept Do_Disable()do
                Disable();
            end Do_Disable;
        or
            delay 5.0;
            Controller.getStatus();
            if(Controller.status == newStatus)
                Train := Controller.status;
    end Engineer;

    procedure Enable (Engineer : in Engineer_ID;
                    Train : Trains.Train_ID;
                    Controller : in Hand_Controllers.Hand_Controller_ID)is
    begin
        null;
    end Enable;

    procedure Disable (Engineers : in Engineer_ID) is
    begin
        null;
    end Disable;
end Engineers;
