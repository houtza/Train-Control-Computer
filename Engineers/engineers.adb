with Trains;
with Ada.Real_Time;
with Hand_Controllers;

package body Engineers is
-- HINT: You will find it helpful to create an 'Engineer' task type which
   -- includes a rendezvous for enabling and disabling, as well as a selective
   -- accept that reads from the hand controller after a fixed polling rate.
   -- The engineer can then pass on any changes in state to the Train it
   -- controls.

    task type Engineer_Task is 
        entry Enable ( E_ID : in Engineer_ID;
                        T_ID : in Trains.Train_ID;
                        C_ID : in Hand_Controllers.Hand_Controller_ID);
        entry Disable;
    end Engineer_Task;

    task body Engineer_Task is
        Engineer : Engineer_ID;
        Train : Train_ID;
        Controller : Hand_Controller_ID;
        Stop : Hand_Controllers.Button_Type;
        Horn : Hand_Controllers.Button_Type;
        Turn : Hand_Controllers.Turn_Switch_Type;
        Direction : Hand_Controllers.Direction_Switch_Type;
        Prev_Stop : Hand_Controllers.Button_Type := Hand_Controllers.Up;
        Prev_Horn : Hand_Controllers.Button_Type := Hand_Controllers.Up;
        Prev_Turn : Hand_Controllers.Turn_Switch_Type := Hand_Controllers.Centered;
        Prev_Direction : Hand_Controllers.Turn_Switch_Type := Hand_Controllers.Forward;

    begin
        loop
            accept Enable(E_ID : in Engineer;
                            T_ID : in Train;
                            C_ID : in Controller) do
                Engineer := E_ID;
                Train := T_ID;
                Controller := C_ID;
            end Enable;

            Read_Loop:
                loop
                    select
                        accept Disable;
                        exit Read_Loop;
                    or 
                        delay 0.2;
                        Hand_Controllers.Get_Digital_State ( C_ID => Controller,
                                                            Red_Button => Stop,
                                                            Black_Button => Horn,
                                                            Direction_Switch => Direction
                                                            Turn_Switch => Turn);
                        if Horn = Hand_Controllers.Down and 
                            Prev_Horn = Hand_Controllers.Up then 
                            Trains.Operations.Sound_Horn( Train_ID => Train);
                        end if;

                        if Stop = Hand_Controllers.Down and
                            Prev_Stop = Hand_Controllers.Up then
                            Trains.Operations.Stop ( Train_ID => Train);
                        end if;

                        if Direction /= Prev_Direction and 
                            Prev_Direction = Hand_Controllers.Centered then
                            if Direction = Hand_Controllers.Forward then
                               Trains.Operations.Set_Direction (Train => Train, Direction => Trains.Forward);
                            else
                                Trains.Operations.Set_Direction (Train => Train, Direction => Trains.Backward);
                            end if;
                        end if;

                        if Turn /= Prev_Turn and 
                            Turn /= Centered then
                            if Turn = Hand_Controllers.Left then
                               Trains.Operations.Turn (Train => Train, Direction => Layout.Left);
                            else
                               Train.Operations.Turn(Train => Train, Direction => Layout.Right);
                            end if;

                            Hand_Controllers.Get_Analog_State (Controller, Throttle);
                            if Throttle /= Prev_Throttle then
                               Trains.Operations.Set_Throttle (Train, Integer(Throttle));
                            end if;
                        end if;

                        Prev_Horn = Horn;
                        Prev_Stop = Stop;
                        Prev_Direction = Forward;
                        Prev_Turn = Centered;
                    end select;
            end Read_Loop;
        end loop;

        Engineer_Tasks : array (Engineer_ID) of Engineer_Task;


    procedure Enable (Engineer : in Engineer_ID;
                    Train : Trains.Train_ID;
                    Controller : in Hand_Controllers.Hand_Controller_ID)is
    begin
        Engineer_Tasks(Engineer).Enable(Engineer, Train, Controller);
    end Enable;

    procedure Disable (Engineers : in Engineer_ID) is
    begin
        Engineer_Task(Engineer).Disable;
    end Disable;
end Engineers;
