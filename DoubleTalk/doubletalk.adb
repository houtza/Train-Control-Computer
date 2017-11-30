with Port_IO;
with Bounded_Queue;
with Ada.Unchecked_Conversion;
package body DoubleTalk is

   use Phrase_Strings;

   -- Control characters for Doubletalk
   Terminator : constant Character := Character'Val (0);  -- null character
   Command    : constant Character := Character'Val (1);  -- control A

   -- Address of Doubletalk's Text To Speech port
   TTS_Port : constant Port_IO.Address_Range := 16#31F#;

   -- For checking the status of Doubletalk
   type Port_Status is array (0 .. 7) of Boolean;
   for Port_Status'Component_Size use 1;
   for Port_Status'Size use 8;
   function To_Status is new Ada.Unchecked_Conversion (Source => Port_IO.Byte,
                                                       Target => Port_Status);

   -- Array of commands to set voices
   -- Voice commands consist of a digit from 0 to 7 followed by
   -- a Capital Letter O
   subtype Command_String is String (1 .. 3);
   type Voice_Command_Array is array (Voice_Range range Paul .. Robo)
                            of Command_String;
   Voice_Command : constant Voice_Command_Array :=
                     (Paul  => Command & "0O",
                      Vader => Command & "1O",
                      Bob   => Command & "2O",
                      Pete  => Command & "3O",
                      Randy => Command & "4O",
                      Biff  => Command & "5O",
                      Skip  => Command & "6O",
                      Robo  => Command & "7O");

   -----------------------------------------------------------------------------

   -- Create a queue package for bounded length phrase string queues.
   type Phrase_Rec is
      record
         Data  : Phrase_Strings.Bounded_String;  -- The string to speak
         Voice : Voice_Range;                    -- Which voice to use
      end record;

   package  Phrase_Queue  is new Bounded_Queue (Element_Type => Phrase_Rec);


   ----------------------------------------------------------------------------
   -- Here is the buffer to store phrases that come in faster
   -- than they can be spoken.
   protected Phrase_Buffer is
      procedure Enqueue (Phrase : in  Phrase_Rec);
      entry     Dequeue (Phrase : out Phrase_Rec);
   private
      The_Buffer : Phrase_Queue.Queue_Type (Max_Size => Buffer_Size);
   end Phrase_Buffer;

   -------------------------------------

   protected body Phrase_Buffer is

      -----------------
      procedure Enqueue (Phrase : in Phrase_Rec) is
         Trash : Phrase_Rec;  -- Phrase to throw away
      begin
         -- If Buffer is full, throw away the oldest value
         if Phrase_Queue.Full (The_Buffer) then
            Phrase_Queue.Dequeue (Queue => The_Buffer,
                                  Item  => Trash);
         end if;
         -- Guaranteed room for Phrase in the queue
         Phrase_Queue.Enqueue (Queue => The_Buffer,
                               Item  => Phrase);
      end Enqueue;

      -------------
      entry Dequeue (Phrase : out Phrase_Rec)
        when not Phrase_Queue.Empty (The_Buffer) is
      begin
         Phrase_Queue.Dequeue (Queue => The_Buffer,
                               Item  => Phrase);
      end Dequeue;
   end Phrase_Buffer;


   -----------------------------------------------------------------------------
   procedure Send_Character (Ch : in Character) is
   -- Send one characater to DoubleTalk's TTS port
   -- Preconditions : None
   -- Postconditions : Ch is sent to DoubleTalk's TTS port

      Status : Port_Status;    -- Status of Doubletalk board
   begin
      -- Check to see that DoubleTalk is ready to receive a character
      loop
         delay 0.01;  -- wait a bit before checking
         Status := To_Status (Port_IO.In_Byte (TTS_Port));
         exit when Status (4);  -- Bit 4 is the ready bit
      end loop;

      Port_IO.Out_Byte (Address => TTS_Port,
                        Data    => Port_IO.Byte (Character'Pos (Ch)));
   end Send_Character;


   ----------------------------------------------------------------------------
   -- Here is the task that takes phrases out of the buffer and speaks them
   task Speaker;

   task body Speaker is
      Phrase : Phrase_Rec;   -- The next phrase to speak
   begin
      loop -- Forever

         -- Get a phrase from the buffer
         Phrase_Buffer.Dequeue (Phrase);

         -- Send the change voice command for the given voice
         for Index in Command_String'Range loop
            Send_Character (Ch => Voice_Command (Phrase.Voice) (Index));
         end loop;

         -- Send the Phrase to Doubletalk one character at a time
         for Index in 1 .. Length (Phrase.Data)  loop
            Send_Character (Ch => Element (Source => Phrase.Data,
                                           Index  => Index));
         end loop;

         -- Send the phrase terminator
         Send_Character (Ch => Terminator);
      end loop;
   end Speaker;

   ----------------------------------------------------------------------------
   procedure Speak (Phrase : in Phrase_Strings.Bounded_String;
                    Voice  : in Voice_Range) is
      Bounded_Phrase : Phrase_Rec;
   begin

      -- Put the parameters into a record
      Bounded_Phrase.Data  := Phrase;
      Bounded_Phrase.Voice := Voice;

      -- Add Phrase to the buffer
      Phrase_Buffer.Enqueue (Bounded_Phrase);
   end Speak;

end DoubleTalk;
