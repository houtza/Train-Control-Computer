-- From McCormick, Singhoff, and Hugues, Building Parallel, Embedded, and
-- Real-Time Systems, 2011

generic
   type Element_Type is private;
package Bounded_Queue is

   type Queue_Type (Max_Size : Positive) is limited private;

   Overflow  : exception;
   Underflow : exception;

   procedure Clear (Queue : in out Queue_Type);

   -- Overflow is raised on attempt to Enqueue an element onto
   --          a full queue.  Queue is unchanged.
   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type);

   -- Underflow is raised on attempt to dequeue an element from
   --           an empty Queue.  Queue remains empty.
   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type);

   function Full (Queue : in Queue_Type) return Boolean;

   function Empty (Queue : in Queue_Type) return Boolean;

private

   type Queue_Array is array (Positive range <>) of Element_Type;
   type Queue_Type (Max_Size : Positive) is
      record
        Count : Natural  := 0;
        Front : Positive := 1;
        Rear  : Positive := Max_Size;
        Items : Queue_Array (1 .. Max_Size);
      end record;

end Bounded_Queue;
