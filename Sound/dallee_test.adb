with Ada.Text_IO;
with Dallee;

procedure Dallee_Test is
   type Sound_Type is (Horn, Bell);

   package Sound_IO is new Ada.Text_IO.Enumeration_IO (Sound_Type);
   package Unit_IO is new Ada.Text_IO.Integer_IO (Dallee.Dallee_ID);
   package Setting_IO is new Ada.Text_IO.Enumeration_IO (Dallee.Setting);
   
   Sound : Sound_Type;
   Unit : Dallee.Dallee_ID;
   Setting : Dallee.Setting;

begin
   loop
      Ada.Text_IO.Put_Line ("Enter Unit ID:");
      Unit_IO.Get (Unit);
      Ada.Text_IO.Put_Line ("Enter Sound Type (Horn or Bell):");
      Sound_IO.Get (Sound);
      Ada.Text_IO.Put_Line ("Enter setting (On or Off):");
      Setting_IO.Get (Setting);

      if Sound = Horn then
         Dallee.Set_Horn (Unit, Setting);
      else
         Dallee.Set_Bell (Unit, Setting);
      end if;
   end loop;
end Dallee_Test;