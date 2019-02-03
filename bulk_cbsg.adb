with Ada.Command_Line;
with Ada.Text_IO.Text_Streams;
with Corporate_Bullshit;
with Gnat.Regpat;

procedure Bulk_Cbsg is
   -- Produces massive amounts of bullshit until it finds a match for a regexp
   -- specified on the command line, or a timeout expires.
   
   package Text_Corporate_Bullshit is
      new Corporate_Bullshit (Paragraph_Mark     => "",
                              Paragraph_End_Mark => "",
                              Dialog_Mark        => "");

   Current_Output : constant Ada.Text_IO.Text_Streams.Stream_Access :=
     Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output);
   
   Regexp : constant String := (if Ada.Command_Line.Argument_Count = 0
                                  then "."
                                  else Ada.Command_Line.Argument (1));
   use type GNAT.Regpat.Regexp_Flags;
   Matcher : constant Gnat.Regpat.Pattern_Matcher
      := Gnat.Regpat.Compile (Regexp, Flags => GNAT.Regpat.Case_Insensitive + GNAT.Regpat.Single_Line);
begin
   loop
      declare
         Sentence : constant String := Text_Corporate_Bullshit.Sentence;
      begin
         if GNAT.Regpat.Match (Self => Matcher, Data => Sentence) then
            String'Write (Current_Output, Sentence);
            Character'Write (Current_Output, ASCII.LF);
         end if;
      end;
   end loop;
end Bulk_Cbsg;

