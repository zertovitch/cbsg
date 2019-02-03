with Ada.Command_Line;
with Ada.Text_IO.Text_Streams;                       use Ada.Text_IO;
with Corporate_Bullshit;
with GNAT.Regpat;

procedure Produce_Corporate_Bullshit is

   package HTML_Corporate_Bullshit is
      new Corporate_Bullshit (Paragraph_Mark     => ASCII.LF & ASCII.LF & "<li>",
                              Paragraph_End_Mark => "</li>",
                              Dialog_Mark        => "");

   package Text_Corporate_Bullshit is
      new Corporate_Bullshit (Paragraph_Mark     => "",
                              Paragraph_End_Mark => "",
                              Dialog_Mark        => "");

   procedure Produce_Workshop_Report;
   procedure Produce_Workshop_Report is
      F : File_Type;
   begin
      Create (F, Out_File, "bullshit.html");
      declare
         Output : constant Text_Streams.Stream_Access := Text_Streams.Stream (F);
      begin
         String'Write (Output, "<head>");
         String'Write (Output, "   <title>Corporate bullshit</title>");
         String'Write (Output, "   <style type=""text/css"">");
         String'Write (Output, "   li { margin-bottom: 1em }");
         String'Write (Output, "   </style>");
         String'Write (Output, "</head>");
         String'Write (Output, "<body><p><h1>Workshop minutes</h1>");
         String'Write (Output, "<ul>");
         String'Write (Output, HTML_Corporate_Bullshit.Workshop);
         String'Write (Output, "</ul></p></body></html>");
      end;
      Close (F);
      Ada.Text_IO.Put_Line ("New corporate bullshit is in bullshit.html.  Enjoy.");
   end Produce_Workshop_Report;

   Current_Output : constant Text_Streams.Stream_Access
      := Text_Streams.Stream (Ada.Text_IO.Current_Output);
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Produce_Workshop_Report;
   end if;
   for J in 1 .. Ada.Command_Line.Argument_Count loop
      if Ada.Command_Line.Argument (J) = "--one"
        or Ada.Command_Line.Argument (J) = "--sig"
        or Ada.Command_Line.Argument (J) = "-1"
      then
         String'Write (Current_Output, Text_Corporate_Bullshit.Sentence);
         Character'Write (Current_Output, ASCII.LF);
      end if;
      if Ada.Command_Line.Argument (J) = "--workshop"
        or Ada.Command_Line.Argument (J) = "-w"
      then
         Produce_Workshop_Report;
      end if;
      if Ada.Command_Line.Argument (J) = "--bulk"
        or Ada.Command_Line.Argument (J) = "-b"
      then
         declare
            Regexp : constant String := (if Ada.Command_Line.Argument_Count = J
                                           then "."
                                           else Ada.Command_Line.Argument (J + 1));
            use type GNAT.Regpat.Regexp_Flags;
            Matcher : constant GNAT.Regpat.Pattern_Matcher
               := GNAT.Regpat.Compile (Regexp, Flags => GNAT.Regpat.Case_Insensitive
                                                      + GNAT.Regpat.Single_Line);
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
         end;
      end if;
   end loop;
end Produce_Corporate_Bullshit;
