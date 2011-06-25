with Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Corporate_Bullshit;

procedure Produce_Corporate_Bullshit is

   package HTML_Corporate_Bullshit is
      new Corporate_Bullshit (Paragraph => ASCII.LF & ASCII.LF & "<li>", Dialog_Mark => "");

   package Text_Corporate_Bullshit is
      new Corporate_Bullshit (Paragraph => "", Dialog_Mark => "");

   procedure Produce_Workshop_Report;
   procedure Produce_Workshop_Report is
      F : File_Type;
   begin
      Create (F, Out_File, "bullshit.html");
      Put_Line (F, "<head>");
      Put_Line (F, "   <title>Corporate bullshit</title>");
      Put_Line (F, "   <style type=""text/css"">");
      Put_Line (F, "   li { margin-bottom: 1em }");
      Put_Line (F, "   </style>");
      Put_Line (F, "</head>");
      Put_Line (F, "<body><p><h1>Workshop minutes</h1>");
      Put_Line (F, "<ul>");
      Put_Line (F, HTML_Corporate_Bullshit.Workshop);
      Put_Line (F, "</ul></body></html>");
      Close (F);
      Ada.Text_IO.Put_Line ("New corporate bullshit is in bullshit.html.  Enjoy.");
   end Produce_Workshop_Report;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Produce_Workshop_Report;
   end if;
   for J in 1 .. Ada.Command_Line.Argument_Count loop
      if Ada.Command_Line.Argument (J) = "--one"
        or Ada.Command_Line.Argument (J) = "--sig"
        or Ada.Command_Line.Argument (J) = "-1"
      then
         Ada.Text_IO.Put_Line (Text_Corporate_Bullshit.Sentence);
      end if;
      if Ada.Command_Line.Argument (J) = "--workshop"
        or Ada.Command_Line.Argument (J) = "-w"
      then
         Produce_Workshop_Report;
      end if;
   end loop;
end Produce_Corporate_Bullshit;
