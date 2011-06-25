with Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Corporate_Bullshit;

procedure Produce_Corporate_Bullshit is

   package HTML_Corporate_Bullshit is
      new Corporate_Bullshit(paragraph => "<p>", dialog_mark => "- ");

   package Text_Corporate_Bullshit is
      new Corporate_Bullshit(paragraph => "", dialog_mark => "");

   procedure Produce_Workshop_Report is
      f: File_Type;
   begin
      Create(f,out_file,"bullshit.html");
      Put_Line(f,"<html><head><title>Corporate bullshit</title></head>");
      Put_Line(f,"<body><p><h1>Workshop minutes</h1>");
      Put_Line(f,HTML_Corporate_Bullshit.Workshop);
      Put_Line(f,"</body></html>");
      Close(f);
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
