with Ada.Text_IO;                       use Ada.Text_IO;
with Corporate_Bullshit;

procedure Produce_Corporate_Bullshit is

  package HTML_Corporate_Bullshit is
    new Corporate_Bullshit(
      paragraph => "<p>",
      dialog_mark => "- "
    );

  f: File_Type;

begin
  Create(f,out_file,"bullshit.html");
  Put_Line(f,"<html><head><title>Corporate bullshit</title></head>");
  Put_Line(f,"<body><p><h1>Workshop minutes</h1>");
  Put_Line(f,HTML_Corporate_Bullshit.Workshop);
  Put_Line(f,"</body></html>");
  Close(f);
end Produce_Corporate_Bullshit;