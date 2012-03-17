-- Corporate Bullshit Live !
-- http://cbsg.sf.net/cgi-bin/live (full, unredirected URL)
-- 16-Mar-2012
-- Thx to Frédéric Praca for the help about CGI!
--
with Corporate_Bullshit;
with CGI;                               use CGI;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Live is
   package HTML_Corporate_Bullshit is
      new Corporate_Bullshit (Paragraph => ASCII.LF & ASCII.LF & "<li>", Dialog_Mark => "");
begin
  Put_CGI_Header;
  Put_HTML_Head("Corporate Bullshit Generator");
  -- We ignore CGI.Input_Received (in a later version, we will include some input :-) )
  Put_Line("<font face=""Calibri, Arial"">");
  Put_Line("<h1>The Corporate Bullshit Generator</h1>");
  Put_Line(
    "<form method=""POST"">Click here for more bullshit !" &
    " &rarr; &rarr; &rarr;" &
    " <input type=""submit"" value=""More bullshit!""></form>"
  );
  Put_Line("The CBSG project site is <a href=http://sf.net/projects/cbsg/>here</a>.");
  Put_Line("<hr><ul>");
  --
  Put_Line(HTML_Corporate_Bullshit.Short_Workshop); --<-- Here happens the whole magic :-)
  --
  Put_Line("</ul>");
  Put_Line("</font><hr>");
  Put_Line("<center>");
  Put_Line(
    "<img align=absmiddle src=""http://sflogo.sf.net/sflogo.php?group_id=" &
    "557838&amp;type=16"" width=""150"" height=""40"" alt=""SourceForge.net"" />"
  );
  Put_Line("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
  Put_Line(
    "<img align=absmiddle width=100 height=64 src=durada.gif" &
    " alt=""Powered by Ada"">"
  );
  Put_Line("</center>");
  Put_HTML_Tail;
end Live;
