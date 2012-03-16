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
  Put_CGI_Header;   -- We will reply with a generated HTML document.
  Put_HTML_Head("Minimal Form Demonstration"); -- Start generating HTML.
  -- We ignore CGI.Input_Received (in a later version, we will include some input :-) )
  Put_Line("<h1>The Corporate Bullshit Generator</h1>");
  Put_Line(
    "<form method=""POST"">Click here for more bullshit!" &
    " &rarr; &rarr; &rarr; <input type=""submit""></form>"
  );
  Put_Line("The CBSG project site is here: <a href=http://sf.net/projects/cbsg/>here</a>");
  Put_Line("<hr><ul>");
  Put_Line(HTML_Corporate_Bullshit.Short_Workshop);
  Put_Line("</ul>");
  Put_HTML_Tail;  -- End the HTML document, sending </body></html>
end Live;
