-- Corporate Bullshit Live !
-- http://cbsg.sf.net/cgi-bin/live (full, unredirected URL)
-- 16-Mar-2012
-- Thx to Frédéric Praca for the help about CGI!
-- 19-Mar-2012
-- Use of a template file. Contribution by François Fabien.

with Corporate_Bullshit;
with CGI;                use CGI;
with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;

procedure Live is

  package HTML_Corporate_Bullshit is new Corporate_Bullshit (
                                                             Paragraph => ASCII.LF & ASCII.LF & "<li>",
                                                             Dialog_Mark => "");

  -- The HTML template input containing Tags
  -- currently in dir /cgi-bin ; if you relocate, adjust the string
  HTML_Template : constant String := "cbsg.tpl";
  Template      : File_Type;

begin
  -- 1/ Send header
  Put_CGI_Header; -- send Content-type: text/html

  -- 2/ Generate HTML from template
  begin
    Open (File => Template, Mode => In_File, Name => HTML_Template);
  exception
    when E : others =>
      Put_Line ("<html><head><title>CBSG File error</title></head>");
      Put_Line
        ("<body>Unexpected error :" &
         Exception_Information (E) &
         "</body></html>");
      return;
  end;
  Set_Input (Template);
  while not End_Of_File (Template) loop
    declare
      -- Current line of the template file
      Line: constant String:= Get_Line;
      -- Each of these tags will be dynamically replaced by a special content
      type Special_tag is (sentence, short_workshop);
      -- The tags in the template appear as: "@_" & Special_tag'Image(t) & "_@";
      special_tag_found: Boolean:= False;
    begin
      for t in Special_tag loop
        -- Check if there is a Sentence_Tag within the line
        declare
          tag_match: constant String:= "@_" & Special_tag'Image(t) & "_@";
          Pos: Natural := Index (Line, tag_match);
        begin
          if Pos > 0 then
            -- There is a Tag. We admit only one tag per line.
            Put (Line (1 .. Pos - 1));
            case t is
              when sentence =>
                Put (HTML_Corporate_Bullshit.Sentence);
              when short_workshop =>
                Put (HTML_Corporate_Bullshit.Short_Workshop);
              end case;
            Put_Line (Line (Pos + tag_match'Length .. Line'Last));
            special_tag_found:= True;
          end if;
        end;
      end loop;
      if not special_tag_found then
        -- Plain HTML => just output as is
        Put_Line (Line);
      end if;
    end;
  end loop;
  Close (Template);
exception
  when E : others =>
    Put_Line ("Unexpected error" & Exception_Information (E));
end Live;
