-------------------------------------------------------------------------
--  The Corporate Bullshit Generator (CBSG)
--
--  Sources:
--
--  - personal notes from long years of professional experience
--  - new words from the Internet by feeding a search
--      engine with sentences of the generator
--  - very valuable, appreciated and proactive contributions
--      from my colleagues and friends, especially:
--        Mili Eppler, Nigel Findlater, Emilio Nualart,
--        Bernhard Maertl, Paul Della Marta, Georges Modol,
--        Andrew Fox, Kurt Dickmann, Georg Bauhaus, Frederic Praca
--  - high-level, responsive empowerments by Ludovic Brenta
--
--  Legal licensing note:
--
--  Copyright (c) Gautier de Montmollin 2006 .. 2013
--  CH-8810 Horgen
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php
-------------------------------------------------------------------------

generic
   Paragraph   : String;
   Dialog_Mark : String;
package Corporate_Bullshit is

   function Sentence return String;

   function Workshop return String;

   function Short_Workshop return String;

   function Financial_Report return String;

end Corporate_Bullshit;
