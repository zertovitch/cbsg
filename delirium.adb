-------------------------------------------------------------------------
--  Delirium - Helper package for random recursive grammar
--
--  Package body
--
--  Legal licensing note:
--
--  Copyright (c) Gautier de Montmollin 2006 .. 2019
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
--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php
-------------------------------------------------------------------------

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

package body Delirium is

   ---------------------------
   -- English grammar tools --
   ---------------------------

   Vowel : constant array (Character) of Boolean :=
     ('a' | 'e' | 'i' | 'o' | 'u' | 'y' |
      'A' | 'E' | 'I' | 'O' | 'U' | 'Y' => True, others => False);

   function Make_Eventual_Plural (S : String; P : Plurality) return String is
      abbr : Natural;
   begin
      if S'Length < 3 or P = Singular then
         return S;
      end if;
      abbr := Index (S, " (");
      if abbr > 0 then
         --  Example: Quality Management Systems (QMS)
         return Make_Eventual_Plural (S (S'First .. abbr - 1), P) & S (abbr .. S'Last);
      elsif S = "matrix" then
         return "matrices";
      elsif S = "analysis" then
         return "analyses";
      elsif S (S'Last - 1 .. S'Last) = "gh" then  --  E.g.: breakthrough
         return S & 's';
      else
         case S (S'Last) is
            when 's' | 'x' | 'z' | 'h' =>
               return S & "es";
            when 'y' =>
               if Vowel (S (S'Last - 1)) then  --  wAy -> wAys
                  return S & 's';
               else  --  fLy -> fLies
                  return S (S'First .. S'Last - 1) & "ies";
               end if;
            when others =>
               return S & 's';
         end case;
      end if;
   end Make_Eventual_Plural;

   function Build_Plural_Verb (Verb : String; P : Plurality) return String is
      Last : Natural;
   begin
      Last := Verb'Last;
      for I in reverse Verb'First + 1 .. Verb'Last loop
         if Verb (I) = ' ' then
            Last := I - 1;
         end if;
      end loop;
      declare
         Rest : constant String := Verb (Last + 1 .. Verb'Last);
         Proper_Verb : constant String := Verb (Verb'First .. Last);
      begin
         if Proper_Verb = "be" then
            case P is
               when Singular => return "is" & Rest;
               when Plural   => return "are" & Rest;
            end case;
         elsif Proper_Verb = "have" then
            case P is
               when Singular => return "has" & Rest;
               when Plural   => return Verb;
            end case;
         end if;
         case P is
            when Plural   =>
               return Verb;
            when Singular =>
               case Verb (Last) is
                  when 'o' | 's' | 'z' =>  --  echo -> echoes; do -> does
                     return Proper_Verb & "es" & Rest;
                  when 'h' =>
                     case Verb (Last - 1) is
                        when 'c' | 's' => -- catch -> catches; establish -> establishes
                          return Proper_Verb & "es" & Rest;
                        when others => -- plough -> ploughs
                          return Proper_Verb & 's' & Rest;
                     end case;
                  when 'y' =>
                     if Vowel (Verb (Last - 1)) then  --  plOy -> plOys
                        return Proper_Verb & 's' & Rest;
                     else  --  tRy -> tRies
                        return Verb (Verb'First .. Last - 1) & "ies" & Rest;
                     end if;
                  when others =>
                     return Proper_Verb & 's' & Rest;
               end case;
         end case;
      end;
   end Build_Plural_Verb;

   function Add_Indefinite_Article (P : Plurality; To : String) return String is
   begin
      case P is
         when Singular =>
            if Vowel (To (To'First)) then
               return "an " & To;
            else
               return "a " & To;
            end if;
         when Plural =>
            return To;
      end case;
   end Add_Indefinite_Article;

   function Silly_Abbreviation_Generator_SAG (X : String) return String is
      space   : Natural;
      initial : Character;
      lower_X : constant String := To_Lower (X);
   begin
      if X'Length = 0 then
         return "";
      end if;
      if Index (lower_X, "experience") = lower_X'First then
         initial := 'X';  --  Special case: the 'x' in "experience"
      else
         initial := X (X'First);
      end if;
      space := Index (X, " ");
      if space > X'First then
         --  There is a space. We split the problem "a la Lisp".
         return initial & Silly_Abbreviation_Generator_SAG (X (space + 1 .. X'Last));
      end if;
      return (1 => initial);
   end Silly_Abbreviation_Generator_SAG;

   ----------------------
   -- Random functions --
   ----------------------

   package Rand_Pl is new Ada.Numerics.Discrete_Random (Plurality);

   Seed_Pl : Rand_Pl.Generator;

   function Random_Plural return Plurality is
   begin
      return Rand_Pl.Random (Seed_Pl);
   end Random_Plural;

   Seed : Generator;
   function R (N : Positive) return Positive is
      S : constant Natural := Integer (Float (N) * Random (Seed));
   begin
      if S >= N then -- this has a 0 probability of happening
         --  Same choice as GNAT's run-time library for Ada.Numerics.Discrete_Random:
         return 1;
      else
         return 1 + S;
      end if;
   end R;

   function Abbreviate (Long : String; Probability : Float) return String is
   begin
     if Random (Seed) < Probability then
        return Long & " (" & Silly_Abbreviation_Generator_SAG (Long) & ')';
     else
        return Long;
     end if;
   end Abbreviate;

   --  Below, a Windows CMD script for producing the function bodies:
   --
   --  for /l %i in (1,1,500) do echo    function R%i return T%i is (T%i (R (%i))); >>body.txt

   function R1 return T1 is (T1 (R (1)));
   function R2 return T2 is (T2 (R (2)));
   function R3 return T3 is (T3 (R (3)));
   function R4 return T4 is (T4 (R (4)));
   function R5 return T5 is (T5 (R (5)));
   function R6 return T6 is (T6 (R (6)));
   function R7 return T7 is (T7 (R (7)));
   function R8 return T8 is (T8 (R (8)));
   function R9 return T9 is (T9 (R (9)));
   function R10 return T10 is (T10 (R (10)));
   function R11 return T11 is (T11 (R (11)));
   function R12 return T12 is (T12 (R (12)));
   function R13 return T13 is (T13 (R (13)));
   function R14 return T14 is (T14 (R (14)));
   function R15 return T15 is (T15 (R (15)));
   function R16 return T16 is (T16 (R (16)));
   function R17 return T17 is (T17 (R (17)));
   function R18 return T18 is (T18 (R (18)));
   function R19 return T19 is (T19 (R (19)));
   function R20 return T20 is (T20 (R (20)));
   function R21 return T21 is (T21 (R (21)));
   function R22 return T22 is (T22 (R (22)));
   function R23 return T23 is (T23 (R (23)));
   function R24 return T24 is (T24 (R (24)));
   function R25 return T25 is (T25 (R (25)));
   function R26 return T26 is (T26 (R (26)));
   function R27 return T27 is (T27 (R (27)));
   function R28 return T28 is (T28 (R (28)));
   function R29 return T29 is (T29 (R (29)));
   function R30 return T30 is (T30 (R (30)));
   function R31 return T31 is (T31 (R (31)));
   function R32 return T32 is (T32 (R (32)));
   function R33 return T33 is (T33 (R (33)));
   function R34 return T34 is (T34 (R (34)));
   function R35 return T35 is (T35 (R (35)));
   function R36 return T36 is (T36 (R (36)));
   function R37 return T37 is (T37 (R (37)));
   function R38 return T38 is (T38 (R (38)));
   function R39 return T39 is (T39 (R (39)));
   function R40 return T40 is (T40 (R (40)));
   function R41 return T41 is (T41 (R (41)));
   function R42 return T42 is (T42 (R (42)));
   function R43 return T43 is (T43 (R (43)));
   function R44 return T44 is (T44 (R (44)));
   function R45 return T45 is (T45 (R (45)));
   function R46 return T46 is (T46 (R (46)));
   function R47 return T47 is (T47 (R (47)));
   function R48 return T48 is (T48 (R (48)));
   function R49 return T49 is (T49 (R (49)));
   function R50 return T50 is (T50 (R (50)));
   function R51 return T51 is (T51 (R (51)));
   function R52 return T52 is (T52 (R (52)));
   function R53 return T53 is (T53 (R (53)));
   function R54 return T54 is (T54 (R (54)));
   function R55 return T55 is (T55 (R (55)));
   function R56 return T56 is (T56 (R (56)));
   function R57 return T57 is (T57 (R (57)));
   function R58 return T58 is (T58 (R (58)));
   function R59 return T59 is (T59 (R (59)));
   function R60 return T60 is (T60 (R (60)));
   function R61 return T61 is (T61 (R (61)));
   function R62 return T62 is (T62 (R (62)));
   function R63 return T63 is (T63 (R (63)));
   function R64 return T64 is (T64 (R (64)));
   function R65 return T65 is (T65 (R (65)));
   function R66 return T66 is (T66 (R (66)));
   function R67 return T67 is (T67 (R (67)));
   function R68 return T68 is (T68 (R (68)));
   function R69 return T69 is (T69 (R (69)));
   function R70 return T70 is (T70 (R (70)));
   function R71 return T71 is (T71 (R (71)));
   function R72 return T72 is (T72 (R (72)));
   function R73 return T73 is (T73 (R (73)));
   function R74 return T74 is (T74 (R (74)));
   function R75 return T75 is (T75 (R (75)));
   function R76 return T76 is (T76 (R (76)));
   function R77 return T77 is (T77 (R (77)));
   function R78 return T78 is (T78 (R (78)));
   function R79 return T79 is (T79 (R (79)));
   function R80 return T80 is (T80 (R (80)));
   function R81 return T81 is (T81 (R (81)));
   function R82 return T82 is (T82 (R (82)));
   function R83 return T83 is (T83 (R (83)));
   function R84 return T84 is (T84 (R (84)));
   function R85 return T85 is (T85 (R (85)));
   function R86 return T86 is (T86 (R (86)));
   function R87 return T87 is (T87 (R (87)));
   function R88 return T88 is (T88 (R (88)));
   function R89 return T89 is (T89 (R (89)));
   function R90 return T90 is (T90 (R (90)));
   function R91 return T91 is (T91 (R (91)));
   function R92 return T92 is (T92 (R (92)));
   function R93 return T93 is (T93 (R (93)));
   function R94 return T94 is (T94 (R (94)));
   function R95 return T95 is (T95 (R (95)));
   function R96 return T96 is (T96 (R (96)));
   function R97 return T97 is (T97 (R (97)));
   function R98 return T98 is (T98 (R (98)));
   function R99 return T99 is (T99 (R (99)));
   function R100 return T100 is (T100 (R (100)));
   function R101 return T101 is (T101 (R (101)));
   function R102 return T102 is (T102 (R (102)));
   function R103 return T103 is (T103 (R (103)));
   function R104 return T104 is (T104 (R (104)));
   function R105 return T105 is (T105 (R (105)));
   function R106 return T106 is (T106 (R (106)));
   function R107 return T107 is (T107 (R (107)));
   function R108 return T108 is (T108 (R (108)));
   function R109 return T109 is (T109 (R (109)));
   function R110 return T110 is (T110 (R (110)));
   function R111 return T111 is (T111 (R (111)));
   function R112 return T112 is (T112 (R (112)));
   function R113 return T113 is (T113 (R (113)));
   function R114 return T114 is (T114 (R (114)));
   function R115 return T115 is (T115 (R (115)));
   function R116 return T116 is (T116 (R (116)));
   function R117 return T117 is (T117 (R (117)));
   function R118 return T118 is (T118 (R (118)));
   function R119 return T119 is (T119 (R (119)));
   function R120 return T120 is (T120 (R (120)));
   function R121 return T121 is (T121 (R (121)));
   function R122 return T122 is (T122 (R (122)));
   function R123 return T123 is (T123 (R (123)));
   function R124 return T124 is (T124 (R (124)));
   function R125 return T125 is (T125 (R (125)));
   function R126 return T126 is (T126 (R (126)));
   function R127 return T127 is (T127 (R (127)));
   function R128 return T128 is (T128 (R (128)));
   function R129 return T129 is (T129 (R (129)));
   function R130 return T130 is (T130 (R (130)));
   function R131 return T131 is (T131 (R (131)));
   function R132 return T132 is (T132 (R (132)));
   function R133 return T133 is (T133 (R (133)));
   function R134 return T134 is (T134 (R (134)));
   function R135 return T135 is (T135 (R (135)));
   function R136 return T136 is (T136 (R (136)));
   function R137 return T137 is (T137 (R (137)));
   function R138 return T138 is (T138 (R (138)));
   function R139 return T139 is (T139 (R (139)));
   function R140 return T140 is (T140 (R (140)));
   function R141 return T141 is (T141 (R (141)));
   function R142 return T142 is (T142 (R (142)));
   function R143 return T143 is (T143 (R (143)));
   function R144 return T144 is (T144 (R (144)));
   function R145 return T145 is (T145 (R (145)));
   function R146 return T146 is (T146 (R (146)));
   function R147 return T147 is (T147 (R (147)));
   function R148 return T148 is (T148 (R (148)));
   function R149 return T149 is (T149 (R (149)));
   function R150 return T150 is (T150 (R (150)));
   function R151 return T151 is (T151 (R (151)));
   function R152 return T152 is (T152 (R (152)));
   function R153 return T153 is (T153 (R (153)));
   function R154 return T154 is (T154 (R (154)));
   function R155 return T155 is (T155 (R (155)));
   function R156 return T156 is (T156 (R (156)));
   function R157 return T157 is (T157 (R (157)));
   function R158 return T158 is (T158 (R (158)));
   function R159 return T159 is (T159 (R (159)));
   function R160 return T160 is (T160 (R (160)));
   function R161 return T161 is (T161 (R (161)));
   function R162 return T162 is (T162 (R (162)));
   function R163 return T163 is (T163 (R (163)));
   function R164 return T164 is (T164 (R (164)));
   function R165 return T165 is (T165 (R (165)));
   function R166 return T166 is (T166 (R (166)));
   function R167 return T167 is (T167 (R (167)));
   function R168 return T168 is (T168 (R (168)));
   function R169 return T169 is (T169 (R (169)));
   function R170 return T170 is (T170 (R (170)));
   function R171 return T171 is (T171 (R (171)));
   function R172 return T172 is (T172 (R (172)));
   function R173 return T173 is (T173 (R (173)));
   function R174 return T174 is (T174 (R (174)));
   function R175 return T175 is (T175 (R (175)));
   function R176 return T176 is (T176 (R (176)));
   function R177 return T177 is (T177 (R (177)));
   function R178 return T178 is (T178 (R (178)));
   function R179 return T179 is (T179 (R (179)));
   function R180 return T180 is (T180 (R (180)));
   function R181 return T181 is (T181 (R (181)));
   function R182 return T182 is (T182 (R (182)));
   function R183 return T183 is (T183 (R (183)));
   function R184 return T184 is (T184 (R (184)));
   function R185 return T185 is (T185 (R (185)));
   function R186 return T186 is (T186 (R (186)));
   function R187 return T187 is (T187 (R (187)));
   function R188 return T188 is (T188 (R (188)));
   function R189 return T189 is (T189 (R (189)));
   function R190 return T190 is (T190 (R (190)));
   function R191 return T191 is (T191 (R (191)));
   function R192 return T192 is (T192 (R (192)));
   function R193 return T193 is (T193 (R (193)));
   function R194 return T194 is (T194 (R (194)));
   function R195 return T195 is (T195 (R (195)));
   function R196 return T196 is (T196 (R (196)));
   function R197 return T197 is (T197 (R (197)));
   function R198 return T198 is (T198 (R (198)));
   function R199 return T199 is (T199 (R (199)));
   function R200 return T200 is (T200 (R (200)));
   function R201 return T201 is (T201 (R (201)));
   function R202 return T202 is (T202 (R (202)));
   function R203 return T203 is (T203 (R (203)));
   function R204 return T204 is (T204 (R (204)));
   function R205 return T205 is (T205 (R (205)));
   function R206 return T206 is (T206 (R (206)));
   function R207 return T207 is (T207 (R (207)));
   function R208 return T208 is (T208 (R (208)));
   function R209 return T209 is (T209 (R (209)));
   function R210 return T210 is (T210 (R (210)));
   function R211 return T211 is (T211 (R (211)));
   function R212 return T212 is (T212 (R (212)));
   function R213 return T213 is (T213 (R (213)));
   function R214 return T214 is (T214 (R (214)));
   function R215 return T215 is (T215 (R (215)));
   function R216 return T216 is (T216 (R (216)));
   function R217 return T217 is (T217 (R (217)));
   function R218 return T218 is (T218 (R (218)));
   function R219 return T219 is (T219 (R (219)));
   function R220 return T220 is (T220 (R (220)));
   function R221 return T221 is (T221 (R (221)));
   function R222 return T222 is (T222 (R (222)));
   function R223 return T223 is (T223 (R (223)));
   function R224 return T224 is (T224 (R (224)));
   function R225 return T225 is (T225 (R (225)));
   function R226 return T226 is (T226 (R (226)));
   function R227 return T227 is (T227 (R (227)));
   function R228 return T228 is (T228 (R (228)));
   function R229 return T229 is (T229 (R (229)));
   function R230 return T230 is (T230 (R (230)));
   function R231 return T231 is (T231 (R (231)));
   function R232 return T232 is (T232 (R (232)));
   function R233 return T233 is (T233 (R (233)));
   function R234 return T234 is (T234 (R (234)));
   function R235 return T235 is (T235 (R (235)));
   function R236 return T236 is (T236 (R (236)));
   function R237 return T237 is (T237 (R (237)));
   function R238 return T238 is (T238 (R (238)));
   function R239 return T239 is (T239 (R (239)));
   function R240 return T240 is (T240 (R (240)));
   function R241 return T241 is (T241 (R (241)));
   function R242 return T242 is (T242 (R (242)));
   function R243 return T243 is (T243 (R (243)));
   function R244 return T244 is (T244 (R (244)));
   function R245 return T245 is (T245 (R (245)));
   function R246 return T246 is (T246 (R (246)));
   function R247 return T247 is (T247 (R (247)));
   function R248 return T248 is (T248 (R (248)));
   function R249 return T249 is (T249 (R (249)));
   function R250 return T250 is (T250 (R (250)));
   function R251 return T251 is (T251 (R (251)));
   function R252 return T252 is (T252 (R (252)));
   function R253 return T253 is (T253 (R (253)));
   function R254 return T254 is (T254 (R (254)));
   function R255 return T255 is (T255 (R (255)));
   function R256 return T256 is (T256 (R (256)));
   function R257 return T257 is (T257 (R (257)));
   function R258 return T258 is (T258 (R (258)));
   function R259 return T259 is (T259 (R (259)));
   function R260 return T260 is (T260 (R (260)));
   function R261 return T261 is (T261 (R (261)));
   function R262 return T262 is (T262 (R (262)));
   function R263 return T263 is (T263 (R (263)));
   function R264 return T264 is (T264 (R (264)));
   function R265 return T265 is (T265 (R (265)));
   function R266 return T266 is (T266 (R (266)));
   function R267 return T267 is (T267 (R (267)));
   function R268 return T268 is (T268 (R (268)));
   function R269 return T269 is (T269 (R (269)));
   function R270 return T270 is (T270 (R (270)));
   function R271 return T271 is (T271 (R (271)));
   function R272 return T272 is (T272 (R (272)));
   function R273 return T273 is (T273 (R (273)));
   function R274 return T274 is (T274 (R (274)));
   function R275 return T275 is (T275 (R (275)));
   function R276 return T276 is (T276 (R (276)));
   function R277 return T277 is (T277 (R (277)));
   function R278 return T278 is (T278 (R (278)));
   function R279 return T279 is (T279 (R (279)));
   function R280 return T280 is (T280 (R (280)));
   function R281 return T281 is (T281 (R (281)));
   function R282 return T282 is (T282 (R (282)));
   function R283 return T283 is (T283 (R (283)));
   function R284 return T284 is (T284 (R (284)));
   function R285 return T285 is (T285 (R (285)));
   function R286 return T286 is (T286 (R (286)));
   function R287 return T287 is (T287 (R (287)));
   function R288 return T288 is (T288 (R (288)));
   function R289 return T289 is (T289 (R (289)));
   function R290 return T290 is (T290 (R (290)));
   function R291 return T291 is (T291 (R (291)));
   function R292 return T292 is (T292 (R (292)));
   function R293 return T293 is (T293 (R (293)));
   function R294 return T294 is (T294 (R (294)));
   function R295 return T295 is (T295 (R (295)));
   function R296 return T296 is (T296 (R (296)));
   function R297 return T297 is (T297 (R (297)));
   function R298 return T298 is (T298 (R (298)));
   function R299 return T299 is (T299 (R (299)));
   function R300 return T300 is (T300 (R (300)));
   function R301 return T301 is (T301 (R (301)));
   function R302 return T302 is (T302 (R (302)));
   function R303 return T303 is (T303 (R (303)));
   function R304 return T304 is (T304 (R (304)));
   function R305 return T305 is (T305 (R (305)));
   function R306 return T306 is (T306 (R (306)));
   function R307 return T307 is (T307 (R (307)));
   function R308 return T308 is (T308 (R (308)));
   function R309 return T309 is (T309 (R (309)));
   function R310 return T310 is (T310 (R (310)));
   function R311 return T311 is (T311 (R (311)));
   function R312 return T312 is (T312 (R (312)));
   function R313 return T313 is (T313 (R (313)));
   function R314 return T314 is (T314 (R (314)));
   function R315 return T315 is (T315 (R (315)));
   function R316 return T316 is (T316 (R (316)));
   function R317 return T317 is (T317 (R (317)));
   function R318 return T318 is (T318 (R (318)));
   function R319 return T319 is (T319 (R (319)));
   function R320 return T320 is (T320 (R (320)));
   function R321 return T321 is (T321 (R (321)));
   function R322 return T322 is (T322 (R (322)));
   function R323 return T323 is (T323 (R (323)));
   function R324 return T324 is (T324 (R (324)));
   function R325 return T325 is (T325 (R (325)));
   function R326 return T326 is (T326 (R (326)));
   function R327 return T327 is (T327 (R (327)));
   function R328 return T328 is (T328 (R (328)));
   function R329 return T329 is (T329 (R (329)));
   function R330 return T330 is (T330 (R (330)));
   function R331 return T331 is (T331 (R (331)));
   function R332 return T332 is (T332 (R (332)));
   function R333 return T333 is (T333 (R (333)));
   function R334 return T334 is (T334 (R (334)));
   function R335 return T335 is (T335 (R (335)));
   function R336 return T336 is (T336 (R (336)));
   function R337 return T337 is (T337 (R (337)));
   function R338 return T338 is (T338 (R (338)));
   function R339 return T339 is (T339 (R (339)));
   function R340 return T340 is (T340 (R (340)));
   function R341 return T341 is (T341 (R (341)));
   function R342 return T342 is (T342 (R (342)));
   function R343 return T343 is (T343 (R (343)));
   function R344 return T344 is (T344 (R (344)));
   function R345 return T345 is (T345 (R (345)));
   function R346 return T346 is (T346 (R (346)));
   function R347 return T347 is (T347 (R (347)));
   function R348 return T348 is (T348 (R (348)));
   function R349 return T349 is (T349 (R (349)));
   function R350 return T350 is (T350 (R (350)));
   function R351 return T351 is (T351 (R (351)));
   function R352 return T352 is (T352 (R (352)));
   function R353 return T353 is (T353 (R (353)));
   function R354 return T354 is (T354 (R (354)));
   function R355 return T355 is (T355 (R (355)));
   function R356 return T356 is (T356 (R (356)));
   function R357 return T357 is (T357 (R (357)));
   function R358 return T358 is (T358 (R (358)));
   function R359 return T359 is (T359 (R (359)));
   function R360 return T360 is (T360 (R (360)));
   function R361 return T361 is (T361 (R (361)));
   function R362 return T362 is (T362 (R (362)));
   function R363 return T363 is (T363 (R (363)));
   function R364 return T364 is (T364 (R (364)));
   function R365 return T365 is (T365 (R (365)));
   function R366 return T366 is (T366 (R (366)));
   function R367 return T367 is (T367 (R (367)));
   function R368 return T368 is (T368 (R (368)));
   function R369 return T369 is (T369 (R (369)));
   function R370 return T370 is (T370 (R (370)));
   function R371 return T371 is (T371 (R (371)));
   function R372 return T372 is (T372 (R (372)));
   function R373 return T373 is (T373 (R (373)));
   function R374 return T374 is (T374 (R (374)));
   function R375 return T375 is (T375 (R (375)));
   function R376 return T376 is (T376 (R (376)));
   function R377 return T377 is (T377 (R (377)));
   function R378 return T378 is (T378 (R (378)));
   function R379 return T379 is (T379 (R (379)));
   function R380 return T380 is (T380 (R (380)));
   function R381 return T381 is (T381 (R (381)));
   function R382 return T382 is (T382 (R (382)));
   function R383 return T383 is (T383 (R (383)));
   function R384 return T384 is (T384 (R (384)));
   function R385 return T385 is (T385 (R (385)));
   function R386 return T386 is (T386 (R (386)));
   function R387 return T387 is (T387 (R (387)));
   function R388 return T388 is (T388 (R (388)));
   function R389 return T389 is (T389 (R (389)));
   function R390 return T390 is (T390 (R (390)));
   function R391 return T391 is (T391 (R (391)));
   function R392 return T392 is (T392 (R (392)));
   function R393 return T393 is (T393 (R (393)));
   function R394 return T394 is (T394 (R (394)));
   function R395 return T395 is (T395 (R (395)));
   function R396 return T396 is (T396 (R (396)));
   function R397 return T397 is (T397 (R (397)));
   function R398 return T398 is (T398 (R (398)));
   function R399 return T399 is (T399 (R (399)));
   function R400 return T400 is (T400 (R (400)));
   function R401 return T401 is (T401 (R (401)));
   function R402 return T402 is (T402 (R (402)));
   function R403 return T403 is (T403 (R (403)));
   function R404 return T404 is (T404 (R (404)));
   function R405 return T405 is (T405 (R (405)));
   function R406 return T406 is (T406 (R (406)));
   function R407 return T407 is (T407 (R (407)));
   function R408 return T408 is (T408 (R (408)));
   function R409 return T409 is (T409 (R (409)));
   function R410 return T410 is (T410 (R (410)));
   function R411 return T411 is (T411 (R (411)));
   function R412 return T412 is (T412 (R (412)));
   function R413 return T413 is (T413 (R (413)));
   function R414 return T414 is (T414 (R (414)));
   function R415 return T415 is (T415 (R (415)));
   function R416 return T416 is (T416 (R (416)));
   function R417 return T417 is (T417 (R (417)));
   function R418 return T418 is (T418 (R (418)));
   function R419 return T419 is (T419 (R (419)));
   function R420 return T420 is (T420 (R (420)));
   function R421 return T421 is (T421 (R (421)));
   function R422 return T422 is (T422 (R (422)));
   function R423 return T423 is (T423 (R (423)));
   function R424 return T424 is (T424 (R (424)));
   function R425 return T425 is (T425 (R (425)));
   function R426 return T426 is (T426 (R (426)));
   function R427 return T427 is (T427 (R (427)));
   function R428 return T428 is (T428 (R (428)));
   function R429 return T429 is (T429 (R (429)));
   function R430 return T430 is (T430 (R (430)));
   function R431 return T431 is (T431 (R (431)));
   function R432 return T432 is (T432 (R (432)));
   function R433 return T433 is (T433 (R (433)));
   function R434 return T434 is (T434 (R (434)));
   function R435 return T435 is (T435 (R (435)));
   function R436 return T436 is (T436 (R (436)));
   function R437 return T437 is (T437 (R (437)));
   function R438 return T438 is (T438 (R (438)));
   function R439 return T439 is (T439 (R (439)));
   function R440 return T440 is (T440 (R (440)));
   function R441 return T441 is (T441 (R (441)));
   function R442 return T442 is (T442 (R (442)));
   function R443 return T443 is (T443 (R (443)));
   function R444 return T444 is (T444 (R (444)));
   function R445 return T445 is (T445 (R (445)));
   function R446 return T446 is (T446 (R (446)));
   function R447 return T447 is (T447 (R (447)));
   function R448 return T448 is (T448 (R (448)));
   function R449 return T449 is (T449 (R (449)));
   function R450 return T450 is (T450 (R (450)));
   function R451 return T451 is (T451 (R (451)));
   function R452 return T452 is (T452 (R (452)));
   function R453 return T453 is (T453 (R (453)));
   function R454 return T454 is (T454 (R (454)));
   function R455 return T455 is (T455 (R (455)));
   function R456 return T456 is (T456 (R (456)));
   function R457 return T457 is (T457 (R (457)));
   function R458 return T458 is (T458 (R (458)));
   function R459 return T459 is (T459 (R (459)));
   function R460 return T460 is (T460 (R (460)));
   function R461 return T461 is (T461 (R (461)));
   function R462 return T462 is (T462 (R (462)));
   function R463 return T463 is (T463 (R (463)));
   function R464 return T464 is (T464 (R (464)));
   function R465 return T465 is (T465 (R (465)));
   function R466 return T466 is (T466 (R (466)));
   function R467 return T467 is (T467 (R (467)));
   function R468 return T468 is (T468 (R (468)));
   function R469 return T469 is (T469 (R (469)));
   function R470 return T470 is (T470 (R (470)));
   function R471 return T471 is (T471 (R (471)));
   function R472 return T472 is (T472 (R (472)));
   function R473 return T473 is (T473 (R (473)));
   function R474 return T474 is (T474 (R (474)));
   function R475 return T475 is (T475 (R (475)));
   function R476 return T476 is (T476 (R (476)));
   function R477 return T477 is (T477 (R (477)));
   function R478 return T478 is (T478 (R (478)));
   function R479 return T479 is (T479 (R (479)));
   function R480 return T480 is (T480 (R (480)));
   function R481 return T481 is (T481 (R (481)));
   function R482 return T482 is (T482 (R (482)));
   function R483 return T483 is (T483 (R (483)));
   function R484 return T484 is (T484 (R (484)));
   function R485 return T485 is (T485 (R (485)));
   function R486 return T486 is (T486 (R (486)));
   function R487 return T487 is (T487 (R (487)));
   function R488 return T488 is (T488 (R (488)));
   function R489 return T489 is (T489 (R (489)));
   function R490 return T490 is (T490 (R (490)));
   function R491 return T491 is (T491 (R (491)));
   function R492 return T492 is (T492 (R (492)));
   function R493 return T493 is (T493 (R (493)));
   function R494 return T494 is (T494 (R (494)));
   function R495 return T495 is (T495 (R (495)));
   function R496 return T496 is (T496 (R (496)));
   function R497 return T497 is (T497 (R (497)));
   function R498 return T498 is (T498 (R (498)));
   function R499 return T499 is (T499 (R (499)));
   function R500 return T500 is (T500 (R (500)));

begin
   Reset (Seed);
   Rand_Pl.Reset (Seed_Pl);
end Delirium;
