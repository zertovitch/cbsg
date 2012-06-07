-------------------------------------------------------------------------
--  Delirium - Helper package for random recursive grammar
--
--  Package body
--
--  Legal licensing note:
--
--  Copyright (c) Gautier de Montmollin 2006..2012
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

with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;

package body Delirium is

   ---------------------------
   -- English grammar tools --
   ---------------------------

   function Make_Eventual_Plural (S: String; P: Plurality) return String is
   begin
      if S'Length < 1 or P = Singular then
         return S;
      elsif S = "matrix" then
         return "matrices";
      elsif S = "analysis" then
         return "analyses";
      else
         case S (S'Last) is
            when 's'|'x'|'z'|'h' =>
               return S & "es";
            when 'y' =>
               return S (S'First..S'Last-1) & "ies";
            when others =>
               return S & 's';
         end case;
      end if;
   end Make_Eventual_Plural;

   Vowel: constant array (Character) of Boolean:=
     ('a'|'e'|'i'|'o'|'u' => True, others => False);

   function Build_Plural_Verb (Verb: String; P: Plurality) return String is
      Last: Natural;
   begin
      Last:= Verb'Last;
      for I in reverse Verb'First + 1 .. Verb'Last loop
         if Verb (I) = ' ' then
            Last := I - 1;
         end if;
      end loop;
      case P is
         when Plural   => return Verb;
         when Singular =>
            case Verb (Last) is
               when 'o' | 's' | 'z' =>
                  return Verb (Verb'First .. Last) & "es" & Verb (Last+1 .. Verb'Last);
               when 'h' =>
                  case Verb (Last - 1) is
                     when 'c' | 's' => -- catch -> catches; establish -> establishes
                       return Verb (Verb'First .. Last) & "es" & Verb (Last+1 .. Verb'Last);
                     when others => -- plough -> ploughs
                       return Verb (Verb'First .. Last) & 's' & Verb (Last+1 .. Verb'Last);
                  end case;
               when 'y' =>
                  if Vowel (Verb (Last - 1)) then -- ploy -> ploys
                     return Verb (Verb'First .. Last) & 's' & Verb (Last+1 .. Verb'Last);
                  else -- try -> tries
                     return Verb (Verb'First .. Last - 1) & "ies" & Verb (Last+1 .. Verb'Last);
                  end if;
               when others =>
                  return Verb (Verb'First .. Last) & 's' & Verb (Last+1 .. Verb'Last);
            end case;
      end case;
   end Build_Plural_Verb;

   function Add_Indefinite_Article (P: Plurality; To: String) return String is
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

   ----------------------
   -- Random functions --
   ----------------------

   package Rand_Pl is new Ada.Numerics.Discrete_Random (Plurality);

   Seed_Pl : Rand_Pl.Generator;

   function Random_Plural return Plurality is
   begin
      return Rand_Pl.Random (Seed_Pl);
   end Random_Plural;

   Seed: Generator;
   function R (N: Positive) return Positive is
      S: constant Natural:= Integer (Float (N)*Random (Seed));
   begin
      if S >= N then -- this has a 0 probability of happening
         return 1;
         -- ^ same choice as GNAT's run-time library for Ada.Numerics.Discrete_Random
      else
         return 1 + S;
      end if;
   end R;
   -- for /l %i in (1,1,400) do echo    function R%i return T%i is begin return T%i (R (%i)); end; >>body.txt
   function R1 return T1 is begin return T1 (R (1)); end;
   function R2 return T2 is begin return T2 (R (2)); end;
   function R3 return T3 is begin return T3 (R (3)); end;
   function R4 return T4 is begin return T4 (R (4)); end;
   function R5 return T5 is begin return T5 (R (5)); end;
   function R6 return T6 is begin return T6 (R (6)); end;
   function R7 return T7 is begin return T7 (R (7)); end;
   function R8 return T8 is begin return T8 (R (8)); end;
   function R9 return T9 is begin return T9 (R (9)); end;
   function R10 return T10 is begin return T10 (R (10)); end;
   function R11 return T11 is begin return T11 (R (11)); end;
   function R12 return T12 is begin return T12 (R (12)); end;
   function R13 return T13 is begin return T13 (R (13)); end;
   function R14 return T14 is begin return T14 (R (14)); end;
   function R15 return T15 is begin return T15 (R (15)); end;
   function R16 return T16 is begin return T16 (R (16)); end;
   function R17 return T17 is begin return T17 (R (17)); end;
   function R18 return T18 is begin return T18 (R (18)); end;
   function R19 return T19 is begin return T19 (R (19)); end;
   function R20 return T20 is begin return T20 (R (20)); end;
   function R21 return T21 is begin return T21 (R (21)); end;
   function R22 return T22 is begin return T22 (R (22)); end;
   function R23 return T23 is begin return T23 (R (23)); end;
   function R24 return T24 is begin return T24 (R (24)); end;
   function R25 return T25 is begin return T25 (R (25)); end;
   function R26 return T26 is begin return T26 (R (26)); end;
   function R27 return T27 is begin return T27 (R (27)); end;
   function R28 return T28 is begin return T28 (R (28)); end;
   function R29 return T29 is begin return T29 (R (29)); end;
   function R30 return T30 is begin return T30 (R (30)); end;
   function R31 return T31 is begin return T31 (R (31)); end;
   function R32 return T32 is begin return T32 (R (32)); end;
   function R33 return T33 is begin return T33 (R (33)); end;
   function R34 return T34 is begin return T34 (R (34)); end;
   function R35 return T35 is begin return T35 (R (35)); end;
   function R36 return T36 is begin return T36 (R (36)); end;
   function R37 return T37 is begin return T37 (R (37)); end;
   function R38 return T38 is begin return T38 (R (38)); end;
   function R39 return T39 is begin return T39 (R (39)); end;
   function R40 return T40 is begin return T40 (R (40)); end;
   function R41 return T41 is begin return T41 (R (41)); end;
   function R42 return T42 is begin return T42 (R (42)); end;
   function R43 return T43 is begin return T43 (R (43)); end;
   function R44 return T44 is begin return T44 (R (44)); end;
   function R45 return T45 is begin return T45 (R (45)); end;
   function R46 return T46 is begin return T46 (R (46)); end;
   function R47 return T47 is begin return T47 (R (47)); end;
   function R48 return T48 is begin return T48 (R (48)); end;
   function R49 return T49 is begin return T49 (R (49)); end;
   function R50 return T50 is begin return T50 (R (50)); end;
   function R51 return T51 is begin return T51 (R (51)); end;
   function R52 return T52 is begin return T52 (R (52)); end;
   function R53 return T53 is begin return T53 (R (53)); end;
   function R54 return T54 is begin return T54 (R (54)); end;
   function R55 return T55 is begin return T55 (R (55)); end;
   function R56 return T56 is begin return T56 (R (56)); end;
   function R57 return T57 is begin return T57 (R (57)); end;
   function R58 return T58 is begin return T58 (R (58)); end;
   function R59 return T59 is begin return T59 (R (59)); end;
   function R60 return T60 is begin return T60 (R (60)); end;
   function R61 return T61 is begin return T61 (R (61)); end;
   function R62 return T62 is begin return T62 (R (62)); end;
   function R63 return T63 is begin return T63 (R (63)); end;
   function R64 return T64 is begin return T64 (R (64)); end;
   function R65 return T65 is begin return T65 (R (65)); end;
   function R66 return T66 is begin return T66 (R (66)); end;
   function R67 return T67 is begin return T67 (R (67)); end;
   function R68 return T68 is begin return T68 (R (68)); end;
   function R69 return T69 is begin return T69 (R (69)); end;
   function R70 return T70 is begin return T70 (R (70)); end;
   function R71 return T71 is begin return T71 (R (71)); end;
   function R72 return T72 is begin return T72 (R (72)); end;
   function R73 return T73 is begin return T73 (R (73)); end;
   function R74 return T74 is begin return T74 (R (74)); end;
   function R75 return T75 is begin return T75 (R (75)); end;
   function R76 return T76 is begin return T76 (R (76)); end;
   function R77 return T77 is begin return T77 (R (77)); end;
   function R78 return T78 is begin return T78 (R (78)); end;
   function R79 return T79 is begin return T79 (R (79)); end;
   function R80 return T80 is begin return T80 (R (80)); end;
   function R81 return T81 is begin return T81 (R (81)); end;
   function R82 return T82 is begin return T82 (R (82)); end;
   function R83 return T83 is begin return T83 (R (83)); end;
   function R84 return T84 is begin return T84 (R (84)); end;
   function R85 return T85 is begin return T85 (R (85)); end;
   function R86 return T86 is begin return T86 (R (86)); end;
   function R87 return T87 is begin return T87 (R (87)); end;
   function R88 return T88 is begin return T88 (R (88)); end;
   function R89 return T89 is begin return T89 (R (89)); end;
   function R90 return T90 is begin return T90 (R (90)); end;
   function R91 return T91 is begin return T91 (R (91)); end;
   function R92 return T92 is begin return T92 (R (92)); end;
   function R93 return T93 is begin return T93 (R (93)); end;
   function R94 return T94 is begin return T94 (R (94)); end;
   function R95 return T95 is begin return T95 (R (95)); end;
   function R96 return T96 is begin return T96 (R (96)); end;
   function R97 return T97 is begin return T97 (R (97)); end;
   function R98 return T98 is begin return T98 (R (98)); end;
   function R99 return T99 is begin return T99 (R (99)); end;
   function R100 return T100 is begin return T100 (R (100)); end;
   function R101 return T101 is begin return T101 (R (101)); end;
   function R102 return T102 is begin return T102 (R (102)); end;
   function R103 return T103 is begin return T103 (R (103)); end;
   function R104 return T104 is begin return T104 (R (104)); end;
   function R105 return T105 is begin return T105 (R (105)); end;
   function R106 return T106 is begin return T106 (R (106)); end;
   function R107 return T107 is begin return T107 (R (107)); end;
   function R108 return T108 is begin return T108 (R (108)); end;
   function R109 return T109 is begin return T109 (R (109)); end;
   function R110 return T110 is begin return T110 (R (110)); end;
   function R111 return T111 is begin return T111 (R (111)); end;
   function R112 return T112 is begin return T112 (R (112)); end;
   function R113 return T113 is begin return T113 (R (113)); end;
   function R114 return T114 is begin return T114 (R (114)); end;
   function R115 return T115 is begin return T115 (R (115)); end;
   function R116 return T116 is begin return T116 (R (116)); end;
   function R117 return T117 is begin return T117 (R (117)); end;
   function R118 return T118 is begin return T118 (R (118)); end;
   function R119 return T119 is begin return T119 (R (119)); end;
   function R120 return T120 is begin return T120 (R (120)); end;
   function R121 return T121 is begin return T121 (R (121)); end;
   function R122 return T122 is begin return T122 (R (122)); end;
   function R123 return T123 is begin return T123 (R (123)); end;
   function R124 return T124 is begin return T124 (R (124)); end;
   function R125 return T125 is begin return T125 (R (125)); end;
   function R126 return T126 is begin return T126 (R (126)); end;
   function R127 return T127 is begin return T127 (R (127)); end;
   function R128 return T128 is begin return T128 (R (128)); end;
   function R129 return T129 is begin return T129 (R (129)); end;
   function R130 return T130 is begin return T130 (R (130)); end;
   function R131 return T131 is begin return T131 (R (131)); end;
   function R132 return T132 is begin return T132 (R (132)); end;
   function R133 return T133 is begin return T133 (R (133)); end;
   function R134 return T134 is begin return T134 (R (134)); end;
   function R135 return T135 is begin return T135 (R (135)); end;
   function R136 return T136 is begin return T136 (R (136)); end;
   function R137 return T137 is begin return T137 (R (137)); end;
   function R138 return T138 is begin return T138 (R (138)); end;
   function R139 return T139 is begin return T139 (R (139)); end;
   function R140 return T140 is begin return T140 (R (140)); end;
   function R141 return T141 is begin return T141 (R (141)); end;
   function R142 return T142 is begin return T142 (R (142)); end;
   function R143 return T143 is begin return T143 (R (143)); end;
   function R144 return T144 is begin return T144 (R (144)); end;
   function R145 return T145 is begin return T145 (R (145)); end;
   function R146 return T146 is begin return T146 (R (146)); end;
   function R147 return T147 is begin return T147 (R (147)); end;
   function R148 return T148 is begin return T148 (R (148)); end;
   function R149 return T149 is begin return T149 (R (149)); end;
   function R150 return T150 is begin return T150 (R (150)); end;
   function R151 return T151 is begin return T151 (R (151)); end;
   function R152 return T152 is begin return T152 (R (152)); end;
   function R153 return T153 is begin return T153 (R (153)); end;
   function R154 return T154 is begin return T154 (R (154)); end;
   function R155 return T155 is begin return T155 (R (155)); end;
   function R156 return T156 is begin return T156 (R (156)); end;
   function R157 return T157 is begin return T157 (R (157)); end;
   function R158 return T158 is begin return T158 (R (158)); end;
   function R159 return T159 is begin return T159 (R (159)); end;
   function R160 return T160 is begin return T160 (R (160)); end;
   function R161 return T161 is begin return T161 (R (161)); end;
   function R162 return T162 is begin return T162 (R (162)); end;
   function R163 return T163 is begin return T163 (R (163)); end;
   function R164 return T164 is begin return T164 (R (164)); end;
   function R165 return T165 is begin return T165 (R (165)); end;
   function R166 return T166 is begin return T166 (R (166)); end;
   function R167 return T167 is begin return T167 (R (167)); end;
   function R168 return T168 is begin return T168 (R (168)); end;
   function R169 return T169 is begin return T169 (R (169)); end;
   function R170 return T170 is begin return T170 (R (170)); end;
   function R171 return T171 is begin return T171 (R (171)); end;
   function R172 return T172 is begin return T172 (R (172)); end;
   function R173 return T173 is begin return T173 (R (173)); end;
   function R174 return T174 is begin return T174 (R (174)); end;
   function R175 return T175 is begin return T175 (R (175)); end;
   function R176 return T176 is begin return T176 (R (176)); end;
   function R177 return T177 is begin return T177 (R (177)); end;
   function R178 return T178 is begin return T178 (R (178)); end;
   function R179 return T179 is begin return T179 (R (179)); end;
   function R180 return T180 is begin return T180 (R (180)); end;
   function R181 return T181 is begin return T181 (R (181)); end;
   function R182 return T182 is begin return T182 (R (182)); end;
   function R183 return T183 is begin return T183 (R (183)); end;
   function R184 return T184 is begin return T184 (R (184)); end;
   function R185 return T185 is begin return T185 (R (185)); end;
   function R186 return T186 is begin return T186 (R (186)); end;
   function R187 return T187 is begin return T187 (R (187)); end;
   function R188 return T188 is begin return T188 (R (188)); end;
   function R189 return T189 is begin return T189 (R (189)); end;
   function R190 return T190 is begin return T190 (R (190)); end;
   function R191 return T191 is begin return T191 (R (191)); end;
   function R192 return T192 is begin return T192 (R (192)); end;
   function R193 return T193 is begin return T193 (R (193)); end;
   function R194 return T194 is begin return T194 (R (194)); end;
   function R195 return T195 is begin return T195 (R (195)); end;
   function R196 return T196 is begin return T196 (R (196)); end;
   function R197 return T197 is begin return T197 (R (197)); end;
   function R198 return T198 is begin return T198 (R (198)); end;
   function R199 return T199 is begin return T199 (R (199)); end;
   function R200 return T200 is begin return T200 (R (200)); end;
   function R201 return T201 is begin return T201 (R (201)); end;
   function R202 return T202 is begin return T202 (R (202)); end;
   function R203 return T203 is begin return T203 (R (203)); end;
   function R204 return T204 is begin return T204 (R (204)); end;
   function R205 return T205 is begin return T205 (R (205)); end;
   function R206 return T206 is begin return T206 (R (206)); end;
   function R207 return T207 is begin return T207 (R (207)); end;
   function R208 return T208 is begin return T208 (R (208)); end;
   function R209 return T209 is begin return T209 (R (209)); end;
   function R210 return T210 is begin return T210 (R (210)); end;
   function R211 return T211 is begin return T211 (R (211)); end;
   function R212 return T212 is begin return T212 (R (212)); end;
   function R213 return T213 is begin return T213 (R (213)); end;
   function R214 return T214 is begin return T214 (R (214)); end;
   function R215 return T215 is begin return T215 (R (215)); end;
   function R216 return T216 is begin return T216 (R (216)); end;
   function R217 return T217 is begin return T217 (R (217)); end;
   function R218 return T218 is begin return T218 (R (218)); end;
   function R219 return T219 is begin return T219 (R (219)); end;
   function R220 return T220 is begin return T220 (R (220)); end;
   function R221 return T221 is begin return T221 (R (221)); end;
   function R222 return T222 is begin return T222 (R (222)); end;
   function R223 return T223 is begin return T223 (R (223)); end;
   function R224 return T224 is begin return T224 (R (224)); end;
   function R225 return T225 is begin return T225 (R (225)); end;
   function R226 return T226 is begin return T226 (R (226)); end;
   function R227 return T227 is begin return T227 (R (227)); end;
   function R228 return T228 is begin return T228 (R (228)); end;
   function R229 return T229 is begin return T229 (R (229)); end;
   function R230 return T230 is begin return T230 (R (230)); end;
   function R231 return T231 is begin return T231 (R (231)); end;
   function R232 return T232 is begin return T232 (R (232)); end;
   function R233 return T233 is begin return T233 (R (233)); end;
   function R234 return T234 is begin return T234 (R (234)); end;
   function R235 return T235 is begin return T235 (R (235)); end;
   function R236 return T236 is begin return T236 (R (236)); end;
   function R237 return T237 is begin return T237 (R (237)); end;
   function R238 return T238 is begin return T238 (R (238)); end;
   function R239 return T239 is begin return T239 (R (239)); end;
   function R240 return T240 is begin return T240 (R (240)); end;
   function R241 return T241 is begin return T241 (R (241)); end;
   function R242 return T242 is begin return T242 (R (242)); end;
   function R243 return T243 is begin return T243 (R (243)); end;
   function R244 return T244 is begin return T244 (R (244)); end;
   function R245 return T245 is begin return T245 (R (245)); end;
   function R246 return T246 is begin return T246 (R (246)); end;
   function R247 return T247 is begin return T247 (R (247)); end;
   function R248 return T248 is begin return T248 (R (248)); end;
   function R249 return T249 is begin return T249 (R (249)); end;
   function R250 return T250 is begin return T250 (R (250)); end;
   function R251 return T251 is begin return T251 (R (251)); end;
   function R252 return T252 is begin return T252 (R (252)); end;
   function R253 return T253 is begin return T253 (R (253)); end;
   function R254 return T254 is begin return T254 (R (254)); end;
   function R255 return T255 is begin return T255 (R (255)); end;
   function R256 return T256 is begin return T256 (R (256)); end;
   function R257 return T257 is begin return T257 (R (257)); end;
   function R258 return T258 is begin return T258 (R (258)); end;
   function R259 return T259 is begin return T259 (R (259)); end;
   function R260 return T260 is begin return T260 (R (260)); end;
   function R261 return T261 is begin return T261 (R (261)); end;
   function R262 return T262 is begin return T262 (R (262)); end;
   function R263 return T263 is begin return T263 (R (263)); end;
   function R264 return T264 is begin return T264 (R (264)); end;
   function R265 return T265 is begin return T265 (R (265)); end;
   function R266 return T266 is begin return T266 (R (266)); end;
   function R267 return T267 is begin return T267 (R (267)); end;
   function R268 return T268 is begin return T268 (R (268)); end;
   function R269 return T269 is begin return T269 (R (269)); end;
   function R270 return T270 is begin return T270 (R (270)); end;
   function R271 return T271 is begin return T271 (R (271)); end;
   function R272 return T272 is begin return T272 (R (272)); end;
   function R273 return T273 is begin return T273 (R (273)); end;
   function R274 return T274 is begin return T274 (R (274)); end;
   function R275 return T275 is begin return T275 (R (275)); end;
   function R276 return T276 is begin return T276 (R (276)); end;
   function R277 return T277 is begin return T277 (R (277)); end;
   function R278 return T278 is begin return T278 (R (278)); end;
   function R279 return T279 is begin return T279 (R (279)); end;
   function R280 return T280 is begin return T280 (R (280)); end;
   function R281 return T281 is begin return T281 (R (281)); end;
   function R282 return T282 is begin return T282 (R (282)); end;
   function R283 return T283 is begin return T283 (R (283)); end;
   function R284 return T284 is begin return T284 (R (284)); end;
   function R285 return T285 is begin return T285 (R (285)); end;
   function R286 return T286 is begin return T286 (R (286)); end;
   function R287 return T287 is begin return T287 (R (287)); end;
   function R288 return T288 is begin return T288 (R (288)); end;
   function R289 return T289 is begin return T289 (R (289)); end;
   function R290 return T290 is begin return T290 (R (290)); end;
   function R291 return T291 is begin return T291 (R (291)); end;
   function R292 return T292 is begin return T292 (R (292)); end;
   function R293 return T293 is begin return T293 (R (293)); end;
   function R294 return T294 is begin return T294 (R (294)); end;
   function R295 return T295 is begin return T295 (R (295)); end;
   function R296 return T296 is begin return T296 (R (296)); end;
   function R297 return T297 is begin return T297 (R (297)); end;
   function R298 return T298 is begin return T298 (R (298)); end;
   function R299 return T299 is begin return T299 (R (299)); end;
   function R300 return T300 is begin return T300 (R (300)); end;
   function R301 return T301 is begin return T301 (R (301)); end;
   function R302 return T302 is begin return T302 (R (302)); end;
   function R303 return T303 is begin return T303 (R (303)); end;
   function R304 return T304 is begin return T304 (R (304)); end;
   function R305 return T305 is begin return T305 (R (305)); end;
   function R306 return T306 is begin return T306 (R (306)); end;
   function R307 return T307 is begin return T307 (R (307)); end;
   function R308 return T308 is begin return T308 (R (308)); end;
   function R309 return T309 is begin return T309 (R (309)); end;
   function R310 return T310 is begin return T310 (R (310)); end;
   function R311 return T311 is begin return T311 (R (311)); end;
   function R312 return T312 is begin return T312 (R (312)); end;
   function R313 return T313 is begin return T313 (R (313)); end;
   function R314 return T314 is begin return T314 (R (314)); end;
   function R315 return T315 is begin return T315 (R (315)); end;
   function R316 return T316 is begin return T316 (R (316)); end;
   function R317 return T317 is begin return T317 (R (317)); end;
   function R318 return T318 is begin return T318 (R (318)); end;
   function R319 return T319 is begin return T319 (R (319)); end;
   function R320 return T320 is begin return T320 (R (320)); end;
   function R321 return T321 is begin return T321 (R (321)); end;
   function R322 return T322 is begin return T322 (R (322)); end;
   function R323 return T323 is begin return T323 (R (323)); end;
   function R324 return T324 is begin return T324 (R (324)); end;
   function R325 return T325 is begin return T325 (R (325)); end;
   function R326 return T326 is begin return T326 (R (326)); end;
   function R327 return T327 is begin return T327 (R (327)); end;
   function R328 return T328 is begin return T328 (R (328)); end;
   function R329 return T329 is begin return T329 (R (329)); end;
   function R330 return T330 is begin return T330 (R (330)); end;
   function R331 return T331 is begin return T331 (R (331)); end;
   function R332 return T332 is begin return T332 (R (332)); end;
   function R333 return T333 is begin return T333 (R (333)); end;
   function R334 return T334 is begin return T334 (R (334)); end;
   function R335 return T335 is begin return T335 (R (335)); end;
   function R336 return T336 is begin return T336 (R (336)); end;
   function R337 return T337 is begin return T337 (R (337)); end;
   function R338 return T338 is begin return T338 (R (338)); end;
   function R339 return T339 is begin return T339 (R (339)); end;
   function R340 return T340 is begin return T340 (R (340)); end;
   function R341 return T341 is begin return T341 (R (341)); end;
   function R342 return T342 is begin return T342 (R (342)); end;
   function R343 return T343 is begin return T343 (R (343)); end;
   function R344 return T344 is begin return T344 (R (344)); end;
   function R345 return T345 is begin return T345 (R (345)); end;
   function R346 return T346 is begin return T346 (R (346)); end;
   function R347 return T347 is begin return T347 (R (347)); end;
   function R348 return T348 is begin return T348 (R (348)); end;
   function R349 return T349 is begin return T349 (R (349)); end;
   function R350 return T350 is begin return T350 (R (350)); end;
   function R351 return T351 is begin return T351 (R (351)); end;
   function R352 return T352 is begin return T352 (R (352)); end;
   function R353 return T353 is begin return T353 (R (353)); end;
   function R354 return T354 is begin return T354 (R (354)); end;
   function R355 return T355 is begin return T355 (R (355)); end;
   function R356 return T356 is begin return T356 (R (356)); end;
   function R357 return T357 is begin return T357 (R (357)); end;
   function R358 return T358 is begin return T358 (R (358)); end;
   function R359 return T359 is begin return T359 (R (359)); end;
   function R360 return T360 is begin return T360 (R (360)); end;
   function R361 return T361 is begin return T361 (R (361)); end;
   function R362 return T362 is begin return T362 (R (362)); end;
   function R363 return T363 is begin return T363 (R (363)); end;
   function R364 return T364 is begin return T364 (R (364)); end;
   function R365 return T365 is begin return T365 (R (365)); end;
   function R366 return T366 is begin return T366 (R (366)); end;
   function R367 return T367 is begin return T367 (R (367)); end;
   function R368 return T368 is begin return T368 (R (368)); end;
   function R369 return T369 is begin return T369 (R (369)); end;
   function R370 return T370 is begin return T370 (R (370)); end;
   function R371 return T371 is begin return T371 (R (371)); end;
   function R372 return T372 is begin return T372 (R (372)); end;
   function R373 return T373 is begin return T373 (R (373)); end;
   function R374 return T374 is begin return T374 (R (374)); end;
   function R375 return T375 is begin return T375 (R (375)); end;
   function R376 return T376 is begin return T376 (R (376)); end;
   function R377 return T377 is begin return T377 (R (377)); end;
   function R378 return T378 is begin return T378 (R (378)); end;
   function R379 return T379 is begin return T379 (R (379)); end;
   function R380 return T380 is begin return T380 (R (380)); end;
   function R381 return T381 is begin return T381 (R (381)); end;
   function R382 return T382 is begin return T382 (R (382)); end;
   function R383 return T383 is begin return T383 (R (383)); end;
   function R384 return T384 is begin return T384 (R (384)); end;
   function R385 return T385 is begin return T385 (R (385)); end;
   function R386 return T386 is begin return T386 (R (386)); end;
   function R387 return T387 is begin return T387 (R (387)); end;
   function R388 return T388 is begin return T388 (R (388)); end;
   function R389 return T389 is begin return T389 (R (389)); end;
   function R390 return T390 is begin return T390 (R (390)); end;
   function R391 return T391 is begin return T391 (R (391)); end;
   function R392 return T392 is begin return T392 (R (392)); end;
   function R393 return T393 is begin return T393 (R (393)); end;
   function R394 return T394 is begin return T394 (R (394)); end;
   function R395 return T395 is begin return T395 (R (395)); end;
   function R396 return T396 is begin return T396 (R (396)); end;
   function R397 return T397 is begin return T397 (R (397)); end;
   function R398 return T398 is begin return T398 (R (398)); end;
   function R399 return T399 is begin return T399 (R (399)); end;
   function R400 return T400 is begin return T400 (R (400)); end;

begin
   Reset (Seed);
   Rand_Pl.Reset (Seed_Pl);
end Delirium;
