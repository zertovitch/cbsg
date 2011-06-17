-------------------------------------------------------------------------
--  Delirium - Helper package for random recursive grammar
--  Package specification
--
--  Legal licensing note:
--
--  Copyright (c) Gautier de Montmollin 2006..2010
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

-- Examples:
--
--   Corporate_bullshit, The Corporate Bullshit Generator (CBSG)

package Delirium is

  -- Some categories

  type Plurality is (singular, plural);

  function Random_plural return Plurality;

  function Make_eventual_plural(s: String; p: Plurality) return String;

  type Blending is (bullshit, normal, custom);

  -- All the following T* and R* is to make the typing in the random
  -- recursive grammar the shortest possible.
  -- No parenthesis, nor "when others => null;" is needed then.
  -- The bounds of the random generation are well under control.

  -- for /l %i in (1,1,200) do echo   type T%i is range 1..%i; function R%i return T%i; >>spec.txt
  type T1 is range 1..1; function R1 return T1;
  type T2 is range 1..2; function R2 return T2;
  type T3 is range 1..3; function R3 return T3;
  type T4 is range 1..4; function R4 return T4;
  type T5 is range 1..5; function R5 return T5;
  type T6 is range 1..6; function R6 return T6;
  type T7 is range 1..7; function R7 return T7;
  type T8 is range 1..8; function R8 return T8;
  type T9 is range 1..9; function R9 return T9;
  type T10 is range 1..10; function R10 return T10;
  type T11 is range 1..11; function R11 return T11;
  type T12 is range 1..12; function R12 return T12;
  type T13 is range 1..13; function R13 return T13;
  type T14 is range 1..14; function R14 return T14;
  type T15 is range 1..15; function R15 return T15;
  type T16 is range 1..16; function R16 return T16;
  type T17 is range 1..17; function R17 return T17;
  type T18 is range 1..18; function R18 return T18;
  type T19 is range 1..19; function R19 return T19;
  type T20 is range 1..20; function R20 return T20;
  type T21 is range 1..21; function R21 return T21;
  type T22 is range 1..22; function R22 return T22;
  type T23 is range 1..23; function R23 return T23;
  type T24 is range 1..24; function R24 return T24;
  type T25 is range 1..25; function R25 return T25;
  type T26 is range 1..26; function R26 return T26;
  type T27 is range 1..27; function R27 return T27;
  type T28 is range 1..28; function R28 return T28;
  type T29 is range 1..29; function R29 return T29;
  type T30 is range 1..30; function R30 return T30;
  type T31 is range 1..31; function R31 return T31;
  type T32 is range 1..32; function R32 return T32;
  type T33 is range 1..33; function R33 return T33;
  type T34 is range 1..34; function R34 return T34;
  type T35 is range 1..35; function R35 return T35;
  type T36 is range 1..36; function R36 return T36;
  type T37 is range 1..37; function R37 return T37;
  type T38 is range 1..38; function R38 return T38;
  type T39 is range 1..39; function R39 return T39;
  type T40 is range 1..40; function R40 return T40;
  type T41 is range 1..41; function R41 return T41;
  type T42 is range 1..42; function R42 return T42;
  type T43 is range 1..43; function R43 return T43;
  type T44 is range 1..44; function R44 return T44;
  type T45 is range 1..45; function R45 return T45;
  type T46 is range 1..46; function R46 return T46;
  type T47 is range 1..47; function R47 return T47;
  type T48 is range 1..48; function R48 return T48;
  type T49 is range 1..49; function R49 return T49;
  type T50 is range 1..50; function R50 return T50;
  type T51 is range 1..51; function R51 return T51;
  type T52 is range 1..52; function R52 return T52;
  type T53 is range 1..53; function R53 return T53;
  type T54 is range 1..54; function R54 return T54;
  type T55 is range 1..55; function R55 return T55;
  type T56 is range 1..56; function R56 return T56;
  type T57 is range 1..57; function R57 return T57;
  type T58 is range 1..58; function R58 return T58;
  type T59 is range 1..59; function R59 return T59;
  type T60 is range 1..60; function R60 return T60;
  type T61 is range 1..61; function R61 return T61;
  type T62 is range 1..62; function R62 return T62;
  type T63 is range 1..63; function R63 return T63;
  type T64 is range 1..64; function R64 return T64;
  type T65 is range 1..65; function R65 return T65;
  type T66 is range 1..66; function R66 return T66;
  type T67 is range 1..67; function R67 return T67;
  type T68 is range 1..68; function R68 return T68;
  type T69 is range 1..69; function R69 return T69;
  type T70 is range 1..70; function R70 return T70;
  type T71 is range 1..71; function R71 return T71;
  type T72 is range 1..72; function R72 return T72;
  type T73 is range 1..73; function R73 return T73;
  type T74 is range 1..74; function R74 return T74;
  type T75 is range 1..75; function R75 return T75;
  type T76 is range 1..76; function R76 return T76;
  type T77 is range 1..77; function R77 return T77;
  type T78 is range 1..78; function R78 return T78;
  type T79 is range 1..79; function R79 return T79;
  type T80 is range 1..80; function R80 return T80;
  type T81 is range 1..81; function R81 return T81;
  type T82 is range 1..82; function R82 return T82;
  type T83 is range 1..83; function R83 return T83;
  type T84 is range 1..84; function R84 return T84;
  type T85 is range 1..85; function R85 return T85;
  type T86 is range 1..86; function R86 return T86;
  type T87 is range 1..87; function R87 return T87;
  type T88 is range 1..88; function R88 return T88;
  type T89 is range 1..89; function R89 return T89;
  type T90 is range 1..90; function R90 return T90;
  type T91 is range 1..91; function R91 return T91;
  type T92 is range 1..92; function R92 return T92;
  type T93 is range 1..93; function R93 return T93;
  type T94 is range 1..94; function R94 return T94;
  type T95 is range 1..95; function R95 return T95;
  type T96 is range 1..96; function R96 return T96;
  type T97 is range 1..97; function R97 return T97;
  type T98 is range 1..98; function R98 return T98;
  type T99 is range 1..99; function R99 return T99;
  type T100 is range 1..100; function R100 return T100;
  type T101 is range 1..101; function R101 return T101;
  type T102 is range 1..102; function R102 return T102;
  type T103 is range 1..103; function R103 return T103;
  type T104 is range 1..104; function R104 return T104;
  type T105 is range 1..105; function R105 return T105;
  type T106 is range 1..106; function R106 return T106;
  type T107 is range 1..107; function R107 return T107;
  type T108 is range 1..108; function R108 return T108;
  type T109 is range 1..109; function R109 return T109;
  type T110 is range 1..110; function R110 return T110;
  type T111 is range 1..111; function R111 return T111;
  type T112 is range 1..112; function R112 return T112;
  type T113 is range 1..113; function R113 return T113;
  type T114 is range 1..114; function R114 return T114;
  type T115 is range 1..115; function R115 return T115;
  type T116 is range 1..116; function R116 return T116;
  type T117 is range 1..117; function R117 return T117;
  type T118 is range 1..118; function R118 return T118;
  type T119 is range 1..119; function R119 return T119;
  type T120 is range 1..120; function R120 return T120;
  type T121 is range 1..121; function R121 return T121;
  type T122 is range 1..122; function R122 return T122;
  type T123 is range 1..123; function R123 return T123;
  type T124 is range 1..124; function R124 return T124;
  type T125 is range 1..125; function R125 return T125;
  type T126 is range 1..126; function R126 return T126;
  type T127 is range 1..127; function R127 return T127;
  type T128 is range 1..128; function R128 return T128;
  type T129 is range 1..129; function R129 return T129;
  type T130 is range 1..130; function R130 return T130;
  type T131 is range 1..131; function R131 return T131;
  type T132 is range 1..132; function R132 return T132;
  type T133 is range 1..133; function R133 return T133;
  type T134 is range 1..134; function R134 return T134;
  type T135 is range 1..135; function R135 return T135;
  type T136 is range 1..136; function R136 return T136;
  type T137 is range 1..137; function R137 return T137;
  type T138 is range 1..138; function R138 return T138;
  type T139 is range 1..139; function R139 return T139;
  type T140 is range 1..140; function R140 return T140;
  type T141 is range 1..141; function R141 return T141;
  type T142 is range 1..142; function R142 return T142;
  type T143 is range 1..143; function R143 return T143;
  type T144 is range 1..144; function R144 return T144;
  type T145 is range 1..145; function R145 return T145;
  type T146 is range 1..146; function R146 return T146;
  type T147 is range 1..147; function R147 return T147;
  type T148 is range 1..148; function R148 return T148;
  type T149 is range 1..149; function R149 return T149;
  type T150 is range 1..150; function R150 return T150;
  type T151 is range 1..151; function R151 return T151;
  type T152 is range 1..152; function R152 return T152;
  type T153 is range 1..153; function R153 return T153;
  type T154 is range 1..154; function R154 return T154;
  type T155 is range 1..155; function R155 return T155;
  type T156 is range 1..156; function R156 return T156;
  type T157 is range 1..157; function R157 return T157;
  type T158 is range 1..158; function R158 return T158;
  type T159 is range 1..159; function R159 return T159;
  type T160 is range 1..160; function R160 return T160;
  type T161 is range 1..161; function R161 return T161;
  type T162 is range 1..162; function R162 return T162;
  type T163 is range 1..163; function R163 return T163;
  type T164 is range 1..164; function R164 return T164;
  type T165 is range 1..165; function R165 return T165;
  type T166 is range 1..166; function R166 return T166;
  type T167 is range 1..167; function R167 return T167;
  type T168 is range 1..168; function R168 return T168;
  type T169 is range 1..169; function R169 return T169;
  type T170 is range 1..170; function R170 return T170;
  type T171 is range 1..171; function R171 return T171;
  type T172 is range 1..172; function R172 return T172;
  type T173 is range 1..173; function R173 return T173;
  type T174 is range 1..174; function R174 return T174;
  type T175 is range 1..175; function R175 return T175;
  type T176 is range 1..176; function R176 return T176;
  type T177 is range 1..177; function R177 return T177;
  type T178 is range 1..178; function R178 return T178;
  type T179 is range 1..179; function R179 return T179;
  type T180 is range 1..180; function R180 return T180;
  type T181 is range 1..181; function R181 return T181;
  type T182 is range 1..182; function R182 return T182;
  type T183 is range 1..183; function R183 return T183;
  type T184 is range 1..184; function R184 return T184;
  type T185 is range 1..185; function R185 return T185;
  type T186 is range 1..186; function R186 return T186;
  type T187 is range 1..187; function R187 return T187;
  type T188 is range 1..188; function R188 return T188;
  type T189 is range 1..189; function R189 return T189;
  type T190 is range 1..190; function R190 return T190;
  type T191 is range 1..191; function R191 return T191;
  type T192 is range 1..192; function R192 return T192;
  type T193 is range 1..193; function R193 return T193;
  type T194 is range 1..194; function R194 return T194;
  type T195 is range 1..195; function R195 return T195;
  type T196 is range 1..196; function R196 return T196;
  type T197 is range 1..197; function R197 return T197;
  type T198 is range 1..198; function R198 return T198;
  type T199 is range 1..199; function R199 return T199;
  type T200 is range 1..200; function R200 return T200;

end Delirium;