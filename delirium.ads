-------------------------------------------------------------------------
--  Delirium - Helper package for random recursive grammar
--
--  Package specification
--
--  Legal licensing note:
--
--  Copyright (c) Gautier de Montmollin 2006 .. 2024
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

--  Examples:
--
--   Corporate_bullshit, The Corporate Bullshit Generator (CBSG)

package Delirium is

   --  Some categories

   type Plurality is (Singular, Plural);

   function Random_Plural return Plurality;

   ---------------------------
   -- English grammar tools --
   ---------------------------

   function Make_Eventual_Plural (S : String; P : Plurality) return String;

   --  Input: infinitive form of a verb (without the "to "): be, have, go, see, ...
   --  Complements can be appended to the input verb.
   function Build_Plural_Verb (Verb : String; P : Plurality) return String;

   function Add_Indefinite_Article (P : Plurality; To : String) return String;

   --

   type Blending is (Bullshit, Normal, Custom);

   --  Silly Abbreviation Generator (SAG)
   function Silly_Abbreviation_Generator_SAG (X : String) return String;

   --  R (N) returns a random number between 1 and N.
   function R (N : Positive) return Positive;

   --  Abbreviate appends with a given probability an expression with its acronym
   --  Example: Chief Internal Audit Officer (CIAO).
   function Abbreviate (Long : String; Probability : Float) return String;

   --  All the following T* and R* is to make the typing in the random
   --  recursive grammar the shortest possible.
   --  No parenthesis, nor "when others => null;" is needed then.
   --  The bounds of the random generation are well under control.
   --  Below, a Windows CMD script for producing them:
   --
   --  for /l %i in (1,1,600) do echo    type T%i is range 1 .. %i; function R%i return T%i;>>spec.txt

   type T1 is range 1 .. 1; function R1 return T1;
   type T2 is range 1 .. 2; function R2 return T2;
   type T3 is range 1 .. 3; function R3 return T3;
   type T4 is range 1 .. 4; function R4 return T4;
   type T5 is range 1 .. 5; function R5 return T5;
   type T6 is range 1 .. 6; function R6 return T6;
   type T7 is range 1 .. 7; function R7 return T7;
   type T8 is range 1 .. 8; function R8 return T8;
   type T9 is range 1 .. 9; function R9 return T9;
   type T10 is range 1 .. 10; function R10 return T10;
   type T11 is range 1 .. 11; function R11 return T11;
   type T12 is range 1 .. 12; function R12 return T12;
   type T13 is range 1 .. 13; function R13 return T13;
   type T14 is range 1 .. 14; function R14 return T14;
   type T15 is range 1 .. 15; function R15 return T15;
   type T16 is range 1 .. 16; function R16 return T16;
   type T17 is range 1 .. 17; function R17 return T17;
   type T18 is range 1 .. 18; function R18 return T18;
   type T19 is range 1 .. 19; function R19 return T19;
   type T20 is range 1 .. 20; function R20 return T20;
   type T21 is range 1 .. 21; function R21 return T21;
   type T22 is range 1 .. 22; function R22 return T22;
   type T23 is range 1 .. 23; function R23 return T23;
   type T24 is range 1 .. 24; function R24 return T24;
   type T25 is range 1 .. 25; function R25 return T25;
   type T26 is range 1 .. 26; function R26 return T26;
   type T27 is range 1 .. 27; function R27 return T27;
   type T28 is range 1 .. 28; function R28 return T28;
   type T29 is range 1 .. 29; function R29 return T29;
   type T30 is range 1 .. 30; function R30 return T30;
   type T31 is range 1 .. 31; function R31 return T31;
   type T32 is range 1 .. 32; function R32 return T32;
   type T33 is range 1 .. 33; function R33 return T33;
   type T34 is range 1 .. 34; function R34 return T34;
   type T35 is range 1 .. 35; function R35 return T35;
   type T36 is range 1 .. 36; function R36 return T36;
   type T37 is range 1 .. 37; function R37 return T37;
   type T38 is range 1 .. 38; function R38 return T38;
   type T39 is range 1 .. 39; function R39 return T39;
   type T40 is range 1 .. 40; function R40 return T40;
   type T41 is range 1 .. 41; function R41 return T41;
   type T42 is range 1 .. 42; function R42 return T42;
   type T43 is range 1 .. 43; function R43 return T43;
   type T44 is range 1 .. 44; function R44 return T44;
   type T45 is range 1 .. 45; function R45 return T45;
   type T46 is range 1 .. 46; function R46 return T46;
   type T47 is range 1 .. 47; function R47 return T47;
   type T48 is range 1 .. 48; function R48 return T48;
   type T49 is range 1 .. 49; function R49 return T49;
   type T50 is range 1 .. 50; function R50 return T50;
   type T51 is range 1 .. 51; function R51 return T51;
   type T52 is range 1 .. 52; function R52 return T52;
   type T53 is range 1 .. 53; function R53 return T53;
   type T54 is range 1 .. 54; function R54 return T54;
   type T55 is range 1 .. 55; function R55 return T55;
   type T56 is range 1 .. 56; function R56 return T56;
   type T57 is range 1 .. 57; function R57 return T57;
   type T58 is range 1 .. 58; function R58 return T58;
   type T59 is range 1 .. 59; function R59 return T59;
   type T60 is range 1 .. 60; function R60 return T60;
   type T61 is range 1 .. 61; function R61 return T61;
   type T62 is range 1 .. 62; function R62 return T62;
   type T63 is range 1 .. 63; function R63 return T63;
   type T64 is range 1 .. 64; function R64 return T64;
   type T65 is range 1 .. 65; function R65 return T65;
   type T66 is range 1 .. 66; function R66 return T66;
   type T67 is range 1 .. 67; function R67 return T67;
   type T68 is range 1 .. 68; function R68 return T68;
   type T69 is range 1 .. 69; function R69 return T69;
   type T70 is range 1 .. 70; function R70 return T70;
   type T71 is range 1 .. 71; function R71 return T71;
   type T72 is range 1 .. 72; function R72 return T72;
   type T73 is range 1 .. 73; function R73 return T73;
   type T74 is range 1 .. 74; function R74 return T74;
   type T75 is range 1 .. 75; function R75 return T75;
   type T76 is range 1 .. 76; function R76 return T76;
   type T77 is range 1 .. 77; function R77 return T77;
   type T78 is range 1 .. 78; function R78 return T78;
   type T79 is range 1 .. 79; function R79 return T79;
   type T80 is range 1 .. 80; function R80 return T80;
   type T81 is range 1 .. 81; function R81 return T81;
   type T82 is range 1 .. 82; function R82 return T82;
   type T83 is range 1 .. 83; function R83 return T83;
   type T84 is range 1 .. 84; function R84 return T84;
   type T85 is range 1 .. 85; function R85 return T85;
   type T86 is range 1 .. 86; function R86 return T86;
   type T87 is range 1 .. 87; function R87 return T87;
   type T88 is range 1 .. 88; function R88 return T88;
   type T89 is range 1 .. 89; function R89 return T89;
   type T90 is range 1 .. 90; function R90 return T90;
   type T91 is range 1 .. 91; function R91 return T91;
   type T92 is range 1 .. 92; function R92 return T92;
   type T93 is range 1 .. 93; function R93 return T93;
   type T94 is range 1 .. 94; function R94 return T94;
   type T95 is range 1 .. 95; function R95 return T95;
   type T96 is range 1 .. 96; function R96 return T96;
   type T97 is range 1 .. 97; function R97 return T97;
   type T98 is range 1 .. 98; function R98 return T98;
   type T99 is range 1 .. 99; function R99 return T99;
   type T100 is range 1 .. 100; function R100 return T100;
   type T101 is range 1 .. 101; function R101 return T101;
   type T102 is range 1 .. 102; function R102 return T102;
   type T103 is range 1 .. 103; function R103 return T103;
   type T104 is range 1 .. 104; function R104 return T104;
   type T105 is range 1 .. 105; function R105 return T105;
   type T106 is range 1 .. 106; function R106 return T106;
   type T107 is range 1 .. 107; function R107 return T107;
   type T108 is range 1 .. 108; function R108 return T108;
   type T109 is range 1 .. 109; function R109 return T109;
   type T110 is range 1 .. 110; function R110 return T110;
   type T111 is range 1 .. 111; function R111 return T111;
   type T112 is range 1 .. 112; function R112 return T112;
   type T113 is range 1 .. 113; function R113 return T113;
   type T114 is range 1 .. 114; function R114 return T114;
   type T115 is range 1 .. 115; function R115 return T115;
   type T116 is range 1 .. 116; function R116 return T116;
   type T117 is range 1 .. 117; function R117 return T117;
   type T118 is range 1 .. 118; function R118 return T118;
   type T119 is range 1 .. 119; function R119 return T119;
   type T120 is range 1 .. 120; function R120 return T120;
   type T121 is range 1 .. 121; function R121 return T121;
   type T122 is range 1 .. 122; function R122 return T122;
   type T123 is range 1 .. 123; function R123 return T123;
   type T124 is range 1 .. 124; function R124 return T124;
   type T125 is range 1 .. 125; function R125 return T125;
   type T126 is range 1 .. 126; function R126 return T126;
   type T127 is range 1 .. 127; function R127 return T127;
   type T128 is range 1 .. 128; function R128 return T128;
   type T129 is range 1 .. 129; function R129 return T129;
   type T130 is range 1 .. 130; function R130 return T130;
   type T131 is range 1 .. 131; function R131 return T131;
   type T132 is range 1 .. 132; function R132 return T132;
   type T133 is range 1 .. 133; function R133 return T133;
   type T134 is range 1 .. 134; function R134 return T134;
   type T135 is range 1 .. 135; function R135 return T135;
   type T136 is range 1 .. 136; function R136 return T136;
   type T137 is range 1 .. 137; function R137 return T137;
   type T138 is range 1 .. 138; function R138 return T138;
   type T139 is range 1 .. 139; function R139 return T139;
   type T140 is range 1 .. 140; function R140 return T140;
   type T141 is range 1 .. 141; function R141 return T141;
   type T142 is range 1 .. 142; function R142 return T142;
   type T143 is range 1 .. 143; function R143 return T143;
   type T144 is range 1 .. 144; function R144 return T144;
   type T145 is range 1 .. 145; function R145 return T145;
   type T146 is range 1 .. 146; function R146 return T146;
   type T147 is range 1 .. 147; function R147 return T147;
   type T148 is range 1 .. 148; function R148 return T148;
   type T149 is range 1 .. 149; function R149 return T149;
   type T150 is range 1 .. 150; function R150 return T150;
   type T151 is range 1 .. 151; function R151 return T151;
   type T152 is range 1 .. 152; function R152 return T152;
   type T153 is range 1 .. 153; function R153 return T153;
   type T154 is range 1 .. 154; function R154 return T154;
   type T155 is range 1 .. 155; function R155 return T155;
   type T156 is range 1 .. 156; function R156 return T156;
   type T157 is range 1 .. 157; function R157 return T157;
   type T158 is range 1 .. 158; function R158 return T158;
   type T159 is range 1 .. 159; function R159 return T159;
   type T160 is range 1 .. 160; function R160 return T160;
   type T161 is range 1 .. 161; function R161 return T161;
   type T162 is range 1 .. 162; function R162 return T162;
   type T163 is range 1 .. 163; function R163 return T163;
   type T164 is range 1 .. 164; function R164 return T164;
   type T165 is range 1 .. 165; function R165 return T165;
   type T166 is range 1 .. 166; function R166 return T166;
   type T167 is range 1 .. 167; function R167 return T167;
   type T168 is range 1 .. 168; function R168 return T168;
   type T169 is range 1 .. 169; function R169 return T169;
   type T170 is range 1 .. 170; function R170 return T170;
   type T171 is range 1 .. 171; function R171 return T171;
   type T172 is range 1 .. 172; function R172 return T172;
   type T173 is range 1 .. 173; function R173 return T173;
   type T174 is range 1 .. 174; function R174 return T174;
   type T175 is range 1 .. 175; function R175 return T175;
   type T176 is range 1 .. 176; function R176 return T176;
   type T177 is range 1 .. 177; function R177 return T177;
   type T178 is range 1 .. 178; function R178 return T178;
   type T179 is range 1 .. 179; function R179 return T179;
   type T180 is range 1 .. 180; function R180 return T180;
   type T181 is range 1 .. 181; function R181 return T181;
   type T182 is range 1 .. 182; function R182 return T182;
   type T183 is range 1 .. 183; function R183 return T183;
   type T184 is range 1 .. 184; function R184 return T184;
   type T185 is range 1 .. 185; function R185 return T185;
   type T186 is range 1 .. 186; function R186 return T186;
   type T187 is range 1 .. 187; function R187 return T187;
   type T188 is range 1 .. 188; function R188 return T188;
   type T189 is range 1 .. 189; function R189 return T189;
   type T190 is range 1 .. 190; function R190 return T190;
   type T191 is range 1 .. 191; function R191 return T191;
   type T192 is range 1 .. 192; function R192 return T192;
   type T193 is range 1 .. 193; function R193 return T193;
   type T194 is range 1 .. 194; function R194 return T194;
   type T195 is range 1 .. 195; function R195 return T195;
   type T196 is range 1 .. 196; function R196 return T196;
   type T197 is range 1 .. 197; function R197 return T197;
   type T198 is range 1 .. 198; function R198 return T198;
   type T199 is range 1 .. 199; function R199 return T199;
   type T200 is range 1 .. 200; function R200 return T200;
   type T201 is range 1 .. 201; function R201 return T201;
   type T202 is range 1 .. 202; function R202 return T202;
   type T203 is range 1 .. 203; function R203 return T203;
   type T204 is range 1 .. 204; function R204 return T204;
   type T205 is range 1 .. 205; function R205 return T205;
   type T206 is range 1 .. 206; function R206 return T206;
   type T207 is range 1 .. 207; function R207 return T207;
   type T208 is range 1 .. 208; function R208 return T208;
   type T209 is range 1 .. 209; function R209 return T209;
   type T210 is range 1 .. 210; function R210 return T210;
   type T211 is range 1 .. 211; function R211 return T211;
   type T212 is range 1 .. 212; function R212 return T212;
   type T213 is range 1 .. 213; function R213 return T213;
   type T214 is range 1 .. 214; function R214 return T214;
   type T215 is range 1 .. 215; function R215 return T215;
   type T216 is range 1 .. 216; function R216 return T216;
   type T217 is range 1 .. 217; function R217 return T217;
   type T218 is range 1 .. 218; function R218 return T218;
   type T219 is range 1 .. 219; function R219 return T219;
   type T220 is range 1 .. 220; function R220 return T220;
   type T221 is range 1 .. 221; function R221 return T221;
   type T222 is range 1 .. 222; function R222 return T222;
   type T223 is range 1 .. 223; function R223 return T223;
   type T224 is range 1 .. 224; function R224 return T224;
   type T225 is range 1 .. 225; function R225 return T225;
   type T226 is range 1 .. 226; function R226 return T226;
   type T227 is range 1 .. 227; function R227 return T227;
   type T228 is range 1 .. 228; function R228 return T228;
   type T229 is range 1 .. 229; function R229 return T229;
   type T230 is range 1 .. 230; function R230 return T230;
   type T231 is range 1 .. 231; function R231 return T231;
   type T232 is range 1 .. 232; function R232 return T232;
   type T233 is range 1 .. 233; function R233 return T233;
   type T234 is range 1 .. 234; function R234 return T234;
   type T235 is range 1 .. 235; function R235 return T235;
   type T236 is range 1 .. 236; function R236 return T236;
   type T237 is range 1 .. 237; function R237 return T237;
   type T238 is range 1 .. 238; function R238 return T238;
   type T239 is range 1 .. 239; function R239 return T239;
   type T240 is range 1 .. 240; function R240 return T240;
   type T241 is range 1 .. 241; function R241 return T241;
   type T242 is range 1 .. 242; function R242 return T242;
   type T243 is range 1 .. 243; function R243 return T243;
   type T244 is range 1 .. 244; function R244 return T244;
   type T245 is range 1 .. 245; function R245 return T245;
   type T246 is range 1 .. 246; function R246 return T246;
   type T247 is range 1 .. 247; function R247 return T247;
   type T248 is range 1 .. 248; function R248 return T248;
   type T249 is range 1 .. 249; function R249 return T249;
   type T250 is range 1 .. 250; function R250 return T250;
   type T251 is range 1 .. 251; function R251 return T251;
   type T252 is range 1 .. 252; function R252 return T252;
   type T253 is range 1 .. 253; function R253 return T253;
   type T254 is range 1 .. 254; function R254 return T254;
   type T255 is range 1 .. 255; function R255 return T255;
   type T256 is range 1 .. 256; function R256 return T256;
   type T257 is range 1 .. 257; function R257 return T257;
   type T258 is range 1 .. 258; function R258 return T258;
   type T259 is range 1 .. 259; function R259 return T259;
   type T260 is range 1 .. 260; function R260 return T260;
   type T261 is range 1 .. 261; function R261 return T261;
   type T262 is range 1 .. 262; function R262 return T262;
   type T263 is range 1 .. 263; function R263 return T263;
   type T264 is range 1 .. 264; function R264 return T264;
   type T265 is range 1 .. 265; function R265 return T265;
   type T266 is range 1 .. 266; function R266 return T266;
   type T267 is range 1 .. 267; function R267 return T267;
   type T268 is range 1 .. 268; function R268 return T268;
   type T269 is range 1 .. 269; function R269 return T269;
   type T270 is range 1 .. 270; function R270 return T270;
   type T271 is range 1 .. 271; function R271 return T271;
   type T272 is range 1 .. 272; function R272 return T272;
   type T273 is range 1 .. 273; function R273 return T273;
   type T274 is range 1 .. 274; function R274 return T274;
   type T275 is range 1 .. 275; function R275 return T275;
   type T276 is range 1 .. 276; function R276 return T276;
   type T277 is range 1 .. 277; function R277 return T277;
   type T278 is range 1 .. 278; function R278 return T278;
   type T279 is range 1 .. 279; function R279 return T279;
   type T280 is range 1 .. 280; function R280 return T280;
   type T281 is range 1 .. 281; function R281 return T281;
   type T282 is range 1 .. 282; function R282 return T282;
   type T283 is range 1 .. 283; function R283 return T283;
   type T284 is range 1 .. 284; function R284 return T284;
   type T285 is range 1 .. 285; function R285 return T285;
   type T286 is range 1 .. 286; function R286 return T286;
   type T287 is range 1 .. 287; function R287 return T287;
   type T288 is range 1 .. 288; function R288 return T288;
   type T289 is range 1 .. 289; function R289 return T289;
   type T290 is range 1 .. 290; function R290 return T290;
   type T291 is range 1 .. 291; function R291 return T291;
   type T292 is range 1 .. 292; function R292 return T292;
   type T293 is range 1 .. 293; function R293 return T293;
   type T294 is range 1 .. 294; function R294 return T294;
   type T295 is range 1 .. 295; function R295 return T295;
   type T296 is range 1 .. 296; function R296 return T296;
   type T297 is range 1 .. 297; function R297 return T297;
   type T298 is range 1 .. 298; function R298 return T298;
   type T299 is range 1 .. 299; function R299 return T299;
   type T300 is range 1 .. 300; function R300 return T300;
   type T301 is range 1 .. 301; function R301 return T301;
   type T302 is range 1 .. 302; function R302 return T302;
   type T303 is range 1 .. 303; function R303 return T303;
   type T304 is range 1 .. 304; function R304 return T304;
   type T305 is range 1 .. 305; function R305 return T305;
   type T306 is range 1 .. 306; function R306 return T306;
   type T307 is range 1 .. 307; function R307 return T307;
   type T308 is range 1 .. 308; function R308 return T308;
   type T309 is range 1 .. 309; function R309 return T309;
   type T310 is range 1 .. 310; function R310 return T310;
   type T311 is range 1 .. 311; function R311 return T311;
   type T312 is range 1 .. 312; function R312 return T312;
   type T313 is range 1 .. 313; function R313 return T313;
   type T314 is range 1 .. 314; function R314 return T314;
   type T315 is range 1 .. 315; function R315 return T315;
   type T316 is range 1 .. 316; function R316 return T316;
   type T317 is range 1 .. 317; function R317 return T317;
   type T318 is range 1 .. 318; function R318 return T318;
   type T319 is range 1 .. 319; function R319 return T319;
   type T320 is range 1 .. 320; function R320 return T320;
   type T321 is range 1 .. 321; function R321 return T321;
   type T322 is range 1 .. 322; function R322 return T322;
   type T323 is range 1 .. 323; function R323 return T323;
   type T324 is range 1 .. 324; function R324 return T324;
   type T325 is range 1 .. 325; function R325 return T325;
   type T326 is range 1 .. 326; function R326 return T326;
   type T327 is range 1 .. 327; function R327 return T327;
   type T328 is range 1 .. 328; function R328 return T328;
   type T329 is range 1 .. 329; function R329 return T329;
   type T330 is range 1 .. 330; function R330 return T330;
   type T331 is range 1 .. 331; function R331 return T331;
   type T332 is range 1 .. 332; function R332 return T332;
   type T333 is range 1 .. 333; function R333 return T333;
   type T334 is range 1 .. 334; function R334 return T334;
   type T335 is range 1 .. 335; function R335 return T335;
   type T336 is range 1 .. 336; function R336 return T336;
   type T337 is range 1 .. 337; function R337 return T337;
   type T338 is range 1 .. 338; function R338 return T338;
   type T339 is range 1 .. 339; function R339 return T339;
   type T340 is range 1 .. 340; function R340 return T340;
   type T341 is range 1 .. 341; function R341 return T341;
   type T342 is range 1 .. 342; function R342 return T342;
   type T343 is range 1 .. 343; function R343 return T343;
   type T344 is range 1 .. 344; function R344 return T344;
   type T345 is range 1 .. 345; function R345 return T345;
   type T346 is range 1 .. 346; function R346 return T346;
   type T347 is range 1 .. 347; function R347 return T347;
   type T348 is range 1 .. 348; function R348 return T348;
   type T349 is range 1 .. 349; function R349 return T349;
   type T350 is range 1 .. 350; function R350 return T350;
   type T351 is range 1 .. 351; function R351 return T351;
   type T352 is range 1 .. 352; function R352 return T352;
   type T353 is range 1 .. 353; function R353 return T353;
   type T354 is range 1 .. 354; function R354 return T354;
   type T355 is range 1 .. 355; function R355 return T355;
   type T356 is range 1 .. 356; function R356 return T356;
   type T357 is range 1 .. 357; function R357 return T357;
   type T358 is range 1 .. 358; function R358 return T358;
   type T359 is range 1 .. 359; function R359 return T359;
   type T360 is range 1 .. 360; function R360 return T360;
   type T361 is range 1 .. 361; function R361 return T361;
   type T362 is range 1 .. 362; function R362 return T362;
   type T363 is range 1 .. 363; function R363 return T363;
   type T364 is range 1 .. 364; function R364 return T364;
   type T365 is range 1 .. 365; function R365 return T365;
   type T366 is range 1 .. 366; function R366 return T366;
   type T367 is range 1 .. 367; function R367 return T367;
   type T368 is range 1 .. 368; function R368 return T368;
   type T369 is range 1 .. 369; function R369 return T369;
   type T370 is range 1 .. 370; function R370 return T370;
   type T371 is range 1 .. 371; function R371 return T371;
   type T372 is range 1 .. 372; function R372 return T372;
   type T373 is range 1 .. 373; function R373 return T373;
   type T374 is range 1 .. 374; function R374 return T374;
   type T375 is range 1 .. 375; function R375 return T375;
   type T376 is range 1 .. 376; function R376 return T376;
   type T377 is range 1 .. 377; function R377 return T377;
   type T378 is range 1 .. 378; function R378 return T378;
   type T379 is range 1 .. 379; function R379 return T379;
   type T380 is range 1 .. 380; function R380 return T380;
   type T381 is range 1 .. 381; function R381 return T381;
   type T382 is range 1 .. 382; function R382 return T382;
   type T383 is range 1 .. 383; function R383 return T383;
   type T384 is range 1 .. 384; function R384 return T384;
   type T385 is range 1 .. 385; function R385 return T385;
   type T386 is range 1 .. 386; function R386 return T386;
   type T387 is range 1 .. 387; function R387 return T387;
   type T388 is range 1 .. 388; function R388 return T388;
   type T389 is range 1 .. 389; function R389 return T389;
   type T390 is range 1 .. 390; function R390 return T390;
   type T391 is range 1 .. 391; function R391 return T391;
   type T392 is range 1 .. 392; function R392 return T392;
   type T393 is range 1 .. 393; function R393 return T393;
   type T394 is range 1 .. 394; function R394 return T394;
   type T395 is range 1 .. 395; function R395 return T395;
   type T396 is range 1 .. 396; function R396 return T396;
   type T397 is range 1 .. 397; function R397 return T397;
   type T398 is range 1 .. 398; function R398 return T398;
   type T399 is range 1 .. 399; function R399 return T399;
   type T400 is range 1 .. 400; function R400 return T400;
   type T401 is range 1 .. 401; function R401 return T401;
   type T402 is range 1 .. 402; function R402 return T402;
   type T403 is range 1 .. 403; function R403 return T403;
   type T404 is range 1 .. 404; function R404 return T404;
   type T405 is range 1 .. 405; function R405 return T405;
   type T406 is range 1 .. 406; function R406 return T406;
   type T407 is range 1 .. 407; function R407 return T407;
   type T408 is range 1 .. 408; function R408 return T408;
   type T409 is range 1 .. 409; function R409 return T409;
   type T410 is range 1 .. 410; function R410 return T410;
   type T411 is range 1 .. 411; function R411 return T411;
   type T412 is range 1 .. 412; function R412 return T412;
   type T413 is range 1 .. 413; function R413 return T413;
   type T414 is range 1 .. 414; function R414 return T414;
   type T415 is range 1 .. 415; function R415 return T415;
   type T416 is range 1 .. 416; function R416 return T416;
   type T417 is range 1 .. 417; function R417 return T417;
   type T418 is range 1 .. 418; function R418 return T418;
   type T419 is range 1 .. 419; function R419 return T419;
   type T420 is range 1 .. 420; function R420 return T420;
   type T421 is range 1 .. 421; function R421 return T421;
   type T422 is range 1 .. 422; function R422 return T422;
   type T423 is range 1 .. 423; function R423 return T423;
   type T424 is range 1 .. 424; function R424 return T424;
   type T425 is range 1 .. 425; function R425 return T425;
   type T426 is range 1 .. 426; function R426 return T426;
   type T427 is range 1 .. 427; function R427 return T427;
   type T428 is range 1 .. 428; function R428 return T428;
   type T429 is range 1 .. 429; function R429 return T429;
   type T430 is range 1 .. 430; function R430 return T430;
   type T431 is range 1 .. 431; function R431 return T431;
   type T432 is range 1 .. 432; function R432 return T432;
   type T433 is range 1 .. 433; function R433 return T433;
   type T434 is range 1 .. 434; function R434 return T434;
   type T435 is range 1 .. 435; function R435 return T435;
   type T436 is range 1 .. 436; function R436 return T436;
   type T437 is range 1 .. 437; function R437 return T437;
   type T438 is range 1 .. 438; function R438 return T438;
   type T439 is range 1 .. 439; function R439 return T439;
   type T440 is range 1 .. 440; function R440 return T440;
   type T441 is range 1 .. 441; function R441 return T441;
   type T442 is range 1 .. 442; function R442 return T442;
   type T443 is range 1 .. 443; function R443 return T443;
   type T444 is range 1 .. 444; function R444 return T444;
   type T445 is range 1 .. 445; function R445 return T445;
   type T446 is range 1 .. 446; function R446 return T446;
   type T447 is range 1 .. 447; function R447 return T447;
   type T448 is range 1 .. 448; function R448 return T448;
   type T449 is range 1 .. 449; function R449 return T449;
   type T450 is range 1 .. 450; function R450 return T450;
   type T451 is range 1 .. 451; function R451 return T451;
   type T452 is range 1 .. 452; function R452 return T452;
   type T453 is range 1 .. 453; function R453 return T453;
   type T454 is range 1 .. 454; function R454 return T454;
   type T455 is range 1 .. 455; function R455 return T455;
   type T456 is range 1 .. 456; function R456 return T456;
   type T457 is range 1 .. 457; function R457 return T457;
   type T458 is range 1 .. 458; function R458 return T458;
   type T459 is range 1 .. 459; function R459 return T459;
   type T460 is range 1 .. 460; function R460 return T460;
   type T461 is range 1 .. 461; function R461 return T461;
   type T462 is range 1 .. 462; function R462 return T462;
   type T463 is range 1 .. 463; function R463 return T463;
   type T464 is range 1 .. 464; function R464 return T464;
   type T465 is range 1 .. 465; function R465 return T465;
   type T466 is range 1 .. 466; function R466 return T466;
   type T467 is range 1 .. 467; function R467 return T467;
   type T468 is range 1 .. 468; function R468 return T468;
   type T469 is range 1 .. 469; function R469 return T469;
   type T470 is range 1 .. 470; function R470 return T470;
   type T471 is range 1 .. 471; function R471 return T471;
   type T472 is range 1 .. 472; function R472 return T472;
   type T473 is range 1 .. 473; function R473 return T473;
   type T474 is range 1 .. 474; function R474 return T474;
   type T475 is range 1 .. 475; function R475 return T475;
   type T476 is range 1 .. 476; function R476 return T476;
   type T477 is range 1 .. 477; function R477 return T477;
   type T478 is range 1 .. 478; function R478 return T478;
   type T479 is range 1 .. 479; function R479 return T479;
   type T480 is range 1 .. 480; function R480 return T480;
   type T481 is range 1 .. 481; function R481 return T481;
   type T482 is range 1 .. 482; function R482 return T482;
   type T483 is range 1 .. 483; function R483 return T483;
   type T484 is range 1 .. 484; function R484 return T484;
   type T485 is range 1 .. 485; function R485 return T485;
   type T486 is range 1 .. 486; function R486 return T486;
   type T487 is range 1 .. 487; function R487 return T487;
   type T488 is range 1 .. 488; function R488 return T488;
   type T489 is range 1 .. 489; function R489 return T489;
   type T490 is range 1 .. 490; function R490 return T490;
   type T491 is range 1 .. 491; function R491 return T491;
   type T492 is range 1 .. 492; function R492 return T492;
   type T493 is range 1 .. 493; function R493 return T493;
   type T494 is range 1 .. 494; function R494 return T494;
   type T495 is range 1 .. 495; function R495 return T495;
   type T496 is range 1 .. 496; function R496 return T496;
   type T497 is range 1 .. 497; function R497 return T497;
   type T498 is range 1 .. 498; function R498 return T498;
   type T499 is range 1 .. 499; function R499 return T499;
   type T500 is range 1 .. 500; function R500 return T500;
   type T501 is range 1 .. 501; function R501 return T501;
   type T502 is range 1 .. 502; function R502 return T502;
   type T503 is range 1 .. 503; function R503 return T503;
   type T504 is range 1 .. 504; function R504 return T504;
   type T505 is range 1 .. 505; function R505 return T505;
   type T506 is range 1 .. 506; function R506 return T506;
   type T507 is range 1 .. 507; function R507 return T507;
   type T508 is range 1 .. 508; function R508 return T508;
   type T509 is range 1 .. 509; function R509 return T509;
   type T510 is range 1 .. 510; function R510 return T510;
   type T511 is range 1 .. 511; function R511 return T511;
   type T512 is range 1 .. 512; function R512 return T512;
   type T513 is range 1 .. 513; function R513 return T513;
   type T514 is range 1 .. 514; function R514 return T514;
   type T515 is range 1 .. 515; function R515 return T515;
   type T516 is range 1 .. 516; function R516 return T516;
   type T517 is range 1 .. 517; function R517 return T517;
   type T518 is range 1 .. 518; function R518 return T518;
   type T519 is range 1 .. 519; function R519 return T519;
   type T520 is range 1 .. 520; function R520 return T520;
   type T521 is range 1 .. 521; function R521 return T521;
   type T522 is range 1 .. 522; function R522 return T522;
   type T523 is range 1 .. 523; function R523 return T523;
   type T524 is range 1 .. 524; function R524 return T524;
   type T525 is range 1 .. 525; function R525 return T525;
   type T526 is range 1 .. 526; function R526 return T526;
   type T527 is range 1 .. 527; function R527 return T527;
   type T528 is range 1 .. 528; function R528 return T528;
   type T529 is range 1 .. 529; function R529 return T529;
   type T530 is range 1 .. 530; function R530 return T530;
   type T531 is range 1 .. 531; function R531 return T531;
   type T532 is range 1 .. 532; function R532 return T532;
   type T533 is range 1 .. 533; function R533 return T533;
   type T534 is range 1 .. 534; function R534 return T534;
   type T535 is range 1 .. 535; function R535 return T535;
   type T536 is range 1 .. 536; function R536 return T536;
   type T537 is range 1 .. 537; function R537 return T537;
   type T538 is range 1 .. 538; function R538 return T538;
   type T539 is range 1 .. 539; function R539 return T539;
   type T540 is range 1 .. 540; function R540 return T540;
   type T541 is range 1 .. 541; function R541 return T541;
   type T542 is range 1 .. 542; function R542 return T542;
   type T543 is range 1 .. 543; function R543 return T543;
   type T544 is range 1 .. 544; function R544 return T544;
   type T545 is range 1 .. 545; function R545 return T545;
   type T546 is range 1 .. 546; function R546 return T546;
   type T547 is range 1 .. 547; function R547 return T547;
   type T548 is range 1 .. 548; function R548 return T548;
   type T549 is range 1 .. 549; function R549 return T549;
   type T550 is range 1 .. 550; function R550 return T550;
   type T551 is range 1 .. 551; function R551 return T551;
   type T552 is range 1 .. 552; function R552 return T552;
   type T553 is range 1 .. 553; function R553 return T553;
   type T554 is range 1 .. 554; function R554 return T554;
   type T555 is range 1 .. 555; function R555 return T555;
   type T556 is range 1 .. 556; function R556 return T556;
   type T557 is range 1 .. 557; function R557 return T557;
   type T558 is range 1 .. 558; function R558 return T558;
   type T559 is range 1 .. 559; function R559 return T559;
   type T560 is range 1 .. 560; function R560 return T560;
   type T561 is range 1 .. 561; function R561 return T561;
   type T562 is range 1 .. 562; function R562 return T562;
   type T563 is range 1 .. 563; function R563 return T563;
   type T564 is range 1 .. 564; function R564 return T564;
   type T565 is range 1 .. 565; function R565 return T565;
   type T566 is range 1 .. 566; function R566 return T566;
   type T567 is range 1 .. 567; function R567 return T567;
   type T568 is range 1 .. 568; function R568 return T568;
   type T569 is range 1 .. 569; function R569 return T569;
   type T570 is range 1 .. 570; function R570 return T570;
   type T571 is range 1 .. 571; function R571 return T571;
   type T572 is range 1 .. 572; function R572 return T572;
   type T573 is range 1 .. 573; function R573 return T573;
   type T574 is range 1 .. 574; function R574 return T574;
   type T575 is range 1 .. 575; function R575 return T575;
   type T576 is range 1 .. 576; function R576 return T576;
   type T577 is range 1 .. 577; function R577 return T577;
   type T578 is range 1 .. 578; function R578 return T578;
   type T579 is range 1 .. 579; function R579 return T579;
   type T580 is range 1 .. 580; function R580 return T580;
   type T581 is range 1 .. 581; function R581 return T581;
   type T582 is range 1 .. 582; function R582 return T582;
   type T583 is range 1 .. 583; function R583 return T583;
   type T584 is range 1 .. 584; function R584 return T584;
   type T585 is range 1 .. 585; function R585 return T585;
   type T586 is range 1 .. 586; function R586 return T586;
   type T587 is range 1 .. 587; function R587 return T587;
   type T588 is range 1 .. 588; function R588 return T588;
   type T589 is range 1 .. 589; function R589 return T589;
   type T590 is range 1 .. 590; function R590 return T590;
   type T591 is range 1 .. 591; function R591 return T591;
   type T592 is range 1 .. 592; function R592 return T592;
   type T593 is range 1 .. 593; function R593 return T593;
   type T594 is range 1 .. 594; function R594 return T594;
   type T595 is range 1 .. 595; function R595 return T595;
   type T596 is range 1 .. 596; function R596 return T596;
   type T597 is range 1 .. 597; function R597 return T597;
   type T598 is range 1 .. 598; function R598 return T598;
   type T599 is range 1 .. 599; function R599 return T599;
   type T600 is range 1 .. 600; function R600 return T600;

end Delirium;
