-------------------------------------------------------------------------
--  Delirium - Helper package for random recursive grammar
--  Package body
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

with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;

package body Delirium is

  package Rand_pl is new Ada.Numerics.Discrete_Random(Plurality);

  seed_pl: Rand_pl.Generator;

  function Random_plural return Plurality is
  begin
    return Rand_pl.Random(seed_pl);
  end Random_plural;

  function Make_eventual_plural(s: String; p: Plurality) return String is
  begin
    if s'Length < 1 or p = singular then
      return s;
    elsif s = "matrix" then
      return "matrices";
    elsif s = "analysis" then
      return "analyses";
    else
      case s(s'Last) is
        when 's'|'x'|'z'|'h' =>
          return s & "es";
        when 'y' =>
          return s(s'first..s'Last-1) & "ies";
        when others =>
          return s & 's';
      end case;
    end if;
  end Make_eventual_plural;

  seed: Generator;
  function R(n: Positive) return Positive is
    s: constant Natural:= Integer(Float(n)*Random(seed));
  begin
    if s >= n then -- this has a 0 probability of happening
      return 1;
      -- ^ same choice as GNAT's run-time library for Ada.Numerics.Discrete_Random
    else
      return 1 + s;
    end if;
  end R;
  -- for /l %i in (1,1,200) do echo   function R%i return T%i is begin return T%i(R(%i)); end; >>body.txt
  function R1 return T1 is begin return T1(R(1)); end;
  function R2 return T2 is begin return T2(R(2)); end;
  function R3 return T3 is begin return T3(R(3)); end;
  function R4 return T4 is begin return T4(R(4)); end;
  function R5 return T5 is begin return T5(R(5)); end;
  function R6 return T6 is begin return T6(R(6)); end;
  function R7 return T7 is begin return T7(R(7)); end;
  function R8 return T8 is begin return T8(R(8)); end;
  function R9 return T9 is begin return T9(R(9)); end;
  function R10 return T10 is begin return T10(R(10)); end;
  function R11 return T11 is begin return T11(R(11)); end;
  function R12 return T12 is begin return T12(R(12)); end;
  function R13 return T13 is begin return T13(R(13)); end;
  function R14 return T14 is begin return T14(R(14)); end;
  function R15 return T15 is begin return T15(R(15)); end;
  function R16 return T16 is begin return T16(R(16)); end;
  function R17 return T17 is begin return T17(R(17)); end;
  function R18 return T18 is begin return T18(R(18)); end;
  function R19 return T19 is begin return T19(R(19)); end;
  function R20 return T20 is begin return T20(R(20)); end;
  function R21 return T21 is begin return T21(R(21)); end;
  function R22 return T22 is begin return T22(R(22)); end;
  function R23 return T23 is begin return T23(R(23)); end;
  function R24 return T24 is begin return T24(R(24)); end;
  function R25 return T25 is begin return T25(R(25)); end;
  function R26 return T26 is begin return T26(R(26)); end;
  function R27 return T27 is begin return T27(R(27)); end;
  function R28 return T28 is begin return T28(R(28)); end;
  function R29 return T29 is begin return T29(R(29)); end;
  function R30 return T30 is begin return T30(R(30)); end;
  function R31 return T31 is begin return T31(R(31)); end;
  function R32 return T32 is begin return T32(R(32)); end;
  function R33 return T33 is begin return T33(R(33)); end;
  function R34 return T34 is begin return T34(R(34)); end;
  function R35 return T35 is begin return T35(R(35)); end;
  function R36 return T36 is begin return T36(R(36)); end;
  function R37 return T37 is begin return T37(R(37)); end;
  function R38 return T38 is begin return T38(R(38)); end;
  function R39 return T39 is begin return T39(R(39)); end;
  function R40 return T40 is begin return T40(R(40)); end;
  function R41 return T41 is begin return T41(R(41)); end;
  function R42 return T42 is begin return T42(R(42)); end;
  function R43 return T43 is begin return T43(R(43)); end;
  function R44 return T44 is begin return T44(R(44)); end;
  function R45 return T45 is begin return T45(R(45)); end;
  function R46 return T46 is begin return T46(R(46)); end;
  function R47 return T47 is begin return T47(R(47)); end;
  function R48 return T48 is begin return T48(R(48)); end;
  function R49 return T49 is begin return T49(R(49)); end;
  function R50 return T50 is begin return T50(R(50)); end;
  function R51 return T51 is begin return T51(R(51)); end;
  function R52 return T52 is begin return T52(R(52)); end;
  function R53 return T53 is begin return T53(R(53)); end;
  function R54 return T54 is begin return T54(R(54)); end;
  function R55 return T55 is begin return T55(R(55)); end;
  function R56 return T56 is begin return T56(R(56)); end;
  function R57 return T57 is begin return T57(R(57)); end;
  function R58 return T58 is begin return T58(R(58)); end;
  function R59 return T59 is begin return T59(R(59)); end;
  function R60 return T60 is begin return T60(R(60)); end;
  function R61 return T61 is begin return T61(R(61)); end;
  function R62 return T62 is begin return T62(R(62)); end;
  function R63 return T63 is begin return T63(R(63)); end;
  function R64 return T64 is begin return T64(R(64)); end;
  function R65 return T65 is begin return T65(R(65)); end;
  function R66 return T66 is begin return T66(R(66)); end;
  function R67 return T67 is begin return T67(R(67)); end;
  function R68 return T68 is begin return T68(R(68)); end;
  function R69 return T69 is begin return T69(R(69)); end;
  function R70 return T70 is begin return T70(R(70)); end;
  function R71 return T71 is begin return T71(R(71)); end;
  function R72 return T72 is begin return T72(R(72)); end;
  function R73 return T73 is begin return T73(R(73)); end;
  function R74 return T74 is begin return T74(R(74)); end;
  function R75 return T75 is begin return T75(R(75)); end;
  function R76 return T76 is begin return T76(R(76)); end;
  function R77 return T77 is begin return T77(R(77)); end;
  function R78 return T78 is begin return T78(R(78)); end;
  function R79 return T79 is begin return T79(R(79)); end;
  function R80 return T80 is begin return T80(R(80)); end;
  function R81 return T81 is begin return T81(R(81)); end;
  function R82 return T82 is begin return T82(R(82)); end;
  function R83 return T83 is begin return T83(R(83)); end;
  function R84 return T84 is begin return T84(R(84)); end;
  function R85 return T85 is begin return T85(R(85)); end;
  function R86 return T86 is begin return T86(R(86)); end;
  function R87 return T87 is begin return T87(R(87)); end;
  function R88 return T88 is begin return T88(R(88)); end;
  function R89 return T89 is begin return T89(R(89)); end;
  function R90 return T90 is begin return T90(R(90)); end;
  function R91 return T91 is begin return T91(R(91)); end;
  function R92 return T92 is begin return T92(R(92)); end;
  function R93 return T93 is begin return T93(R(93)); end;
  function R94 return T94 is begin return T94(R(94)); end;
  function R95 return T95 is begin return T95(R(95)); end;
  function R96 return T96 is begin return T96(R(96)); end;
  function R97 return T97 is begin return T97(R(97)); end;
  function R98 return T98 is begin return T98(R(98)); end;
  function R99 return T99 is begin return T99(R(99)); end;
  function R100 return T100 is begin return T100(R(100)); end;
  function R101 return T101 is begin return T101(R(101)); end;
  function R102 return T102 is begin return T102(R(102)); end;
  function R103 return T103 is begin return T103(R(103)); end;
  function R104 return T104 is begin return T104(R(104)); end;
  function R105 return T105 is begin return T105(R(105)); end;
  function R106 return T106 is begin return T106(R(106)); end;
  function R107 return T107 is begin return T107(R(107)); end;
  function R108 return T108 is begin return T108(R(108)); end;
  function R109 return T109 is begin return T109(R(109)); end;
  function R110 return T110 is begin return T110(R(110)); end;
  function R111 return T111 is begin return T111(R(111)); end;
  function R112 return T112 is begin return T112(R(112)); end;
  function R113 return T113 is begin return T113(R(113)); end;
  function R114 return T114 is begin return T114(R(114)); end;
  function R115 return T115 is begin return T115(R(115)); end;
  function R116 return T116 is begin return T116(R(116)); end;
  function R117 return T117 is begin return T117(R(117)); end;
  function R118 return T118 is begin return T118(R(118)); end;
  function R119 return T119 is begin return T119(R(119)); end;
  function R120 return T120 is begin return T120(R(120)); end;
  function R121 return T121 is begin return T121(R(121)); end;
  function R122 return T122 is begin return T122(R(122)); end;
  function R123 return T123 is begin return T123(R(123)); end;
  function R124 return T124 is begin return T124(R(124)); end;
  function R125 return T125 is begin return T125(R(125)); end;
  function R126 return T126 is begin return T126(R(126)); end;
  function R127 return T127 is begin return T127(R(127)); end;
  function R128 return T128 is begin return T128(R(128)); end;
  function R129 return T129 is begin return T129(R(129)); end;
  function R130 return T130 is begin return T130(R(130)); end;
  function R131 return T131 is begin return T131(R(131)); end;
  function R132 return T132 is begin return T132(R(132)); end;
  function R133 return T133 is begin return T133(R(133)); end;
  function R134 return T134 is begin return T134(R(134)); end;
  function R135 return T135 is begin return T135(R(135)); end;
  function R136 return T136 is begin return T136(R(136)); end;
  function R137 return T137 is begin return T137(R(137)); end;
  function R138 return T138 is begin return T138(R(138)); end;
  function R139 return T139 is begin return T139(R(139)); end;
  function R140 return T140 is begin return T140(R(140)); end;
  function R141 return T141 is begin return T141(R(141)); end;
  function R142 return T142 is begin return T142(R(142)); end;
  function R143 return T143 is begin return T143(R(143)); end;
  function R144 return T144 is begin return T144(R(144)); end;
  function R145 return T145 is begin return T145(R(145)); end;
  function R146 return T146 is begin return T146(R(146)); end;
  function R147 return T147 is begin return T147(R(147)); end;
  function R148 return T148 is begin return T148(R(148)); end;
  function R149 return T149 is begin return T149(R(149)); end;
  function R150 return T150 is begin return T150(R(150)); end;
  function R151 return T151 is begin return T151(R(151)); end;
  function R152 return T152 is begin return T152(R(152)); end;
  function R153 return T153 is begin return T153(R(153)); end;
  function R154 return T154 is begin return T154(R(154)); end;
  function R155 return T155 is begin return T155(R(155)); end;
  function R156 return T156 is begin return T156(R(156)); end;
  function R157 return T157 is begin return T157(R(157)); end;
  function R158 return T158 is begin return T158(R(158)); end;
  function R159 return T159 is begin return T159(R(159)); end;
  function R160 return T160 is begin return T160(R(160)); end;
  function R161 return T161 is begin return T161(R(161)); end;
  function R162 return T162 is begin return T162(R(162)); end;
  function R163 return T163 is begin return T163(R(163)); end;
  function R164 return T164 is begin return T164(R(164)); end;
  function R165 return T165 is begin return T165(R(165)); end;
  function R166 return T166 is begin return T166(R(166)); end;
  function R167 return T167 is begin return T167(R(167)); end;
  function R168 return T168 is begin return T168(R(168)); end;
  function R169 return T169 is begin return T169(R(169)); end;
  function R170 return T170 is begin return T170(R(170)); end;
  function R171 return T171 is begin return T171(R(171)); end;
  function R172 return T172 is begin return T172(R(172)); end;
  function R173 return T173 is begin return T173(R(173)); end;
  function R174 return T174 is begin return T174(R(174)); end;
  function R175 return T175 is begin return T175(R(175)); end;
  function R176 return T176 is begin return T176(R(176)); end;
  function R177 return T177 is begin return T177(R(177)); end;
  function R178 return T178 is begin return T178(R(178)); end;
  function R179 return T179 is begin return T179(R(179)); end;
  function R180 return T180 is begin return T180(R(180)); end;
  function R181 return T181 is begin return T181(R(181)); end;
  function R182 return T182 is begin return T182(R(182)); end;
  function R183 return T183 is begin return T183(R(183)); end;
  function R184 return T184 is begin return T184(R(184)); end;
  function R185 return T185 is begin return T185(R(185)); end;
  function R186 return T186 is begin return T186(R(186)); end;
  function R187 return T187 is begin return T187(R(187)); end;
  function R188 return T188 is begin return T188(R(188)); end;
  function R189 return T189 is begin return T189(R(189)); end;
  function R190 return T190 is begin return T190(R(190)); end;
  function R191 return T191 is begin return T191(R(191)); end;
  function R192 return T192 is begin return T192(R(192)); end;
  function R193 return T193 is begin return T193(R(193)); end;
  function R194 return T194 is begin return T194(R(194)); end;
  function R195 return T195 is begin return T195(R(195)); end;
  function R196 return T196 is begin return T196(R(196)); end;
  function R197 return T197 is begin return T197(R(197)); end;
  function R198 return T198 is begin return T198(R(198)); end;
  function R199 return T199 is begin return T199(R(199)); end;
  function R200 return T200 is begin return T200(R(200)); end;

begin
  Reset(seed);
  Rand_pl.Reset(seed_pl);
end Delirium;
