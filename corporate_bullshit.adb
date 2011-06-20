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
--        Andrew Fox, Kurt Dickmann
--  - high-level, responsive empowerments by Ludovic Brenta
--
--  Legal licensing note:
--
--  Copyright (c) Gautier de Montmollin 2006..2011
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

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Delirium;                          use Delirium;

package body Corporate_Bullshit is

  -- To do:
  --   * Enrich the Proposition function
  --   * Person_adjecive: "commited"
  --   * specific place for BAD, negative items, to be fought (
  --      "issues", "intricacies", "organizational diseconomies" ,
  --      "black swan")
  --   * other sentances; rhetorical questions
  --   * Fix bugs marked with !!
  --   * "integrate into"
  --   * think "big picture", "outside the box"
  --   * Bullshit ratio (Emilio)
  --   * mix with specific vocabulary (combined with previous:
  --       blending of (bullshit, normal, custom); type in Delirium)
  --   * http://www.standardshop.com/bs15000.htm
  --   * "Let us take this offline"

  -- Persons or groups --

  function Boss return String is

    function Managing return String is
    begin
      case R4 is
        when 1 => return "Managing ";
        when others => return "";
      end case;
    end;

    function Title return String is
      function Vice return String is
      begin
        case R4 is
          when 1 => return "Vice ";
          when others => return "";
        end case;
      end;
    begin
      case R4 is
        when 1 => return Vice & "Director";
        when 2 => return "Chief";
        when 3 => return "Head";
        when 4 => return Vice & "President";
      end case;
    end Title;

    function Age return String is
    begin
      case R4 is
        when 1 => return "Senior ";
        when others => return "";
      end case;
    end;

    function Exec return String is
    begin
      case R6 is
        when 1 => return "Executive ";
        when others => return "";
      end case;
    end;

    function Department return String is
    begin
      case R6 is
        when 1 => return "Human Resources";
        when 2 => return "Global Controlling";
        when 3 => return "Internal Audit";
        when 4 => return "Legal";
        when 5 => return "Operations";
        when 6 => return "Management Office";
      end case;
    end;

  begin
    case R4 is
      when 1 =>
        return Managing & Age & Exec & Title & " of " & Department;
      when others =>
        return "Chief " & Department & " Officer";
    end case;
  end Boss;

  function Thing_atom(p: Plurality) return String;

  function Person(p: Plurality) return String is
  begin
    case p is
      when singular =>
        case R15 is
          when 1  => return "steering committee";
          when 2  => return "group";
          when 3  => return "project manager";
          when 4  => return Thing_atom(Random_plural) & " champion";
          when 5  => return "community";
          when 6  => return "sales manager";
          when 7  => return "enabler"; -- also thing
          when 8  => return "powerful champion";
          when 9  => return "thought leader"; -- Thanks Andy!
          when 10 => return "gatekeeper"; -- ME - UWM 2010
          when 11 => return "resource";   -- no, we're not people, we're "resources"
          when others =>
            return Boss;
        end case;
      when plural =>
        case R8 is
          when 1  => return "key people";
          when 2  => return "human resources";
          when 3  => return "customers";
          when 4  => return "clients";
          when 5  => return "resources";
          when 6  => return "team players";
          when 7  => return "enablers"; -- also thing
          when 8  => return "stakeholders";
        end case;
    end case;
  end Person;

  function Matrix_or_so return String is
  begin
    case R3 is
      when 1 => return "silo";   -- classic 1-dimension units in organizations
      when 2 => return "matrix"; -- 2nd dimension, with dotted lines
      when 3 => return "cube";   -- 3rd dimension (at last then, the company has become totally dysfunctional)
    end case;
  end Matrix_or_so;

  -- Things --

  function Thing_adjective return String is
  begin
    case R86 is
      when 1  => return "efficient";
      when 2  => return "strategic";
      when 3  => return "constructive";
      when 4  => return "proactive";
      when 5  => return "strong";
      when 6  => return "key";
      when 7  => return "global";
      when 8  => return "corporate";
      when 9  => return "cost-effective";
      when 10 => return "focused";
      when 11 => return "top-line";
      when 12 => return "credible";
      when 13 => return "agile";
      when 14 => return "holistic";
      when 15 => return "new"; -- something neutral to add credibility ;-)
      when 16 => return "adaptive";
      when 17 => return "optimal";
      when 18 => return "unique";
      when 19 => return "core";
      when 20 => return "compliant";
      when 21 => return "goal-oriented";
      when 22 => return "non-linear";
      when 23 => return "problem-solving";
      when 24 => return "prioritizing";
      when 25 => return "cultural";
      when 26 => return "future-oriented";
      when 27 => return "potential";
      when 28 => return "versatile";
      when 29 => return "leading";
      when 30 => return "dynamic";
      when 31 => return "progressive";
      when 32 => return "non-deterministic"; -- NF 15-May-2008
      -- Added 21-Nov-2008:
      when 33 => return "informed";
      when 34 => return "leveraged";
      when 35 => return "challenging";
      when 36 => return "intelligent";
      when 37 => return "controlled";
      when 38 => return "educated";
      when 39 => return "non-standard";
      when 40 => return "underlying";
      when 41 => return "centralized";
      when 42 => return "decentralized";
      when 43 => return "reliable";
      when 44 => return "consistent";
      when 45 => return "competent";
      when 46 => return "prospective";
      --
      when 47 => return "collateral";
      -- the pyramid-cube 2004, added 2009:
      when 48 => return "functional";
      --
      when 49 => return "tolerably expensive";
      when 50 => return "organic";
      when 51 => return "forward-looking";
      when 52 => return "high level";
      when 53 => return "executive";
      when 54 => return "seamless";
      when 55 => return "spectral";
      when 56 => return "balanced";
      when 57 => return "effective";
      -- Buzz Phrase Generator.xls (Kurt)
      when 58 => return "integrated";
      when 59 => return "systematized";
      when 60 => return "parallel";
      when 61 => return "responsive";
      when 62 => return "synchronized";
      when 63 => return "compatible";
      --
      when 64 => return "carefully thought-out";
      -- BBC office-speak phrases
      when 65 => return "cascading";
      when 66 => return "high-level";
      when 67 => return "siloed";
      --
      when 68 => return "operational";
      when 69 => return "future-ready";
      -- The Blending
      when 70 => return "flexible";
      when 71 => return "movable";
      when 72 => return "right";
      when 73 => return "productive";
      when 74 => return "evolutionary";
      when 75 => return "overarching";
      when 76 => return "documented";
      when 77 => return "awesome"; -- ME
      -- UW Presentation Nov 2010
      when 78 => return "coordinated";
      when 79 => return "aligned";
      when 80 => return "enhanced"; -- ME 15-Jun-2011
      -- Ludovic:
      when 81 => return "replacement";
      when 82 => return "industry-standard";
      when 83 => return "accepted";
      when 84 => return "agreed-upon";
      when 85 => return "target";
      --
      when 86 => return "well-communicated";
    end case;
  end Thing_adjective;

  function Timeless_event return String is
  begin
    case R4 is
      when 1 => return "kick-off";
      when 2 => return "roll-out";
      when 3 => return "client event";
      when 4 => return "quarter results";
    end case;
  end Timeless_event;

  function Growth return String is
    function Adjective return String is
    begin
      case R4 is
        when 1 => return "organic";
        when 2 => return "double-digit";
        when 3 => return "upper single-digit";
        when 4 => return "breakout";
      end case;
    end Adjective;
  begin
    return Adjective & " growth";
  end Growth;

  function Thing_atom(p: Plurality) return String is

    function Inner return String is -- can be made plural with 's'
    begin
      case R94 is
        when 1 => return "mission";
        when 2 => return "vision";
        when 3 => return "guideline";
        when 4 => return "roadmap";
        when 5 => return "timeline";
        when 6 => return Matrix_or_so;
        when 7 => return "win-win solution";
        when 8 => return "baseline starting point";
        when 9 => return "sign-off";
        when 10 => return "escalation";
        when 12 => return "system";
        when 13 => return "Management Information System";
        when 14 => return "Quality Management System";
        when 15 => return "planning";
        when 16 => return "target";
        when 17 => return "calibration";
        when 18 => return "Control Information System";
        when 19 => return "process";
        when 20 => return "talent";
        when 21 => return "execution"; -- Winner 2006!
        when 22 => return "leadership";
        when 23 => return "performance";
        when 24 => return "solution provider";
        when 25 => return "value";
        when 26 => return "value creation";
        when 27 => return "feedback";
        when 28 => return "document";
        when 29 => return "bottom line";
        when 30 => return "momentum";
        when 31 => return "intelligence";
        when 32 => return "governance";
        when 33 => return "issue";
        when 34 => return "core meeting";
        when 35 => return "platform";
        when 36 => return "niche";
        when 37 => return "content";
        when 38 => return "communication";
        when 39 => return "goal";
        when 40 => return "skill";
        when 41 => return "alternative";
        when 42 => return "culture";
        when 43 => return "requirement";
        when 44 => return "potential";
        when 45 => return "challenge";
        when 46 => return "empowerment";
        when 47 => return "benchmarking";
        when 48 => return "framework";
        when 49 => return "benchmark";
        when 50 => return "implication";
        when 51 => return "integration";
        when 52 => return "enabler"; -- also person
        when 53 => return "control";
        when 54 => return "trend";
        -- the pyramid-cube 2004, added 2009:
        when 55 => return "business case";
        when 56 => return "architecture";
        when 57 => return "action plan";
        when 58 => return "project";
        when 59 => return "review cycle";
        when 11 => return "trigger event";
        when 60 => return "strategy formulation";
        when 61 => return "decision";
        when 62 => return "enhanced data capture";
        when 63 => return "energy";
        when 64 => return "plan";
        when 65 => return "initiative";
        when 66 => return "priority";
        when 67 => return "synergy";
        when 68 => return "incentive";
        when 69 => return "dialogue";
        -- Buzz Phrase Generator.xls (Kurt)
        when 70 => return "concept";
        when 71 => return "time-phase";
        when 72 => return "projection";
        -- Merger buzz 2009:
        when 73 => return "blended approach";
        -- BBC office-speak phrases
        when 74 => return "low hanging fruit";
        when 75 => return "forward planning";
        when 76 => return "pre-plan";
        when 77 => return "pipeline";
        when 78 => return "bandwidth";
        when 79 => return "workshop";
        when 80 => return "paradigm";
        when 81 => return "paradigm shift";
        when 82 => return "strategic staircase";
        --
        when 83 => return "cornerstone";
        when 84 => return "executive talent";
        when 85 => return "evolution";
        when 86 => return "workflow";
        when 87 => return "message";
        -- GAC 2010
        when 88 => return "known unknown";
        when 89 => return "unknown unknown";
        when 90 => return "pillar";
        -- Andy
        when 91 => return "internal client";
        when 92 => return "consistency";
        -- Ludovic
        when 93 => return "on-boarding process";
        --
        when 94 => return "dotted line";
      end case;
    end Inner;

  begin
    case p is
      when singular =>
        case R127 is
          when 1  => return Timeless_event;
          when 2  => return "efficiency";
          when 3  => return "team building";
          when 4  => return "focus";
          when 5  => return "strategy";
          when 6  => return "planning granularity";
          when 7  => return "best practice";
          when 8  => return "core business";
          when 9  => return Growth;
          when 10 => return "opportunity";
          when 11 => return "credibility";
          when 12 => return "ROE";
          when 13 => return "EBITDA";
          when 14 => return "enterprise content management";
          when 15 => return "excellence";
          when 16 => return "industry";
          when 17 => return "philosophy";
          when 18 => return "commitment";
          when 19 => return "perspective";
          when 20 => return "risk appetite";
          when 21 => return "trust";
          when 22 => return "respect";
          when 23 => return "openness";
          when 24 => return "transparency";
          when 25 => return "Quality Research";
          when 26 => return "decision making";
          when 27 => return "risk management";
          when 28 => return "enterprise risk management";
          -- Added 21-Nov-2008:
          when 29 => return "leverage";
          when 30 => return "diversification";
          when 31 => return "successful execution";
          when 32 => return "effective execution";
          when 33 => return "brand identity";
          -- Mili:
          when 34 => return "customer centricity";
          -- Andrew:
          when 35 => return "shareholder value";
          -- Directly pasted from a management presentation (2009)
          when 36 => return "selectivity";
          when 37 => return "optionality";
          when 38 => return "expertise";
          when 39 => return "attitude";
          when 40 => return "mindset";
          when 41 => return "awareness";
          when 42 => return "broader thinking";
          when 43 => return "client focus";
          -- Buzz Phrase Generator.xls (Kurt)
          when 44 => return "flexibility";
          when 45 => return "thought leadership"; -- Thanks Andy!
          when 46 => return "quest for quality"; -- caracal
          -- BBC office-speak phrases
          when 47 => return "360-degree thinking";
          when 48 => return "granularity";
          when 49 => return "drill-down";
          when 50 => return "impetus";
          when 51 => return "fairness";
          when 52 => return "intellect";
          when 53 => return "engagement";
          -- GAC 2010
          when 54 => return "emotional impact";
          when 55 => return "emotional intelligence";
          when 56 => return "adaptability";
          when 57 => return "stress management";
          when 58 => return "self-awareness";
          when 59 => return "strategic thinking";
          when 60 => return "pyramid";
          -- Andy
          when 61 => return "cross fertilization";
          -- UW Presentation Nov 2010
          when 62 => return "effectiveness";
          when 63 => return "customer experience";
          when 64 => return "centerpiece";
          --
          when 65 => return "SWOT analysis";
          when others => return Inner;
        end case;
      when plural =>
        case R80 is
          when 1  => return "key target markets";
          when 2  => return "opportunities";
          when 3  => return "style guidelines";
          when 4  => return "key performance indicators";
          when 5  => return "metrics";
          when 6  => return "measures";
          when 7  => return "measurements";
          when 16 => return "intricacies"; -- Georges Modol [!! negative word]
          when 17 => return "philosophies";
          -- Directly pasted from a management presentation (2009)
          when 18 => return "cost efficiencies";
          when 19 => return "lessons learned";
          when 20 => return "tactics";
          --
          when 21 => return "organizing principles";
          -- GAC 2010
          when 22 => return "interpersonal skills";
          -- UWM 2010
          when 23 => return "soft cycle issues"; -- [!! negative word]
          when others => return Make_eventual_plural(Inner, plural);
        end case;
    end case;
  end Thing_atom;

  function Thing(p: Plurality) return String is
  begin
    case R100 is
      when  1.. 9 => return Thing_adjective & ", " &
                            Thing_adjective & ", " & Thing_atom(p);
      when 10..14 => return Thing_adjective & " and " &
                            Thing_adjective & ' ' & Thing_atom(p);
      when 15..70 => return Thing_adjective & ' ' & Thing_atom(p);
      when others => return Thing_atom(p);
    end case;
  end Thing;

  -- Verbs --

  function Eventual_Adverb return String is
  begin
    case R33 is
      when 1 => return "interactively ";
      when 2 => return "credibly ";
      when 3 => return "quickly ";
      when 4 => return "proactively ";
      when 5 => return "200% ";
      when 6 => return "24/7 ";
      -- UW Presentation Nov 2010
      when 7 => return "globally ";
      when 8 => return "culturally ";
      when 9 => return "technically ";
      when others => return "";
    end case;
  end Eventual_Adverb;

  function Add_random_Article(p: Plurality; to: String) return String;

  function Eventual_postfixed_Adverb return String is
    P : constant Plurality := Random_Plural;
  begin
    case R120 is
      when 1 => return " going forward";
      when 2 => return " within the industry";
      when 3 => return " across the board";
      -- BBC office-speak phrases
      when 4 => return " in this space";
      when 5 => return " from the get-go";
      when 6 => return " at the end of the day";
      when 7 => return " throughout the organization";
      when 8 => return " as part of the plan";
      when 9 => return " by thinking outside of the box";
      when 10 => return " using " & Add_Random_Article (P, Thing (p));
      when 11 => return " by leveraging " & Add_Random_Article (P, Thing (p));
      when 12 => return " taking advantage of " & Add_Random_Article (P, Thing (p));
      when others => return "";
    end case;
  end Eventual_postfixed_Adverb;

  vowel: constant array(Character) of Boolean:=
     ('a'|'e'|'i'|'o'|'u' => True, others => False);

  function Build_plural_verb(verb: String; p: Plurality) return String is
    last: Natural;
  begin
    last:= verb'Last;
    for i in reverse verb'First+1..verb'Last loop
      if verb(i)=' ' then
        last:= i-1;
      end if;
    end loop;
    case p is
      when plural   => return verb;
      when singular =>
        case verb(last) is
          when 'o' | 's' | 'z' =>
            return verb(verb'First..last) & "es" & verb(last+1..verb'Last);
          when 'h' =>
            if Verb (Last - 1) = 'c' then -- catch -> catches
              return verb(verb'First..last) & "es" & verb(last+1..verb'Last);
            else -- plough -> ploughs
              return verb(verb'First..last) & 's' & verb(last+1..verb'Last);
            end if;
          when 'y' =>
            if Vowel (Verb (Last - 1)) then -- ploy -> ploys
              return verb(verb'First..last) & 's' & verb(last+1..verb'Last);
            else -- try -> tries
              return verb(verb'First..Last - 1) & "ies" & verb(last+1..verb'Last);
            end if;
          when others =>
            return verb(verb'First..last) & 's' & verb(last+1..verb'Last);
        end case;
    end case;
  end Build_plural_verb;

  function Person_verb_having_thing_complement(p: Plurality) return String is
    function Inner return String is
    begin
      case R31 is
        when 1  => return "manage";
        when 2  => return "target";
        when 3  => return "streamline";
        when 4  => return "improve";
        when 5  => return "optimize";
        when 6  => return "achieve";
        when 7  => return "secure";
        when 8  => return "address";
        when 9  => return "boost";
        when 10 => return "deploy";
        when 11 => return "innovate";
        when 12 => return "right-scale";
        when 13 => return "formulate";
        when 14 => return "transition";
        when 15 => return "leverage";
        when 16 => return "focus on";
        when 17 => return "synergize";
        when 18 => return "generate";
        when 19 => return "analyse";
        when 20 => return "integrate";
        when 21 => return "empower";
        when 22 => return "benchmark";
        when 23 => return "learn";
        when 24 => return "adapt";
        when 25 => return "enable";
        when 26 => return "strategize";
        when 27 => return "prioritize";
        -- BBC office-speak phrases
        when 28 => return "pre-prepare";
        --
        when 29 => return "deliver";
        when 30 => return "champion";
        when 31 => return "embrace";
      end case;
    end Inner;
  begin
    return Build_plural_verb(Inner,p);
  end Person_verb_having_thing_complement;

  -- (thing) verb (thing)

  function Thing_verb_having_thing_complement(p: Plurality) return String is
    function Inner return String is
    begin
      case R15 is
        when 1  => return "streamline";
        when 2  => return "interact with";
        when 3  => return "boost";
        when 4  => return "generate";
        when 5  => return "impact";
        when 6  => return "enhance";
        when 7  => return "leverage";
        when 8  => return "synergize";
        when 9  => return "generate";
        when 10 => return "empower";
        when 11 => return "enable";
        when 12 => return "prioritize";
        when 13 => return "transfer";
        when 14 => return "drive";
        when 15 => return "result in";
      end case;
    end Inner;
  begin
    return Build_plural_verb(Inner,p);
  end Thing_verb_having_thing_complement;

  -- (thing) verb (person)

  function Thing_verb_having_person_complement(p: Plurality) return String is
    function Inner return String is
    begin
      case R10 is
        when 1 => return "motivate";
        when 2 => return "target";
        when 3 => return "enable";
        when 4 => return "drive";
        when 5 => return "synergize";
        when 6 => return "empower";
        when 7 => return "prioritize";
        -- BBC office-speak phrases
        when 8 => return "incentivise";
        when 9 => return "inspire";
        --
        when 10 => return "transfer";
      end case;
    end Inner;
  begin
    return Build_plural_verb(Inner,p);
  end Thing_verb_having_person_complement;

  function Person_verb_and_complement(p: Plurality) return String is
  -- NB: this function produces an eventual complement after the verb, or
  -- no complement at all.
    function Inner return String is
    begin
      case R16 is
        when 1  => return "streamline the process";
        when 2  => return "address the overarching issues";
        when 3  => return "benchmark the portfolio";
        when 4  => return "manage the cycle";     -- Fad of 2004
        when 5  => return "figure out where we come from, where we are going to";
        when 6  => return "maximize the value";
        when 7  => return "execute the strategy"; -- Obsessive in 2006
        when 8  => return "think out of the box";
        when 9  => return "think differently";
        when 10 => return "manage the balance";
        -- BBC office-speak phrases
        when 11 => return "loop back";
        when 12 => return "conversate";
        when 13 => return "go forward together";
        --
        when 14 => return "achieve efficiencies";
        when 15 => return "deliver"; -- deliver, form without complement
        -- GAC 2010
        when 16 => return "connect the dots";
      end case;
    end Inner;
  begin
    return Build_plural_verb(Inner,p);
  end Person_verb_and_complement;

  function Add_indefinite_Article(p: Plurality; to: String) return String is
  begin
    case p is
      when singular =>
        if vowel(to(to'First)) then
          return "an " & to;
        else
          return "a " & to;
        end if;
      when plural =>
        return to;
    end case;
  end Add_indefinite_Article;

  function Add_random_Article(p: Plurality; to: String) return String is
  begin
    case R15 is
      when 1..2  => return "the " & to;
      when 3..6  => return "our " & to;
      when 7..15 => return Add_indefinite_Article(p,to);
      -- Indefinite is preferred in BS language.
    end case;
  end;

  function Thing_verb_and_ending(p: Plurality) return String is
    compl_sp: constant Plurality:= Random_plural;
  begin
    case R101 is
      when 1..55  =>
        return Thing_verb_having_thing_complement(p) &
          ' ' &
          Add_random_Article(compl_sp, Thing(compl_sp));
      when 56..100 =>
        return Thing_verb_having_person_complement(p) &
          " the " & Person(compl_sp);
      when 101 =>
        return Build_plural_verb("add value",p);
    end case;
  end Thing_verb_and_ending;

  function Faukon return String is
  begin
    case R5 is
      when 1 => return "we must";
      when 2 => return "we've got to";
      when 3 => return "the reporting unit should";
      when 4 => return "controlling should";
      when 5 => return "we must activate the " & Matrix_or_so & " to";
    end case;
  end;

  function Proposition return String is
    sp1, sp2: constant Plurality:= Random_plural;
  begin
    case R100 is
      when 1..5    => -- "We need to..."
        return
          Faukon & ' ' &
          Eventual_Adverb &
          Person_verb_and_complement(plural) &
          Eventual_postfixed_Adverb;
        -- infinitive written same as present plural
      when 6..10    =>
        return
          "the " & person(sp1) & ' ' &
          Eventual_Adverb &
          Person_verb_and_complement(sp1) &
          Eventual_postfixed_Adverb;
      when 11..60  => -- ** THING...
        return
          Add_random_Article(sp1, Thing(sp1)) & ' ' &
          Eventual_Adverb &
          Thing_verb_and_ending(sp1) &
          Eventual_postfixed_Adverb;
      when 61..100 => -- ** PERSON...
        return
          "the " & person(sp1) & ' ' &
          Eventual_Adverb &
          Person_verb_having_thing_complement(sp1) &
          ' ' &
          Add_random_Article(sp2, Thing(sp2)) &
          Eventual_postfixed_Adverb;
    end case;
  end Proposition;

  function Articulated_propositions return String is
  begin
    case R26 is
      when 1..17  => return Proposition;
      when 18     => return Proposition & "; this is why " & Proposition;
      when 19     => return Proposition & "; nevertheless " & Proposition;
      when 20     => return Proposition & ", whereas " & Proposition;
      when 21     => return "our gut-feeling is that " & Proposition;
      when 22..25 => return Proposition & ", while " & Proposition;
      when 26     => return Proposition & "; in the same time " & Proposition;
    end case;
  end;

  function Sentence return String is
    ap: String:= Articulated_propositions;
  begin
    ap(1):= To_Upper(ap(1));
    return ap & ". ";
  end;

  function Sentences(possible_dialog_mark: String) return String is
  begin
    case R40 is
      when 1      => return Sentence;
      when 2..30  => return Sentences(possible_dialog_mark) & Sentence;
      when 31..40 => return Sentences(possible_dialog_mark) & Paragraph & possible_dialog_mark & Sentence;
    end case;
  end Sentences;

  function Sentence_garanteed_amount(count: Positive; possible_dialog_mark: String) return String is
    element: constant String:=
      Paragraph &
      possible_dialog_mark & Sentences(possible_dialog_mark);
  begin
    if count > 1 then
      return
        Sentence_garanteed_amount(count-1, possible_dialog_mark) &
        element;
    else
      return element;
    end if;
  end Sentence_garanteed_amount;

  function Workshop return String is
  begin
    return Sentence_garanteed_amount(500, dialog_mark);
  end Workshop;

  function Financial_Report return String is
  begin
    return Sentences(""); -- !! charts !!
  end Financial_Report;

end Corporate_Bullshit;

