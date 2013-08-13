-------------------------------------------------------------------------
--  The Corporate Bullshit Generator (CBSG)
--  See specification for sources, authors, contributors, license.
-------------------------------------------------------------------------
--  NB: this package is done with the functional programming paradigm.
--  http://en.wikipedia.org/wiki/Functional_programming

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Delirium;                          use Delirium;

package body Corporate_Bullshit is

   -- To do:
   --   * Enrich the Proposition function
   --   * Person_adjective: "commited", "multi-skilled"
   --   * other sentances; rhetorical questions
   --   * "It's not about X, it's about Y"
   --   * "It's about breaking down the silos"
   --   * Fix bugs marked with !!
   --   * "integrate into"
   --   * Bullshit ratio (Emilio)
   --   * mix with specific vocabulary (combined with previous:
   --       blending of (bullshit, normal, custom); type in Delirium)
   --   * http://www.standardshop.com/bs15000.htm
   --   * ...a [s-whatever] that puts [s-whatever] at the center
   --       of the organization's [whatever]
   --   * A Silly Abbreviation Generator (SAG).

   -- Persons or groups --

   function Boss return String is

      function Managing return String is
      begin
         case R8 is
            when 1 => return "Managing ";
            when 2 => return "Acting "; -- Well, we could have an actor too
            when others => return "";
         end case;
      end Managing;

      function Title return String is
         function Vice return String is
         begin
            case R4 is
               when 1 => return "Vice ";
               when others => return "";
            end case;
         end Vice;
         function Co return String is
         begin
            case R5 is
               when 1 => return "Co-";
               when others => return "";
            end case;
         end Co;
      begin
         case R4 is
            when 1 => return Vice & "Director";
            when 2 => return "Chief";
            when 3 => return Co & "Head";
            when 4 => return Vice & "President";
         end case;
      end Title;

      function Age return String is
      begin
         case R4 is
            when 1 => return "Senior ";
            when others => return "";
         end case;
      end Age;

      function Exec return String is
      begin
         case R6 is
            when 1 => return "Executive ";
            when others => return "";
         end case;
      end Exec;

      function Groupal return String is
      begin
         case R20 is
            when 1 => return "Group ";
            when 2 => return "Global ";
            when others => return "";
         end case;
      end Groupal;

      function Department return String is
      begin
         case R14 is
            when 1  => return "Human Resources";
            when 2  => return "Controlling";
            when 3  => return "Internal Audit";
            when 4  => return "Legal";
            when 5  => return "Operations";
            when 6  => return "Management Office";
            when 7  => return "Customer Relations";
            when 8  => return "Client Leadership";
            when 9  => return "Client Relationship";
            when 10 => return "Business Planning";
            when 11 => return "Business Operations";
            when 12 => return "IT Strategy";
            when 13 => return "IT Operations";
            when 14 => return "Marketing";
         end case;
      end Department;

      function Department_or_Top_Role return String is
      begin
        case R17 is
          when 1..14 =>
            return Department;
          when 15 =>
            return "Visionary";
          when 16 =>
            return "Digital";
          when 17 =>
            return "Technical";
        end case;
      end Department_or_Top_Role;

   begin
      case R4 is
         when 1 =>      -- A fully normal boss (eventually, a managing one)
            return Managing & Age & Exec & Title & " of " & Department;
         when others => -- Chief X Officer
            return Groupal & "Chief " & Department_or_Top_Role & " Officer";
      end case;
   end Boss;

   function Thing_Atom (P: Plurality) return String;

   function Person (P: Plurality) return String is
   begin
      case P is
         when Singular =>
            case R17 is
               when 1  => return "steering committee";
               when 2  => return "group";
               when 3  => return "project manager";
               when 4  => return Thing_Atom (Random_Plural) & " champion";
               when 5  => return "community";
               when 6  => return "sales manager";
               when 7  => return "enabler"; -- also thing
               when 8  => return "powerful champion";
               when 9  => return "thought leader"; -- Thanks Andy!
               when 10 => return "gatekeeper"; -- ME - UWM 2010
               when 11 => return "resource";   -- no, we're not people, we're "resources"
               when 12 => return "senior support staff";
               when others =>
                  return Boss;
            end case;
         when Plural =>
            case R11 is
               when 1  => return "key people";
               when 2  => return "human resources";
               when 3  => return "customers";
               when 4  => return "clients";
               when 5  => return "resources";
               when 6  => return "team players";
               when 7  => return "enablers"; -- also thing
               when 8  => return "stakeholders";
               when 9  => return "standard-setters";
               when 10 => return "partners";
               when 11 => return "business leaders";
            end case;
      end case;
   end Person;

   function Matrix_Or_So return String is
   begin
      case R12 is
         when 1 .. 2  => return "organization"; -- a bit flat, but flashy combined with "within the "
         when 3 .. 6  => return "silo";   -- classic 1-dimension units in organizations
         when 7 .. 10 => return "matrix"; -- 2nd dimension, with dotted lines
         when 11      => return "cube";   -- 3rd dimension (Converium); at last then, the company has become totally dysfunctional)
         when 12      => return "sphere"; -- another esoteric 3-dimensional structure - ME 20-Jun-2011
      end case;
   end Matrix_Or_So;

   -- Things --

   function Thing_Adjective return String is
   begin
      case R239 is
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
         when 52 => return "next-level";
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
         when 86 => return "customer-centric";
         when 87 => return "wide-spectrum";
         --
         when 88 => return "well-communicated";
         -- PDM, July 2011
         when 89 => return "cutting-edge";
         when 90 => return "best-in-class";
         when 91 => return "state-of-the-art";
         when 92 => return "verifiable";
         --
         when 93  => return "solid";
         when 94  => return "inspiring";
         when 95  => return "growing";
         when 96  => return "market-altering";
         when 97  => return "vertical";
         when 98  => return "emerging";
         when 99  => return "differentiating";
         when 100 => return "integrative";
         when 101 => return "cross-functional";
         when 102 => return "measurable";
         when 103 => return "well-planned";
         when 104 => return "accessible";
         when 105 => return "actionable";
         when 106 => return "accurate";
         when 107 => return "insightful";
         when 108 => return "relevant";
         when 109 => return "long-term";
         when 110 => return "top";
         when 111 => return "tactical";
         when 112 => return "best-of-breed";
         when 113 => return "robust";
         when 114 => return "targeted";
         when 115 => return "personalized";
         when 116 => return "interactive";
         when 117 => return "streamlined";
         when 118 => return "transparent";
         when 119 => return "traceable";
         when 120 => return "far-reaching";
         when 121 => return "powerful";
         when 122 => return "improved";
         when 123 => return "executive-level";
         when 124 => return "goal-based";
         when 125 => return "top-level";
         when 126 => return "cooperative";
         when 127 => return "value-adding";
         when 128 => return "streamlining";
         when 129 => return "time-honored"; -- PDM
         when 130 => return "idiosyncratic"; -- ED
         when 131 => return "sustainable";
         when 132 => return "in-depth";
         when 133 => return "immersive";
         when 134 => return "cross-industry";
         when 135 => return "time-phased";
         when 136 => return "day-to-day";
         when 137 => return "present-day";
         when 138 => return "medium-to-long-term";
         when 139 => return "profit-maximizing";
         when 140 => return "generic";
         when 141 => return "granular";
         when 142 => return "market-driven";
         when 143 => return "value-driven";
         when 144 => return "well-defined";
         when 145 => return "outward-looking";
         when 146 => return "scalable";
         when 147 => return "strategy-focused";
         when 148 => return "promising";
         when 149 => return "collaborative";
         when 150 => return "scenario-based";
         when 151 => return "principle-based";
         when 152 => return "vision-setting";
         when 153 => return "client-oriented";
         when 154 => return "long-established";
         when 155 => return "established";
         when 156 => return "organizational";
         when 157 => return "visionary";
         when 158 => return "trusted";
         when 159 => return "full-scale";
         when 160 => return "firm-wide";
         when 161 => return "fast-growth";
         when 162 => return "performance-based";
         when 163 => return "high-performing";
         when 164 => return "top-down";
         when 165 => return "cross-enterprise";
         when 166 => return "outsourced"; -- BBC - LGA banned words
         when 167 => return "situational"; -- BBC - LGA banned words
         when 168 => return "bottom-up"; -- BBC - LGA banned words
         when 169 => return "multidisciplinary"; -- BBC - LGA banned words
         when 170 => return "one-to-one";
         when 171 => return "goal-directed";
         when 172 => return "intra-organisational";
         when 173 => return "high-performing";
         when 174 => return "multi-source";
         when 175 => return "360-degree"; -- !! <-> 360-degree thinking
         when 176 => return "motivational";
         when 177 => return "differentiated";
         when 178 => return "solutions-based";
         when 179 => return "compelling";
         when 180 => return "structural";
         when 181 => return "go-to-market";
         when 182 => return "on-message";
         when 183 => return "adequate";
         when 184 => return "value-enhancing";
         when 185 => return "mission-critical";
         when 186 => return "business enabling";
         when 187 => return "transitional";
         when 188 => return "future";
         when 189 => return "game-changing";
         when 190 => return "enterprise-wide";
         when 191 => return "rock-solid";
         when 192 => return "bullet-proof";
         when 193 => return "superior";
         when 194 => return "genuine";
         when 195 => return "alert";
         when 196 => return "nimble";
         when 197 => return "phased";
         when 198 => return "selective";
         when 199 => return "macroscopic";
         when 200 => return "low-risk high-yield";
         when 201 => return "interconnected";
         when 202 => return "high-margin";
         when 203 => return "resilient";
         when 204 => return "high-definition";
         when 205 => return "well-crafted";
         when 206 => return "fine-grained";
         when 207 => return "context-aware";
         when 208 => return "multi-tasked";
         when 209 => return "feedback-based";
         when 210 => return "analytics-based";
         when 211 => return "fact-based";
         when 212 => return "customer-facing";
         when 213 => return "multi-channel";
         when 214 => return "omni-channel";
         when 215 => return "pre-approved";
         when 216 => return "specific";
         when 217 => return "heart-of-the-business";
         when 218 => return "responsible";
         when 219 => return "socially conscious";
         when 220 => return "results-centric";
         when 221 => return "business-led";
         when 222 => return "well-positioned";
         when 223 => return "end-to-end";
         when 224 => return "high-quality";
         when 225 => return "siloed";
         when 226 => return "modular";
         when 227 => return "service-oriented";
         when 228 => return "competitive";
         when 229 => return "scale-as-you-grow";
         when 230 => return "outside-in";
         when 231 => return "hyper-hybrid";
         when 232 => return "long-running";
         when 233 => return "large-scale";
         when 234 => return "wide-ranging";
         when 235 => return "unified";
         when 236 => return "active";
         when 237 => return "dramatic";
         when 238 => return "aggressive";
         when 239 => return "innovative";
      end case;
   end Thing_Adjective;

   function Timeless_Event return String is
   begin
      case R4 is
         when 1 => return "kick-off";
         when 2 => return "roll-out";
         when 3 => return "client event";
         when 4 => return "quarter results";
      end case;
   end Timeless_Event;

   function Growth return String is
      function Superlative return String is
      begin
         case R8 is
            when 1 => return "organic";
            when 2 => return "double-digit";
            when 3 => return "upper single-digit";
            when 4 => return "breakout";
               -- Ludovic
            when 5 => return "unprecedented";
            when 6 => return "unparallelled";
            when 7 => return "proven";
            when 8 => return "measured";
         end case;
      end Superlative;

      function Improvement return String is
      begin
         case R5 is
            -- Ludovic
            when 1 => return " growth";
            when 2 => return " improvement";
            when 3 => return " throughput increase";
            when 4 => return " efficiency gain";
            when 5 => return " yield enhancement";
         end case;
      end Improvement;
   begin
      return Superlative & Improvement;
   end Growth;

   function Thing_Atom (P: Plurality) return String is

      function Inner return String is -- can be made plural
      begin
         case R184 is
            when 1 => return "mission";
            when 2 => return "vision";
            when 3 => return "guideline";
            when 4 => return "roadmap";
            when 5 => return "timeline";
            when 6 => return Matrix_Or_So;
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
            when 31 => return "opportunity";
            when 32 => return "credibility";
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
            when 76 => return "pre-plan"; -- also a verb
            when 77 => return "pipeline";
            when 78 => return "bandwidth";
            when 79 => return "workshop";
            when 80 => return "paradigm";
            when 81 => return "paradigm shift";
            when 82 => return "strategic staircase";
               --
            when 83  => return "cornerstone";
            when 84  => return "executive talent";
            when 85  => return "evolution";
            when 86  => return "workflow";
            when 87  => return "message";
               -- GAC 2010
            when 88  => return "risk/return profile";
            when 89  => return "efficient frontier";
            when 90  => return "pillar";
               -- Andy
            when 91  => return "internal client";
            when 92  => return "consistency";
               -- Ludovic
            when 93  => return "on-boarding process";
               --
            when 94  => return "dotted line";
            when 95  => return "action item";
            when 96  => return "cost efficiency";
            when 97  => return "channel";
            when 98  => return "convergence";
            when 99  => return "infrastructure";
            when 100 => return "metric";
            when 101 => return "technology";
            when 102 => return "relationship";
            when 103 => return "partnership";
            when 104 => return "supply-chain";
            when 105 => return "portal";
            when 106 => return "solution";
            when 107 => return "business line";
            when 108 => return "white paper";
            when 109 => return "scalability";
            when 110 => return "innovation";
            when 111 => return "Strategic Management System";
            when 112 => return "Balanced Scorecard";
            when 113 => return "differentiator"; -- PDM
            when 114 => return "case study";
            when 115 => return "idiosyncrasy"; -- ED
            when 116 => return "benefit";
            when 117 => return "say/do ratio";
            when 118 => return "segmentation";
            when 119 => return "image";
            when 120 => return "realignment";
            when 121 => return "business model";
            when 122 => return "business philosophy";
            when 123 => return "branding";
            when 124 => return "methodology";
            when 125 => return "profile";
            when 126 => return "measure";
            when 127 => return "measurement";
            when 128 => return "philosophy";
            when 129 => return "branding strategy";
            when 130 => return "efficiency";
            when 131 => return "industry";
            when 132 => return "commitment";
            when 133 => return "perspective";
            when 134 => return "risk appetite";
            when 135 => return "best practice";
            when 136 => return "brand identity";
            when 137 => return "customer centricity"; -- Mili
            when 138 => return "shareholder value"; -- Andrew
            when 139 => return "attitude";
            when 140 => return "mindset";
            when 141 => return "flexibility";
            when 142 => return "granularity";
            when 143 => return "engagement";
            when 144 => return "pyramid";
            when 145 => return "market";
            when 146 => return "diversity";
            when 147 => return "interdependency";
            when 148 => return "scaling";
            when 149 => return "asset";
            when 150 => return "flow charting";
            when 151 => return "value proposition";
            when 152 => return "performance culture";
            when 153 => return "change";
            when 154 => return "reward";
            when 155 => return "learning";
            when 156 => return "next step";
            when 157 => return "delivery framework";
            when 158 => return "structure";
            when 159 => return "support structure";
            when 160 => return "standardization";
            when 161 => return "objective";
            when 162 => return "footprint";
            when 163 => return "transformation process";
            when 164 => return "policy";
            when 165 => return "sales target";
            when 166 => return "ecosystem";
            when 167 => return "landscape";
            when 168 => return "atmosphere";
            when 169 => return "environment";
            when 170 => return "core competency";
            when 171 => return "market practice";
            when 172 => return "operating strategy";
            when 173 => return "insight";
            when 174 => return "accomplishment";
            when 175 => return "correlation";
            when 176 => return "touchpoint";
            when 177 => return "knowledge transfer";
            when 178 => return "correlation";
            when 179 => return "capability";
            when 180 => return "gamification";
            when 181 => return "smooth transition";
            when 182 => return "leadership strategy";
            when 183 => return "collaboration";
            when 184 => return "success factor";
         end case;
      end Inner;

   begin
      case P is
         when Singular =>
            case R250 is
               -- Things where plural would sound clunky.
               when 1  => return Timeless_Event;
               when 2  => return "team building";
               when 3  => return "focus";
               when 4  => return "strategy";
               when 5  => return "planning granularity";
               when 6  => return "core business";
               when 7  => return "implementation";
               when 8  => return "intelligence";
               when 9  => return "governance";
               when 10 => return "ROE";
               when 11 => return "EBITDA";
               when 12 => return "enterprise content management";
               when 13 => return "excellence";
               when 14 => return "trust";
               when 15 => return "respect";
               when 16 => return "openness";
               when 17 => return "transparency";
               when 18 => return "Quality Research";
               when 19 => return "decision making";
               when 20 => return "risk management";
               when 21 => return "enterprise risk management";
               when 22 => return "leverage";
               when 23 => return "diversification";
               when 24 => return "successful execution";
               when 25 => return "effective execution";
                  -- Directly pasted from a management presentation (2009)
               when 26 => return "selectivity";
               when 27 => return "optionality";
               when 28 => return "expertise";
               when 29 => return "awareness";
               when 30 => return "broader thinking";
               when 31 => return "client focus";
               when 32 => return "thought leadership"; -- Thanks Andy!
               when 33 => return "quest for quality"; -- caracal
                  -- BBC office-speak phrases
               when 34 => return "360-degree thinking";
               when 35 => return "drill-down";
               when 36 => return "impetus";
               when 37 => return "fairness";
               when 38 => return "intellect";
               when 39 => return "emotional impact";
               when 40 => return "emotional intelligence";
               when 41 => return "adaptability";
               when 42 => return "stress management";
               when 43 => return "self-awareness";
               when 44 => return "strategic thinking";
               when 45 => return "cross fertilization"; -- Andy
               when 46 => return "cross-breeding";
               when 47 => return "customer experience";
               when 48 => return "centerpiece";
               when 49 => return "SWOT analysis";
               when 50 => return "responsibility";
               when 51 => return "accountability";
               when 52 => return "ROI";
               when 53 => return "line of business";
               when 54 => return "serviceability";
               when 55 => return "responsiveness";
               when 56 => return "simplicity";
               when 57 => return "portfolio shaping";
               when 58 => return "knowledge sharing";
               when 59 => return "continuity";
               when 60 => return "visual thinking";
               when 61 => return "interoperability";
               when 62 => return "compliance";
               when 63 => return "teamwork";
               when 64 => return "self-efficacy";
               when 65 => return "decision-making";
               when 66 => return "line-of-sight";
               when 67 => return "scoping"; -- BBC - LGA banned words
               when 68 => return "line-up";
               when 69 => return "predictability";
               when 70 => return "recognition";
               when 71 => return "investor confidence";
               when 72 => return "competitive advantage";
               when 73 => return "uniformity";
               when 74 => return "connectivity";
               when 75 => return "big picture";
               when 76 => return "big-picture thinking";
               when 77 => return "quality";
               when 78 => return "upside focus";
               when 79 => return "sustainability";
               when 80 => return "resiliency";
               when 81 => return "social sphere";
               when 82 => return "intuitiveness";
               when 83 => return "effectiveness";
               when 84 => return "competitiveness";
               when 85 => return "resourcefulness";
               when 86 => return "informationalization";
               when 87 => return "role building";
               when 88 => return "talent retention";
               when others => return Inner;
            end case;
         when Plural =>
            case R200 is
               -- Things you find usually as plural
               when 1  => return "key target markets";
               when 2  => return "style guidelines";
               when 3  => return "key performance indicators";
               when 4  => return "market conditions";
               when 5  => return "market forces";
               when 6  => return "market opportunities";
               when 7  => return "tactics";
                  --
               when 8 => return "organizing principles";
                  -- GAC 2010
               when 9 => return "interpersonal skills";
                  -- UWM 2010
               when 10 => return "roles and responsibilities";
               when 11 => return "cost savings";
                  -- Directly pasted from a management presentation (2009)
               when 12 => return "lessons learned";
               when others => return Make_Eventual_Plural (Inner, Plural);
            end case;
      end case;
   end Thing_Atom;

   function Thing (P: Plurality) return String is
   begin
      case R110 is
         when  1 .. 9 =>   -- 2 adjectives, comma separated
            return
               Thing_Adjective & ", " &
               Thing_Adjective & ", " &
               Thing_Atom (P);
         when 10 .. 14 =>  -- 2 adjectives, separated by "and"
            return
               Thing_Adjective & " and " &
               Thing_Adjective & ' ' &
               Thing_Atom (P);
         when 15 .. 70 =>  -- 1 adjective
            return
               Thing_Adjective & ' ' &
               Thing_Atom (P);
         when 71 .. 72 =>  -- 2 adjectives, separated by "and/or"
            return
               Thing_Adjective & " and/or " &
               Thing_Adjective & ' ' &
               Thing_Atom (P);
         when 73 .. 74 =>
            return Growth; -- already has a superlative, don't add an adjective
         when 75 .. 80 =>  -- 3 adjectives
            return
               Thing_Adjective & ", " &
               Thing_Adjective & " and " &
               Thing_Adjective & ' ' &
               Thing_Atom (P);
         when 81 .. 84 =>  -- 4 adjectives
            return
               Thing_Adjective & ", " &
               Thing_Adjective & ", " &
               Thing_Adjective & " and " &
               Thing_Adjective & ' ' &
               Thing_Atom (P);
         when others =>
            return Thing_Atom (P);
      end case;
   end Thing;

   -- Bad things.
   --
   -- They are always in plural. Singular is avoided for two reasons:
   --
   -- 1. It would be too specific - someone would be tempted to ask for details!
   -- 2. It may be the beginning of a finger-pointing session. Better stay
   --    impersonal to survive the meeting...

   function Bad_Things return String is
   begin
      case R19 is
         when 1  => return "issues";
         when 2  => return "intricacies";
         when 3  => return "organizational diseconomies";
         when 4  => return "black swans";
         when 5  => return "gaps";
         when 6  => return "inefficiencies";
         when 7  => return "overlaps";
         when 8  => return "known unknowns";
         when 9  => return "unknown unknowns";
         when 10 => return "soft cycle issues";
         when 11 => return "obstacles";
         when 12 => return "surprises";
         when 13 => return "weaknesses"; -- The W in SWOT
         when 14 => return "threats";    -- The T in SWOT
         when 15 => return "barriers to success";
         when 16 => return "barriers";
         when 17 => return "shortcomings";
         when 18 => return "problems";
         when 19 => return "uncertainties";
      end case;
   end Bad_Things;

   -- Verbs --

   function Eventual_Adverb return String is
   begin
      case R84 is -- proportion: 3/4 empty adverb
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
         --
         when 10 => return "strategically ";
         when 11 => return "swiftly ";
         when 12 => return "cautiously ";
         when 13 => return "expediently ";
         when 14 => return "organically ";
         when 15 => return "carefully ";
         when 16 => return "significantly ";
         when 17 => return "conservatively ";
         when 18 => return "adequately ";
         when 19 => return "genuinely ";
         when 20 => return "efficiently ";
         when 21 => return "seamlessly ";
         when others => return "";
      end case;
   end Eventual_Adverb;

   function Add_Random_Article (P: Plurality; To: String) return String is
   begin
      case R15 is
         when 1 .. 2  => return "the " & To;
         when 3 .. 6  => return "our " & To;
         when 7 .. 15 => return Add_Indefinite_Article (P,To);
            -- Indefinite is preferred in BS language.
      end case;
   end Add_Random_Article;

   function Eventual_Postfixed_Adverb return String is
      P : constant Plurality := Random_Plural;
   begin
      case R155 is
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
         when 10 => return " using " & Add_Random_Article (P, Thing (P));
         when 11 => return " by leveraging " & Add_Random_Article (P, Thing (P));
         when 12 => return " taking advantage of " & Add_Random_Article (P, Thing (P));
         when 13 => return " within the " & Matrix_Or_So;
         when 14 => return " across the " & Make_Eventual_Plural (Matrix_Or_So, Plural);
         when 15 => return " across and beyond the " & Make_Eventual_Plural (Matrix_Or_So, Plural);
         when 16 => return " resulting in " & Growth;
         when 17 => return " reaped from our " & Growth;
         when 18 => return " as a consequence of " & Growth;
         when 19 => return " because " & Add_Random_Article (P, Thing (P))
                           & ' ' & Build_Plural_Verb ("produce", P) & ' ' & Growth;
         when 20 => return " ahead of schedule";
         when 21 => return ", relative to our peers";
         when 22 => return " on a transitional basis";
         when 23 => return " by expanding boundaries";
         when 24 => return " by nurturing talent";
         when 25 => return ", as a Tier 1 company";
         when 26 => return " up-front";
         when 27 => return " on-the-fly";
         when 28 => return " across our portfolio";
         when 29 => return " 50/50";
         when 30 => return " up, down and across the " & Matrix_Or_So;
         when 31 => return " in the marketplace";
         when 32 => return " by thinking and acting beyond boundaries";
         when 33 => return " at the individual, team and organizational level";
         when others => return "";
      end case;
   end Eventual_Postfixed_Adverb;

   function Person_Verb_Having_Thing_Complement (P: Plurality) return String is
      function Inner return String is
      begin
         case R63 is
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
            --
            when 32 => return "enhance";
            when 33 => return "engineer";
            when 34 => return "envision";
            when 35 => return "incentivize";
            when 36 => return "maximize";
            when 37 => return "visualize";
            when 38 => return "whiteboard";
            when 39 => return "institutionalize";
            when 40 => return "promote";
            when 41 => return "overdeliver";
            when 42 => return "right-size";
            when 43 => return "rebalance";
            when 44 => return "re-imagine";
            when 45 => return "influence";
            when 46 => return "facilitate";
            when 47 => return "drive";
            when 48 => return "structure";
            when 49 => return "standardize";
            when 50 => return "accelerate";
            when 51 => return "deepen";
            when 52 => return "strengthen";
            when 53 => return "broaden";
            when 54 => return "enforce";
            when 55 => return "establish";
            when 56 => return "foster";
            when 57 => return "build";
            when 58 => return "differentiate";
            when 59 => return "take a bite out of";
            when 60 => return "table";
            when 61 => return "flesh out";
            when 62 => return "reach out";
            when 63 => return "jump-start";
         end case;
      end Inner;
   begin
      return Build_Plural_Verb (Inner,P);
   end Person_Verb_Having_Thing_Complement;

   -- Something Bad is going to happen. Fortunately Supermarketman is there
   -- with his secret weapon to clean the Evil thing and rescue the Business.
   -- Well, at least there will be a meeting to begin a discussion about it.

   function Person_Verb_Having_Bad_Thing_Complement (P: Plurality) return String is
      function Inner return String is
      begin
         case R4 is
            when 1  => return "address";
            when 2  => return "identify";
            when 3  => return "avoid";
            when 4  => return "mitigate";
         end case;
      end Inner;
   begin
      return Build_Plural_Verb (Inner,P);
   end Person_Verb_Having_Bad_Thing_Complement;

   -- (thing) verb (thing)

   function Thing_Verb_Having_Thing_Complement (P: Plurality) return String is
      function Inner return String is
      begin
         case R29 is
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
            when 16 => return "promote";
            when 17 => return "influence";
            when 18 => return "facilitate";
            when 19 => return "aggregate";
            when 20 => return "architect";
            when 21 => return "cultivate";
            when 22 => return "engage";
            when 23 => return "structure";
            when 24 => return "standardize";
            when 25 => return "accelerate";
            when 26 => return "deepen";
            when 27 => return "strengthen";
            when 28 => return "enforce";
            when 29 => return "foster";
         end case;
      end Inner;
   begin
      return Build_Plural_Verb (Inner,P);
   end Thing_Verb_Having_Thing_Complement;

   -- (thing) verb (person)

   function Thing_Verb_Having_Person_Complement (P: Plurality) return String is
      function Inner return String is
      begin
         case R13 is
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
            when 11 => return "promote";
            when 12 => return "influence";
            when 13 => return "strengthen";
         end case;
      end Inner;
   begin
      return Build_Plural_Verb (Inner,P);
   end Thing_Verb_Having_Person_Complement;

   function Person_Verb_And_Complement (P: Plurality) return String is
      -- NB: this function produces an eventual complement after the verb, or
      -- no complement at all.
      function Inner return String is
      begin
         case R57 is
            when 1  => return "streamline the process";
            when 2  => return "address the overarching issues";
            when 3  => return "benchmark the portfolio";
            when 4  => return "manage the cycle";     -- Fad of 2004
            when 5  => return "figure out where we come from, where we are going to";
            when 6  => return "maximize the value";
            when 7  => return "execute the strategy"; -- Obsessive in 2006
            when 8  => return "think out of the box";
            when 9  => return "think differently";
            when 10 => return "think across the full value chain";
               -- BBC office-speak phrases
            when 11 => return "loop back";
            when 12 => return "conversate";
            when 13 => return "go forward together";
               --
            when 14 => return "achieve efficiencies";
            when 15 => return "deliver"; -- deliver, form without complement
                                         -- GAC 2010
            when 16 => return "stay in the mix";
            when 17 => return "stay in the zone";
            when 18 => return "evolve";
            when 19 => return "exceed expectations";
            when 20 => return "develop the plan";
            when 21 => return "develop the blue print for execution";
            when 22 => return "grow and diversify";
            when 23 => return "fuel changes";
            when 24 => return "nurture talent";
            when 25 => return "cultivate talent";
            when 26 => return "make it possible";
            when 27 => return "manage the portfolio";
            when 28 => return "align resources";
            when 29 => return "drive the business forward";
            when 30 => return "make things happen";
            when 31 => return "stay ahead";
            when 32 => return "outperform peers";
            when 33 => return "surge ahead";
            when 34 => return "manage the downside";
            when 35 => return "stay in the wings";
            when 36 => return "come to a landing";
            when 37 => return "shoot it over";
            when 38 => return "move the needle";
            when 39 => return "connect the dots";
            when 40 => return "connect the dots to the end game";
            when 41 => return "reset the benchmark";
            when 42 => return "take it offline";
            when 43 => return "peel the onion";
            when 44 => return "drill down";
            when 45 => return "get from here to here";
            when 46 => return "do things differently";
            when 47 => return "stretch the status quo";
            when 48 => return "challenge the status quo";
            when 49 => return "challenge established ideas";
            when 50 => return "increase customer satisfaction";
            when 51 => return "enable customer interaction";
            when 52 => return "manage the balance";
            when 53 => return "turn every stone";
            when 54 => return "drive revenue";
            when 55 => return "rise to the challenge";
            when 56 => return "keep it on the radar";
            when 57 => return "stay on trend";
         end case;
      end Inner;
   begin
      return Build_Plural_Verb (Inner,P);
   end Person_Verb_And_Complement;

   -- Verb + Ending. Ending is a Complement or something else

   function Thing_Verb_And_Ending (P: Plurality) return String is
      Compl_Sp: constant Plurality:= Random_Plural;
   begin
      case R101 is
         when 1 .. 55  =>
            return Thing_Verb_Having_Thing_Complement (P) &
              ' ' &
              Add_Random_Article (Compl_Sp, Thing (Compl_Sp));
         when 56 .. 100 =>
            return Thing_Verb_Having_Person_Complement (P) &
              " the " & Person (Compl_Sp);
         when 101 =>
            return Build_Plural_Verb ("add", P) & " value";
      end case;
   end Thing_Verb_And_Ending;

   function Person_Verb_And_Ending (P: Plurality) return String is
      Compl_Sp: constant Plurality:= Random_Plural;
   begin
      case R95 is
         when  1 .. 10  =>
            return Person_Verb_And_Complement (P);
         when 11 .. 15  => -- Fight-the-Evil situation
            return
              Person_Verb_Having_Bad_Thing_Complement (P) &
              ' ' &
              Add_Random_Article (Plural, Bad_Things);
         when 16 .. 95 =>
            return
              Person_Verb_Having_Thing_Complement (P) &
              ' ' &
              Add_Random_Article (Compl_Sp, Thing (Compl_Sp));
      end case;
   end Person_Verb_And_Ending;

   -- "We need to..." and similar forward-looking constructions
   --
   function Faukon return String is
   begin
      case R6 is
         when 1 => return "we need to";
         when 2 => return "we've got to";
         when 3 => return "the reporting unit should";
         when 4 => return "controlling should";
         when 5 => return "we must activate the " & Matrix_Or_So & " to";
         when 6 => return "pursuing this route will enable us to";
      end case;
   end Faukon;

   function Proposition return String is
      Sp1, Sp2: constant Plurality:= Random_Plural;
   begin
      case R100 is
         when 1 .. 5    => -- "We need to..."
            return
            Faukon & ' ' &
            Eventual_Adverb &
            Person_Verb_And_Ending (Plural) &
            Eventual_Postfixed_Adverb;
            -- infinitive written same as present plural
         when 6 .. 50    => -- ** PERSON...
            return
              "the " & Person (Sp1) & ' ' &
              Eventual_Adverb &
              Person_Verb_And_Ending (Sp1) &
              Eventual_Postfixed_Adverb;
         when 51 .. 92   => -- ** THING...
            return
            Add_Random_Article (Sp1, Thing (Sp1)) & ' ' &
            Eventual_Adverb &
            Thing_Verb_And_Ending (Sp1) &
            Eventual_Postfixed_Adverb;
         when 93..97     => -- ** thing and thing ...
            return -- nb: no article, no adjective
            Thing_Atom (Singular) & " and " &
            Thing_Atom (Singular) & ' ' &
            Eventual_Adverb &
            Thing_Verb_And_Ending (Plural) &
            Eventual_Postfixed_Adverb;
         when 98..100    => -- ** thing, thing and thing ...
            return -- nb: no article, no adjective
            Thing_Atom (Singular) & ", " &
            Thing_Atom (Singular) & " and " &
            Thing_Atom (Singular) & ' ' &
            Eventual_Adverb &
            Thing_Verb_And_Ending (Plural) &
            Eventual_Postfixed_Adverb;
      end case;
   end Proposition;

   function Articulated_Propositions return String is
   begin
      case R28 is
         when 1 .. 17  => return Proposition;
         when 18       => return Proposition & "; this is why " & Proposition;
         when 19       => return Proposition & "; nevertheless " & Proposition;
         when 20       => return Proposition & ", whereas " & Proposition;
         when 21       => return "our gut-feeling is that " & Proposition;
         when 22 .. 25 => return Proposition & ", while " & Proposition;
         when 26       => return Proposition & ". In the same time, " & Proposition;
         when 27       => return Proposition & ". As a result, " & Proposition;
         when 28       => return Proposition & ", whilst " & Proposition;
      end case;
   end Articulated_Propositions;

   function Sentence return String is
      Ap : constant String := Articulated_Propositions;
   begin
      return To_Upper (Ap (Ap'First)) & Ap(Ap'First+1..Ap'Last) & ". ";
   end Sentence;

   function Sentences (Possible_Dialog_Mark: String) return String is
   begin
      case R40 is
         when 1        =>
            return Sentence;
         when 2 .. 30  =>
            return Sentences (Possible_Dialog_Mark) & Sentence;
         when 31 .. 40 =>
            return
               Sentences (Possible_Dialog_Mark) &
               Paragraph_End_Mark & Paragraph_Mark &
               Possible_Dialog_Mark & Sentence;
      end case;
   end Sentences;

   function Sentence_Guaranteed_Amount (Count: Positive; Possible_Dialog_Mark: String) return String is
      Element : constant String:=
        Paragraph_Mark &
        Possible_Dialog_Mark & Sentences (Possible_Dialog_Mark) &
        Paragraph_End_Mark;
   begin
      if Count > 1 then
         return
           Sentence_Guaranteed_Amount (Count - 1, Possible_Dialog_Mark) &
           Element;
      else
         return Element;
      end if;
   end Sentence_Guaranteed_Amount;

   function Workshop return String is
   begin
      return Sentence_Guaranteed_Amount (500, Dialog_Mark);
   end Workshop;

   function Short_Workshop return String is
   begin
      return Sentence_Guaranteed_Amount (5, Dialog_Mark);
   end Short_Workshop;

   function Financial_Report return String is
   begin
      return Sentence_Guaranteed_Amount (1, "");
      -- !! charts (especially, pie charts) !!
   end Financial_Report;

end Corporate_Bullshit;
