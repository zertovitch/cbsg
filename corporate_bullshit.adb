-------------------------------------------------------------------------
--  The Corporate Bullshit Generator (CBSG)
--  See specification for sources, authors, contributors, license.
-------------------------------------------------------------------------
--  NB: this package happens to apply the functional programming paradigm.
--  http://en.wikipedia.org/wiki/Functional_programming

with Ada.Characters.Handling;
with Delirium;

package body Corporate_Bullshit is

   --  To do:
   --   * Enrich the Proposition function
   --   * Person_adjective: "committed", "multi-skilled", "inspirational",
   --                       "high-caliber", "cross-trained", "multi-cultural",
   --                       "fully-fledged", "performance-driven",
   --                       "execution-focused"
   --   * other sentences; rhetorical questions
   --   * Slogans, such as: "It's about breaking down the silos"
   --     "less is more", "it's about quality, not about quantity",
   --     "Do one thing. Do it right."
   --   * Fix bugs marked with !!
   --   * "integrate into"
   --   * Bullshit ratio (Emilio)
   --   * mix with specific vocabulary (combined with previous:
   --       blending of (bullshit, normal, custom); type in Delirium)
   --   * ...a [s-whatever] that puts [s-whatever] at the center
   --       of the organization's [whatever]
   --   * "...is top-of-mind", "...is key", "...is critical"
   --   * "How to win?"

   use Delirium;

   -- Persons or groups --

   function Boss return String is

      function Managing return String is
      (case R8 is
          when 1      => "Managing ",  --  The others only pretend to manage.
          when 2      => "Acting ",    --  We could have an actor too...
          when 3      => "General ",
          when others => "");

      function Title return String is

         function Vice return String is
         (case R40 is
             when 1 .. 10 => "Vice ",
             when 11      => "Corporate Vice ",
             when 12      => "Deputy ",
             when others  => "");

         function Co return String is
         (case R5 is
             when 1      => "Co-",
             when others => "");

      begin
         return
            (case R6 is
                when 1 => Vice & Co & "Director",
                when 2 =>        Co & "Chief",
                when 3 =>        Co & "Head",
                when 4 => Vice & Co & "President",
                when 5 =>             "Supervisor",
                when 6 =>        Co & "Manager");
      end Title;

      function Age return String is
      (case R4 is
          when 1      => "Senior ",
          when others => "");

      function Exec return String is
      (case R12 is
          when 1      => "Executive ",
          when 2      => "Principal ",
          when others => "");

      function Groupal return String is
      (case R20 is
          when 1      => "Group ",
          when 2      => "Global ",
          when 3      => "Enterprise ",
          when others => "");

      function Department return String is
      (case R52 is
          when 1  => "Human Resources",
          when 2  => "Controlling",
          when 3  => "Internal Audit",
          when 4  => "Legal",
          when 5  => "Operations",
          when 6  => "Management Office",
          when 7  => "Customer Relations",
          when 8  => "Client Leadership",
          when 9  => "Client Relationship",
          when 10 => "Business Planning",
          when 11 => "Business Operations",
          when 12 => "IT Strategy",
          when 13 => "IT Operations",
          when 14 => "Marketing",
          when 15 => "Strategic Planning",      -- from a Dilbert cartoon
          when 16 => "Facilities Management",   -- from a Dilbert cartoon
          when 17 => "Innovation",
          when 18 => "Identity",
          when 19 => "Branding",
          when 20 => "Diversity and Inclusion",
          when 21 => "Media Relations",
          when 22 => "Value Added Services",
          when 23 => "Technology",
          when 24 => "Campaigning",
          when 25 => "Digital Marketing",
          when 26 => "Digital Transformation Office",
          when 27 => "Communications",
          when 28 => "Architecture",
          when 29 => "Data & Analytics",
          when 30 => "Compliance",
          when 31 => "Research & Development",
          when 32 => "Networking Enhancement",     --  "Bullshit jobs" article, Les Echos
          when 33 => "Innovative Strategies",      --  "Bullshit jobs" article, Les Echos
          when 34 => "Global Innovation Insight",  --  "Bullshit jobs" article, Les Echos
          when 35 => "Transition Transformation",  --  "Bullshit jobs" article, Les Echos
          when 36 => "Change Management",          --  "Bullshit jobs" article, Les Echos
          when 37 => "Global Strategy",            --  "Bullshit jobs" article, Les Echos
          when 38 => "Creativity and Innovation",  --  "Bullshit jobs" article, Les Echos
          when 39 => "Information Security",  --  http://yetanotherico.com
          when 40 => "Corporate Planning",
          when 41 => "Customer Experience", -- (c) Maria Marino, 2018
          when 42 => "Growth Initiatives",
          when 43 => "Finance",
          when 44 => "AI Strategy",
          when 45 => "Business Agility",  --  https://dilbert.com/strip/2019-10-28
          --  From an article in https://insideparadeplatz.ch , 2023-01-11 :
          when 46 => "Digital Client Lifecyle",
          when 47 => "Transformation Office",
          when 48 => "Project Portfolio Management",
          when 49 => "Organizational Change Management",
          when 50 => "Business Excellence",
          when 51 => "Digital Excellence",
          when 52 => "Data Governance");

      --  This function includes fictitious departments under the broader
      --  denomination "role". In this inclusive setup, the Chief is expected
      --  to be an actor and actual work is strictly forbidden.
      --
      function Department_or_Top_Role return String is
      (case R68 is
          when  1 => "Visionary",
          when  2 => "Digital",
          when  3 => "Technical",
          when  4 => "Manifesto",  --  Guffpedia
          when  5 => "Operating",
          when  6 => "Product",
          when  7 => "Scheme",
          when  8 => "Growth",
          when  9 => "Brand",                 --  http://yetanotherico.com
          when 10 => "Sales",                 --  http://yetanotherico.com
          when 11 => "Networking",            --  http://yetanotherico.com
          when 12 => "Content",               --  http://yetanotherico.com
          when 13 => "Holacracy",        --  "Bullshit jobs" article, Les Echos
          when 14 => "Data Protection",  --  GDPR mania, 2018
          when 15 => "Data Privacy",     --  GDPR again (2024)
          when 16 => "Risk Appetite",    --  Job offering in a bank, 2018
          --  Bloomberg 2018-10-29:
          --  Snapchat CEO Named Chief Business Officer, Then Changed His Mind
          when 17 => "Business",
          when 18 => "People",
          when 19 => "Disruption",  --  Chief Disruption Officer (Business Bullshit, brand eins 1/2024)
          when others =>
             Department);

      function Officer_or_Catalyst return String is
      (case R20 is
          when 1 => "Catalyst",  --  Guffpedia
          when 2 => "Futurist",  --  Tech bubble 2.0
          when 3 => "Strategist",
          when 4 => "Technologist",
          when 5 => "Evangelist",
          when 6 => "Solutionist",  --  https://dilbert.com/strip/2019-10-28
          when 7 => "Influencer",   --  https://dilbert.com/strip/2019-10-28
          when others =>
             "Officer");

   begin
      case R2 is
         when 1 =>      -- A fully normal boss (eventually, a managing one)
            return Managing & Age & Exec & Title & " of " & Department;
         when others => -- Chief X Officer
            return
               Groupal &
               Abbreviate ("Chief " & Department_or_Top_Role & ' ' & Officer_or_Catalyst, 0.6);
      end case;
   end Boss;

   function Thing_Atom (P : Plurality) return String;

   function Person (P : Plurality) return String is
   (case P is
       when Singular =>
          (case R48 is
              when 1  => "steering committee",
              when 2  => "group",
              when 3  => "project manager",
              when 4  => Thing_Atom (Random_Plural) & " champion",
              when 5  => "community",
              when 6  => "sales manager",
              when 7  => "enabler", -- also thing
              when 8  => "powerful champion",
              when 9  => "thought leader", -- Thanks Andy!
              when 10 => "gatekeeper", -- ME - UWM 2010
              when 11 => "resource",   -- no, we're not people, we're "resources"
              when 12 => "senior support staff",
              when 13 => "brand manager",        -- from a Dilbert cartoon
              when 14 => "category manager",     -- from a Dilbert cartoon
              when 15 => "account executive",    -- from a Dilbert cartoon
              when 16 => "project leader",       -- from a Dilbert cartoon
              when 17 => "product manager",      -- from a Dilbert cartoon
              when 18 => "naming committee",
              when 19 => "executive committee",
              when 20 => "management committee",
              when 21 => "innovator",      --  (obtained by bootstrapping)
              when 22 => "game changer",
              when 23 => "visionary",
              when 24 => "market thinker",
              when 25 => "network",
              when 26 => "initiator",
              when 27 => "change agent",
              when 28 => "rockstar",
              when 29 => "facilitator",
              when 30 => "disruptor",
              when 31 => "challenger",
              when 32 => "six-sigma black belt",
              when 33 => "white-collar workforce",
              when others =>  --  ~ 1/3
                 Boss),
         when Plural =>
            (case R34 is
                when 1  => "key people",
                when 2  => "human resources",
                when 3  => "customers",
                when 4  => "clients",
                when 5  => "resources",
                when 6  => "team players",
                when 7  => "enablers", -- also a thing
                when 8  => "stakeholders",
                when 9  => "standard-setters",
                when 10 => "partners",
                when 11 => "business leaders",
                when 12 => "thinkers/planners",
                when 13 => "white-collar workers",
                when 14 => "board-level executives",  --  (obtained by bootstrapping)
                when 15 => "key representatives",     --  (obtained by bootstrapping)
                when 16 => "innovators",     --  (obtained by bootstrapping)
                when 17 => "policy makers",  --  (obtained by bootstrapping)
                when 18 => "pioneers",
                when 19 => "game changers",
                when 20 => "market thinkers",
                when 21 => "thought leaders",
                when 22 => "mediators",
                when 23 => "facilitators",
                when 24 => "attackers",
                when 25 => "initiators",
                when 26 => "decision makers",
                when 27 => "Growth Hackers",
                when 28 => "Digital Marketers",
                when 29 => "Creative Technologists",
                when 30 => "Products Managers",
                when 31 => "Products Owners",
                when 32 => "disruptors",
                when 33 => "challengers",
                when 34 => "growers"));  --  1996 article about Enron and others

   function Matrix_Or_So return String is
   (case R13 is
       when 1 .. 2  => "organization",  --  A bit flat, but flashy combined with "within the "
       when 3 .. 6  => "silo",          --  Classic 1-dimension units in organizations
       when 7 .. 10 => "matrix",        --  2nd dimension, with dotted lines
       when 11      => "cube",          --  3rd dimension (Converium); at last then, the company
                                        --    has become totally dysfunctional)
       when 12      => "sphere",        --  Another esoteric 3-dimensional structure - ME 20-Jun-2011
       when 13      => "pyramid");      --  With a benevolent dictator for life at the top, of course.
                                        --    Also a Thing.

   -- Things --

   function Thing_Adjective return String is
   (case R501 is
       when 1  => "efficient",
       when 2  => "strategic",
       when 3  => "constructive",
       when 4  => "proactive",
       when 5  => "strong",
       when 6  => "key",
       when 7  => "global",
       when 8  => "corporate",
       when 9  => "cost-effective",
       when 10 => "focused",
       when 11 => "top-line",
       when 12 => "credible",
       when 13 => "agile",
       when 14 => "holistic",
       when 15 => "new",  --  something neutral to add credibility ,-)
       when 16 => "adaptive",
       when 17 => "optimal",
       when 18 => "unique",
       when 19 => "core",
       when 20 => "compliant",
       when 21 => "goal-oriented",
       when 22 => "non-linear",
       when 23 => "problem-solving",
       when 24 => "prioritizing",
       when 25 => "cultural",
       when 26 => "future-oriented",
       when 27 => "potential",
       when 28 => "versatile",
       when 29 => "leading",
       when 30 => "dynamic",
       when 31 => "progressive",
       when 32 => "non-deterministic",  --  NF 15-May-2008
                                        --  Added 21-Nov-2008:
       when 33 => "informed",
       when 34 => "leveraged",
       when 35 => "challenging",
       when 36 => "intelligent",
       when 37 => "controlled",
       when 38 => "educated",
       when 39 => "non-standard",
       when 40 => "underlying",
       when 41 => "centralized",
       when 42 => "decentralized",
       when 43 => "reliable",
       when 44 => "consistent",
       when 45 => "competent",
       when 46 => "prospective",
          --
       when 47 => "collateral",
          --  the pyramid-cube 2004, added 2009:
       when 48 => "functional",
          --
       when 49 => "tolerably expensive",
       when 50 => "organic",
       when 51 => "forward-looking",
       when 52 => "next-level",
       when 53 => "executive",
       when 54 => "seamless",
       when 55 => "spectral",
       when 56 => "balanced",
       when 57 => "effective",
          --  Buzz Phrase Generator.xls (Kurt)
       when 58 => "integrated",
       when 59 => "systematized",
       when 60 => "parallel",
       when 61 => "responsive",
       when 62 => "synchronized",
       when 63 => "carefully-designed",
       when 64 => "carefully thought-out",
          --  BBC office-speak phrases
       when 65 => "cascading",
       when 66 => "high-level",
       when 67 => "siloed",
          --
       when 68 => "operational",
       when 69 => "future-ready",
          --  The Blending
       when 70 => "flexible",
       when 71 => "movable",
       when 72 => "right",
       when 73 => "productive",
       when 74 => "evolutionary",
       when 75 => "overarching",
       when 76 => "documented",
       when 77 => "awesome", -- ME
                             --  UW Presentation Nov 2010
       when 78 => "coordinated",
       when 79 => "aligned",
       when 80 => "enhanced", -- ME 15-Jun-2011
       when 81 => "control-based",

          --  Ludovic:
       when 82 => "industry-standard",
       when 83 => "accepted",
       when 84 => "agreed-upon",
       when 85 => "target",
       when 86 => "customer-centric",
       when 87 => "wide-spectrum",
       --
       when 88 => "well-communicated",
       --  PDM, July 2011
       when 89 => "cutting-edge",
       when 90 => "state-of-the-art",
       when 91 => "verifiable",
       --
       when 92  => "six-sigma",
       when 93  => "solid",
       when 94  => "inspiring",
       when 95  => "growing",
       when 96  => "market-altering",
       when 97  => "vertical",
       when 98  => "emerging",
       when 99  => "differentiating",
       when 100 => "integrative",
       when 101 => "cross-functional",
       when 102 => "measurable",
       when 103 => "well-planned",
       when 104 => "accessible",
       when 105 => "actionable",
       when 106 => "accurate",
       when 107 => "insightful",
       when 108 => "relevant",
       when 109 => "long-term",
       when 110 => "longer-term",
       when 111 => "tactical",
       when 112 => "best-of-breed",
       when 113 => "robust",
       when 114 => "targeted",
       when 115 => "personalized",
       when 116 => "interactive",
       when 117 => "streamlined",
       when 118 => "transparent",
       when 119 => "traceable",
       when 120 => "far-reaching",
       when 121 => "powerful",
       when 122 => "improved",
       when 123 => "executive-level",
       when 124 => "goal-based",
       when 125 => "top-level",
       when 126 => "value-added",
       when 127 => "value-adding",
       when 128 => "streamlining",
       when 129 => "time-honored", -- PDM
       when 130 => "idiosyncratic", -- ED
       when 131 => "sustainable",
       when 132 => "in-depth",
       when 133 => "immersive",
       when 134 => "cross-industry",
       when 135 => "time-phased",
       when 136 => "day-to-day",
       when 137 => "present-day",
       when 138 => "modern-day",
       when 139 => "profit-maximizing",
       when 140 => "generic",
       when 141 => "granular",
       when 142 => "values-based",
       when 143 => "value-driven",
       when 144 => "well-defined",
       when 145 => "outward-looking",
       when 146 => "scalable",
       when 147 => "strategy-focused",
       when 148 => "promising",
       when 149 => "collaborative",
       when 150 => "scenario-based",
       when 151 => "principle-based",
       when 152 => "vision-setting",
       when 153 => "client-oriented",
       when 154 => "long-established",
       when 155 => "established",
       when 156 => "organizational",
       when 157 => "visionary",
       when 158 => "trusted",
       when 159 => "full-scale",
       when 160 => "firm-wide",
       when 161 => "fast-growth",
       when 162 => "performance-based",
       when 163 => "high-performing",
       when 164 => "high-performance",
       when 165 => "cross-enterprise",
       when 166 => "outsourced", -- BBC - LGA banned words
       when 167 => "situational", -- BBC - LGA banned words
       when 168 => "bottom-up", -- BBC - LGA banned words
       when 169 => "multidisciplinary", -- BBC - LGA banned words
       when 170 => "one-to-one",
       when 171 => "goal-directed",
       when 172 => "intra-organisational",
       when 173 => "data-inspired",
       when 174 => "multi-source",
       when 175 => "360-degree", -- !! <-> 360-degree thinking
       when 176 => "motivational",
       when 177 => "differentiated",
       when 178 => "solutions-based",
       when 179 => "compelling",
       when 180 => "structural",
       when 181 => "go-to-market",
       when 182 => "on-message",
       when 183 => "productivity-enhancing",
       when 184 => "value-enhancing",
       when 185 => "mission-critical",
       when 186 => "business-enabling",
       when 187 => "transitional",
       when 188 => "future",
       when 189 => "game-changing",
       when 190 => "enterprise-wide",
       when 191 => "rock-solid",
       when 192 => "bullet-proof",
       when 193 => "superior",
       when 194 => "genuine",
       when 195 => "alert",
       when 196 => "nimble",
       when 197 => "phased",
       when 198 => "selective",
       when 199 => "macroscopic",
       when 200 => "low-risk high-yield",
       when 201 => "interconnected",
       when 202 => "high-margin",
       when 203 => "resilient",
       when 204 => "high-definition",
       when 205 => "well-crafted",
       when 206 => "fine-grained",
       when 207 => "context-aware",
       when 208 => "multi-tasked",
       when 209 => "feedback-based",
       when 210 => "analytics-based",
       when 211 => "fact-based",
       when 212 => "usage-based",
       when 213 => "multi-channel",
       when 214 => "omni-channel",
       when 215 => "cross-channel",
       when 216 => "specific",
       when 217 => "heart-of-the-business",
       when 218 => "responsible",
       when 219 => "socially conscious",
       when 220 => "results-centric",
       when 221 => "business-led",
       when 222 => "well-positioned",
       when 223 => "end-to-end",
       when 224 => "high-quality",
       when 225 => "siloed",
       when 226 => "modular",
       when 227 => "service-oriented",
       when 228 => "competitive",
       when 229 => "scale-as-you-grow",
       when 230 => "outside-in",
       when 231 => "hyper-hybrid",
       when 232 => "long-running",
       when 233 => "large-scale",
       when 234 => "wide-ranging",
       when 235 => "wide-range",
       when 236 => "stellar",
       when 237 => "dramatic",
       when 238 => "aggressive",
       when 239 => "innovative",
       when 240 => "high-powered",
       when 241 => "above-average",
       when 242 => "result-driven",
       when 243 => "innovation-driven",
       when 244 => "customized",
       when 245 => "outstanding",
       when 246 => "non-mainstream",
       when 247 => "customer-facing",
       when 248 => "consumer-facing",
       when 249 => "unified",
       when 250 => "cooperative",
       when 251 => "laser-focused",  --  From a top diplomate
       when 252 => "well-implemented",
       when 253 => "diversifying",
       when 254 => "market-changing",
       when 255 => "metrics-driven",  --  Found in an article about criminal activities.
       when 256 => "pre-integrated",  --  (obtained by bootstrapping)
       when 257 => "solution-oriented",
       when 258 => "impactful",
       when 259 => "world-class",
       when 260 => "front-end",
       when 261 => "leading-edge",
       when 262 => "cost-competitive",
       when 263 => "extensible",
       when 264 => "under-the-radar",
       when 265 => "high-grade",  --  Subprime crisis explanation by The Long Johns ...
       when 266 => "structured",  --  ... http://www.youtube.com/watch?v=z-oIMJMGd1Q
       when 267 => "trust-based",
       when 268 => "intra-company",
       when 269 => "inter-company",
       when 270 => "profit-oriented",
       when 271 => "sizeable",
       when 272 => "highly satisfactory",
       when 273 => "bi-face",
       when 274 => "tri-face",
       when 275 => "disruptive",
       when 276 => "technological",
       when 277 => "marketplace",  --  Noun used as an adjective
       when 278 => "fast-evolving",
       when 279 => "open",
       when 280 => "fully networked",
       when 281 => "adoptable",
       when 282 => "trustworthy",
       when 283 => "science-based",
       when 284 => "non-manufacturing",
       when 285 => "multi-divisional",
       when 286 => "controllable",
       when 287 => "high-priority",
       when 288 => "market-driven",
       when 289 => "market-driving",
       when 290 => "ingenious",              --  (obtained by bootstrapping)
       when 291 => "business-for-business",  --  (obtained by bootstrapping)
       when 292 => "inspirational",          --  (obtained by bootstrapping)
       when 293 => "winning",           --  (obtained by bootstrapping)
       when 294 => "boundaryless",      --  (obtained by bootstrapping)
       when 295 => "reality-based",     --  (obtained by bootstrapping)
       when 296 => "customer-focused",  --  (obtained by bootstrapping)
       when 297 => "preemptive",  --  (obtained by bootstrapping)
       when 298 => "location-specific",    --  (obtained by bootstrapping)
       when 299 => "revealing",            --  (obtained by bootstrapping)
       when 300 => "inventory-planning",   --  (obtained by bootstrapping)
       when 301 => "ubiquitous",  --  (obtained by bootstrapping)
       when 302 => "number-one",  --  (obtained by bootstrapping)
       when 303 => "results-oriented",  --  (obtained by bootstrapping)
       when 304 => "socially enabled",  --  (obtained by bootstrapping)
       when 305 => "well-scoped",    --  (obtained by bootstrapping)
       when 306 => "insight-based",  --  (obtained by bootstrapping)
       when 307 => "high-impact",        --  (obtained by bootstrapping)
       when 308 => "technology-driven",  --  (obtained by bootstrapping)
       when 309 => "knowledge-based",      --  (obtained by bootstrapping)
       when 310 => "information-age",      --  (obtained by bootstrapping)
       when 311 => "technology-centered",  --  (obtained by bootstrapping)
       when 312 => "critical",      --  (obtained by bootstrapping)
       when 313 => "cognitive",     --  (obtained by bootstrapping)
       when 314 => "acculturated",  --  (obtained by bootstrapping)
       when 315 => "client-centric",  --  2012 Golden Flannel Awards article
       when 316 => "comprehensive",   --  2011 Golden Flannel Awards article
       when 317 => "ground-breaking",
       when 318 => "long-standing",
       when 319 => "accelerating",
       when 320 => "forward-thinking",
       when 321 => "mind-blowing",  --  DM
       when 322 => "jaw-dropping",  --  DM
       when 323 => "transformative",
       when 324 => "better-than-planned",
       when 325 => "vital",
       when 326 => "radical",
       when 327 => "expanding",
       when 328 => "fierce",
       when 329 => "single-minded",
       when 330 => "mindful",
       when 331 => "top-down",
       when 332 => "hands-on",
       when 333 => "one-on-one",
       when 334 => "analytic",
       when 335 => "top",
       when 336 => "elite",
       when 337 => "dedicated",
       when 338 => "curated",
       when 339 => "highly-curated",
       when 340 => "re-imagined",
       when 341 => "thought-provoking",
       when 342 => "quality-oriented",
       when 343 => "task-oriented",
       when 344 => "teamwork-oriented",
       when 345 => "high-growth",
       when 346 => "next-gen",
       when 347 => "next-generation",
       when 348 => "new-generation",
       when 349 => "best-in-class",
       when 350 => "best-of-class",
       when 351 => "first-class",
       when 352 => "top-class",
       when 353 => "superior-quality",
       when 354 => "synergistic",
       when 355 => "micro-macro",
       when 356 => "organization-wide",
       when 357 => "clear-cut",
       when 358 => "data-driven",
       when 359 => "evidence-based",
       when 360 => "transformational",
       when 361 => "fast-paced",
       when 362 => "real-time",
       when 363 => "pre-approved",
       when 364 => "unconventional",
       when 365 => "advanced-analytics",
       when 366 => "insight-driven",
       when 367 => "sprint-based",
       when 368 => "digitized",
       when 369 => "hypothesis-driven",
       when 370 => "governance-related",
       when 371 => "convergent",
       when 372 => "leadership-defined",
       when 373 => "operations-oriented",
       when 374 => "long-range",
       when 375 => "dimensional",
       when 376 => "award-winning",
       when 377 => "user-centric",
       when 378 => "first-to-market",
       when 379 => "first-mover",
       --  Next one is from http://dilbert.com/strip/2017-04-10: Asok Is In The Jargon Matrix:
       when 380 => "cross-platform",
       when 381 => "on-the-go",
       when 382 => "all-encompassing",
       when 383 => "matrixed",
       when 384 => "growth-enabling",
       when 385 => "skills-based",
       when 386 => "bottom-line",
       when 387 => "top-shelf",
       when 388 => "insourced",
       when 389 => "out-of-the-box",
       when 390 => "engaging",
       when 391 => "on- and offline",
       when 392 => "goals-based",
       when 393 => "enriching",
       when 394 => "medium-to-long-term",
       when 395 => "adequate",
       when 396 => "awareness-raising",
       when 397 => "compatible",
       when 398 => "supportive",
       when 399 => "inspired",
       when 400 => "high-return",
       when 401 => "turn-key",
       when 402 => "turnkey",
       when 403 => "decision-ready",
       when 404 => "diversified",
       when 405 => "demanding",          --  (bootstrapped)
       when 406 => "ambitious",          --  (bootstrapped)
       when 407 => "domain-relevant",    --  (bootstrapped)
       when 408 => "novel",              --  (bootstrapped)
       when 409 => "pre-planned",        --  (bootstrapped)
       when 410 => "well-respected",
       when 411 => "market-based",
       when 412 => "distributor-based",
       when 413 => "area-wide",
       when 414 => "movements-based",
       when 415 => "ever-changing",
       when 416 => "purpose-driven",
       when 417 => "resourceful",
       when 418 => "real-life",
       when 419 => "vibrant",
       when 420 => "bright",
       when 421 => "pure-play",
       when 422 => "bespoke",
       when 423 => "pivotal",
       when 424 => "efficiency-enhancing",
       when 425 => "multi-level",
       when 426 => "rich",
       when 427 => "frictionless",
       when 428 => "up-to-the-minute",
       when 429 => "sourced",
       when 430 => "outcome-driven",
       when 431 => "hyperaware",
       when 432 => "high-velocity",
       when 433 => "lean",
       when 434 => "unmatched",
       when 435 => "industry-leading",
       when 436 => "multi-sided",
       when 437 => "tailor-made",
       when 438 => "contingent",
       when 439 => "tangent",
       when 440 => "moment-centric",
       when 441 => "real-world",
       when 442 => "inclusive",
       when 443 => "efficiency-enabling",
       when 444 => "value-creating",
       when 445 => "alternative",
       when 446 => "fit-for-purpose",
       when 447 => "fast-changing",
       when 448 => "onboarded",
       when 449 => "active",
       when 450 => "container packaged",
       when 451 => "dynamically managed",
       when 452 => "microservices-oriented",
       when 453 => "higher-quality",
       when 454 => "brute-force",
       when 455 => "enterprise-sales-driven",
       when 456 => "developer-led",
       when 457 => "fast-track",
       when 458 => "highly differentiated",
       when 459 => "quick-to-deploy",
       when 460 => "efficiency-focused",
       when 461 => "as-a-service",
       when 462 => "cloud-based",
       when 463 => "activity-centric",
       when 464 => "data-centric",
       when 465 => "activity-focused",
       when 466 => "data-focused",
       when 467 => "workforce-focused",
       when 468 => "organization-focused",
       when 469 => "spot-on",       --  FOSDEM 2019
       when 470 => "distributed",   --  FOSDEM 2019, with regards to Fred Praca.
       when 471 => "deterministic", --  but "non-deterministic" is just as good, see above
       when 472 => "converged",
       when 473 => "on-premise",
       when 474 => "company-first",
       when 475 => "multi-vendor",
       when 476 => "contextual",
       when 477 => "hybrid",
       when 478 => "higher-level",  --  High-level is not enough.
       when 479 => "user-driven",
       when 480 => "full-stack",    --  E.g.: full-stack startup.
       when 481 => "build-as-you-go",
       when 482 => "fully-digital",
       when 483 => "agent-based",
       when 484 => "socio-economic",
       when 485 => "managerial",
       when 486 => "industry-recognized",
       when 487 => "top-ranking",
       when 488 => "empowering",
       when 489 => "courage-building",
       when 490 => "AI-assisted",
       when 491 => "AI-first",
       when 492 => "AI-ready",
       when 493 => "distinctive",  --  1996 article about Enron and others
       when 494 => "manageable",   --  1996 article about Enron and others
       when 495 => "bifocal",      --  1996 article about Enron and others
       when 496 => "cross-sector",
       when 497 => "interdisciplinary",
       when 498 => "data-dependent",
       when 499 => "gradual",
       when 500 => "industry-first",  --  https://www.weforum.org/organizations/ftx
       when 501 => "multi-class");

   function Timeless_Event return String is
   (case R4 is
       when 1 => "kick-off",
       when 2 => "roll-out",
       when 3 => "client event",
       when 4 => "quarterly results");

   function Growth_Atom return String is
   (case R19 is
       when 1  => "growth",
       when 2  => "improvement",
       when 3  => "throughput increase",
       when 4  => "efficiency gain",
       when 5  => "yield enhancement",
       when 6  => "expansion",
       when 7  => "productivity improvement",
       when 8  => "gain in task efficiency",
       when 9  => "shift in value",  --  (obtained by bootstrapping)
       when 10 => "cost reduction scaling",
       when 11 => "cost reduction",
       when 12 => "cost effectiveness",
       when 13 => "level of change",
       when 14 => "revenue growth",
       when 15 => "profits growth",   --  Ironical, from D. Stockman:
                                      --    https://dailyreckoning.com/hurricane-bearing-casino/
       when 16 => "growth momentum",  --  (bootstrapped)
       when 17 => "increase in sales",
       when 18 => "run-rate efficiency",  --  Convoluted form of "cost cutting"
       when 19 => "increase in margins");

   function Growth return String is

      function Superlative return String is
      (case R35 is
          when 1 => "organic",
          when 2 => "double-digit",
          when 3 => "upper single-digit",
          when 4 => "breakout",
             --  Ludovic
          when 5 => "unprecedented",
          when 6 => "unparalleled",
          when 7 => "proven",
          when 8 => "measured",
             --  2014 stock exchange fads
             --    Some adjectives are also in the common list
          when  9 => "sustained",
          when 10 => "sustainable",
          when 11 => "robust",
          when 12 => "solid",
          when 13 => "rock-solid",
          when 14 => "healthy",
          when 15 => "incremental",
          when 16 => "significant",
          when 17 => "recurring",
          when 18 => "sizeable",  --  (obtained by bootstrapping)
          when 19 => "rapid",     --  (obtained by bootstrapping)
          when 20 => "breakneck",    --  (obtained by bootstrapping) - usual pre-crash adjective
          when 21 => "profitable",   --  (obtained by bootstrapping) - usual post-crash adjective
          when 22 => "disciplined",  --  (obtained by bootstrapping)
          when 23 => "accelerated",  --  (obtained by bootstrapping)
          when 24 => "impressive",
          when 25 => "superior",  --  opposite of "subpar"
          when 26 => "attractive-enough",
          when 27 => "continual",
          when 28 => "above-potential",
          when 29 => "better-than-average",
          when 30 => "exponential",  --  Search: "Use of the phrase 'exponential growth' by decade"
          when 31 => "long-term",    --  Used when the growth is not quite happening now...
          when 32 => "future",       --  Used when the growth is not quite happening now...
          when 33 => "step-function",
          when 34 => "outsized",
          when 35 => "step-change");
   begin
      return Superlative & ' ' & Growth_Atom;
   end Growth;

   function Thing_Atom (P : Plurality) return String is

      function Inner return String is  --  Items that can be made plural.
      (case R274 is
          when 1   => "mission",
          when 2   => "vision",
          when 3   => "guideline",
          when 4   => "roadmap",
          when 5   => "timeline",
          when 6   => Matrix_Or_So,
          when 7   => "win-win solution",
          when 8   => "baseline starting point",
          when 9   => "sign-off",
          when 10  => "escalation",
          when 12  => "system",
          when 13  => Abbreviate ("Management Information System", 0.5),
          when 14  => Abbreviate ("Quality Management System", 0.5),
          when 15  => "planning",
          when 16  => "target",
          when 17  => "calibration",
          when 18  => Abbreviate ("Control Information System", 0.5),
          when 19  => "process",
          when 20  => "talent",
          when 21  => "execution", -- Winner 2006!
          when 22  => "leadership",
          when 23  => "performance",
          when 24  => "solution provider",
          when 25  => "value",
          when 26  => "value creation",
          when 27  => "value realization",
          when 28  => "document",
          when 29  => "bottom line",
          when 30  => "momentum",
          when 31  => "opportunity",
          when 32  => "credibility",
          when 33  => "issue",
          when 34  => "core meeting",
          when 35  => "platform",
          when 36  => "niche",
          when 37  => "content",
          when 38  => "communication",
          when 39  => "goal",
          when 40  => "value creation goal",
          when 41  => "alternative",
          when 42  => "culture",
          when 43  => "requirement",
          when 44  => "potential",
          when 45  => "challenge",
          when 46  => "empowerment",
          when 47  => "benchmarking",
          when 48  => "framework",
          when 49  => "benchmark",
          when 50  => "implication",
          when 51  => "integration",
          when 52  => "enabler", -- also person
          when 53  => "control",
          when 54  => "trend",
             --  the pyramid-cube 2004, added 2009:
          when 55  => "business case",
          when 56  => "architecture",
          when 57  => "action plan",
          when 58  => "project",
          when 59  => "review cycle",
          when 11  => "trigger event",
          when 60  => "strategy formulation",
          when 61  => "decision",
          when 62  => "enhanced data capture",
          when 63  => "energy",
          when 64  => "plan",
          when 65  => "initiative",
          when 66  => "priority",
          when 67  => "synergy",
          when 68  => "incentive",
          when 69  => "dialogue",
             --  Buzz Phrase Generator.xls (Kurt)
          when 70  => "concept",
          when 71  => "time-phase",
          when 72  => "projection",
             --  Merger buzz 2009:
          when 73  => "blended approach",
             --  BBC office-speak phrases
          when 74  => "low hanging fruit",
          when 75  => "forward planning",
          when 76  => "pre-plan", -- also a verb
          when 77  => "pipeline",
          when 78  => "bandwidth",
          when 79  => "brand image",
          when 80  => "paradigm",
          when 81  => "paradigm shift",
          when 82  => "strategic staircase",
             --
          when 83  => "cornerstone",
          when 84  => "executive talent",
          when 85  => "evolution",
          when 86  => "workflow",
          when 87  => "message",
             --  GAC 2010
          when 88  => "risk/return profile",
          when 89  => "efficient frontier",
          when 90  => "pillar",
             --  Andy
          when 91  => "internal client",
          when 92  => "consistency",
             --  Ludovic
          when 93  => "on-boarding process",
             --
          when 94  => "dotted line",
          when 95  => "action item",
          when 96  => "cost efficiency",
          when 97  => "channel",
          when 98  => "convergence",
          when 99  => "infrastructure",
          when 100 => "metric",
          when 101 => "technology",
          when 102 => "relationship",
          when 103 => "partnership",
          when 104 => "supply-chain",
          when 105 => "portal",
          when 106 => "solution",
          when 107 => "business line",
          when 108 => "white paper",
          when 109 => "scalability",
          when 110 => "innovation",
          when 111 => Abbreviate ("Strategic Management System", 0.5),
          when 112 => "Balanced Scorecard",
          when 113 => "key differentiator", -- PDM
          when 114 => "competitive differentiator",  --  Variant of key differentiator
          when 115 => "idiosyncrasy", -- ED
          when 116 => "benefit",
          when 117 => "say/do ratio",
          when 118 => "segmentation",
          when 119 => "image",
          when 120 => "business model",
          when 121 => Abbreviate ("Business Model Innovation", 1.0),
          when 122 => "business philosophy",
          when 123 => "business platform",
          when 124 => "methodology",
          when 125 => "profile",
          when 126 => "measure",
          when 127 => "measurement",
          when 128 => "philosophy",
          when 129 => "branding strategy",
          when 130 => "efficiency",
          when 131 => "industry",
          when 132 => "commitment",
          when 133 => "perspective",
          when 134 => "risk appetite",
          when 135 => "best practice",
          when 136 => "brand identity",
          when 137 => "customer centricity", -- Mili
          when 138 => "shareholder value", -- Andrew
          when 139 => "attitude",
          when 140 => "mindset",
          when 141 => "flexibility",
          when 142 => "granularity",
          when 143 => "engagement",
          when 144 => "pyramid",
          when 145 => "market",
          when 146 => "diversity",
          when 147 => "interdependency",
          when 148 => "scaling",
          when 149 => "asset",
          when 150 => "flow charting",
          when 151 => "value proposition",
          when 152 => "performance culture",
          when 153 => "change",
          when 154 => "reward",
          when 155 => "learning",
          when 156 => "next step",
          when 157 => "delivery framework",
          when 158 => "structure",
          when 159 => "support structure",
          when 160 => "standardization",
          when 161 => "objective",
          when 162 => "footprint",
          when 163 => "transformation process",
          when 164 => "policy",
          when 165 => "sales target",
          when 166 => "ecosystem",
          when 167 => "landscape",
          when 168 => "atmosphere",
          when 169 => "environment",
          when 170 => "core competency",
          when 171 => "market practice",
          when 172 => "operating strategy",
          when 173 => "insight",
          when 174 => "accomplishment",
          when 175 => "correlation",
          when 176 => "touch point",
          when 177 => "knowledge transfer",
          when 178 => "correlation",
          when 179 => "capability",
          when 180 => "gamification",
          when 181 => "smooth transition",  --  I was caught saying that...
          when 182 => "leadership strategy",
          when 183 => "collaboration",
          when 184 => "success factor",
          when 185 => "lever",
          when 186 => "breakthrough",
          when 187 => "open-door policy",
          when 188 => "recalibration",
          when 189 => "wow factor", -- (obtained by bootstrapping)
          when 190 => "onboarding solution", -- (obtained by bootstrapping)
          when 191 => "brand pyramid",
          when 192 => "dashboard",
          when 193 => "branding",
          when 194 => "local-for-local strategy",
          when 195 => "cross-sell message",   --  (obtained by bootstrapping)
          when 196 => "up-sell message",      --  (obtained by bootstrapping)
          when 197 => "divisional structure", --  (obtained by bootstrapping)
          when 198 => "value chain",  --  (obtained by bootstrapping)
          when 199 => "microsegment",  --  (obtained by bootstrapping)
          when 200 => "rollout plan",  --  (obtained by bootstrapping)
          when 201 => Abbreviate ("Leadership Development System", 0.5),
          when 202 => "architectural approach",         --  (obtained by bootstrapping)
          when 203 => "brand value",
          when 204 => "milestone",  --  2012 Golden Flannel Awards article
          when 205 => "co-innovation",
          when 206 => "speedup",
          when 207 => "validation",
          when 208 => "skill",
          when 209 => "skillset",      --  Guffpedia
          when 210 => "feedback",
          when 211 => "learnability",  --  Guffpedia
          when 212 => "visibility",    --  Guffpedia
          when 213 => "agility",
          when 214 => "simplification",
          when 215 => "digitization",
          when 216 => "streamlining",
          when 217 => "brainstorming space",
          when 218 => "crowdsourcing",
          when 219 => "big-bang approach",
          when 220 => "execution message",
          when 221 => "criticality",
          when 222 => "opportunity pipeline",
          when 223 => "reorganization",
          when 224 => "synergization",
          when 225 => "socialization",
          when 226 => "strategic shift",
          when 227 => "growth engine",
          when 228 => "tailwind",
          when 229 => "accelerator",
          when 230 => "deliverable",
          when 231 => "takeaway",
          when 232 => "insourcing",
          when 233 => "outsourcing",
          when 234 => "careful consideration",
          when 235 => "conviction",
          when 236 => "initiator",
          when 237 => "operating model",
          when 238 => "proof-point",
          when 239 => "bounce rate",
          when 240 => "marketing funnel",
          when 241 => "offshoring",
          when 242 => "quick-win",
          when 243 => "cross-pollination",
          when 244 => "hybridation",
          when 245 => "positioning",
          when 246 => "reinvention",
          when 247 => "functionality",
          when 248 => "mindshare",
          when 249 => "mobility space",
          when 250 => "decision-to-execution cycle",
          when 251 => "adjustment",                --  Article about doublespeak
          when 252 => "force management program",  --  Article about doublespeak
          when 253 => "launchpad",
          when 254 => "value-chain",
          when 255 => "motion",
          when 256 => "customer-orientation",
          when 257 => "realignment",
          when 258 => "governmentalization", -- FOSDEM 2019
          when 259 => "case study",
          when 260 => "aspirational destination",
          when 261 => "Innovation Incubator",
          when 262 => "input",
          when 263 => "scope",
          when 264 => "action",
          when 265 => "context",
          when 266 => "next level",
          when 267 => "topology",
          when 268 => "data point",
          when 269 => "enablement",
          when 270 => "test-first design",    --  Agile development 2.0
          when 271 => "R&D initiative",
          when 272 => "blockchain",  --  Thx Vinzent Hoefler
          when 273 => "blockchain technology",
          when 274 => "product portfolio");

   begin
      return
         (case P is
             when Singular =>
                (case R494 is
                 --  Items where plural would sound clunky.
                 --  Assume equiprobability between explicit singular and "others => ..." items.
                    when 1   => Timeless_Event,
                    when 2   => "team building",
                    when 3   => "focus",
                    when 4   => "strategy",
                    when 5   => "planning granularity",
                    when 6   => "core business",
                    when 7   => "implementation",
                    when 8   => "intelligence",
                    when 9   => "change management",
                    when 10  => "ROE",
                    when 11  => "EBITDA",
                    when 12  => "enterprise content management",
                    when 13  => "excellence",
                    when 14  => "trust",
                    when 15  => "respect",
                    when 16  => "openness",
                    when 17  => "transparency",
                    when 18  => Abbreviate ("Quality Research", 0.5),
                    when 19  => "decision making",
                    when 20  => "risk management",
                    when 21  => "enterprise risk management",
                    when 22  => "leverage",
                    when 23  => "diversification",
                    when 24  => "successful execution",
                    when 25  => "effective execution",
                       --  Directly pasted from a management presentation (2009)
                    when 26  => "selectivity",
                    when 27  => "optionality",
                    when 28  => "expertise",
                    when 29  => "awareness",
                    when 30  => "broader thinking",
                    when 31  => "client focus",
                    when 32  => "thought leadership", -- Thanks Andy!
                    when 33  => "quest for quality", -- caracal
                       --  BBC office-speak phrases
                    when 34  => "360-degree thinking",
                    when 35  => "drill-down",
                    when 36  => "impetus",
                    when 37  => "fairness",
                    when 38  => "intellect",
                    when 39  => "emotional impact",
                    when 40  => "emotional intelligence",
                    when 41  => "adaptability",
                    when 42  => "stress management",
                    when 43  => "self-awareness",
                    when 44  => "strategic thinking",
                    when 45  => "cross-fertilization", -- Andy
                    when 46  => "cross-breeding",
                    when 47  => Abbreviate ("Customer Experience", 0.5),
                    when 48  => Abbreviate ("Customer Experience Management", 0.5),
                    when 49  => "SWOT analysis",
                    when 50  => "responsibility",
                    when 51  => "accountability",
                    when 52  => "ROI",
                    when 53  => "line of business",
                    when 54  => "serviceability",
                    when 55  => "responsiveness",
                    when 56  => "simplicity",
                    when 57  => "portfolio shaping",
                    when 58  => "knowledge sharing",
                    when 59  => "continuity",
                    when 60  => "visual thinking",
                    when 61  => "interoperability",
                    when 62  => "compliance",
                    when 63  => "teamwork",
                    when 64  => "self-efficacy",
                    when 65  => "decision-maker",
                    when 66  => "line-of-sight",
                    when 67  => "scoping", -- BBC - LGA banned words
                    when 68  => "line-up",
                    when 69  => "predictability",
                    when 70  => "recognition",
                    when 71  => "investor confidence",
                    when 72  => "competitive advantage",
                    when 73  => "uniformity",
                    when 74  => "connectivity",
                    when 75  => "big picture",
                    when 76  => "big-picture thinking",
                    when 77  => "quality",
                    when 78  => "upside focus",
                    when 79  => "sustainability",
                    when 80  => "resiliency",
                    when 81  => "social sphere",
                    when 82  => "intuitiveness",
                    when 83  => "effectiveness",
                    when 84  => "competitiveness",
                    when 85  => "resourcefulness",
                    when 86  => "informationalization",
                    when 87  => "role building",
                    when 88  => "talent retention",
                    when 89  => "innovativeness",
                    when 90  => "Economic Value Creation",
                    when 91  => "intellectual capital",
                    when 92  => "high quality",
                    when 93  => "full range of products",
                    when 94  => "technical strength",
                    when 95  => "quality assurance",
                    when 96  => "specification quality",
                    when 97  => "market environment",
                    when 98  => "client perspective",
                    when 99  => "solution orientation",
                    when 100 => "client satisfaction", -- new old buzzword in 2014
                    when 101 => "integrity",
                    when 102 => "reputation",
                    when 103 => "time-to-market",
                    when 104 => "innovative edge",
                    when 105 => "book value growth",
                    when 106 => "global network",
                    when 107 => "ability to deliver",
                    when 108 => "active differentiation",
                    when 109 => "solid profitability",
                    when 110 => "core capacity",
                    when 111 => "digital economy",
                    when 112 => "white-collar productivity",
                    when 113 => "white-collar efficiency",
                    when 114 => "governance",
                    when 115 => "corporate governance",
                    when 116 => "business development",
                    when 117 => "corporate identity",
                    when 118 => "attractiveness",  --  (obtained by bootstrapping)
                    when 119 => "design philosophy",  --  (obtained by bootstrapping)
                    when 120 => "global footprint",   --  (obtained by bootstrapping)
                    when 121 => "risk taking",        --  (obtained by bootstrapping)
                    when 122 => "focus on speed",     --  (obtained by bootstrapping)
                    when 123 => "business equation",  --  (obtained by bootstrapping)
                    when 124 => "edge",               --  (obtained by bootstrapping)
                    when 125 => "ownership",          --  (obtained by bootstrapping)
                    when 126 => "competitive success",  --  (obtained by bootstrapping)
                    when 127 => "discipline",  --  (obtained by bootstrapping)
                    when 128 => "knowledge management",  --  (obtained by bootstrapping)
                    when 129 => "ability to move fast",
                    when 130 => "ingenuity",
                    when 131 => "insightfulness",
                    when 132 => "integrativeness",  --  (obtained by bootstrapping)
                    when 133 => "customer footprint",  --  2012 Golden Flannel Awards article
                    when 134 => "time-to-value",       --  2011 Golden Flannel Awards article
                    when 135 => "efficacy",            --  2015 Golden Flannel Awards article
                    when 136 => "DNA",
                    when 137 => "dedication",
                    when 138 => "franchise",
                    when 139 => "global reach",
                    when 140 => "global touch-base",  --  2016 Golden Flannel Awards
                    when 141 => "technical excellence",
                    when 142 => "values congruence",
                    when 143 => "purpose",
                    when 144 => "catalyst for growth",
                    when 145 => "goal setting",
                    when 146 => "craftsmanship",
                    when 147 => "operational excellence",
                    when 148 => "re-engineering",
                    when 149 => "mindfulness",
                    when 150 => "quality thinking",
                    --  Next 3 are from http://dilbert.com/strip/2017-04-11: Dilbert Enters The Jargon Matrix:
                    when 151 => "user experience",
                    when 152 => "speed of execution",
                    when 153 => "responsive design",
                    when 154 => "readiness to go ""all-in""",
                    when 155 => "machine intelligence",
                    when 156 => "creativity",
                    when 157 => "can-do attitude",
                    when 158 => "relevance",
                    when 159 => "disruption",             --  (obtained by bootstrapping)
                    when 160 => "dematerialization",      --  (obtained by bootstrapping)
                    when 161 => "disintermediation",      --  (obtained by bootstrapping)
                    when 162 => "disaggregation",         --  (obtained by bootstrapping)
                    when 163 => "wave of change",         --  (obtained by bootstrapping)
                    when 164 => "digitalization",         --  (obtained by bootstrapping)
                    when 165 => "CAPEX",                  --  (obtained by bootstrapping)
                    when 166 => "window of opportunity",
                    when 167 => "beta",
                    when 168 => "coopetition",
                    when 169 => "digital change",
                    when 170 => "business excellence",
                    when 171 => "business impact",
                    when 172 => "business acumen",
                    when 173 => "leadership culture",
                    when 174 => "glocalization",         --  Article about doublespeak
                    when 175 => "re-equitizing",         --  Article about doublespeak
                    when 176 => "cost rationalization",  --  Article about doublespeak
                    when 177 => "strategic optionality",
                    when 178 => "product expertise",
                    when 179 => "velocity",
                    when 180 => "elasticity",
                    when 181 => "value stream management",
                    when 182 => "digital acceleration",
                    when 183 => "quality control",
                    when 184 => "decision-making",
                    when 185 => "digital business",
                    when 186 => "Organizational Intelligence",
                    when 187 => "Business Intelligence",
                    when 188 => "self-actualization",
                    when 189 => "leadership effectiveness",
                    when 190 => "customer's journey", -- FOSDEM 2019
                    when 191 => "adding services", -- FOSDEM 2019
                    when 192 => "centerpiece",
                    when 193 => "modern simplicity",
                    when 194 => "cost control",
                    when 195 => "operations delivery",
                    when 196 => "guidance",
                    when 197 => "onboarding",
                    when 198 => "cost structure",
                    when 199 => "traction",
                    when 200 => "ethos",  --  Thx Elias!
                    when 201 => "auditability",
                    when 202 => "business agility",
                    when 203 => "capital agility",
                    when 204 => "agile planning",
                    when 205 => "data science innovation",
                    when 206 => "project management",
                    when 207 => "business process quality engineering",
                    when 208 => "field workforce optimization",
                    when 209 => "business operations strategy design and velocity",
                    when 210 => "delivery of business value",
                    when 211 => "client-centricity",
                    --  2021-01-12: https://finance.yahoo.com/ :
                    --              "2021's buzziest phrase has already been a winner" :
                    when 212 => "operating leverage",
                    when 213 => "interplay between " &
                                   Thing_Atom (Random_Plural) & " and " &
                                   Thing_Atom (Random_Plural),
                    when 214 => "next stage of growth",
                    when 215 => "high-volume production",
                    when 216 => Abbreviate ("Artificial Intelligence", 0.75),
                    when 217 => "DNA sequencing",
                    when 218 => "value added experience",
                    when 219 => "optimization of the business activities",
                    when 220 => "deliverability",
                    --  Equiprobable:
                    when others => Inner),
         when Plural =>
            (case R320 is
               --  Things you find usually as plural.
               --  Assume equiprobability between explicit plural and "others => ..." items.
               when 1  => "key target markets",
               when 2  => "style guidelines",
               when 3  => "key performance indicators",
               when 4  => "market conditions",
               when 5  => "market forces",
               when 6  => "market opportunities",
               when 7  => "tactics",
                  --
               when 8 => "organizing principles",
                  --  GAC 2010
               when 9 => "interpersonal skills",
                  --  UWM 2010
               when 10 => "roles and responsibilities",
               when 11 => "cost savings",
                  --  Directly pasted from a management presentation (2009)
               when 12 => "lessons learned",
               when 13 => "client needs",
               when 14 => "requests / solutions",
               when 15 => "mobile strategies",
               when 16 => "expectations and allocations",
               when 17 => "workshops",
               when 18 => "dynamics",  --  (obtained by bootstrapping)
               when 19 => "options",   --  (obtained by bootstrapping)
               when 20 => "aspirations",  --  (obtained by bootstrapping)
               when 21 => "swim lanes",  --  2015 Golden Flannel Awards article
               when 22 => "pockets of opportunities",
               when 23 => "social implications",
               when 24 => "analytics",
               when 25 => "advanced analytics",
               when 26 => "growth years",
               when 27 => "big data",
               when 28 => "adjacencies",
               when 29 => "core competences",
               when 30 => "strengths",
               when 31 => "corporate values",
               when 32 => "core values",
               when 33 => "competitive dynamics",   --  Article about doublespeak
               when 34 => "workforce adjustments",  --  Article about doublespeak
               when 35 => "lessons learned",
               when 36 => "core verticals",
               when 37 => "metrics", -- FOSDEM 2019
               when 38 => "cost-control measures",
               when 39 => "expectations",
               when 40 => "data practices",
               when 41 => "industry market shifts",
               when 42 => "regulatory pivots",
               when 43 => "customer behavior patterns",
               when 44 => "robotics",
               when 45 => "guiding principles",
               when 46 => "tech stacks",

               --  Equiprobable:
               when others => Make_Eventual_Plural (Inner, Plural)));
   end Thing_Atom;

   function Thing (P : Plurality) return String is
   begin
      case R160 is
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
         when 15 .. 80 =>  -- 1 adjective
            return
               Thing_Adjective & ' ' &
               Thing_Atom (P);
         when 81 .. 82 =>  -- 2 adjectives, separated by "and/or"
            return
               Thing_Adjective & " and/or " &
               Thing_Adjective & ' ' &
               Thing_Atom (P);
         when 83 .. 84 =>
            return Growth; -- already has a superlative, don't add an adjective
         when 85 .. 90 =>  -- 3 adjectives
            return
               Thing_Adjective & ", " &
               Thing_Adjective & " and " &
               Thing_Adjective & ' ' &
               Thing_Atom (P);
         when 91 .. 94 =>  -- 4 adjectives
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

   -----------------------------------
   --   The Bad Things.   Whaaaa!   --
   -----------------------------------
   --
   --  They are always in plural. Singular is avoided for two reasons:
   --
   --    1. It would be too specific. Someone could be tempted to ask for details!
   --    2. It may be the beginning of a finger-pointing session. Better stay
   --         impersonal to survive the meeting...

   function Bad_Things return String is
   (case R51 is
       when 1  => "issues",
       when 2  => "intricacies",
       when 3  => "organizational diseconomies",
       when 4  => "black swans",
       when 5  => "challenging market conditions",
       when 6  => "inefficiencies",
       when 7  => "overlaps",
       when 8  => "known unknowns",
       when 9  => "unknown unknowns",
       when 10 => "soft cycle issues",
       when 11 => "obstacles",
       when 12 => "surprises",
       when 13 => "weaknesses", -- The W in SWOT
       when 14 => "threats",    -- The T in SWOT
       when 15 => "barriers to success",
       when 16 => "barriers",
       when 17 => "barriers to growth",  --  Seen in a train, on a printed presentation
       when 18 => "problems",
       when 19 => "uncertainties",
       when 20 => "unfavorable developments",
       when 21 => "consumer/agent disconnects",
       when 22 => "underperforming areas",
       when 23 => "information overloads",
       when 24 => "concerns",     --  (obtained by bootstrapping)
       when 25 => "shortfalls",   --  (obtained by bootstrapping)
       when 26 => "limitations",  --  (obtained by bootstrapping)
       when 27 => "downtimes",    --  (obtained by bootstrapping)
       when 28 => "headwinds",
       when 29 => "subpar returns",
       when 30 => "gaps",
       when 31 => "market gaps",
       when 32 => "capability gaps",
       when 33 => "constraints",
       when 34 => "problems/difficulties",
       when 35 => "bottlenecks",
       when 36 => "misunderstandings",
       when 37 => "dilemmas",
       when 38 => "interdependencies",
       when 39 => "discontinuities",
       when 40 => "hiccups",
       when 41 => "vulnerabilities",
       when 42 => "negative cash flows",                --  Article about doublespeak
       when 43 => "net profit revenue deficiencies",    --  Article about doublespeak
       when 44 => "negative contributions to profits",  --  Article about doublespeak
       when 45 => "shortcomings",
       when 46 => "pitfalls",
       when 47 => "friction",  --  FOSDEM 2019, note: not plural
       when 48 => "red flags",
       when 49 => "roadblocks",
       when 50 => "decision-making biases",
       when 51 => "second-round effects");
   -- Verbs --

   function Eventual_Adverb return String is
   (case R136 is  --  proportion: 3/4 empty adverb
       when 1  => "interactively ",
       when 2  => "credibly ",
       when 3  => "quickly ",
       when 4  => "proactively ",
       when 5  => "200% ",
       when 6  => "24/7 ",
          --  UW Presentation Nov 2010
       when 7  => "globally ",
       when 8  => "culturally ",
       when 9  => "technically ",
       --
       when 10 => "strategically ",
       when 11 => "swiftly ",
       when 12 => "cautiously ",
       when 13 => "expediently ",
       when 14 => "organically ",
       when 15 => "carefully ",
       when 16 => "significantly ",
       when 17 => "conservatively ",
       when 18 => "adequately ",
       when 19 => "genuinely ",
       when 20 => "efficiently ",
       when 21 => "seamlessly ",
       when 22 => "consistently ",
       when 23 => "diligently ",
       when 24 => "dramatically ",  --  (obtained by bootstrapping)
       when 25 => "straightforwardly ",  --  (obtained by bootstrapping)
       when 26 => "differentially ",  --  (obtained by bootstrapping)
       when 27 => "gradually ",
       when 28 => "aggressively ",  --  2011 Golden Flannel Awards article
       when 29 => "cost-effectively ",
       when 30 => "proactively ",
       when 31 => "inherently ",
       when 32 => "directionally ",
       when 33 => "relentlessly ",
       when 34 => "radically ",
       when others => "");

   function Add_Random_Article (P : Plurality; To : String) return String is
   (case R15 is
       when 1 .. 2  => "the " & To,
       when 3 .. 6  => "our " & To,
       when 7 .. 15 => Add_Indefinite_Article (P, To));
            --  Indefinite is preferred in BS language.

   function Thing_With_Random_Article (P : Plurality) return String is
   (if P = Singular and then R100 = 1 then
       --  The "why" is always the "why". Why?
       "the ""why"" behind " & Thing_Atom (Random_Plural)  --  FOSDEM 2019
    else
       Add_Random_Article (P, Thing (P)));

   function Eventual_Postfixed_Adverb return String is
      P : constant Plurality := Random_Plural;
   begin
      return
         (case R280 is  --  proportion: ~ 4/5 empty postfixed adverb
             when 1  => " going forward",
             when 2  => " within the industry",
             when 3  => " across the board",
                --  BBC office-speak phrases
             when 4  => " in this space",
             when 5  => " from the get-go",
             when 6  => " at the end of the day",
             when 7  => " throughout the organization",
             when 8  => " as part of the plan",
             when 9  => " by thinking outside of the box",
             when 10 => " using " & Thing_With_Random_Article (P),
             when 11 => " by leveraging " & Thing_With_Random_Article (P),
             when 12 => " taking advantage of " & Thing_With_Random_Article (P),
             when 13 => " within the " & Matrix_Or_So,
             when 14 => " across the " & Make_Eventual_Plural (Matrix_Or_So, Plural),
             when 15 => " across and beyond the " & Make_Eventual_Plural (Matrix_Or_So, Plural),
             when 16 => " resulting in " & Add_Indefinite_Article (Singular, Growth),
             when 17 => " reaped from our " & Growth,
             when 18 => " as a consequence of " & Add_Indefinite_Article (Singular, Growth),
             when 19 => " because " & Thing_With_Random_Article (P)
                        & ' ' & Build_Plural_Verb ("produce", P) & ' '
                        & Add_Indefinite_Article (Singular, Growth),
             when 20 => " ahead of schedule",
             when 21 => ", relative to our peers",
             when 22 => " on a transitional basis",
             when 23 => " by expanding boundaries",
             when 24 => " by nurturing talent",
             when 25 => ", as a Tier 1 company",
             when 26 => " up-front",
             when 27 => " on-the-fly",
             when 28 => " across our portfolio",
             when 29 => " 50/50",
             when 30 => " up, down and across the " & Matrix_Or_So,
             when 31 => " in the marketplace",
             when 32 => " by thinking and acting beyond boundaries",
             when 33 => " at the individual, team and organizational level",
             when 34 => " ensuring " & Add_Indefinite_Article (P, Thing (P)),  --  (bootstrapping)
             when 35 => " over the long term",
             when 36 => " across geographies",  --  2012 Golden Flannel Awards article
             when 37 => " in the core",         --  2011 Golden Flannel Awards article
             when 38 => " across industry sectors",
             when 39 => " across the wider Group",
             when 40 => ", paving the way for " & Add_Indefinite_Article (P, Thing (P)),
             when 41 => " by levelling the playing field",  --  2016 Golden Flannel Awards
             when 42 => " on a day-to-day basis",
             when 43 => " across boundaries",
             when 44 => " within the community",
             when 45 => " from within the data",
             when 46 => " round-the-clock",
             when 47 => " moving forward",
             when 48 => " downstream",
             when 49 => " down the chain",
             when 50 => " in the space",
             when 51 => " across the entire spectrum",  --  Thx Elias!
             when 52 => " as a matter of day-to-day operations",
             when 53 => " by turning data into " & Thing (P),
             when 54 => " without pre-empting or constraining future flexibility",
             when 55 => " as industries transform",
             when 56 => " in supply chains",
             when others => "");
   end Eventual_Postfixed_Adverb;

   function Person_Verb_Having_Thing_Complement (P : Plurality; Infinitive : Boolean) return String is
      function Inner return String is
      (case R103 is
          when   1 => "manage",
          when   2 => "target",
          when   3 => "streamline",
          when   4 => "improve",
          when   5 => "optimize",
          when   6 => "achieve",
          when   7 => "secure",
          when   8 => "address",
          when   9 => "boost",
          when  10 => "deploy",
          when  11 => "innovate",
          when  12 => "right-scale",
          when  13 => "formulate",
          when  14 => "transition",
          when  15 => "leverage",
          when  16 => "focus on",
          when  17 => "synergize",
          when  18 => "generate",
          when  19 => "analyse",
          when  20 => "integrate",
          when  21 => "empower",
          when  22 => "benchmark",
          when  23 => "learn",
          when  24 => "adapt",
          when  25 => "enable",
          when  26 => "strategize",
          when  27 => "prioritize",
             --   BBC office-speak phrases
          when  28 => "pre-prepare",
          when  29 => "deliver",
          when  30 => "champion",
          when  31 => "embrace",
          when  32 => "enhance",
          when  33 => "engineer",
          when  34 => "envision",
          when  35 => "incentivize",
          when  36 => "maximize",
          when  37 => "visualize",
          when  38 => "whiteboard",
          when  39 => "institutionalize",
          when  40 => "promote",
          when  41 => "overdeliver",
          when  42 => "right-size",
          when  43 => "rebalance",
          when  44 => "re-imagine",
          when  45 => "influence",
          when  46 => "facilitate",
          when  47 => "drive",
          when  48 => "structure",
          when  49 => "standardize",
          when  50 => "accelerate",
          when  51 => "deepen",
          when  52 => "strengthen",
          when  53 => "broaden",
          when  54 => "enforce",
          when  55 => "establish",
          when  56 => "foster",
          when  57 => "build",
          when  58 => "differentiate",
          when  59 => "take a bite out of",
          when  60 => "table",
          when  61 => "flesh out",
          when  62 => "reach out",
          when  63 => "jump-start",
          when  64 => "co-create",      --  (obtained by bootstrapping)
          when  65 => "capitalize on",  --  (obtained by bootstrapping)
          when  66 => "calibrate",      --  (obtained by bootstrapping)
          when  67 => "re-aggregate",  --  2011 Golden Flannel Awards article
          when  68 => "articulate",    --  2014 Golden Flannel Awards article
          when  69 => "iterate",       --  2015 Golden Flannel Awards article
          when  70 => "reinvest in",   --  2015 Golden Flannel Awards article
          when  71 => "potentiate",    --  2015 Golden Flannel Awards article
          when  72 => "front-face",    --  2015 Golden Flannel Awards article
          when  73 => "co-develop",
          when  74 => "take control of",
          when  75 => "robustify",  --  Guffpedia
          when  76 => "harness",    --  2016 Golden Flannel Awards
          when  77 => "activate",
          when  78 => "showcase",
          when  79 => "cherry-pick",
          when  80 => "digitize",
          when  81 => "re-invent",
          when  82 => "springboard",
          when  83 => "solutionize",
          when  84 => "re-content",
          when  85 => "commoditize",
          when  86 => "be eager for",
          when  87 => "productize",
          when  88 => "repurpose",
          when  89 => "reenergize",
          when  90 => "co-specify",
          when  91 => "codify",
          when  92 => "cross-pollinate",
          when  93 => "ignite",
          when  94 => "transgenerate",
          when  95 => "orchestrate",
          when  96 => "envisioneer",
          when  97 => "reintermediate",
          when  98 => "reframe",
          when  99 => "control",
          when 100 => "ideate",
          when 101 => "reprioritize",
          when 102 => "operate",   --  with or without ending
          when 103 => "cascade");  --  E.g.: to cascade information
   begin
      return
         (if Infinitive then  --  be /= are
             Inner
          else
             Build_Plural_Verb (Inner, P));
   end Person_Verb_Having_Thing_Complement;

   --  Something Bad is going to happen. Fortunately Supermarketman is there
   --  with his secret weapon to clean the Evil thing and rescue the Business.
   --  Well, at least there will be a meeting to begin a discussion about it.

   function Person_Verb_Having_Bad_Thing_Complement (P : Plurality) return String is
      function Inner return String is
      (case R12 is
          when  1 => "address",
          when  2 => "identify",
          when  3 => "avoid",
          when  4 => "mitigate",
          when  5 => "minimize",
          when  6 => "overcome",
          when  7 => "tackle",
          when  8 => "reduce",
          when  9 => "alleviate",
          when 10 => "filter out",
          when 11 => "remove",  --  FOSDEM 2019
          when 12 => "prevent");
   begin
      return Build_Plural_Verb (Inner, P);
   end Person_Verb_Having_Bad_Thing_Complement;

   --  (thing) verb (thing)

   function Thing_Verb_Having_Thing_Complement (P : Plurality) return String is
      function Inner return String is
      (case R43 is
          when 1  => "streamline",
          when 2  => "interact with",
          when 3  => "boost",
          when 4  => "generate",
          when 5  => "impact",
          when 6  => "enhance",
          when 7  => "leverage",
          when 8  => "synergize",
          when 9  => "generate",
          when 10 => "empower",
          when 11 => "enable",
          when 12 => "prioritize",
          when 13 => "transfer",
          when 14 => "drive",
          when 15 => "result in",
          when 16 => "promote",
          when 17 => "influence",
          when 18 => "facilitate",
          when 19 => "aggregate",
          when 20 => "architect",
          when 21 => "cultivate",
          when 22 => "engage",
          when 23 => "structure",
          when 24 => "standardize",
          when 25 => "accelerate",
          when 26 => "deepen",
          when 27 => "strengthen",
          when 28 => "enforce",
          when 29 => "foster",
          when 30 => "turbocharge",
          when 31 => "granularize",      --  (obtained by bootstrapping)
          when 32 => "operationalize",   --  (obtained by bootstrapping)
          when 33 => "reconceptualize",  --  (obtained by bootstrapping)
          when 34 => "iterate",  --  2014 Golden Flannel Awards article
          when 35 => "revolutionise",
          when 36 => "digitize",
          when 37 => "solutionize",
          when 38 => "lead to",
          when 39 => "reenergize",
          when 40 => "restructure",
          when 41 => "cross-pollinate",
          when 42 => "ignite",
          when 43 => "transgenerate");
   begin
      return Build_Plural_Verb (Inner, P);
   end Thing_Verb_Having_Thing_Complement;

   --  (thing) verb (person)

   function Thing_Verb_Having_Person_Complement (P : Plurality) return String is
      function Inner return String is
      (case R16 is
          when  1 => "motivate",
          when  2 => "target",
          when  3 => "enable",
          when  4 => "drive",
          when  5 => "synergize",
          when  6 => "empower",
          when  7 => "prioritize",
             --  BBC office-speak phrases
          when  8 => "incentivise",
          when  9 => "inspire",
             --
          when 10 => "transfer",
          when 11 => "promote",
          when 12 => "influence",
          when 13 => "strengthen",
          when 14 => "energize",    --  (obtained by bootstrapping)
          when 15 => "invigorate",  --  (obtained by bootstrapping)
          when 16 => "reenergize");
   begin
      return Build_Plural_Verb (Inner, P);
   end Thing_Verb_Having_Person_Complement;

   function Person_Infinitive_Verb_And_Ending return String;

   function Person_Verb_And_Definite_Ending (P : Plurality; Infinitive : Boolean) return String is
      --  NB: this function produces an eventual definite complement
      --     after the verb, or no complement at all.
      function Inner return String is
      (case R129 is
          when   1 => "streamline the process",
          when   2 => "address the overarching issues",
          when   3 => "benchmark the portfolio",
          when   4 => "manage the cycle",     -- Fad of 2004
          when   5 => "figure out where we come from, where we are going to",
          when   6 => "maximize the value",
          when   7 => "execute the strategy",  --  Obsessive in 2006
          when   8 => "think out of the box",
          when   9 => "think differently",
          when  10 => "think across the full value chain",
          --  BBC office-speak phrases:
          when  11 => "loop back",
          when  12 => "conversate",
          when  13 => "go forward together",
          when  14 => "achieve efficiencies",
          when  15 => "deliver",  --  "deliver", form without complement
                                  --  GAC 2010
          when  16 => "stay in the mix",
          when  17 => "stay in the zone",
          when  18 => "evolve",
          when  19 => "exceed expectations",
          when  20 => "develop the plan",
          when  21 => "develop the blue print for execution",
          when  22 => "grow and diversify",
          when  23 => "fuel changes",
          when  24 => "nurture talent",
          when  25 => "cultivate talent",
          when  26 => "make it possible",
          when  27 => "manage the portfolio",
          when  28 => "be able to move quickly and flexibly",
          when  29 => "drive the business forward",
          when  30 => "make things happen",
          when  31 => "stay ahead",
          when  32 => "outperform peers",
          when  33 => "surge ahead",
          when  34 => "manage the downside",
          when  35 => "stay in the wings",
          when  36 => "come to a landing",
          when  37 => "shoot it over",
          when  38 => "move the needle",
          when  39 => "connect the dots",
          when  40 => "connect the dots to the end game",
          when  41 => "reset the benchmark",
          when  42 => "take it offline",
          when  43 => "peel the onion",
          when  44 => "drill down",
          when  45 => "get from here to here",
          when  46 => "do things differently",
          when  47 => "stretch the status quo",
          when  48 => "challenge the status quo",
          when  49 => "challenge established ideas",
          when  50 => "increase customer satisfaction",
          when  51 => "enable customer interaction",
          when  52 => "manage the balance",
          when  53 => "turn every stone",
          when  54 => "drive revenue",
          when  55 => "rise to the challenge",
          when  56 => "keep it on the radar",
          when  57 => "stay on trend",
          when  58 => "hunt the business down",
          when  59 => "push the envelope to the tilt",
          when  60 => "execute on priorities",
          when  61 => "stand out from the crowd",
          when  62 => "make the abstract concrete",
          when  63 => "manage the mix",
          when  64 => "grow",
          when  65 => "accelerate the strategy",
          when  66 => "enhance the strength",
          when  67 => "create long-term value",
          when  68 => "meet the challenges",
          when  69 => "move the progress forward",
          when  70 => "do the right projects",
          when  71 => "do the projects right",
          when  72 => "do more with less",
          when  73 => "build winning teams",     --  (obtained by bootstrapping)
          when  74 => "deliver on commitments",  --  (obtained by bootstrapping)
          when  75 => "execute",                 --  (obtained by bootstrapping)
          when  76 => "deliver",                 --  (obtained by bootstrapping)
          when  77 => "see around the corner",   --  (obtained by bootstrapping)
          when  78 => "meet the surge",   --  (obtained by bootstrapping)
          when  79 => "celebrate the success",  --  Souvenir of Converium...
          when  80 => "circle back",     --  2014 Golden Flannel Awards article
          when  81 => "action forward",  --  2014 Golden Flannel Awards article
          when  82 => "move forward",    --  2015 Golden Flannel Awards article
          when  83 => "take control",
          when  84 => "be cautiously optimistic",
          when  85 => "be committed",
          when  86 => "evolve our culture",
          when  87 => "leverage the benefits of our differentiation",
          when  88 => "stretch our data bucket",  --  Guffpedia
          when  89 => "leapfrog the competition",
          when  90 => "call ""check-mate"" ahead of competition",
          when  91 => "preempt competitors",  --  1996 article about Enron and others
          when  92 => "bring our vision to reality",
          when  93 => "create an environment where " &
             Thing_Atom (Singular) & ", " &
             Thing_Atom (Singular) & " and " &
             Thing_Atom (Singular) & " can thrive",
          when  94 => "seize opportunities",
          when  95 => "create momentum",
          when  96 => "generate company momentum",
          when  97 => "pursue new opportunities",
          when  98 => "increase adherence",
          when  99 => "focus on the right things",
          when 100 => "open the kimono",
          when 101 => "give 110%",
          when 102 => "take it to the next level",
          when 103 => "boil the ocean",
          when 104 => "close the loop",
          when 105 => "create value",
          when 106 => "disrupt the status quo",
          when 107 =>
             --  Alameda Research:
             "make sure everyone is working together toward common " &
             "goals rather than working at cross purposes",
          when 108 =>
             "advance our strategy to " &
             Person_Infinitive_Verb_And_Ending,
          when 109 =>
             "focus on our " & Thing_Atom (Plural) &
             " to " & Person_Infinitive_Verb_And_Ending,
          when 110 => "deliver greater value for our customers",
          when 111 => "generate new value for shareholders",
          when 112 => "strengthen the balance sheet",
          when 113 => "operate",  --  with or without ending
          when 114 => "move up the power curve",
          when 115 => "cut the dry business",
          when 116 => "take the elevator beyond the top floor",  --  Mind your heads!
          when 117 => "stick to the knitting",
          when 118 => "create new business options",   --  1996 article about Enron and others
          when 119 => "create strategic options and opportunities",  --  idem
          when 120 => "carve a competitive position",  --  1996 article about Enron and others
          when 121 => "be on the same page",
          --  Alameda Research:
          when 122 => "make sure everyone is on the same page",
          when 123 => "have a culture of getting stuff done quickly",
          --
          when 124 => "align resources",
          --  https://twitter.com/ParikPatelCFA/status/1710411360927138003
          --    "Twitter isn't burning cash, it's aligning incentives
          --     with users by operating as a nonprofit":
          when 125 => "align incentives with users",
          --  Variants and further findings through search engines:
          when 126 => "align incentives with customers",
          when 127 => "align incentives with customers' success",
          when 128 => "align users' and stakeholders' needs",
          when 129 => "close the loop on our customer-first culture");
   begin
      if Infinitive then
         --  In general we could (mis)use the plural as an infinitive
         --  but there are some exceptions: "be" /= "are"...
         return Inner;
      else
         return Build_Plural_Verb (Inner, P);
      end if;
   end Person_Verb_And_Definite_Ending;

   function Thing_Verb_And_Definite_Ending (P : Plurality; Infinitive : Boolean) return String is
      --  NB: this function produces an eventual definite complement
      --     after the verb, or no complement at all.
      function Inner return String is
      (case R5 is
          when 1 => "add value",
          when 2 => "deliver maximum impact",
          when 3 => "be on track",
          when 4 => "deliver value",
          when 5 => "deliver the best possible value");
   begin
      if Infinitive then
         --  In general we could (mis)use the plural as an infinitive
         --  but there are some exceptions: "be" /= "are"...
         return Inner;
      else
         return Build_Plural_Verb (Inner, P);
      end if;
   end Thing_Verb_And_Definite_Ending;

   --  Verb + Ending. Ending is a Complement or something else

   function Thing_Verb_And_Ending (P : Plurality) return String is
      Compl_Sp : constant Plurality := Random_Plural;
   begin
      case R104 is
         when 1 .. 55  =>
            return Thing_Verb_Having_Thing_Complement (P) &
              ' ' &
              Thing_With_Random_Article (Compl_Sp);
         when 56 .. 100 =>
            return Thing_Verb_Having_Person_Complement (P) &
              " the " & Person (Compl_Sp);
         when 101 .. 104 =>
            return Thing_Verb_And_Definite_Ending (P, Infinitive => False);
      end case;
   end Thing_Verb_And_Ending;

   function Person_Verb_And_Ending (P : Plurality; Infinitive : Boolean) return String is
      Compl_Sp : constant Plurality := Random_Plural;
   begin
      case R95 is
         when  1 .. 10  =>
            return Person_Verb_And_Definite_Ending (P, Infinitive);
         when 11 .. 15  => -- Fight-the-Evil situation
            return
              Person_Verb_Having_Bad_Thing_Complement (P) &
              ' ' &
              Add_Random_Article (Plural, Bad_Things);
         when 16 .. 95 =>
            return
              Person_Verb_Having_Thing_Complement (P, Infinitive) &
              ' ' &
              Thing_With_Random_Article (Compl_Sp);
      end case;
   end Person_Verb_And_Ending;

   --  "We need to..." and similar forward-looking constructions
   --
   function Faukon return String is
   (case R18 is
       when  1 => "we need to",
       when  2 => "we've got to",
       when  3 => "the reporting unit should",
       when  4 => "controlling should",
       when  5 => "we must activate the " & Matrix_Or_So & " to",
       when  6 => "pursuing this route will enable us to",
       when  7 => "we will go the extra mile to",
       when  8 => "we are working hard to",
       when  9 => "we continue to work tirelessly and diligently to",
       when 10 => "we will execute to",
       when 11 => "we will sharpen our business models to",
       when 12 => "to continue our growth, we must",
       when 13 => "we are going to",
       when 14 => "we look forward to working together to",
       when 15 => "in order to improve, you need to",
       when 16 => "trending your numbers should",  --  FOSDEM 2019
       when 17 => "it is really important to",     --  Alameda Research
       when 18 =>
         "we are laser-focused on successfully executing " &
         "our plan and on progressing towards our targets to");

   function Person_Infinitive_Verb_And_Ending return String is
      (Person_Verb_And_Ending (Plural, Infinitive => True));

   function Proposition return String is
      Sp1 : constant Plurality := Random_Plural;
   begin
      case R116 is
         when 1 .. 5    => -- "We need to..."
            return
            Faukon & ' ' &
            Person_Infinitive_Verb_And_Ending &
            Eventual_Postfixed_Adverb;
            --  infinitive written same as present plural
         when 6 .. 50    => -- ** PERSON...
            return
              "the " & Person (Sp1) & ' ' &
              Eventual_Adverb &
              Person_Verb_And_Ending (Sp1, Infinitive => False) &
              Eventual_Postfixed_Adverb;
         when 51 .. 92   => -- ** THING...
            return
            Thing_With_Random_Article (Sp1) & ' ' &
            Eventual_Adverb &
            Thing_Verb_And_Ending (Sp1) &
            Eventual_Postfixed_Adverb;
         when 93 .. 97     => -- ** thing and thing ...
            return -- nb: no article, no adjective
            Thing_Atom (Singular) & " and " &
            Thing_Atom (Singular) & ' ' &
            Eventual_Adverb &
            Thing_Verb_And_Ending (Plural) &
            Eventual_Postfixed_Adverb;
         when 98 .. 100    => -- ** thing, thing and thing ...
            return -- nb: no article, no adjective
            Thing_Atom (Singular) & ", " &
            Thing_Atom (Singular) & " and " &
            Thing_Atom (Singular) & ' ' &
            Eventual_Adverb &
            Thing_Verb_And_Ending (Plural) &
            Eventual_Postfixed_Adverb;
         when 101 =>
            return
            "there can be no " & Growth_Atom &
            " until we can achieve " & Add_Indefinite_Article (Singular, Growth);
         when 102 =>
            return Thing (Plural) & " challenge us to " & Person_Infinitive_Verb_And_Ending;
                                    --  ^ (obtained by bootstrapping)
         when 103 =>
            return Thing (Singular) & " is all about " & Thing (Sp1);  --  (obtained by bootstrapping)
         when 104 =>
            return "there is no alternative to " & Thing_Atom (Sp1);  --  (obtained by bootstrapping)
         when 105 =>
            return
            "the key to " & Thing_Atom (Singular) &
            " is " & Thing_Atom (Singular);  --  (obtained by bootstrapping)
         when 106 =>
            return
            "opting out of " & Thing (Sp1) & " is not a choice";  --  (obtained by bootstrapping)
         when 107 =>
            return
            Add_Indefinite_Article (Singular, Growth) &
            " goes hand-in-hand with " &
            Add_Indefinite_Article (Singular, Growth);  --  (obtained by bootstrapping)
         when 108 =>
            return
               "the " & Person (Sp1) &
               " will be well equipped to " & Person_Infinitive_Verb_And_Ending;
         when 109 =>
            return
               Thing_Atom (Singular) & " is a matter of speed of action";
         when 110 =>
            return
               Thing_Atom (Singular) & " won't happen without " & Thing_Atom (Sp1);
         when 111 =>
            return
               Thing_With_Random_Article (Singular) &
               " will be best positioned to " & Person_Infinitive_Verb_And_Ending;
         when 112 =>
            return
               Thing_Atom (Singular) & " in the digital age calls for " & Thing_Atom (Sp1);
         when 113 =>
            return
               Thing_Atom (Singular) & " moves the company up the value chain";
         when 114 =>
            return
               Thing_Atom (Singular) & " requires that we all pull in the same direction";
         when 115 =>
            return
               Thing_Atom (Singular) & " requires truly optimizing " &
               Thing_Atom (Singular) & " to " &
               Thing_Verb_And_Definite_Ending (Plural, Infinitive => True);
         when 116 =>
            return
               "together, we " & Person_Verb_And_Ending (Plural, Infinitive => False);
      end case;
   end Proposition;

   function Articulated_Propositions return String is
   (case R420 is
       when   1 .. 270 => Proposition,
       when 271 .. 280 => Proposition & ", this is why " & Proposition,
       when 281 .. 290 => Proposition & ", nevertheless " & Proposition,
       when 291 .. 300 => Proposition & ", whereas " & Proposition,
       when 311 .. 350 => Proposition & ", while " & Proposition,
       when 351 .. 360 => Proposition & ". In the same time, " & Proposition,
       when 361 .. 370 => Proposition & ". As a result, " & Proposition,
       when 371 .. 380 => Proposition & ", whilst " & Proposition,
       --  Lower probability constructs
       when 301 .. 303 => "our gut-feeling is that " & Proposition,
       when 304 .. 306 =>
          "the point is not merely to " & Person_Infinitive_Verb_And_Ending &
          ". The point is to " & Person_Infinitive_Verb_And_Ending,
       when 307 .. 310 =>
          "it's not about " & Thing_Atom (Random_Plural) &
          ". It's about " & Thing_With_Random_Article (Random_Plural),
       when 381 .. 383 =>
          "our challenge is not to " & Person_Infinitive_Verb_And_Ending &
          ". Our challenge is to " & Person_Infinitive_Verb_And_Ending,
       when 384 .. 386 => "going forward, " & Proposition,  --  Also as postfix adverb
       when 387 .. 389 => "actually, " & Proposition,  --  2015 Golden Flannel Awards
       when 390 .. 392 => "in the future, " & Proposition,
       when 393 .. 395 => "flat out, " & Proposition,
       when 396 .. 398 => "first and foremost, " & Proposition,
       when 399 .. 402 =>
          "the game is all about " &
            Thing_Atom (Singular) & ", " &
            Thing_Atom (Singular) & ", " &
            Thing_Atom (Singular) & ", " &
            Thing_Atom (Singular) & ", and " &
            Thing_Atom (Singular) & " - not " &
            Thing_Atom (Singular) & ", " &
            Thing_Atom (Singular) & ", " &
            Thing_Atom (Singular) & ", " &
            Thing_Atom (Singular) & ", and " &
            Thing_Atom (Singular),
       when 403 => "in today's fast-changing world, " & Proposition,
       when 404 => "internally and externally, " & Proposition,
       when 405 => "our message is: " & Proposition,
       when 406 => "in a data-first world, " & Proposition,
       when 407 => "the future awaits",
       when 408 =>
          Thing_Atom (Plural) & " not only thrive on change, they initiate it",
       when 409 =>
          "as the pace of " & Thing_Atom (Random_Plural) &
          " continues to accelerate, " & Thing_Atom (Singular) &
          " has become a necessity",
       when 410 =>
          Thing_Atom (Singular) & ", " &
          Thing_Atom (Singular) & ", " &
          Thing_Atom (Singular) & ", " &
          Thing_Atom (Singular) & " - all are competing for the attention of " &
          Person (Plural),
       when 411 => "success brings success",  --  Thx Marco Carenzo!
       when 412 => "everyone is coming to grips with the fact that " & Proposition,
       when 413 =>
          Thing (Plural) &
          " will be a thing of the past over the next decade" &
          " and be fully replaced with " &
          Thing (Random_Plural),
       when 414 =>
          "as the consumer and commerce landscape continues to evolve, " & Proposition,
       when 415 =>
          "in an age of information, " & Proposition,
       when 416 =>
          "in a growing digital environment, " & Proposition,
       when 417 =>
          "to remain relevant, " & Proposition,
       when 418 =>
          "at the crossroads of " &
          Thing_Atom (Singular) & ", " &
          Thing_Atom (Singular) & " and " &
          Thing_Atom (Singular) & ", " &
          Proposition,
       when 419 =>
          "one thing about " & Thing_Atom (Random_Plural) &
          " is clear: " & Proposition,  --  1996 article about Enron and others
       when 420 =>
           "the appropriate strategy for the " & Matrix_Or_So &
           " depends on where it is today and on the state of the world down the road");
           --  1996 article about Enron and others

   function Sentence return String is
      Ap : constant String := Articulated_Propositions;
      use Ada.Characters.Handling;
   begin
      return To_Upper (Ap (Ap'First)) & Ap (Ap'First + 1 .. Ap'Last) & ". ";
   end Sentence;

   --  The total number of sentences X for each external call to the
   --  function Sentences follows a geometric distribution.
   --
   --  E(X) = 1/p, sigma(X) = (1-p) / (p^2)
   --
   function Sentences (Possible_Dialog_Mark : String) return String is
   (case R100 is
       when  1 ..  33 =>  --  Recursion stops in this case. Prob = p.
          Sentence,
       when 34 ..  80 =>
          Sentences (Possible_Dialog_Mark) & Sentence,
       when 81 .. 100 =>
          Sentences (Possible_Dialog_Mark) &
          Paragraph_End_Mark & Paragraph_Mark &
          Possible_Dialog_Mark & Sentence);

   function Sentence_Guaranteed_Amount (Count : Positive; Possible_Dialog_Mark : String) return String is
      Element : constant String :=
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
   (Sentence_Guaranteed_Amount (1250, Dialog_Mark));

   function Short_Workshop return String is
   (Sentence_Guaranteed_Amount (30, Dialog_Mark));

   function Short_Meeting return String is
   (Sentence_Guaranteed_Amount (3, Dialog_Mark));

   function Financial_Report return String is
   (Sentence_Guaranteed_Amount (20, ""));
   --  !! charts (especially, pie charts) !!

end Corporate_Bullshit;
