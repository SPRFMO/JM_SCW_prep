# SCW16 Document Memory Map

```{=html}
<div class="theme-toggle" aria-label="Background theme switch">
  <span>Background</span>
  <div class="theme-toggle__buttons" role="group" aria-label="Choose background">
    <button type="button" data-theme-choice="light" aria-pressed="true">Light</button>
    <button type="button" data-theme-choice="dark" aria-pressed="false">Dark</button>
  </div>
</div>

<style>
:root {
  color-scheme: light;
  --page-bg: #f8f9fb;
  --page-text: #1f2933;
  --muted-text: #52606d;
  --panel-bg: #ffffff;
  --table-head-bg: #eef2f6;
  --table-border: #d9e2ec;
  --link-color: #075985;
  --button-bg: #ffffff;
  --button-text: #1f2933;
  --button-active-bg: #1f2933;
  --button-active-text: #ffffff;
  --blockquote-bg: #eef6ff;
  --code-bg: #eef2f6;
}

:root[data-memory-theme="dark"] {
  color-scheme: dark;
  --page-bg: #111827;
  --page-text: #e5e7eb;
  --muted-text: #cbd5e1;
  --panel-bg: #1f2937;
  --table-head-bg: #263241;
  --table-border: #374151;
  --link-color: #7dd3fc;
  --button-bg: #111827;
  --button-text: #e5e7eb;
  --button-active-bg: #e5e7eb;
  --button-active-text: #111827;
  --blockquote-bg: #172033;
  --code-bg: #263241;
}

body {
  background: var(--page-bg);
  color: var(--page-text);
  transition: background 160ms ease, color 160ms ease;
}

a {
  color: var(--link-color);
}

table {
  background: var(--panel-bg);
  border-collapse: collapse;
}

th,
td {
  border-color: var(--table-border);
}

th {
  background: var(--table-head-bg);
}

blockquote {
  background: var(--blockquote-bg);
  border-left: 4px solid var(--link-color);
  color: var(--page-text);
  margin-left: 0;
  padding: 0.75rem 1rem;
}

code {
  background: var(--code-bg);
  color: var(--page-text);
  padding: 0.1rem 0.25rem;
  border-radius: 4px;
}

.theme-toggle {
  align-items: center;
  background: var(--panel-bg);
  border: 1px solid var(--table-border);
  border-radius: 8px;
  display: inline-flex;
  gap: 0.75rem;
  margin: 0 0 1.25rem;
  padding: 0.45rem 0.55rem 0.45rem 0.75rem;
}

.theme-toggle span {
  color: var(--muted-text);
  font-size: 0.9rem;
}

.theme-toggle__buttons {
  display: inline-flex;
  gap: 0.25rem;
}

.theme-toggle button {
  background: var(--button-bg);
  border: 1px solid var(--table-border);
  border-radius: 6px;
  color: var(--button-text);
  cursor: pointer;
  font: inherit;
  padding: 0.25rem 0.65rem;
}

.theme-toggle button[aria-pressed="true"] {
  background: var(--button-active-bg);
  color: var(--button-active-text);
}
</style>

<script>
(function () {
  const storageKey = "scw16-memory-map-theme";
  const root = document.documentElement;
  const buttons = Array.from(document.querySelectorAll("[data-theme-choice]"));

  function applyTheme(theme) {
    root.setAttribute("data-memory-theme", theme);
    buttons.forEach((button) => {
      button.setAttribute(
        "aria-pressed",
        String(button.dataset.themeChoice === theme)
      );
    });
    try {
      localStorage.setItem(storageKey, theme);
    } catch (error) {
      // Ignore storage failures in local-file viewing contexts.
    }
  }

  let initialTheme = "light";
  try {
    initialTheme = localStorage.getItem(storageKey) || initialTheme;
  } catch (error) {
    initialTheme = "light";
  }

  applyTheme(initialTheme === "dark" ? "dark" : "light");

  buttons.forEach((button) => {
    button.addEventListener("click", () => {
      applyTheme(button.dataset.themeChoice);
    });
  });
})();
</script>
```

## Purpose

This note is a first-pass memory aid for the SCW16 paper set. It starts with the
document number, title, and main topic, then gives a compact recall structure for
learning the list without rereading every paper.

Doc05 is included as the biology paper in the SCW16-Doc05 slot, although the
linked external source is currently named `SCW16-Paper-05-JM_Biology`.

Interactive practice page: [SCW16 document flashcards](SCW16-Doc-Flashcards.html).

## Chair Kick-Off Notes

Use this up front to set the tone for the meeting. The goal is not to reopen
every technical issue at once, but to make sure the workshop produces a clear
set of priorities, data requests, and follow-up work sessions.

Suggested opening message:

- The benchmark material contains many sensitivities: stock structure, index
  selection, catchability and availability, effort creep, selectivity,
  recruitment assumptions, biological inputs, and OM conditioning.
- Because there are many sensitivities, the workshop should focus on which ones
  materially affect advice, OM conditioning, and MSE readiness.
- To the extent practical, members should supplement their reports with a short
  summary of **data extent**: years covered, spatial coverage, fleet or survey
  coverage, observation unit, sample sizes, treatment of missing data, and
  whether uncertainty is available for assessment use.
- Several planned or requested working papers are still important to fit into
  the workshop record, including two Peru papers. These should be treated as
  part of the benchmark evidence base even if their numbering or final report
  status is still being settled.
- Work sessions matter. Some decisions will need plenary agreement, but many
  useful products will come from smaller groups comparing candidate indices,
  reconciling assumptions, and drafting prioritized recommendations.
- Report writing should be treated as part of the technical work, not an
  afterthought. Each session should leave behind short text on decisions,
  unresolved issues, data requests, and recommended follow-up.
- The desired output is a prioritized path forward: what can be accepted now,
  what should be tested as a sensitivity, what needs more documentation, and
  what should be held for later work.

Simple 10-minute overview:

| Time | Message | Cue |
|---|---|---|
| 0-1 min | Welcome and objective | The meeting should convert many technical papers into clear benchmark priorities. |
| 1-3 min | Roadmap | May benchmark work feeds June MSE work; assessment inputs and OM grids need to be coherent. |
| 3-4 min | Planned working papers | Note the planned/requested papers, including two Peru papers, and explain that they need to be captured in the benchmark record. |
| 4-5 min | Sensitivities | Acknowledge the major sensitivities without trying to solve them in the opening. |
| 5-7 min | Data extent request | Ask presenters to add concise data-extent information where it is missing or easy to provide. |
| 7-9 min | Work-session and report-writing expectations | Emphasize small-group work on index ranking, biological inputs, OM assumptions, priority setting, and text for the workshop report. |
| 9-10 min | Decision discipline | Separate accepted inputs, sensitivity runs, documentation gaps, and deferred items. |

Specific request to presenters:

> If your paper proposes or evaluates an abundance index, please be ready to say
> in one slide or verbally: what years it covers, what area and fleet or survey it
> represents, what the observation unit is, how many observations or survey days
> support it, what uncertainty is available, and what major caveat should be kept
> in mind when considering it for the assessment or OM work.

Planned/requested paper tracking:

| Item | Chair note |
|---|---|
| Peru CPUE paper, now Doc15 | Make sure it is included in the CPUE discussion and captured in the index-ranking/data-extent table. |
| Peru length-weight paper, now Doc13 | Make sure it is included in the biological-inputs discussion and linked to biomass-estimation assumptions. |
| Peru length-frequency paper, now Doc14 | Make sure it is included in the biological-inputs discussion and linked to model composition-data decisions. |
| Peru acoustic-index considerations paper, now Doc16 | Make sure it is included in the acoustic discussion and linked to decisions about whether Peru acoustic information is an index, a diagnostic, or supporting context. |
| Other Peru reports noted for the benchmark | Track whether they are formal papers, supporting reports, or follow-up items for the workshop report. |
| Session report text | Assign at least a rough drafter for acoustic, CPUE, biological-input, and OM/report sections before discussion ends. |

Useful chair prompts:

| Prompt | Purpose |
|---|---|
| What decision does this paper support? | Prevents discussion from becoming only descriptive. |
| What data extent underlies this result? | Keeps index rankings grounded in coverage and sample size. |
| Is this a primary input, a sensitivity, a diagnostic, or a hold item? | Forces a practical classification. |
| What would change if we accepted this index or assumption? | Links the paper to assessment and OM consequences. |
| What can be resolved in this meeting, and what needs follow-up? | Keeps the workshop moving toward priorities. |

## First Pattern To Memorize

Memorize the documents in two blocks:

1. **SCW16-Doc00 to Doc05: benchmark scaffold**
   - agenda, base assessment model, CPUE work plan, CPUE history, operating models,
     and biology.
2. **SCW16-Doc06 to Doc12: abundance-index and data support**
   - acoustic indices, CPUE standardization, effort creep, data quality checks, and
     alternate spatio-temporal methods.
3. **SCW16-Doc13 to Doc16: Peru additions**
   - length-weight and length-frequency information for Peruvian jack mackerel.
   - Peru CPUE standardization and acoustic-index considerations.

A short verbal hook is:

> **Agenda, Model, CPUE Plan, CPUE Past, MSE OMs, Biology; then Acoustic, Creep,
> Acoustic, Acoustic, CPUE, Checks, INLA; then Peru Weight, Peru Lengths, Peru
> CPUE, Peru Acoustic.**

## Title And Topic Table

| Doc | Short recall name | Title | Main topic |
|---|---|---|---|
| SCW16-Doc00 | Agenda | [Preliminary agenda](../docs/SCW16-Doc00.html) | Workshop/session flow and expected outputs. |
| SCW16-Doc01 | Base model | [Developments of the base SC13 model for benchmark and MSE considerations](../docs/SCW16-Doc01.html) | Benchmark starting point from the SC13 JJM assessment model, including single-stock and two-stock base configurations. |
| SCW16-Doc02 | CPUE plan | [CPUE coordination priorities and working-paper plan](../docs/SCW16-Doc02-CPUE_Coordination.html) | Forward-looking CPUE benchmark coordination, decision points, and supporting paper plan. |
| SCW16-Doc03 | CPUE past | [Meta-Analysis of CPUE Papers on Jack Mackerel (2022-2025)](../docs/SCW16-Doc03-CPUE_MetaAnalysis.html) | Backward-looking synthesis of earlier CPUE papers and themes for benchmark preparation. |
| SCW16-Doc04 | MSE operating models | [Operating Models for Jack Mackerel MSE](../docs/SCW16-Doc04-Operating_Models.pdf) | Operating-model structure for management strategy evaluation. |
| SCW16-Doc05 | Biology | [JM_Biology](https://sprfmo.github.io/JM_biol/SCW16-Paper-05-JM_Biology.html) | Biological inputs for model 1.14, including natural mortality, maturity, weight-at-age, and spawning biomass calculations. |
| SCW16-Doc06 | North acoustic STM | [Toward a standardised abundance index for Chilean jack mackerel: spatio-temporal modelling of acoustic survey data in northern Chile](../docs/SCW16-Doc06-Northern_Chile_Acoustic_STM.pdf) | Northern Chile acoustic-survey abundance index using spatio-temporal modelling. |
| SCW16-Doc07 | Effort creep | [Implementation of an informed creep correction into the Chile Jack Mackerel CPUE index](../docs/SCW16-Doc07-Chile_CPUE_Effort_Creep.pdf) | Correction for changes in fishing efficiency affecting the Chilean CPUE index. |
| SCW16-Doc08 | Fishery-dependent acoustics | [Interannual variability in distribution, size structure, and biomass of Chilean jack mackerel estimated from fishery-dependent acoustics](../docs/SCW16-Doc08-Fishery_Dependent_Acoustics.pdf) | Fishery-dependent acoustic estimates of distribution, size structure, and biomass. |
| SCW16-Doc09 | Central-north acoustic sdmTMB | [Jack mackerel acoustic density index in the central-north acoustic surveys, estimated using spatiotemporal models with sdmTMB](../docs/SCW16-Doc09-North_Acoustic_Density_sdmTMB.pdf) | Central-north acoustic density index estimated with sdmTMB. |
| SCW16-Doc10 | Chile CPUE sdmTMB | [Update of Chilean jack mackerel CPUE abundance index estimated by spatiotemporal SPDE-based models using sdmTMB](../docs/SCW16-Doc10-Chile_CPUE_sdmTMB.pdf) | Chilean CPUE abundance index update using sdmTMB/SPDE spatio-temporal models. |
| SCW16-Doc11 | Data QC | [Catch/Fishery Data Quality Checks](../docs/SCW16-Doc11-Jack_mack_data_qc.html) | Catch and fishery data quality-control checks. |
| SCW16-Doc12 | Chile CPUE INLA | [Standardization of Chilean jack mackerel CPUE fishery in central-southern Chile using Hierarchical Bayesian Models (INLA)](../docs/SCW16-Doc12-Spatio-temporal_modelling_INLA_CPUE.pdf) | Central-southern Chile CPUE standardization using hierarchical Bayesian INLA models. |
| SCW16-Doc13 | Peru length-weight | [LENGTH-WEIGHT RELATIONSHIP OF JACK MACKEREL (*Trachurus murphyi*) IN PERU AND ITS IMPLICATIONS FOR BIOMASS ESTIMATION](../docs/SCW16-Doc13-Peru_Length_Weight.pdf) | Peru length-weight relationship and biomass-estimation implications. |
| SCW16-Doc14 | Peru length-frequency | [TECHNICAL REPORT ON THE UPDATED LENGTH-FREQUENCY DATA FOR THE PERUVIAN JACK MACKEREL STOCK (FAR-NORTH STOCK) IN PERUVIAN JURISDICTIONAL WATERS](../docs/SCW16-Doc14-Peru_Length_Frequency.pdf) | Updated Peru length-frequency data for the far-north stock in Peruvian jurisdictional waters. |
| SCW16-Doc15 | Peru CPUE | [Standardization of Catch-per-unit-effort (CPUE) for Jack Mackerel (2015-2025) in Peruvian national jurisdictional waters](../docs/SCW16-Doc15-Peru_CPUE.pdf) | Peru CPUE standardization for the far-north stock using Peruvian national-jurisdiction fishery data. |
| SCW16-Doc16 | Peru acoustic index | [CONSIDERATIONS ON THE USE OF THE JACK MACKEREL ACOUSTIC INDEX IN PERUVIAN NATIONAL JURISDICTIONAL WATERS](../docs/SCW16-Doc16-Peru_Acoustic_Index.pdf) | Considerations for using the Peru acoustic index in assessment or supporting interpretation. |

## Number Hooks

Use these as the first memory pegs before trying to recall exact titles.

| Doc | Peg | What to say from memory |
|---|---|---|
| 00 | Zero starts the meeting | Preliminary agenda. |
| 01 | First assessment paper | Base SC13 model development for benchmark and MSE. |
| 02 | Two means coordination | CPUE priorities and working-paper plan. |
| 03 | Three looks backward | Meta-analysis of past CPUE papers. |
| 04 | Four moves to MSE | Operating models for jack mackerel MSE. |
| 05 | Five is biology | Biology inputs for the assessment. |
| 06 | Six starts the index block | Northern Chile acoustic spatio-temporal model. |
| 07 | Seven adjusts effort | Effort-creep correction for Chilean CPUE. |
| 08 | Eight stays acoustic | Fishery-dependent acoustic variability. |
| 09 | Nine is acoustic plus sdmTMB | Central-north acoustic density with sdmTMB. |
| 10 | Ten is CPUE plus sdmTMB | Chilean CPUE update with sdmTMB/SPDE. |
| 11 | Eleven checks the data | Catch/fishery data quality checks. |
| 12 | Twelve is CPUE plus INLA | Central-southern Chile CPUE with INLA. |
| 13 | Thirteen weighs Peru fish | Peru length-weight and biomass estimation. |
| 14 | Fourteen measures Peru lengths | Peru length-frequency for the far-north stock. |
| 15 | Fifteen catches Peru effort | Peru CPUE standardization for 2015-2025. |
| 16 | Sixteen sounds Peru waters | Peru acoustic-index considerations. |

## Contrast Pairs

These are the pairs most likely to be confused.

| Pair | Difference to remember |
|---|---|
| Doc02 vs Doc03 | Doc02 is the **forward plan** for SCW16 CPUE work; Doc03 is the **backward review** of earlier CPUE papers. |
| Doc04 vs Doc01 | Doc01 documents the **assessment base model**; Doc04 describes **operating models** for MSE. |
| Doc06 vs Doc09 | Both are acoustic and spatio-temporal; Doc06 is northern Chile acoustic STM, while Doc09 is central-north acoustic density with sdmTMB. |
| Doc10 vs Doc12 | Both are Chilean CPUE standardization papers; Doc10 uses sdmTMB/SPDE, while Doc12 uses hierarchical Bayesian INLA. |
| Doc07 vs Doc10/Doc12 | Doc07 is not a full alternate standardization framework; it focuses on informed effort-creep correction. |
| Doc05 vs Doc13/Doc14 | Doc05 is the broad biology paper; Doc13 and Doc14 are specific Peru biological-input reports. |
| Doc13 vs Doc14 | Doc13 is length-weight and biomass estimation; Doc14 is length-frequency composition data. |
| Doc15 vs Doc16 | Doc15 is fishery-dependent CPUE standardization; Doc16 is acoustic-index interpretation for Peru. |

## South-Central Chile CPUE Spatio-Temporal Model Comparison

Use this table when the CPUE session turns from paper titles to the actual
candidate index products. The main decision is not simply `sdmTMB` versus INLA;
it is which annual index product is most defensible for assessment, OM
conditioning, or sensitivity use.

| Feature | `sdmTMB` spatio-temporal model | `sdmTMB` spatio-temporal GLMM | INLA hierarchical Bayesian model |
|---|---|---|---|
| Main document | SCW16-Doc10 | SCW16-Doc10 | SCW16-Doc12 |
| Authors / institution | Payá / IFOP | Payá / IFOP | Vásquez and Sepúlveda / INPESCA |
| Data | Set-level purse-seine CPUE, 1994-2025 first semester | Same | Set-level purse-seine CPUE, 1994-2026 |
| Response | CPUE density, catch per set style index | CPUE density | `log(CPUE)` for positive sets |
| Distribution | Tweedie, log link | Tweedie, log link | Gaussian on log CPUE |
| Core year treatment | Year-to-year variability mainly through spatio-temporal random fields | Year fixed effects estimate annual abundance | Year fixed effects plus year-specific spatial fields |
| Seasonal effect | Not explicit in the main formula | Month fixed effect | Quarter fixed effect |
| Vessel effect | Smooth of hold capacity `s(hc)` | Smooth of hold capacity `s(hc)` | Nonlinear effect of log hold capacity, RW2 |
| Effort / trip duration | Not explicit as an offset in the reported formula | Not explicit as an offset in the reported formula | Includes log fishing-trip duration / days-from-port term |
| Environmental covariates | Not included | Not included | Tested SST, SST anomaly, and chlorophyll; selected parsimonious model excludes them despite small WAIC gains |
| Spatial structure | SPDE Matérn field with anisotropy | Spatial plus spatio-temporal random effects with anisotropy | SPDE Matérn spatial fields using INLA |
| Temporal spatial structure | AR1 spatio-temporal random field | IID spatio-temporal fields plus year fixed effects | Selected model uses independent spatial fields replicated by year; AR1 tested but not selected |
| Prediction grid | Original-data grid and regular grid tested | Original-data grid, fixed year effect, and regular grid tested | Regular spatial grid, averaged over space and quarters |
| Reference vessel | Regular-grid predictions use 1458 m3 hold capacity | Same | Standardized through model covariates; no identical 1458 m3 reference-vessel convention emphasized |
| Recommended index | Regular-grid and model-based index broadly similar, but final text has a start-year inconsistency | Recommended as year-effect index | Selected INLA model m2, independent yearly spatial fields |
| Recommended start year | Ambiguous: abstract says use from 2000; conclusion says use from 2010 | Same ambiguity in Doc10 | Full series 1994-2026 reported, but early years have low spatially referenced catch proportions |
| Recent trend | Lower recent values than the traditional trip GLM; regular-grid variants show recent decline | Decline in last 4-5 years, but random-effect prediction decline is smaller than fixed-effect index | 2026 broadly consistent with recent levels; uncertainty higher |
| Main strength | Directly models spatial redistribution and can generate spatial predictions | Produces a clean annual fixed-effect index that matches regular-grid predictions | Bayesian uncertainty, explicit model selection, and operational update through 2026 |
| Main benchmark concern | Choice of prediction grid and start year; no environmental covariates | Whether year fixed effects are preferable to full prediction-based indices | Whether positive-catch lognormal formulation and selected m2 structure should replace or augment `sdmTMB` products |

Practical benchmark takeaway: the `sdmTMB` GLMM and INLA model are the most
directly comparable candidate assessment indices. The `sdmTMB` GLMM is simpler
to plug into JJM because annual year effects produce the index directly, while
INLA is more explicitly Bayesian and has a clearer model-selection comparison.
The start-year inconsistency in Doc10 should be clarified before the benchmark
settles on an assessment-ready Chile CPUE series.

## Agenda Alignment And Gaps

Use this to connect the paper list to the preliminary agenda in `SCW16-Doc00`.

Quick count:

- **Broad topic alignment:** 15 agenda items align with SCW16 papers.
- **Strict paper-presentation alignment:** 8 agenda items are clearly tied to specific papers or near-exact paper topics.
- **Broadly covered papers:** Doc01, Doc02, Doc04, Doc05, Doc06, Doc07, Doc08, Doc09, Doc10, Doc12, Doc13, Doc14, Doc15, and Doc16.
- **Weak or missing agenda coverage:** Doc03 and Doc11.

| Agenda area | Aligned paper(s) | Memory note |
|---|---|---|
| Day 1 acoustic backscatter with `sdmTMB` | Doc09 | Central-north acoustic density using sdmTMB. |
| Day 1 northern Chile acoustic spatio-temporal index | Doc06 | Northern Chile acoustic STM. |
| Day 1 fishery-dependent acoustics | Doc08 | Commercial/fishery-dependent acoustic biomass and distribution. |
| Day 1 Peru acoustic review | Doc16 | Peru acoustic-index considerations and whether the series is usable as an index, diagnostic, or supporting context. |
| Day 2 CPUE session background | Doc02 | Forward-looking CPUE plan and decision points. |
| Day 2 CPUE methods/comparisons | Doc10, Doc12 | Chilean CPUE standardization methods: sdmTMB/SPDE vs INLA. |
| Day 2 effort-creep review | Doc07 | Effort-creep correction for Chilean CPUE. |
| Day 2 Peruvian CPUE report | Doc15 | Peru CPUE standardization for 2015-2025, with data extent and uncertainty needed for index ranking. |
| Day 3 SCW16-Doc01 summary | Doc01 | Base SC13 model and benchmark simplification. |
| Day 3 biological inputs | Doc05, Doc13, Doc14 | Biology assumptions, Peru length-weight, and Peru length-frequency inputs for assessment and OM conditioning. |
| Day 4 OM specifications and MCMC evaluations | Doc04 | Operating models for MSE. |

Main gaps:

| Gap | Why it matters |
|---|---|
| Doc03 has no obvious agenda presentation slot | It is important background for CPUE history, but the agenda emphasizes forward CPUE decisions rather than a formal CPUE-history review. |
| Doc11 has no obvious agenda presentation slot | Data QC is likely relevant to recap/report drafting and final decisions, but it is not named as a specific agenda topic. |
| Doc10 and Doc12 are method-aligned but not title-aligned | They fit Day 2 CPUE methods and comparative analysis, but the agenda does not list them as explicit paper presentations. |
| Some agenda topics still need explicit document crosswalks | Doc15 and Doc16 now cover the Peru CPUE and Peru acoustic-index reports; remaining unnumbered discussion topics should still be classified as formal papers, supporting reports, or workshop-report text. |

## Index Ranking Approach

The ranking target should be **index products**, not papers. A paper can provide
background, diagnostics, corrections, or data-quality evidence without being the
index that should enter the assessment likelihood, OM conditioning, or HCR input.

Start by ranking indices within each use case, then compare across use cases:

| Use case | Ranking question | Likely evidence |
|---|---|---|
| Assessment likelihood | Which index should directly inform recent abundance in the benchmark assessment? | Temporal coverage, contrast, uncertainty, diagnostics, independence, and ability to explain assessment residuals. |
| OM conditioning | Which index should help condition operating-model states and uncertainty? | Long-term representativeness, uncertainty propagation, AUC/ROC tracking skill against OM SSB, sensitivity to stock-structure assumptions, and consistency with biology. |
| MP/HCR input | Which index product is stable, repeatable, and operational enough for management-procedure use? | Reproducibility, annual availability, robustness to method updates, and clear treatment of availability/catchability. |
| Cross-check or diagnostic | Which index is most useful for explaining conflict but not necessarily fitting directly? | Contradictory trends, spatial coverage, fleet behavior, and sensitivity to environmental or availability shifts. |

Use a two-stage screen before assigning ranks:

1. **Eligibility screen:** exclude or demote indices with unresolved data-quality,
   sampling, duplication, or interpretation problems.
2. **Scoring screen:** score eligible indices on evidence that matters for the
   stated use case.

Suggested score scale: `0 = weak or unresolved`, `1 = usable with caveats`,
`2 = strong`, `3 = strongest among candidates`. Keep notes on the reason for
each score; the explanation is more important than the arithmetic.

| Criterion | Weight | What to check |
|---|---:|---|
| Representativeness | 3 | Does the index cover the right area, fleet, season, size range, and stock component? |
| Temporal coverage and update continuity | 2 | Is the time series long enough and current enough for benchmark and projections? |
| Standardization quality | 3 | Are fleet, spatial, temporal, environmental, and observation-level effects handled coherently? |
| Bias and catchability risk | 3 | Are effort creep, availability shifts, nearshore concentration, El Niño effects, and survey expansion issues treated explicitly? |
| Uncertainty treatment | 2 | Are standard errors, model uncertainty, and sensitivity runs available for assessment use? |
| OM tracking skill / AUC | 2 | If Doc04-style ROC/AUC diagnostics are available, does the index track changes in OM spawning biomass better than alternatives? |
| Independence and redundancy | 2 | Does the index add information, or is it duplicating another series from the same process? |
| Assessment behavior | 3 | Does it fit without dominating, causing unstable estimates, or contradicting other accepted evidence without explanation? |
| Operational repeatability | 1 | Can the index be reproduced annually with a stable protocol? |

How to use AUC from Doc04:

- Treat AUC as a **value-of-information diagnostic**: higher AUC means the index
  better discriminates changes in OM spawning biomass in that test.
- Use it most directly for **OM conditioning** and as supporting evidence for
  **assessment likelihood** ranking.
- Do not use AUC as a stand-alone rank. A high-AUC index can still be a poor
  management input if it has unresolved bias, weak representativeness, duplicated
  information, or unstable annual production.
- Compare AUC values only when they were calculated over the same period,
  reference biomass, OM hypothesis, and candidate-index set.
- Keep separate full-period and recent-period AUC scores when both are available;
  recent-period skill may matter more for near-term advice, while full-period skill
  may matter more for OM conditioning.

Candidate ranking worksheet:

| Candidate index/product | Main source papers | Index type | Evidence to pull before ranking | Provisional rank/use |
|---|---|---|---|---|
| Northern Chile acoustic STM index | Doc06 | Acoustic survey | Diagnostics, spatial coverage, survey design, treatment of spatio-temporal structure, uncertainty. | Score for assessment and OM use. |
| Central-north acoustic density index with `sdmTMB` | Doc09 | Acoustic survey | Compare against Doc06 for duplicate information, survey-area definition, model diagnostics, and uncertainty. | Score against Doc06 before choosing both. |
| Fishery-dependent acoustic biomass/distribution | Doc08 | Fishery-dependent acoustic | Evaluate bias from fishing behavior, spatial targeting, school detection, and usefulness as an independent abundance signal. | Likely diagnostic unless bias treatment is strong. |
| Chilean CPUE with effort-creep correction | Doc07 | CPUE correction | Decide whether the correction should modify an accepted CPUE series or serve as a sensitivity. | Score as corrected CPUE input or sensitivity. |
| Chilean CPUE with `sdmTMB`/SPDE | Doc10 | CPUE standardization | Diagnostics, effort/availability treatment, spatial extrapolation, uncertainty, and comparison with INLA. | Compare directly with Doc12. |
| Chilean CPUE with INLA | Doc12 | CPUE standardization | Diagnostics, hierarchical structure, uncertainty, spatial/temporal effects, and comparison with `sdmTMB`. | Compare directly with Doc10. |
| Peruvian CPUE report | Doc15 | CPUE standardization | Data coverage, observation unit, standardization model, uncertainty, diagnostics, and compatibility with Chilean/offshore series. | Score as a candidate far-north CPUE index once diagnostics are reviewed. |
| Peruvian acoustic-index considerations | Doc16 | Acoustic/support | Clarify whether this is a usable index, a critique of index use, or supporting interpretation; extract survey years, spatial extent, conversion assumptions, and uncertainty. | Rank as acoustic candidate only if it meets data-extent and uncertainty screens; otherwise keep as diagnostic/context. |
| Peru length-weight relationship | Doc13 | Biological support | Length range, sex/season/area coverage, sample size, fitted relationship, and effect on biomass conversion. | Use to screen biomass-estimation and biological-input assumptions. |
| Peru length-frequency update | Doc14 | Biological/composition support | Years, areas, fisheries/surveys, sample sizes, length bins, weighting/raising, and compatibility with model composition data. | Use to screen far-north composition inputs and stock-structure assumptions. |
| Existing SC13 abundance-index set | Doc01, Doc03 | Assessment baseline/background | Use as baseline for leave-one-out, leave-one-in, and reduced-index comparisons. | Baseline comparator, not automatically best. |
| Catch/fishery data QC evidence | Doc11 | Support screen | Use to screen catch/fishery-data dependent indices before final ranking. | Gatekeeper, not an abundance index. |

Rank labels:

| Label | Meaning |
|---|---|
| A: primary candidate | Strong enough to consider in the benchmark assessment or OM conditioning for the stated use case. |
| B: secondary/sensitivity | Useful, but either redundant, less stable, or dependent on unresolved assumptions. |
| C: diagnostic/context | Valuable for interpretation, but not a direct fitting index yet. |
| Hold | Do not rank until missing diagnostics, data QC, or paper-number mapping is resolved. |

Practical rule: rank acoustic indices against acoustic alternatives first, CPUE
indices against CPUE alternatives first, and only then decide the cross-type
balance in the assessment model. This avoids treating a precise but biased CPUE
series as better than a noisy but more representative acoustic series, or vice
versa.

## Five-Minute Drill

1. Write the numbers `00` through `12` down the page.
2. Fill in only the short recall names: Agenda, Base model, CPUE plan, CPUE past,
   MSE OMs, Biology, North acoustic STM, Effort creep, Fishery-dependent acoustics,
   Central-north acoustic sdmTMB, Chile CPUE sdmTMB, Data QC, Chile CPUE INLA,
   Peru length-weight, Peru length-frequency, Peru CPUE, Peru acoustic.
3. Check the title table and mark misses.
4. Repeat from the title side: read a title and name the document number.
5. Spend extra time only on the contrast pairs.

## Flashcards

| Front | Back |
|---|---|
| What is SCW16-Doc00? | Preliminary agenda; workshop/session flow and expected outputs. |
| What is SCW16-Doc01? | Developments of the base SC13 model for benchmark and MSE considerations; the base JJM assessment model paper. |
| What is SCW16-Doc02? | CPUE coordination priorities and working-paper plan; forward-looking CPUE benchmark planning. |
| What is SCW16-Doc03? | Meta-analysis of CPUE papers on jack mackerel from 2022-2025; backward-looking CPUE synthesis. |
| What is SCW16-Doc04? | Operating Models for Jack Mackerel MSE; MSE operating-model structure. |
| What is SCW16-Doc05? | JM_Biology; biological inputs for model 1.14. |
| What is SCW16-Doc06? | Northern Chile acoustic survey data modeled spatio-temporally for a standardized abundance index. |
| What is SCW16-Doc07? | Effort-creep correction for the Chilean jack mackerel CPUE index. |
| What is SCW16-Doc08? | Fishery-dependent acoustics paper on distribution, size structure, and biomass variability. |
| What is SCW16-Doc09? | Central-north acoustic density index estimated with sdmTMB. |
| What is SCW16-Doc10? | Chilean CPUE abundance-index update using sdmTMB/SPDE spatio-temporal models. |
| What is SCW16-Doc11? | Catch/fishery data quality checks. |
| What is SCW16-Doc12? | Central-southern Chile CPUE standardization using hierarchical Bayesian INLA models. |
| What is SCW16-Doc13? | Peru length-weight relationship and implications for biomass estimation. |
| What is SCW16-Doc14? | Updated Peru length-frequency data for the Peruvian jack mackerel far-north stock. |
| What is SCW16-Doc15? | Peru CPUE standardization for jack mackerel from 2015-2025 in Peruvian national jurisdictional waters. |
| What is SCW16-Doc16? | Considerations on using the jack mackerel acoustic index in Peruvian national jurisdictional waters. |
