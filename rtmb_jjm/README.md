# RTMB scaffold for jjm.tpl

This folder contains a **pure-R RTMB** scaffold for translating `src/jjm.tpl`.
It focuses on input parsing and data mapping from the ADMB-style `.dat` and
`.ctl` files used in `assessment/input` and `assessment/config`.

## Files

- `jjm_rtmb.R`: RTMB model scaffold and data-mapping utilities.
- `run_rtmb.R`: simple runner to build the RTMB object.
- `prepare_rtmb_debug()`: helper in `jjm_rtmb.R` to build a browser-ready debug environment with parsed data, parameters, and convenience functions for stepping through `jjm_model()`.

## Example

```bash
Rscript run_rtmb.R ../assessment/input/1.14.dat ../assessment/config/h1_1.14.ctl
```

## Interactive debugging

To prepare the current RTMB state and step through the model interactively in R:

```r
source("jjm_rtmb.R")

dbg <- prepare_rtmb_debug(
  "../assessment/input/1.14.dat",
  "../assessment/config/h1_1.14.ctl",
  par_fn = "../assessment/results/h1_1.14.par",
  strict = FALSE
)

dbg$step_model()
```

The returned environment contains:

- `data`
- `parameters`
- `run_model()`
- `step_model()`

If you want those objects copied into the global environment for ad hoc inspection, set `attach = TRUE`.

## Status

- Data and configuration parsing are mapped to the ADMB naming scheme.
- The RTMB objective function is a placeholder and still needs a full
  translation from `jjm.tpl` (population dynamics + likelihoods).

## Next steps (from jjm.tpl)

- Translate data transformations in `LOCAL_CALCS` blocks.
- Implement recruitment, growth, mortality, selectivity, and population
  dynamics.
- Implement likelihoods for:
  - Catch biomass
  - Indices
  - Age and length composition
- Add derived quantities and reporting.
