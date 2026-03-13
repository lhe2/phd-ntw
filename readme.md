# ntw readme

expts/dates/etc are listed with the oldest first.

## field seasons and expts

| field season | expt | notes |
|----|----|----|
| 2023 spring | temps | 2x2 |
| 2023 spring | acc | damage accumulation |
| 2023 spring - fall | NTs (same as ntw) | dev and tents initial |
| 2023 fall | F1s | hatchlings from NTs expt |
| 2024 summer | ntw | dev, partial tents update (longevity; virgin eggs) |
| 2025 summer | ntw | dev and tents update (similar to 2023) |

further details about year-specific workflows and expts are listed below (might be somewhat inaccurate).

## expt details contents

### 2023-v0

the earliest iteration of this repo didn't really have a good workflow 🤪, but see figure filenaming scheme below:

-   figures are vaguely labeled as `[expt]_[response]` :

    -   instar/temp: experiment type

    -   growth/surv: growth or survival analyses

-   and might be further subsetted:

    -   a/l: adult or larva

    -   A/B: cohort

### 2023

-   the big 3 expts (in order of when i ran them during that time period mentioned above). larval growth/dev data collected for all; adult fertility/F1 data collected for `NTs` only

    -   `temps`: 2x2 temps
    -   `acc`: looking for damage accumulation
    -   `NTs`: diff nts

-   bonus/followups expts

    -   `ctrls`: ntw expt-wide & internal controls for the big 3 expts
    -   `tents`: adult fertility followups for `NT` bugs
    -   `F1s`: 2nd gen development/growth stuff for `NT` bugs

### 2024

-   `ntw-compare`: 2023 + 2024 data. for main growth/dev/fertility stats. (might not actually exist anymore?)

### 2025

-   data processing/analysis workflow mostly standardised (see [v2026.01 tag](https://github.com/lhe2/ntw/releases/tag/v2026.01)): each year is processed separately during import/tidying and combined into a single df for wrangle/analysis.

-   data setup in 2025 is most similar to 2023 but with some differences.

## workflow version history (reference only)

the current (2025 summer and onwards) analysis workflow in `_ntw/scripts/` should be compatible with data from all field seasons (see root [readme](/README.md)), but comments on previous versions of workflows/directory structures for the 2023 and 2024 field seasons (`_ntw/.../archive/`) are included below for reference (but also has some out-of-date changes).

### 2023

1.  `cleaning_[EXPT].Rmd`: data cleaning scripts (pulls data from the gsheets); outputs into `./data/`

    -   `ntw`: larval growth/dev stuff for the big 3 expts + F1s

    -   `tents`: parsing out adult/tents stuff (longevity, parents, hatching)

2.  `helpers_[EXPT].R`: data/library loading & pre-wrangling for the analysis scripts; defines some convenience functions/aesthetic objects to be used for analyses

    -   `ntw` and `tents` usage is same as the above

3.  `analyses_[EXPT].Rmd`: analysis scripts for different types of expts (modeling stuff, figure generation). data/things is loaded from the corresponding `helper` script

### 2024

⚠️ as of 2026-01-05: `_ox/`, `outputs/entsoc/`, `outputs/feasibility/` use a similar workflow. see [repo update status](/README.md#script-update-status).

-   `01-cleaning/`: takes raw gsheets and outputs cleaned ones into `data/`

-   `02-wrangle/`: takes cleaned `data/` and manipulates into suitable formats for downstream analyses.

-   `.Rmd`s are purled into `.R`s, which are sourced by the corresponding `03-<task>/` script.

-   some analyses have an additional `<analysis>_util.R` script supplying other helper/convenience functions.

-   `03-<task>/`: sources data objects from `02-wrangle/<analysis>.R`. necessary libraries should be loaded in on a per-script basis here (rather than being imported from `02-wrangle/<analysis>.R`).

    -   `stats`: does modeling

    -   `viz`: does visualisation
