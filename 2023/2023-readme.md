# 2023 readme

the `2023/` directory includes code for spring 2022-fall 2023 ntw experiments. organisation in this is messy bc i didnt rlly plan ahead 😎

## relevant scripts

(listed in the order they should be used)

-   `cleaning_[EXPT].Rmd`: data cleaning scripts (pulls data from the gsheets); outputs into `./data/`

    -   `ntw`: larval growth/dev stuff for the big 3 expts + F1s

    -   `tents`: parsing out adult/tents stuff (longevity, parents, hatching)

-   `helpers_[EXPT].R`: data/library loading & pre-wrangling for the analysis scripts; defines some convenience functions/aesthetic objects to be used for analyses

    -   `ntw` and `tents` usage is same as the above

-   `analyses_[EXPT].Rmd`: analysis scripts for different types of expts (modeling stuff, figure generation). data/things is loaded from the corresponding `helper` script

    -   the big 3 (in order of when i ran them during that time period mentioned above). larval growth/dev data collected for all; adult fertility/F1 data collected for `NTs` only

        -   `temps`: 2x2 temps
        -   `acc`: looking for damage accumulation
        -   `NTs`: diff nts

    -   bonus/followups

        -   `ctrls`: ntw expt-wide & internal controls for the big 3 expts
        -   `tents`: adult fertility followups for `NT` bugs
        -   `F1s`: 2nd gen development/growth stuff for `NT` bugs

## figs/...

generally these are grouped by the date they were created bc i usually only use the most recent version of a figure. good luck navigating this folder 👍

new figures are tracked on google drive and are in the corresponding `figs/` directory for each year (see above)

figures are vaguely labeled as `[expt]_[response]` :

-   instar/temp: experiment type
-   growth/surv: growth or survival analyses

and might be further subsetted:

-   a/l: adult or larva
-   A/B: cohort

## etc/...

this is an untracked directory containing archived versions of data and code for reference (from 2023/when this repo was first set up).
