# 2024 readme

the `2024/` directory includes code for summer-fall 2024 ntw repeat expts (growth & fecundity) + ox str molecular expts (pilot + final).

## analysis scripts

numbered directories are part of the analysis workflow. scripts related to the same analysis share the same file name. there is some extensive usage of `here()` in these scripts that does the heavy-lifting for the relative path navigation.

-   workflow

    -   `01-cleaning/`: takes raw gsheets and outputs cleaned ones into `data/`

    -   `02-wrangle/`: takes cleaned `data/` and manipulates into suitable formats for downstream analyses.

        -   `.Rmd`s are purled into `.R`s, which are sourced by the corresponding `03-<task>/` script.

        -   some analyses have an additional `<analysis>_util.R` script supplying other helper/convenience functions.

    -   `03-<task>/`: sources data objects from `02-wrangle/<analysis>.R`. necessary libraries should be loaded in on a per-script basis here (rather than being imported from ``` 02-wrangle/``<analysis>``.R ```).

        -   `stats`: does modeling

        -   `viz`: does visualisation

-   analyses:

    -   active

        -   `ntw-compare`: 2023 + 2024 data. for main growth/dev/fertility stats.

        -   `ox`: 2024 data only. for analysis of mol assays from bugs used in actual ox growth expt

        -   `ox-growth`: 2024 data only. for growth/dev of bugs used for ox str molecular work (incls pilot + actual expt)

    -   archived

        -   `entsoc`: 2023 data only. for entsoc 2024 poster

        -   `ntw`: 2024 data only. for ntw growth/dev. superseded by `ntw-compare`

        -   `tents`: 2024 data only. for ntw fertility stats. superseded by `ntw-compare`

-   misc files

    -   `feasibility.Rmd`: figs and stats for 2025 feasibility document, exported to corresponding directories in `data/` and `figs/`. (doc writing is in an entirely separate repo!!)

## data/...

top level contains cleaned outputs from `01-cleaning/` scripts that go into the `02-wrangle/` scripts. upon cleaning, a copy of the output is automatically saved into `./archive/` with date (`yymmdd`) prepended into filename. for the most part, this directory is autopopulated by the cleaning scripts.

`./plates/` contains plate data of molecular ox str assays that are manually added.

## figs/...

instead of by date like in `2023/`, loosely grouped by general project (see `[expt]`s above) \> response (responses grouped in subfolders if there's a lot). versions indicated by the appended date in the file name (`_yymmdd`). files are manually saved into this directory.
