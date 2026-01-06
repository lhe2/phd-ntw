# 2025 readme

the `2025/` directory includes code for summer 2025 recovery (tdt) expts and summer 2025 update of the ntw expts. each expt has its own `data/` and `figs/` folder.

## ntw/...

-   `data_*.ext`: data pre-processing scripts (i.e. import, cleaning, wrangle, etc...)

    -   `data-import.Rmd`: just handles 2025 data import/cleaning

    -   `tidy` and `wrangle` scripts handle 2023-25 data collation and wrangling! wrangle exports into an `.RData` object for consistent handling.

-   `analysis_*.Rmd`: stats/viz of 2023-25 ntw results

-   `utils.R`: provides utility functions for wrangling/tidying/analysis scripts.

-   `archive/`: primarily for archival of old versions of the cleaning/tidying scripts.

## tdt/...

-   `cleaning.Rmd`: imports data from gsheets and preps for wrangling.

-   `wrangle.Rmd`: puts data into tidy format. is purled for use in analysis scripts.

-   `analysis_*.Rmd`: visualisation and statistics.

    -   use the most recent "round" of analysis for the stats/viz (reflects the most up-to-date analysis with all exptal cohorts)
