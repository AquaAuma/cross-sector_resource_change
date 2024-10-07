# national exposure to cross-sector climate impacts

Code and analysis on short-term and long-term exposure to climate change across sectors of fisheries, water, and agriculture.

Project aim: assess the cross-sector exposure to shocks and long-term change in water, agriculture, and fisheries resources from climate change and how they challenge current international trade and governance structures.

Data from the [ISIMIP project](https://www.isimip.org).

This project is a product of the Minerva grant ["Climate Change and Great Power Competition"](https://minerva.defense.gov/Research/Funded-Projects/Article/2957063/climate-change-and-great-power-competition/).

## Structure of the repository

-   **code/** contains the R code files to process the ISIMIP simulations, explore methods, apply calculations to the data, and produce figures
-   **data/** contains the primary data files used in the project as well as the processed data files.
-   **functions/** contains the generic functions to complete the data processing
-   **figures/** contains the generated figures

## Data Access

-   The ISIMIP data files are not stored on this repository as ISIMIP has its own repository and data policy (see the [ISIMIP open repository and data policy](https://www.isimip.org/gettingstarted/data-access/)). For this project, the ISIMIP simulations were obtained by requesting access to the [DKRZ server](https://www.isimip.org/dashboard/accessing-isimip-data-dkrz-server/) in agreement with the project coordinators. Additional crop files were accessed on demand by requesting Jonas Jägermeyr.
-   The EEZ land union shapefile is open access and available on the [marine regions website](https://www.marineregions.org/downloads.php), and the most recent version was used (version 3 released in 2020).
-   The data on regional trade agreements was accessed on the World Trade Organization (WTO) website, and downloaded on April 2024.

## Code steps

### Data processing steps

The following code files were applied to complete the data processing:

-   **global_trade_data**: process the regional trade agreement database and select regions of interest

-   **match_regions**: create a unified list of territories and country names by merging the EEZ land union data and the country data from WTO.

-   **data_processing**: merging historical and future ISIMIP simulations, homogenize spatial and temporal resolutions, data aggregation from grid cells to global and country scales, time-series of relative resource change

-   **1.cross-sector_long-term**: calculate % difference resource change and probabilities of long-term change

    **2.cross-sector_shocks**: identify resource shocks and calculate probabilities of short-term change

### Manuscript analyses and figures

-   **figure_1**: code to reproduce figure 1 plotting short-term and long-term country exposure across climate experiments

-   **figure_2**: code to reproduce figure 2 with maps of change, synchrony, and compensation for ssp 5.85 and alternative figure for ssp 1.26

-   **figure_3**: code to reproduce figure 3 on country rankings for ssp 5.85, alternative figure for ssp 1.26, and descriptive statistics

-   **figure_4**: code to reproduce figure 4 on country-to-RTA ratios for ssp 5.85, alternative figure for ssp 1.26, alternative figure with maps, and descriptive statistics

-   **outline_prelim_figures**, **manuscript_prelim_figures**: old code that may be deleted later on

### Additional method steps

These are all RMarkdown documents because they include step-wise description of the methods with illustrative plots for each step. The files also include supplementary analyses and comparisons:

-   **shock_detection**: decomposing how shocks are measured on ISIMIP simulations

-   **smooth_span_si**: sensitivity analyses of aggregated shock metrics to the smooth span

-   **si_shock_frequency**: sensitivity analysis of shock frequency temporal trends across sectors and SSPs

-   **resource_change_metrics**: comparing the SSP effects across metrics of resource change

-   **proba_independence**: test of probability independence assumed for calculation of the compensation metrics short term and long term

-   **resource-to-RTA**: decomposition of the country-to-RTA ratios from ecological model temporal trends to aggregated metrics for the EU and MERCOSUR.

## Processed data products

-   **data/data_processing**: processed data files by sector and aggregation scale

-   **data/long-term_change**: probabilities data files by aggregation scale and metric type

-   **data/short-term_change**: probabilities data files by aggregation scale and metric type

Will be archived on Zenodo

## Contributions

Coding: Aurore Maureaud
