# code

### Data processing steps

The following code files were applied to complete the data processing:

-   **global_trade_data.R**: process the regional trade agreement database and select regions of interest

-   **match_regions.R**: create a unified list of territories and country names by merging the EEZ land union data and the country data from WTO.

-   **data_processing.R**: merging historical and future ISIMIP simulations, homogenize spatial and temporal resolutions, data aggregation from grid cells to global and country scales, time-series of relative resource change

-   **get_nres.R**: extracting the number of resources per country

-   **1.cross-sector_long-term.R**: calculate % difference resource change and probabilities of long-term change for p(X>=2)

-   **1.bis.cross-sector_long-term.R**: calculate % difference resource change and probabilities of long-term change for p(X>=50%)

-   **2.cross-sector_shocks.R**: identify resource shocks and calculate probabilities of short-term change for p(X>=2)

-   **2.bis.cross-sector_shocks.R**: identify resource shocks and calculate probabilities of short-term change for p(X>=50%)

### Manuscript analyses and figures

-   **figure_1.R**: code to reproduce revised figure 1 showing maps of cross-sector change and number of resource per country and related supplementary figures

-   **figure_2.R**: code to reproduce revised figure 2 with shock versus gradual change plotted with labelled countries for ssp 5.85 and per ssp and related supplementary figures

-   **figure_3.R**: code to reproduce revised figure 3 on maps of synchrony and compensation for ssp 5.85 and related supplementary figures

-   **figure_4.R**: code to reproduce revised figure 4 country rankings stratified by the number of resources per country

-   **figure_5.R**: code to reproduce figure 5 on the RTA analysis

### Supplementary files and analyses

All RMarkdown documents because they include step-wise description of the methods with illustrative plots for each step, and supplementary analyses: 

-   **shock_detection.Rmd**: decomposing how shocks are measured on ISIMIP simulations

-   **cross-sector_shocks.Rmd**: method to aggregate shocks per year and across 30 years

-   **smooth_span_si.Rmd**: sensitivity analyses of aggregated shock metrics to the smooth span

-   **si_shock_frequency.Rmd**: sensitivity analysis of shock frequency temporal trends across sectors and SSPs

-   **resource_change_metrics.Rmd**: comparing the SSP effects across metrics of resource change

-   **proba_independence.Rmd**: test of probability independence assumed for calculation of the compensation metrics short term and long term

-   **resource-to-RTA.Rmd**: decomposition of the country-to-RTA ratios from ecological model temporal trends to aggregated metrics for the EU and MERCOSUR.

-   **si_compare_methods_proba.Rmd**: testing the difference between p(X>=2) and p(X>=50%)

-   **si_number_of_resources.Rmd**: testing the sensitivity of p(X>=2) to the number of resources per country

-   **si_baseline_shocks.Rmd**: comparing past and future shock probabilities

