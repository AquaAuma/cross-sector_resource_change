# national exposure to cross-sector climate impacts

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14534829.svg)](https://doi.org/10.5281/zenodo.14534829)

Code and analysis on short-term and long-term exposure to climate change across sectors of fisheries, water, and agriculture related to the following manuscript: 
Maureaud A., Ramsay K., Kasptein E., Blanchard J., Gosling S., Jägermeyr J., Sandall E., Randrianarisoa N., Eddy T., Petrik C., Schewe J., Pinsky M. *Sudden and gradual climate impacts threaten global food systems in a world connected by trade*

Project aim: assess the cross-sector exposure to shocks and long-term change in water, agriculture, and fisheries resources from climate change and how they challenge current international trade and governance structures.

Data from the [ISIMIP project](https://www.isimip.org).

This project is a product of the Minerva grant ["Climate Change and Great Power Competition"](https://minerva.defense.gov/Research/Funded-Projects/Article/2957063/climate-change-and-great-power-competition/).

## Structure of the repository

-   **code/** contains the R code files to process the ISIMIP simulations, explore methods, apply calculations to the data, produce figures, and conduct supplementary analyses
-   **data/** contains the primary data files used in the project as well as the processed data files.
-   **functions/** contains the generic functions to complete the data processing
-   **figures/** contains the generated figures

## Primary data sources

-   The ISIMIP data files are not stored on this repository as ISIMIP has its own repository and data policy (see the [ISIMIP open repository and data policy](https://www.isimip.org/gettingstarted/data-access/)). For this project, the ISIMIP simulations were obtained by requesting access to the [DKRZ server](https://www.isimip.org/dashboard/accessing-isimip-data-dkrz-server/) in agreement with the project coordinators. 
-   Additional crop files (observational yield and crop areas) were accessed on demand by requesting Jonas Jägermeyr and are related to the [Jägermeyr et al. (2021) publication](https://doi.org/10.1038/s43016-021-00400-y)
-   The EEZ land union shapefile is open access and available on the [marine regions website](https://www.marineregions.org/downloads.php), and the most recent version was used (version 3 released in 2020).
-   The data on regional trade agreements was accessed on the [World Trade Organization (WTO) website](https://www.wto.org/english/tratop_e/region_e/region_e.htm), and downloaded on April 2024.

## Processed data files

-   **data/data_processing**: processed data files by sector and aggregation scale

-   **data/long-term_change**: probabilities data files by aggregation scale and metric type, time-series files will be made available at a later stage

-   **data/short-term_change**: probabilities data files by aggregation scale and metric type, time-series files will be made available at a later stage

## Install and run times

- Github repository run time: <5 minutes
- Post-data processing run time: about 10 minutes
- Method demo run time (.Rmd files in **code/**): <5 minutes

## R and R package versions

- R software version XX
- R packages and versions: sf (), dplyr (), tidyverse (), raster (), exactextractr (), readxl (), ncfd4 (), ggplot2 (), abind (), here (),
gridExtra (), ggrepel (), colorspace (), rnaturalearth (), rnaturalearthdata (), ggpubr (), ggExtra (),
ggpattern (), RColorBrewer (), pracma (), zoo ()

## Contributions

Coding: Aurore Maureaud
