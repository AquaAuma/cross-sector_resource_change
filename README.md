# cross-sector resource change & impacts on trade and cooperation

Code and analysis on short-term and long-term exposure to climate change across sectors of fisheries, water, and agriculture.

Project aim: assess the cross-sector exposure to shocks and long-term change in water, agriculture, and fisheries resources from climate change and how they challenge current international trade and governance structures.

Data from the [ISIMIP project](https://www.isimip.org).

This project is a product of the Minerva grant ["Climate Change and Great Power Competition"](https://minerva.defense.gov/Research/Funded-Projects/Article/2957063/climate-change-and-great-power-competition/).

## Structure of the repository

-   **code** contains the R code files to process the ISIMIP simulations, explore methods, apply calculations to the data, and produce figures
-   **data** contains the data files used in the project (EEZ land union shapefile and observational crop yield) as well as the processed data files.
-   **functions**
-   **figures**

## Data Access

-   The ISIMIP data files are not stored on this repository as ISIMIP has its own repository and data policy (see the [ISIMIP open repository and data policy](https://www.isimip.org/gettingstarted/data-access/)). For this project, the ISIMIP simulations were obtained by requesting access to the [DKRZ server](https://www.isimip.org/dashboard/accessing-isimip-data-dkrz-server/) in agreement with the project coordinators. Additional crop files were accessed on demand by requesting Jonas Jägermeyr.
-   The EEZ land union shapefile is open access and available on the [marine regions website](https://www.marineregions.org/downloads.php).
-   The data on regional trade agreements was accessed on the World Trade Organization website.
-   The data on foreign owned land was accessed on the [land matrix database website](https://landmatrix.org)
-   The data on foreign fishing agreements was retrieved from the [Sea Around Us database website](https://www.seaaroundus.org/data/#/eez).

## Data processing steps

0.  data_processing: merging historical and future simulations, homogenize spatial and temporal resolutions, data aggregation from grid cells to global and country scales, time-series of relative resource change
1.  cross-sector_long-term: calculate % difference resource change and probabilities of long-term change
2.  cross-sector_shocks: identify resource shocks and calculate probabilities of short-term change
3.  figure_x.R: code to reproduce figures and related supplementary figures

## Final data products

Will be archived on Zenodo

## Contributions

Coding: Aurore Maureaud
