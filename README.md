# MS: Hotspot getting hotter: Increased knowledge on Tetrapod endemism, habitat loss and the plight of the most threatened savanna in the world

**Researchers involved are: João Paulo dos Santos Vieira-Alencar, Ana Paula Carmignotto, Ricardo J. Sawaya, Luís Fábio Silveira, Paula Hanna Valdujo & Cristiano de Campos Nogueira**

This repository is a version control for the manuscript submitted to [Biological Conservation](https://www.sciencedirect.com/journal/biological-conservation) on 07th December.

Herein we include R scrips, raw-data and outputs of the paper. This repository was created purely to promote full reproductibility in an easy and transparent way. We also hope that our scripts might stimulate and facilitate further analyses with a similar framework.

If you have any doubt, please contact JP Vieira-Alencar at: joaopaulo.valencar@gmail.com

## Analyses reproducibility

This repository incorporates a package version management system facilitated by the `renv` package, ensuring the proper functionality of all functions utilized in the scripts. To leverage this functionality, execute the following code after initializing the Rproj file:

````
#Install the "renv" package
install.packages("renv")

#Restore versions of the utilized packages
renv::restore()
````

**Important Note:**

The provided code will automatically install the required packages, aligning them with the same versions used during the preparation of the scripts. This ensures the reproducibility of the analyses.

## The repository is organized as follow:
  - HotterHotspot.Rproj: R project for the reproductibility of the analyses;
  - data/
    - processed/
      - comming...
    - raw-data/
      - baseunique.csv
      - dbFull_endemics.csv
      - iucn-new.csv
      - list.csv
      - sppList.csv
      - sppRichnessTime.csv
      - table hotspots refs.xlsx
      - typeloc-percs.xlsx
  - outputs/
    - figures/
    - rasters/
    - shapes/
    - tables/
    - tests/
    - uso-raw/
  - QGIS/
    - shapes/
  - R/
    - C_hgh.R: loading objects;
    - D_analyses.R (working - review usefulness from line 99 forward)
    - D_hgh-iucn.R
    - D_iucn-update.R
    - D_land-use.R
    - S_Figs.R