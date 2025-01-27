###########################################################################
# Vieira-Alencar et al. Hostspot getting hotter (manuscript) --------------
###########################################################################
# main data ---------------------------------------------------------------

list <- read.csv(here::here("data", "raw-data", "list.csv"))
kba <- read.csv(here::here("data", "processed", "kba.csv"))
uso <- read.csv(here::here("data", "processed", "usoInfo.csv"), header = TRUE, check.names = FALSE) #without Hylaeamys acritus and Juscelinomys huanchacae
gap <- read.csv(here::here("data", "processed", "gap_hgh_full.csv"))
db_unique <- read.csv(here::here("data", "raw-data", "baseunique.csv"))

# threat data -------------------------------------------------------------

iucn <- read.csv(here::here("data", "raw-data", "iucn-new.csv"))

# Figure 1 - table --------------------------------------------------------

sppRich <- read.csv(here::here("data", "raw-data", "sppRichnessTime.csv"), header = TRUE, check.names = FALSE)
