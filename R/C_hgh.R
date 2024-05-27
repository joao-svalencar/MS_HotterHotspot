# main data ---------------------------------------------------------------

list <- read.csv(here::here("data", "raw-data", "list.csv"))
uso <- read.csv(here::here("data", "processed", "usoInfo.csv"), header = TRUE, check.names = FALSE) #without Hylaeamys acritus and Juscelinomys huanchacae
#range <- read.csv(here::here("data", "processed", "sppAreasFull.csv")) #tem no listnew
#sppAreas <- read.csv(here::here("data", "processed", "sppAreasFull.csv"), header = TRUE, check.names = FALSE)

# threat data -------------------------------------------------------------

iucn <- read.csv(here::here("data", "raw-data", "iucn-new.csv"))
#salve <- read.csv(here::here("data", "raw-data", "tets-salve.csv"))

# Figure 1 - table --------------------------------------------------------

sppRich <- read.csv(here::here("data", "raw-data", "sppRichnessTime.csv"), header = TRUE, check.names = FALSE)



# checking usefullness ----------------------------------------------------
cat.sources <- read.csv(here::here("outputs", "tables", "cat.sources.csv"))
db_unique <- read.csv(here::here("data", "raw-data", "baseunique.csv"))

# possibly delete soon ----------------------------------------------------
#ucs <- read.csv(here::here("data", "processed", "UCs_I-IV.csv"))
#kba <- read.csv(here::here("data", "processed", "kba.csv"))
#gap <- read.csv(here::here("data", "processed", "gap_hgh_full.csv"))


