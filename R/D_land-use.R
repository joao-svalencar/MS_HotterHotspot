# Vieira-Alencar et al. Hostspot getting hotter (manuscript)

# LAND-USE DATA PROCESSING ------------------------------------------------
# This script was used to summarize Google Earth Engine outputs into a single file:
# "usoInfo.csv"
# Check C_hgh.R to load it into your workspace

# Creating classification vectors -----------------------------------------

nat <- c("3","4","11","12","23","29","33")
ant <- c("9","15","20","21","24","25","30","39","40","41","46","47","48","62")

# Creating lulc year vector -----------------------------------------------

lulcyear <- c(2000, 2005, 2010, 2015, 2020)

# Creating an empty object to insert the uso data -------------------------

usoFull <- NULL

# Reading files -----------------------------------------------------------

# uso.csv files were produced on GEE using the MapBiomas database
#sppAreas <- read.csv(here::here("data", "processed", "sppAreasFull.csv"), header = TRUE, check.names = FALSE) #species and rangesize

for(j in 1:length(lulcyear))
{
  uso <- read.csv(here::here("outputs","uso-raw", paste("uso", lulcyear[j], ".csv", sep="")), header = TRUE, check.names = FALSE) #check year
  head(uso)
  uso[is.na.data.frame(uso)] <- 0 #attributing zeros to NAs
  
  # Calculating range within Cerrado ----------------------------------------
  
  for(i in 1:dim(uso)[1])
  {
    uso$range[i] <- sum(uso[i, 2:22]) #range within Cerrado, according to land-use data
  }
  
  # Calculating remaining natural area --------------------------------------
  
  for(i in 1:dim(uso)[1])
  {
    uso$rangeNat[i] <- sum(uso[i, nat]) #sum of natural areas, according to land-use data
  }
  
  # Calculating total anthropic area ----------------------------------------
  
  for(i in 1:dim(uso)[1])
  {
    uso$rangeAnt[i] <- sum(uso[i, ant]) #sum of anthropic areas, according to land-use data
  }
  
  # Calculating  proportional natural area ----------------------------------
  
  for(i in 1:dim(uso)[1])
  {
    uso$percNat[i] <- round(uso$rangeNat[i]/uso$range[i], digits = 4)
  }
  
  #Calculating proportional forest plantation area -------------------------
  
  for(i in 1:dim(uso)[1])
  {
    uso$percForPlant[i] <- round(uso$"9"[i]/uso$range[i], digits = 4)
  }
  
  # Calculating proportional pastureland area -------------------------------
  
  for(i in 1:dim(uso)[1])
  {
    uso$percPast[i] <- round(uso$"15"[i]/uso$range[i], digits = 4)
  }
  
  # Calculating proportional sugar cane area --------------------------------
  
  for(i in 1:dim(uso)[1])
  {
    uso$percSugCan[i] <- round(uso$"20"[i]/uso$range[i], digits = 4)
  }
  
  # Calculating proportional soybean area -----------------------------------
  
  for(i in 1:dim(uso)[1])
  {
    uso$percSoy[i] <- round(uso$"39"[i]/uso$range[i], digits = 4)
  }
  
  uso$lulcYear <- paste(lulcyear[j])
  

  # selecting variables of interest -----------------------------------------

  uso <- uso[,c(1, 23:31)]
  
  # Creating final object ---------------------------------------------------

  usoFull <- rbind(usoFull, uso)
}

head(usoFull)

unique(usoFull$lulcYear) # Checking included years

write.csv(usoFull, here::here("data", "processed", "usoInfo.csv"), row.names = FALSE)