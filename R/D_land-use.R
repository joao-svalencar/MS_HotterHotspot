# Creating classification vectors -----------------------------------------

nat <- c("3","4","11","12","23","29","33")
ant <- c("9","15","20","21","24","25","30","39","40","41","46","47","48","62")

# Reading files -----------------------------------------------------------

# uso.csv files were produced on GEE using the MapBiomas database
sppAreas <- read.csv(here::here("data", "processed", "sppAreasFull.csv"), header = TRUE, check.names = FALSE)
uso <- read.csv(here::here("outputs","uso-raw", "uso2020.csv"), header = TRUE, check.names = FALSE)
uso[is.na.data.frame(uso)] <- 0 #attributing zeros to NAs

# Calculating range within Cerrado ----------------------------------------

for(i in 1:dim(uso)[1])
{
  uso$range[i] <- sum(uso[i, 2:22])
}

# Calculating remaining natural area --------------------------------------

for(i in 1:dim(uso)[1])
{
  uso$rangeNat[i] <- sum(uso[i, nat])
}

# Calculating total anthropic area ----------------------------------------

for(i in 1:dim(uso)[1])
{
  uso$rangeAnt[i] <- sum(uso[i, ant])
}

# Calculating  proportional natural area ----------------------------------

for(i in 1:dim(uso)[1])
{
  uso$percNat[i] <- round(uso$rangeNat[i]/uso$range[i], digits = 4)
}

# Calculating proportional forest plantation area -------------------------

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

uso$lulcYear <- "2020" # ATENTION: Check the year!

head(uso)

uso <- uso[,c(1, 23:31)]

head(uso)

###########################################################################
# REPEAT FOR EACH ANALYSED YEAR!!! ----------------------------------------
###########################################################################
#usoFull <- NULL

usoFull <- rbind(usoFull, uso)

unique(usoFull$lulcYear) # Check included years

write.csv(usoFull, here::here("outputs", "uso-raw", "usoFull.csv"), row.names = FALSE)

###########################################################################
# ADDING OTHER VARIABLES OF INTEREST --------------------------------------
###########################################################################

uso <- read.csv(here::here("outputs", "usoFull.csv"))
head(uso)

list <- read.csv(here::here("data", "raw-data", "list.csv"))
head(list)

varInt <- list[,c(5, 7, 8, 13)]
head(varInt)


usoVar <- merge(uso, varInt, by="species")
head(usoVar)

usoVar <- merge(uso, sppAreas, by="species")

write.csv(usoVar, here::here("outputs", "hghInfo.csv"), row.names = FALSE)

################################################################################
################################################################################

head(uso)

uso <- uso[uso$lulcYear%in%c("2000", "2020"),]
uso$range.cat <- factor(uso$range.cat, levels=c("Restricted", "Partial", "Wide"))
uso$icmbio.cat <- factor(uso$icmbio.cat, levels = c("CR","EN","VU","DD","NT","LC"))

tapply(uso$percNat, INDEX=list(uso$lulcYear, uso$icmbio.cat), FUN=mean)
?tapply
