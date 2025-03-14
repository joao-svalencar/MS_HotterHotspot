###########################################################################
# Vieira-Alencar et al. Hostspot getting hotter (manuscript) --------------
###########################################################################

library(tidyr)

#exploring data
head(list) #loaded in C_hgh.R line 6
table(list$taxa)
table(list$IUCN, list$taxa)
round(apply(table(list$IUCN, list$taxa),2,function(x){x/sum(x)})*100, digits=2)

# Object for fig 4a -------------------------------------------------------

sum(table(list$taxa, list$IUCN))

iucn.class <- as.data.frame(table(list$taxa, list$IUCN))
names(iucn.class)[c(1,2)] <- c("class", "category")

#removing NE species
iucn.class <- iucn.class[iucn.class$category!="-",]

iucn.class$labelN[iucn.class$class==names(table(list$taxa))] <- table(list$taxa[list$IUCN!="-"])
iucn.class$category <- factor(iucn.class$category, levels = c("EX","CR","EN","VU","DD","NT","LC")) 
iucn.class$class <- factor(iucn.class$class, levels = c("Amphibians", "Reptiles", "Birds", "Mammals"))

# Object for fig 4b -------------------------------------------------------

range.iucn <- as.data.frame(table(list$range.cat, list$IUCN))
names(range.iucn) <- c("range.cat", "category", "Freq")

#removing NE species
range.iucn <- range.iucn[range.iucn$category!="-",]
range.iucn$category <- factor(range.iucn$category, levels = c("EX", "CR","EN","VU","DD","NT","LC"))

list.noNE <- list[list$IUCN!="-",]
range.iucn$labelN[range.iucn$range.cat==names(table(list.noNE$range.cat))] <- table(list.noNE$range.cat)
range.iucn$range.cat <- factor(range.iucn$range.cat, levels = c("Restricted", "Partial", "Wide")) 

head(range.iucn)

table(list.noNE$range.cat, list.noNE$IUCN)

# KBAs table --------------------------------------------------------------

head(kba) #loaded in C_hgh.R line 7
kba_rich <- as.data.frame(table(kba$NAME))
kba_unique <- kba[!duplicated(kba$NAME),]

head(kba_rich)
names(kba_rich)[1] <- "NAME"

kba_merge <- merge(kba_rich, kba_unique, by="NAME", )
head(kba_merge)

kba_merge <- kba_merge[,c(1,2,5:11)]
names(kba_merge)[2] <- "richness"

# Object for fig 4c -------------------------------------------------------

head(uso) #loaded in C_hgh.R line 8
uso2020 <- uso[uso$lulcYear=="2020",]
uso2020$percNatCat <- NA
uso2020$percNatCat[uso2020$percNat>0.80] <- ">80%"
uso2020$percNatCat[uso2020$percNat<=0.80] <- "<80%"
uso2020$percNatCat[uso2020$percNat<=0.50] <- "<50%"
uso2020$percNatCat[uso2020$percNat<=0.30] <- "<30%"
head(uso2020)

sppNat <- uso2020[,c(1, 5, 11)] #binomial, percNat, percNatCat ## used in D_analyses.R

list.loss <- merge(list, sppNat, by="binomial") 
list.loss.noNE <- list.loss[list.loss$IUCN!="-",]

hab.cat <- as.data.frame(table(list.loss.noNE$percNatCat, list.loss.noNE$IUCN))
names(hab.cat) <- c("loss", "category", "Freq")

#removing NE species
hab.cat$category <- factor(hab.cat$category, levels = c("EX", "CR","EN","VU","DD","NT","LC")) 
hab.cat$labelN[hab.cat$loss==names(table(list.loss.noNE$percNatCat))] <- table(list.loss.noNE$percNatCat)
hab.cat$loss <- factor(hab.cat$loss, levels=c("<30%", "<50%", "<80%", ">80%"))

# Object for fig 4d -------------------------------------------------------

head(gap) #loaded in C_hgh.R line 9
gap$gap.cat <- NA

gap$protected_range[is.na(gap$protected_range)] <- 0
gap$prot_perc[is.na(gap$prot_perc)] <- 0

gap$gap.cat[gap$prot_perc>=0.17] <- ">17%"
gap$gap.cat[gap$prot_perc<0.17] <- "<17%"
gap$gap.cat[gap$prot_perc<0.05] <- "<5%"
gap$gap.cat[gap$prot_perc<0.01] <- "<1%"
gap$gap.cat[gap$prot_perc==0] <- "0%"

table(gap$gap.cat)

head(list)
head(gap)
sppGap <- gap[, c(1,4)] #binomial, prot_perc ## used in D_analyses.R

iucn.cat <- list[,c(5,18)]
gap <- merge(gap, iucn.cat, by="binomial")

#removing NE species
gap <- gap[gap$IUCN!="-",]
gap$IUCN <- factor(gap$IUCN, levels = c("EX", "CR","EN","VU","DD","NT","LC"))

gap.tab <- as.data.frame(table(gap$IUCN, gap$gap.cat))
labelN <- as.data.frame(table(gap$gap.cat))
names(labelN) <- c("Var2", "labelN")

head(gap.tab)
gap.tab <- merge(gap.tab, labelN, by="Var2")

names(gap.tab) <- c("gap.cat", "IUCN", "Freq", "labelN")
gap.tab$gap.cat <- factor(gap.tab$gap.cat, levels=c("0%", "<1%", "<5%", "<17%", ">17%"))

################################################################################
# Filtering species by remaining habitat ----------------------------------
# To create habitat loss category maps

db_unique_perc <- merge(uso2020, db_unique, by="species") #db_unique loaded in C_hgh.R line 10

spp30 <- db_unique_perc[db_unique_perc$percNatCat=="<30%",]
write.csv(spp30, here::here("data", "processed", "baseunique_30-2.csv"), row.names = FALSE)

spp80 <- db_unique_perc[db_unique_perc$percNatCat==">80%",]
write.csv(spp80, here::here("data", "processed", "baseunique_80.csv"), row.names = FALSE)

# Filtering spp by description year and threat ----------------------------
# adding icmbio.cat and year to db_unique
# to create 2000 richness map

head(list)
list <- list[,c(2, 3, 12)] # threat and class
names(list)[1] <- "species"
head(db_unique)
db_unique <- merge(db_unique, list, by = "species")

db_unique2000 <- db_unique[db_unique$year<=2000,]

write.csv(db_unique2000, here::here("data", "processed", "baseunique2000.csv"), row.names = FALSE)
table(db_unique$icmbio.cat)

db_unique_tr <- db_unique[db_unique$icmbio.cat%in%c("VU", "EN", "CR"),]

table(db_unique_tr$icmbio.cat)
table(db_unique_tr$taxa)

db_unique_tr <- db_unique_tr[,-2]

head(db_unique_tr)

write.csv(db_unique_tr, here::here("data", "processed", "baseunique_tr.csv"), row.names = FALSE)

# Filtering by protection gap ---------------------------------------------
# to create gap 0% map

head(list)
list <- list[,c(1, 22)]
names(list)[1] <- "species"
head(db_unique)
db_unique <- merge(db_unique, list, by = "species")

db_unique_gap0 <- db_unique[db_unique$prot_perc_cat=="0%",]
write.csv(db_unique_gap0, here::here("data", "processed", "baseunique_gap0.csv"), row.names = FALSE)

db_unique_gap1 <- db_unique[db_unique$prot_perc_cat%in%c("0", "<1%"),]
write.csv(db_unique_gap1, here::here("data", "processed", "baseunique_gap1.csv"), row.names = FALSE)

db_unique_gap5 <- db_unique[db_unique$prot_perc_ca%in%c("0", "<1%", "<5%"),]
write.csv(db_unique_gap5, here::here("data", "processed", "baseunique_gap5.csv"), row.names = FALSE)

# Gap analysis processing -------------------------------------------------

gap$fail <- (gap$rangesize-gap$protected_range)
write.csv(list, here::here("outputs", "tables", "listnew.csv"), row.names = FALSE)

head(gap)

head(list)
list <- list[,c(1,9,19)]
names(gap)[1] <- "binomial"
gap <- merge(gap, list, by="binomial")

head(gap)

table(gap$prot_perc_cat, gap$icmbio.cat)

table(gap$prot_perc_cat, gap$range.cat)
