library(tidyr)

#exploring data
table(list$taxa)
table(list$IUCN, list$taxa)
round(apply(table(list$IUCN, list$taxa),2,function(x){x/sum(x)})*100, digits=2)

# Object for fig 4a -------------------------------------------------------
#ok
iucn.class <- as.data.frame(table(list$taxa, list$IUCN))
names(iucn.class)[c(1,2)] <- c("class", "category")
iucn.class$labelN[iucn.class$class==names(table(list$taxa))] <- table(list$taxa)
iucn.class$category <- factor(iucn.class$category, levels = c("EX","CR","EN","VU","DD","NT","LC", "-")) 
iucn.class$class <- factor(iucn.class$class, levels = c("Amphibians", "Reptiles", "Birds", "Mammals"))

# Object for fig 4b -------------------------------------------------------
#ok
range.iucn <- as.data.frame(table(list$range.cat, list$IUCN))
names(range.iucn) <- c("range.cat", "category", "Freq")
table(list$range.cat)
range.iucn$labelN[range.iucn$range.cat==names(table(list$range.cat))] <- table(list$range.cat)
range.iucn$category <- factor(range.iucn$category, levels = c("EX", "CR","EN","VU","DD","NT","LC", "-"))
range.iucn$range.cat <- factor(range.iucn$range.cat, levels = c("Restricted", "Partial", "Wide")) 

# Object for fig 4c -------------------------------------------------------
#ok
uso2020 <- uso[uso$lulcYear=="2020",]
uso2020$percNatCat <- NA
uso2020$percNatCat[uso2020$percNat>0.80] <- ">80%"
uso2020$percNatCat[uso2020$percNat<=0.80] <- "<80%"
uso2020$percNatCat[uso2020$percNat<=0.50] <- "<50%"
uso2020$percNatCat[uso2020$percNat<=0.30] <- "<30%"

sppNatCat <- uso2020[,c(1, 11)] #binomial, percNatCat

list.loss <- merge(list, sppNatCat, by="binomial")
hab.cat <- as.data.frame(table(list.loss$percNatCat, list.loss$IUCN))
names(hab.cat) <- c("loss", "category", "Freq")
hab.cat$labelN[hab.cat$loss==names(table(list.loss$percNatCat))] <- table(list.loss$percNatCat)
hab.cat$category <- factor(hab.cat$category, levels = c("EX", "CR","EN","VU","DD","NT","LC", "-")) 
hab.cat$loss <- factor(hab.cat$loss, levels=c("<30%", "<50%", "<80%", ">80%"))

# Object for fig 4d -------------------------------------------------------

head(gap)
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
iucn.cat <- list[,c(5,18)]

gap <- merge(gap, iucn.cat, by="binomial")

gap.tab <- as.data.frame(table(gap$IUCN, gap$gap.cat))
labelN <- as.data.frame(table(gap$gap.cat))
names(labelN) <- c("Var2", "labelN")

head(gap.tab)
gap.tab <- merge(gap.tab, labelN, by="Var2")

names(gap.tab) <- c("gap.cat", "iucn", "Freq", "labelN")
gap.tab$iucn <- factor(gap.tab$iucn, levels = c("EX", "CR","EN","VU","DD","NT","LC", "-"))
gap.tab$gap.cat <- factor(gap.tab$gap.cat, levels=c("0%", "<1%", "<5%", "<17%", ">17%"))

################################################################################
# Filtering species by remaining habitat ----------------------------------
# To create habitat loss category maps

db_unique_perc <- merge(uso2020, db_unique, by="species")

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


# IUCN processing ---------------------------------------------------------

head(iucn)
table(iucn$yearPublished)
table(iucn$assessmentDate)

min(list.iucn$assessmentDate)
max(list.iucn$assessmentDate)

head(list)

list <- list[,c(6,11)]

head(iucn)

iucn <- iucn[,c(1,2,3,4)]

list.iucn <- merge(list, iucn, by="species")

list.iucn[list.iucn$assessmentDate=="2004-04-30 00:00:00 UTC",]
