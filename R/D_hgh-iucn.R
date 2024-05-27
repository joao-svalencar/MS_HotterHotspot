library(tidyr) #usado para separar a coluna combinada

head(salve)
threats <- salve[,c(1:3,10)]
salve <- salve[,c(1:9)]

head(salve)
head(threats)

names(salve) <- c("class", "species", "species.old", "author", "evaluation date", "category", "criteria", "biome", "pop.trend")
s.cat <- salve[, c(2,3,6)]
head(s.cat)

l.cat <- list[, c(6, 10, 16)]
head(l.cat)

comp.cat <- merge(l.cat, s.cat, by="species", all.x = TRUE)

comp.cat

comp.cat$category[is.na(comp.cat$species.old)] <- "-"


#biomas <- tidyr::separate(data=salve, col="Bioma", into=c("biome.1", "biome.2", "biome.3", "biome.4", "biome.5"), sep=", ")
#head(biomas)

comp.cat <- comp.cat[,c(1:3,5)]
head(comp.cat)
names(comp.cat)[4] <- "Salve"

write.csv(comp.cat, here::here("outputs", "tables", "hgh-iucn-salve.csv"), row.names = FALSE)


head(list)
head(comp.cat)

comp.cat <- comp.cat[,c(1,4)]
names(comp.cat)[2] <- "Salve.Cat"

list <- merge(list, comp.cat, by="species")
write.csv(list, here::here("outputs", "tables", "listnew.csv"), row.names = FALSE)

head(list)

iucn <- as.data.frame(table(list$taxa, list$IUCN))
#salve <- as.data.frame(table(list$taxa, list$Salve.Cat))

names(iucn)[c(1,2)] <- c("class", "cat")
#names(salve)[c(1,2)] <- c("class", "cat")

iucn$source <- "IUCN"
salve$source <- " Salve"

table.1 <- rbind(iucn, salve)


names(table.1)[2] <- "Threat Category"

write.csv(table.1, here::here("outputs", "tables", "cat.sources.csv"), row.names = FALSE)

# objects for fig 4a ------------------------------------------------------
cat.sources <- cat.sources[cat.sources$category!="-",]
cat.sources$class[cat.sources$class=="Aves"] <- "Birds"
cat.sources$class[cat.sources$class=="Reptilia"] <- "Reptiles"

cat.sources$category <- factor(cat.sources$category, levels = c("EX","CR","EN","VU","DD","NT","LC")) 
cat.sources$class <- factor(cat.sources$class, levels = c("Amphibians", "Reptiles", "Birds", "Mammals"))
#cat.sources$source <- factor(cat.sources$source, levels = c("IUCN", " ICMBio"))

# For figs 4b and 4c --------------------------------------------------------

uso2020 <- uso[uso$lulcYear=="2020",]
head(uso2020)
uso2020 <- uso2020[uso2020$IUCN!="-",]

uso2020$percNatCat <- NA
uso2020$percNatCat[uso2020$percNat>0.80] <- ">80%"
uso2020$percNatCat[uso2020$percNat<=0.80] <- "<80%"
uso2020$percNatCat[uso2020$percNat<=0.50] <- "<50%"
uso2020$percNatCat[uso2020$percNat<=0.30] <- "<30%"

uso2020$percNatCat <- factor(uso2020$percNatCat, levels=c("<30%", "<50%", "<80%", ">80%"))
uso2020$range.cat <- factor(uso2020$range.cat, levels=c("Restricted", "Partial", "Wide"))


# objects for figs 4b and 4c ----------------------------------------------
list <- list[list$icmbio.cat!="-",]

hab.cat <- as.data.frame(table(list$percNatCat, list$icmbio.cat))
names(hab.cat) <- c("loss", "category", "Freq")

labelN <- as.data.frame(tapply(hab.cat$Freq, INDEX=hab.cat$loss, FUN=sum))
labelN$loss <- row.names(labelN)
names(labelN) <- c("labelN", "loss")
row.names(labelN) <- NULL
labelN

hab.cat <- merge(hab.cat, labelN, by="loss")

#

range.icmbio <- as.data.frame(table(list$range.cat, list$icmbio.cat))
names(range.icmbio) <- c("Range.Category", "category", "Freq")

labelN <- as.data.frame(tapply(range.icmbio$Freq, INDEX=range.icmbio$Range.Category, FUN=sum))
labelN$Range.Category <- row.names(labelN)
names(labelN) <- c("labelN", "Range.Category")
row.names(labelN) <- NULL
labelN

range.icmbio <- merge(range.icmbio, labelN, by="Range.Category")

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

head(gap)

gap$protected_range[is.na(gap$protected_range)] <- 0
gap$prot_perc[is.na(gap$prot_perc)] <- 0

gap$prot_perc_cat <- NA
gap$prot_perc_cat[gap$prot_perc>=0.17] <- ">17%"
gap$prot_perc_cat[gap$prot_perc<0.17] <- "<17%"
gap$prot_perc_cat[gap$prot_perc<0.05] <- "<5%"
gap$prot_perc_cat[gap$prot_perc<0.01] <- "<1%"
gap$prot_perc_cat[gap$prot_perc==0] <- "0%"

gap$prot_perc_cat <- factor(gap$prot_perc_cat, levels=c("0%", "<1%", "<5%", "<17%", ">17%"))

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



# Salve processing --------------------------------------------------------
table(list.salve$Mês.Ano.Avaliação)
salve[salve$Mês.Ano.Avaliação=="Oct-11",]

names(salve)[2] <- "species"
list.salve <- merge(list, salve, by="species")

list.salve[list.salve$Mês.Ano.Avaliação=="Apr-12",]

list.salve$Categoria <- factor(list.salve$Categoria, levels = c("CR","EN","VU","DD","NT","LC")) 
table(list.salve$Classe, list.salve$Categoria)

table(list$taxa, list$icmbio.cat)
