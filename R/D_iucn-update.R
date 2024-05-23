#updating IUCN info in the main list
head(list) 
head(iucn)
iucn <- iucn[,c(3,15)]
names(iucn)[1] <- "species"

table(list_up$IUCN)
table(list_up$redlistCategory)
list_up$redlistCategory[is.na(list_up$redlistCategory)] <- "-"

list_up <- merge(list, iucn, by="species", all.x = TRUE)

head(list_up)

list_up <- list_up[,-16]
names(list_up)[17] <- "IUCN"

write.csv(list_up, here::here("data", "raw-data", "list.csv"), row.names = FALSE)
