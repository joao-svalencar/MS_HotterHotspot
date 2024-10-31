# Largest PAs Richness ----------------------------------------------------
head(kba)
kba_rich <- as.data.frame(table(kba$NAME))
kba_unique <- kba[!duplicated(kba$NAME),]

head(kba_rich)
names(kba_rich)[1] <- "NAME"

kba_merge <- merge(kba_rich, kba_unique, by="NAME", )
head(kba_merge)

kba_merge <- kba_merge[,c(1,2,5:11)]
names(kba_merge)[2] <- "richness"

# Gap - exploring ---------------------------------------------------------
head(gap)
head(list)

list_gap_info <- list[,c(5,9, 18)] #binomial, range category, iucn

gap_explore <- merge(gap, list_gap_info, by="binomial")

sum(gap_explore$prot_perc<=0.17) #296 (87.05%) species with less than 17% protected

table(gap_explore$range.cat[gap_explore$prot_perc<=0.17])
#Restricted: 129; Partial: 142; Wide: 25
round((129/296)*100, digits=2) #43.58%
round((142/296)*100, digits=2) #47.97%
round((25/296)*100, digits=2) #8.45%

gap_explore$prot_perc[is.na(gap_explore$prot_perc)] <- 0
sum(gap_explore$prot_perc==0) #51 (15%)

table(gap_explore$range.cat[gap_explore$prot_perc==0])
#Restricted: 47; Partial: 4
round((47/51)*100, digits=2) #92.16%
round((4/51)*100, digits=2)  #7.84%

# Preparing for data analyses ---------------------------------------------
head(list)
analyses <- list[,c(1,8,9,18)] #taxa, rangesize, range.cat, IUCN  

head(analyses)

analyses$threatened <- NA
analyses$threatened[analyses$IUCN%in%c("LC", "NT")] <- "No"
analyses$threatened[analyses$IUCN%in%c("VU", "EN", "CR")] <- "Yes"

analyses <- analyses[analyses$IUCN!="-" & analyses$IUCN!="DD",] #all but NA and DD
analyses <- analyses[analyses$IUCN!="-",] #all but NA

analyses$taxa <- factor(analyses$taxa, levels = c("Amphibians", "Reptiles", "Birds", "Mammals"))
analyses$IUCN <- factor(analyses$IUCN, levels = c("CR","EN","VU","DD", "NT","LC"))
analyses$range.cat <- factor(analyses$range.cat, levels = c("Restricted","Partial","Wide"))

str(analyses)


# Range size versus class: Supp ? -----------------------------------------

kruskal.test(log(rangesize)~taxa, data=list)

# Chisquared test of classes and IUCN categories ------------------------

chisq.test(analyses$taxa, analyses$IUCN) #including DD
chisq.test(analyses$taxa, analyses$threatened)

#capture.output(chisq, file = here::here("outputs", "tests", "chisq_class_threat.txt"))

table(analyses$taxa, analyses$IUCN)
#table(analyses$taxa, analyses$IUCN)

a <- table(analyses$taxa, analyses$threatened)

#threatened
a[1,2]/(a[1,1]+a[1,2]) #prop amphibians 0.03389831
a[2,2]/(a[2,1]+a[2,2]) #prop reptiles 0.07826087
a[3,2]/(a[3,1]+a[3,2]) #prop birds 0.422222
a[4,2]/(a[4,1]+a[4,2]) #prop mammals 0.3783784

#data-deficient IUCN
11/(1+0+3+11+5+98) #amphibians 0.09322034
18/(1+5+3+18+7+81) #reptiles 0.1565217
0 #birds
4/(1+9+4+4+1+18) #mammals 0.1081081

#data-deficient IUCN
#36/(3+36+0+0+41+2+0) #amphibians 0.4390244
#17/(3+17+5+0+20+8+9) #reptiles 0.2741935
#0 #birds
#7/(6+0+7+5+1+15+2+1) #mammals 0.1891892


# Espécies diferentes tamanho de range tendem a ter diferentes categorias de ameaça?

kruskal.test(rangesize~IUCN, data=analyses)
kruskal.test(rangesize~threatened, data=analyses)

?kruskal.test

# Espécies ameçadas x não ameaçadas tendem a ter menos habitat remanescente?

kruskal.test(percNat~IUCN, data=analyses)
kruskal.test(percNat~threatened, data=analyses)

# Ou menos area protegida?

kruskal.test(prot_perc~IUCN, data=analyses)
kruskal.test(prot_perc~threatened, data=analyses)

# Espécies descritas recentemente tendem a ter maior probabilidade de extinção?

mod <- lm(log(rangesize)~year, data=analyses)
summary(mod)

## Outra pergunta seria testar se as sp ameacadas diferem ou não em termos de data de descrição.
  ## tb da pra usar DD X Dados Suficientes, em outra pergunta. As DDs tendem a ser as sp descritas agora (provavel que sim)


##############################################################################################################
##############################################################################################################
head(list)
gap$prop <- cbind(sucess = gap$protected_range, fail = gap$rangesize-gap$protected_range)

mod <- glm(prop~IUCN, family=binomial, data=gap)

summary(mod)

##############################################################################################################
##############################################################################################################

# Largest PAs richness ----------------------------------------------------

