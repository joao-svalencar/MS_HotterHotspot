###########################################################################
# Vieira-Alencar et al. Hostspot getting hotter (manuscript) --------------
###########################################################################

###########################################################################
# Section 3.3 - Largest PAs Richness --------------------------------------
###########################################################################

head(kba)
kba_rich <- as.data.frame(table(kba$NAME))
kba_unique <- kba[!duplicated(kba$NAME),]

head(kba_rich)
names(kba_rich)[1] <- "NAME"

kba_merge <- merge(kba_rich, kba_unique, by="NAME", )
head(kba_merge)

kba_merge <- kba_merge[,c(1,2,5:11)]
names(kba_merge)[2] <- "richness"

###########################################################################
# Section 3.3 - Gap species information -----------------------------------
###########################################################################

head(gap)
head(list)

list_gap_info <- list[,c(5,9, 18)] #binomial, range category, iucn

gap_explore <- merge(gap, list_gap_info, by="binomial")

#defining NAs as 0: NAs represented species with no intersection with PAs
gap_explore$prot_perc[is.na(gap_explore$prot_perc)] <- 0

#species with less than 17% protected
sum(gap_explore$prot_perc<=0.17) #296 (87.05%) 

#range-size category x habitat loss
table(gap_explore$range.cat[gap_explore$prot_perc<=0.17]) 
#Restricted: 129; Partial: 142; Wide: 25
round((129/296)*100, digits=2) #43.58%
round((142/296)*100, digits=2) #47.97%
round((25/296)*100, digits=2) #8.45%

#number of gap species: completely outside the current strict protection PAs network
sum(gap_explore$prot_perc==0) #51 (15%) 

#gap species range-size category:
table(gap_explore$range.cat[gap_explore$prot_perc==0]) 
#Restricted: 47; Partial: 4
round((47/51)*100, digits=2) #92.16%
round((4/51)*100, digits=2)  #7.84%

#gap species IUCN status:
table(gap_explore$IUCN[gap_explore$prot_perc==0])
# Absolute number
# -  CR DD EN LC NT VU 
# 13  4 16  5  8  1  4

round((table(gap_explore$IUCN[gap_explore$prot_perc==0])/51)*100, digits=2)
#Percentages
#  -     CR    DD    EN    LC    NT    VU 
#25.49  7.84 31.37  9.80 15.69  1.96  7.84

#uninformed ("-" + DD): 13+16=29
round((29/51)*100, digits=2) #56.86%
#threatened (VU + EN + CR): 4+5+4=13
round((13/51)*100, digits=2) #25.49%
#not threatened (LC + NT): 8+1=9
round((9/51)*100, digits=2) #17.65%)

###########################################################################
# Section 3.4 - Statistic Analyses ----------------------------------------
###########################################################################

round((table(list$range.cat)/340)*100, digits=2)
#  Partial   Restricted     Wide 
# 42.94        49.71       7.35 

# Preparing for data analyses ---------------------------------------------
head(list)
analyses <- list[,c(1,8,9,18)] #taxa, rangesize, range.cat, IUCN  

# transforming interest variables in factors with specified levels
head(analyses)
analyses$range.cat <- factor(analyses$range.cat, levels = c("Restricted","Partial","Wide"))
analyses$taxa <- factor(analyses$taxa, levels = c("Amphibians", "Reptiles", "Birds", "Mammals"))
analyses$IUCN <- factor(analyses$IUCN, levels = c("CR","EN","VU","DD", "NT","LC"))

# creating a column to classify species as threatened and non-treatened (sensu Borgelt et al. 2022)
analyses$threatened <- NA
analyses$threatened[analyses$IUCN%in%c("LC", "NT")] <- "No"
analyses$threatened[analyses$IUCN%in%c("VU", "EN", "CR")] <- "Yes"

# filtering non-assessed anda DD species
analyses <- analyses[analyses$IUCN!="-" & analyses$IUCN!="DD",] #all but NA and DD
analyses <- analyses[analyses$IUCN!="-",] #all but NA

# Range size versus class: Supp 2 -----------------------------------------

kruskal.test(log(rangesize)~taxa, data=list)
#chi-sq: 31.76; df: 3; p-value < 0.001

# checked with text

# Exploring habitat loss --------------------------------------------------

head(uso)
unique(uso$binomial) #338 species

usoM <- uso[uso$lulcYear%in%c("2000", "2020"),] #selecting 2000 and 2020 info



usoM$loss <- usoM$rangeNat[usoM$lulcYear==2000]-usoM$rangeNat[usoM$lulcYear==2020]


#find ot where it was used:
head(uso)
#usoVar <- variaveis range.cat e iucn

usoM <- uso[uso$lulcYear%in%c("2000", "2020"),]
usoM$range.cat <- factor(usoM$range.cat, levels=c("Restricted", "Partial", "Wide")) #falta add no uso
usoM$IUCN <- factor(usoM$IUCN, levels = c("-","CR","EN","VU","DD","NT","LC"))
tapply(usoM$percNat, INDEX=list(usoM$lulcYear, usoM$IUCN), FUN=mean) #mean percNat per threat category

# Chisquared test of classes and IUCN categories ------------------------

chisq.test(analyses$taxa, analyses$IUCN) #including DD
chisq.test(analyses$taxa, analyses$threatened)

#capture.output(chisq, file = here::here("outputs", "tests", "chisq_class_threat.txt"))

table(analyses$taxa, analyses$IUCN)

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

