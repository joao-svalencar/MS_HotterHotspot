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

list_gap_info <- list[,c(5,9,18)] #binomial, range category, iucn

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

# adding habitat loss info
usoM <- uso[uso$lulcYear%in%c("2000", "2020"),] #selecting 2000 and 2020 info

spp <- unique(usoM$binomial)
loss <- usoM$rangeNat[usoM$lulcYear==2000]-usoM$rangeNat[usoM$lulcYear==2020] # absolute loss
percLoss <- loss/usoM$rangeNat[usoM$lulcYear==2000] # percentual loss

hab.loss <- as.data.frame(cbind(spp, loss, percLoss))
hab.loss$loss <- as.numeric(hab.loss$loss)
hab.loss$percLoss <- round((as.numeric(hab.loss$percLoss))*100, digits=2)

list <- merge(list, hab.loss, by.x="binomial", by.y="spp",  all.x = TRUE)

# Range size versus class: Supp 2 -----------------------------------------

kruskal.test(log(rangesize)~taxa, data=list)
#chi-sq: 31.76; df: 3; p-value < 0.001

###########################################################################
# Exploring habitat loss --------------------------------------------------
###########################################################################

head(uso)
unique(uso$binomial) #338 species

sum(hab.loss$loss>0) #number of species with habitat loss greater than 0 (no loss)
#294

round((sum(hab.loss$loss>0)/length(unique(usoM$binomial)))*100, digits=2)
# 86.98% species lost natural habitats  

round(summary(percLoss[percLoss>0])*100, digits=2)
# mean, min, max habitat loss percentage

abs(round(summary(percLoss[percLoss<=0])*100, digits=2))
# mean, min, max habitat gain percentage

table(list$range.cat)
table(list$range.cat[list$loss>0]) #habitat loss in range categories
# Restricted    Partial       Wide 
#     129        140           25

round((table(list$range.cat[list$loss>0])/table(list$range.cat))*100, digits=2)
# Restricted    Partial       Wide 
#  76.79        96.55       100.00 

# Habitat gain ------------------------------------------------------------
sum(hab.loss$loss<0) #sum of species with marginal habitat gain
# 39
round((sum(hab.loss$loss<0)/length(hab.loss$spp))*100, digits=2)

abs(round(summary(percLoss[percLoss<0])*100, digits=2))

###########################################################################
# Exploring IUCN assessment -----------------------------------------------
###########################################################################

head(list)
analyses <- list[,c(1,5,8,9,11,18)] #taxa, binomial, rangesize, range.cat, year, IUCN  

# transforming interest variables in factors with specified levels
head(analyses)
analyses$taxa <- factor(analyses$taxa, levels = c("Amphibians", "Reptiles", "Birds", "Mammals"))
analyses$range.cat <- factor(analyses$range.cat, levels = c("Restricted","Partial","Wide"))
analyses$IUCN <- factor(analyses$IUCN, levels = c("EX", "CR","EN","VU","DD", "NT","LC", "-"))

# creating a column to classify species as threatened and non-treatened
analyses$threatened <- NA
analyses$threatened[analyses$IUCN%in%c("LC", "NT")] <- "No"
analyses$threatened[analyses$IUCN%in%c("EX", "VU", "EN", "CR")] <- "Yes"

analyses$threatened <- factor(analyses$threatened, levels=c("No", "Yes"))

table(analyses$taxa, analyses$IUCN, useNA="ifany")
table(analyses$taxa, analyses$threatened, useNA="ifany")

# proportion of assessed species
round((sum(analyses$IUCN!="-", na.rm=TRUE)/length(analyses$taxa))*100, digits=2) #303/340 assessed species

# proportion of DD species
round((table(analyses$IUCN)[5]/sum(table(analyses$IUCN)[-8]))*100, digits=2) #33/303

# proportion of threatened species amongst assessed species:
a <- table(analyses$taxa, analyses$threatened)
a[1,2]/(a[1,1]+a[1,2]) #prop amphibians 0.1794
a[2,2]/(a[2,1]+a[2,2]) #prop reptiles 0.1744
a[3,2]/(a[3,1]+a[3,2]) #prop birds 0.3488372
a[4,2]/(a[4,1]+a[4,2]) #prop mammals 0.2916

table(analyses$IUCN)
table(analyses$threatened, useNA = "ifany") #NA is the sum of One EX, DD and non-assessed species

analyses_iucn <- analyses[analyses$IUCN!="-",] # removing not evaluated species
table(analyses_iucn$IUCN)
analyses_iucn$IUCN <- factor(analyses_iucn$IUCN, levels = c("EX","CR","EN","VU","DD","NT","LC")) 

table(analyses_iucn$threatened, useNA="ifany") #NA = Data Deficient species

# prop IUCN amphibians
round((table(analyses_iucn$taxa, analyses_iucn$IUCN)[1,]/sum(table(analyses_iucn$taxa, analyses_iucn$IUCN)[1,]))*100, digits=2)
#EX    CR    EN    VU    DD    NT    LC 
#0.00  2.44  7.32  7.32  4.88  5.69 72.36 

# prop IUCN reptiles
round((table(analyses_iucn$taxa, analyses_iucn$IUCN)[2,]/sum(table(analyses_iucn$taxa, analyses_iucn$IUCN)[2,]))*100, digits=2)
#EX    CR    EN    VU    DD    NT    LC 
#0.00  2.88  4.81  6.73 17.31  2.88 65.38 

# prop IUCN Birds
round((table(analyses_iucn$taxa, analyses_iucn$IUCN)[3,]/sum(table(analyses_iucn$taxa, analyses_iucn$IUCN)[3,]))*100, digits=2)
#EX    CR    EN    VU    DD    NT    LC 
#0.00  2.33 11.63 20.93  0.00 18.60 46.51 

# prop IUCN Mammals
round((table(analyses_iucn$taxa, analyses_iucn$IUCN)[4,]/sum(table(analyses_iucn$taxa, analyses_iucn$IUCN)[4,]))*100, digits=2)
#EX    CR    EN    VU    DD    NT    LC 
#3.03  0.00 15.15  3.03 27.27  6.06 45.45

# Chisquared test of classes and IUCN categories ------------------------
chisq.test(analyses_iucn$taxa, analyses_iucn$IUCN) # with DD

analyses_iucn_noDD <- analyses_iucn[analyses_iucn$IUCN!="DD",] #removing 33 DD species; N=270

chisq.test(analyses_iucn_noDD$taxa, analyses_iucn_noDD$threatened) # without DD

# range-size versus IUCN categories
analyses_iucn_noDD$threatened <- factor(analyses_iucn_noDD$threatened, levels=c("No", "Yes"))

shapiro.test(analyses_iucn_noDD$rangesize) # not normal - non-parametric test required

kruskal.test(rangesize~threatened, data=analyses_iucn_noDD)
kruskal.test(rangesize~IUCN, data=analyses_iucn)

# remaining habitat versus IUCN categories --------------------------------

#sppNat created in D_hgh-iucn.R line 47
analyses_iucn <- merge(analyses_iucn, sppNat, by="binomial") # remove Hylaeamys acritus and Juscelinomys huanchacae, see text for details
analyses_iucn_noDD <- analyses_iucn[analyses_iucn$IUCN!="DD",] # removing 31 DD species; N=270

analyses_iucn_noDD$threatened <- factor(analyses_iucn_noDD$threatened, levels=c("No", "Yes"))

#normality test
shapiro.test(analyses_iucn_noDD$percNat) # not normal - non-parametric test required

kruskal.test(percNat~threatened, data=analyses_iucn_noDD)
kruskal.test(percNat~IUCN, data=analyses_iucn)

# range protection versus IUCN categories ---------------------------------
# recreating analyses_iucn before continuing (re-add Hylaeamys acritus and Juscelinomys huanchacae)
analyses_iucn <- analyses[analyses$IUCN!="-",] # removing not evaluated species
head(analyses_iucn)

analyses_iucn <- merge(analyses_iucn, sppGap, by="binomial")
analyses_iucn_noDD <- analyses_iucn[analyses_iucn$IUCN!="DD",] # removing 33 DD species; N=270

analyses_iucn_noDD$threatened <- factor(analyses_iucn_noDD$threatened, levels=c("No", "Yes"))

str(analyses_iucn)
str(analyses_iucn_noDD)

# normality test
shapiro.test(analyses_iucn_noDD$prot_perc) # not normal - non-parametric test required

kruskal.test(prot_perc~threatened, data=analyses_iucn_noDD)
kruskal.test(prot_perc~IUCN, data=analyses_iucn)

############################################################################
# Section 3.5 - Vulnerability, Irreplaceability and Discovery -------------
############################################################################

# range size versus year of description -----------------------------------
# Espécies descritas recentemente tendem a ter maior probabilidade de extinção?

head(analyses)
str(analyses)

# normality test
shapiro.test(analyses$rangesize) #not normal - non-parametric test required

# Spearman's rank correlation rho
cor.test(x=analyses$year, y=analyses$rangesize, method = 'spearman')

## Outra pergunta seria testar se as sp ameacadas diferem ou não em termos de data de descrição.
## tb da pra usar DD X Dados Suficientes, em outra pergunta. As DDs tendem a ser as sp descritas agora (provavel que sim)

##############################################################################################################
##############################################################################################################

