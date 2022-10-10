# Harmonisation of pollen records####
library(dplyr)
library(ggplot2)
library(ggpubr)
library(jtools)
library(openxlsx)
library(tidyr)

# Avai’o’vuna Swamp (Tonga)####
  counts<-read.csv("avai_pollen.csv")
  counts<-counts[-c(2)]
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  Site<- counts$Site
  meta_data <- c("Cal_yrs_BP", "Site")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  # delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  
  # delete uncertain taxa
  drop <- c("Indeterminant", "Unknown")
  counts = counts[,!(names(counts) %in% drop)]
  
  # rename some of the columns
  names(counts)[names(counts) == 'Ambrosia.type'] <- 'Ambrosia'
  names(counts)[names(counts) == 'Chenopodiaceae.Amaranthus'] <- 'Amaranthus'
  names(counts)[names(counts) == 'Freyeinetia'] <- 'Freycinetia'
  names(counts)[names(counts) == 'Ipomoea.cf..batatas'] <- 'Ipomoea'
  names(counts)[names(counts) == 'Lorantheceae'] <- 'Loranthaceae'
  names(counts)[names(counts) == 'Monolete.spores'] <- 'Monolete spores'
  names(counts)[names(counts) == 'Omalanthus'] <- 'Homalanthus'
  names(counts)[names(counts) == 'Paplionaceae'] <- 'Fabaceae'
  names(counts)[names(counts) == 'Poaceae..60m'] <- 'Poaceae'
  names(counts)[names(counts) == 'Poaceae.2.pores'] <- 'Poaceae'
  names(counts)[names(counts) == 'Polgonum'] <- 'Polygonum'
  names(counts)[names(counts) == 'Trilete.spores'] <- 'Trilete spores'
  
  # aggregate all taxa names
  ava_pollen<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  ava_pollen<-as.data.frame(ava_pollen)
  
  # Add age and site information
  ava_pollen$Cal_yrs_BP <-Cal_yrs_BP
  ava_pollen$Site <-Site

# Finemui Swamp (Tonga)####
  counts<-read.csv("fine_pollen.csv")
  counts<-counts[-c(2)]
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  Site<- counts$Site
  meta_data <- c("Cal_yrs_BP", "Site")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  # delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  #delete uncertain taxa
  #drop <- c("Euphorbia.sim.", "Excoecaria.sim.", "Filices..Monolete.Lepisorus..sim.")
  #counts = counts[,!(names(counts) %in% drop)]
  
  # rename some of the columns
  names(counts)[names(counts) == 'Filices..Monolete.Psilete.Folded'] <- 'Monolete spores'
  names(counts)[names(counts) == 'Filices..Monolete.Psilete.Large'] <- 'Monolete spores'
  names(counts)[names(counts) == 'Filices..Monolete.Psilete.Small'] <- 'Monolete spores'
  names(counts)[names(counts) == 'Blyxa.comp.'] <- 'Blyxa'
  names(counts)[names(counts) == 'Chenopodiaceae'] <- 'Amaranthaceae'
  names(counts)[names(counts) == 'Cocos.nucifera'] <- 'Cocos nucifera'
  names(counts)[names(counts) == 'Compositae..Liguliflorae.'] <- 'Asteraceae'
  names(counts)[names(counts) == 'Compositae..Tubuliflorae.'] <- 'Asteraceae'
  names(counts)[names(counts) == 'Cyathula.comp.'] <- 'Cyathula'
  names(counts)[names(counts) == 'Elaeocarpus.comp.'] <- 'Elaeocarpus'
  names(counts)[names(counts) == 'Euphorbia.sim.'] <- 'Euphorbia'
  names(counts)[names(counts) == 'Excoecaria.sim.'] <- 'Excoecaria'
  names(counts)[names(counts) == 'Filices..Monolete.Asplenium'] <- 'Asplenium'
  names(counts)[names(counts) == 'Filices..Monolete.Lepisorus..sim.'] <- 'Lepisorus'
  names(counts)[names(counts) == 'Filices..Trilete.Hymenophyllaceae.Comp.'] <- 'Hymenophyllaceae'
  names(counts)[names(counts) == 'Freycinetia.comp.'] <- 'Freycinetia'
  names(counts)[names(counts) == 'Gramineae'] <- 'Poaceae'
  names(counts)[names(counts) == 'Ipomea..cultivated'] <- 'Ipomoea'
  names(counts)[names(counts) == 'Leguminosae'] <- 'Fabaceae'
  names(counts)[names(counts) == 'Leucas.comp.'] <- 'Leucas'
  names(counts)[names(counts) == 'Lycopodium..reticulate'] <- 'Lycopodium'
  names(counts)[names(counts) == 'Potamogeton.comp.'] <- 'Potamogeton'
  names(counts)[names(counts) == 'Premna.comp.'] <- 'Premna'
  names(counts)[names(counts) == 'Tournefortia.comp.'] <- 'Tournefortia'
  names(counts)[names(counts) == 'Typha.latifolia'] <- 'Typha latifolia'
  
  taxa<-colnames(counts)
  
  # aggregate all taxa names
  fin_pollen<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  fin_pollen<-as.data.frame(fin_pollen)
  
  # Add age and site information
  fin_pollen$Cal_yrs_BP <-Cal_yrs_BP
  fin_pollen$Site <-Site

# Lotofoa Swamp (Tonga)####
  counts<-read.csv("loto_pollen.csv")
  counts<-counts[-c(2)]
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  Site<- counts$Site
  meta_data <- c("Cal_yrs_BP", "Site")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  # delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  
  #delete uncertain taxa
  #drop <- c("Euphorbia.sim.", "Excoecaria.sim.", "Maesa.sim.", "Schefflera.sim.")
  #counts = counts[,!(names(counts) %in% drop)]
  
  # rename some of the columns
  names(counts)[names(counts) == 'Filices..Monolete.Psilete.Folded'] <- 'Monolete spores'
  names(counts)[names(counts) == 'Filices..Monolete.Psilete.Large'] <- 'Monolete spores'
  names(counts)[names(counts) == 'Filices..Monolete.Psilete.Small'] <- 'Monolete spores'
  names(counts)[names(counts) == 'Cocos.nucifera'] <- 'Cocos nucifera'
  names(counts)[names(counts) == 'Compositae..Tubuliflorae.'] <- 'Asteraceae'
  names(counts)[names(counts) == 'Elaeocarpus.comp.'] <- 'Elaeocarpus'
  names(counts)[names(counts) == 'Euphorbia.sim.'] <- 'Euphorbia'
  names(counts)[names(counts) == 'Excoecaria.sim.'] <- 'Excoecaria'
  names(counts)[names(counts) == 'Filices..Monolete.Stenochlaena.palust.'] <- 'Stenochlaena palustris'
  names(counts)[names(counts) == 'Gramineae'] <- 'Poaceae'
  names(counts)[names(counts) == 'Ipomea..wild'] <- 'Ipomoea'
  names(counts)[names(counts) == 'Leguminosae'] <- 'Fabaceae'
  names(counts)[names(counts) == 'Maesa.sim.'] <- 'Maesa'
  names(counts)[names(counts) == 'Mallotus.comp.'] <- 'Mallotus'
  names(counts)[names(counts) == 'Momordica.comp.'] <- 'Momordica'
  names(counts)[names(counts) == 'Morinda.comp.'] <- 'Morinda'
  names(counts)[names(counts) == 'Nymphoides.comp.'] <- 'Nymphoides'
  names(counts)[names(counts) == 'Plantago.comp.'] <- 'Plantago'
  names(counts)[names(counts) == 'Premna.comp.'] <- 'Premna'
  names(counts)[names(counts) == 'Schefflera.sim.'] <- 'Schefflera'
  names(counts)[names(counts) == 'Syzygium.comp.'] <- 'Syzygium'
  names(counts)[names(counts) == 'Triumfetta.sp.'] <- 'Triumfetta'
  
  taxa<-colnames(counts)
  
  # aggregate all taxa names
  lot_pollen<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  lot_pollen<-as.data.frame(lot_pollen)
  
  # Add age and site information
  lot_pollen$Cal_yrs_BP <-Cal_yrs_BP
  lot_pollen$Site <-Site

# Ngofe Marsh (Tonga)####
  counts<-read.csv("ngof_pollen.csv")
  counts<-counts[-c(2)]
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  Site<- counts$Site
  meta_data <- c("Cal_yrs_BP", "Site")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  # delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  
  names(counts)[names(counts) == 'Monolete.spores'] <- 'Monolete spores'
  names(counts)[names(counts) == 'Trilete.spores'] <- 'Trilete spores'
  names(counts)[names(counts) == 'Monolete.echinate'] <- 'Monolete echinate'
  
  taxa<-colnames(counts)
  
  # aggregate all taxa names
  ngo_pollen<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  ngo_pollen<-as.data.frame(ngo_pollen)
  
  # Add age and site information
  ngo_pollen$Cal_yrs_BP <-Cal_yrs_BP
  ngo_pollen$Site <-Site

# Add explanatory variable data####

# The interpolated data is found in these .csv files
rsl<- read.csv("sea_level_interpolated.csv")
# Tephra layers are 1s and other depths are 0s
ngo_tephra<-read.csv("ngo_tephra.csv")
lot_tephra<-read.csv("lot_tephra.csv")
fin_tephra<-read.csv("fin_tephra.csv")

# Avai and Ngof are in counts whereas fine and loto are in percentage format
# since fine and loto were digitised they do not add up to 100 so they can also be transformed to %
# transform counts to percentages
ava_meta<-  ava_pollen[c(70:71)]
ava_pollen<- ava_pollen[-c(70:71)]
ava_pollen<- ava_pollen / rowSums(ava_pollen) * 100

fin_meta<-  fin_pollen[c(47:48)]
fin_pollen<- fin_pollen[-c(47:48)]
fin_pollen<- fin_pollen / rowSums(fin_pollen) * 100

lot_meta<-  lot_pollen[c(40:41)]
lot_pollen<- lot_pollen[-c(6,40:41)]
lot_pollen<- lot_pollen / rowSums(lot_pollen) * 100

ngo_meta<-  ngo_pollen[c(56:57)]
ngo_pollen<- ngo_pollen[-c(15,56:57)]
ngo_pollen<- ngo_pollen / rowSums(ngo_pollen) * 100

# add the metadata back again
ava_pollen<- cbind(ava_pollen, ava_meta)
fin_pollen<- cbind(fin_pollen, fin_meta)
lot_pollen<- cbind(lot_pollen, lot_meta)
ngo_pollen<- cbind(ngo_pollen, ngo_meta)

# merge all sites into one
harmonised<- merge(ava_pollen, fin_pollen, all=TRUE)
harmonised<- merge(harmonised, lot_pollen, all=TRUE)
harmonised<- merge(harmonised, ngo_pollen, all=TRUE)

# remove rows with no age
harmonised<-harmonised %>% filter(!is.na(Cal_yrs_BP))

age_site<- harmonised[c("Cal_yrs_BP", "Site")]

# create object called non-pollen
non_pollen <- c("Cal_yrs_BP", "Site")

del <- colnames(harmonised) %in% non_pollen
harmonised <- harmonised[, !del]
# replace NA values with 0
harmonised[is.na(harmonised)] <- 0

# aggregate columns into groups
look_up<- read.csv("LOOK_UP_2.csv")
sel <- match(colnames(harmonised), look_up$Pollen_taxa)
groupid <- look_up$groupid[sel]

spec_group <- t (rowsum(t(harmonised), group=groupid))

spec_group <- data.frame(spec_group, check.names=FALSE)

names(spec_group)[1]<-"DIST"
names(spec_group)[2]<-"LITTO"
names(spec_group)[3]<-"MANG"
names(spec_group)[4]<-"RAIN"
names(spec_group)[5]<-"UNKN"
names(spec_group)[6]<-"WETL"

spec_group<-data.frame(spec_group, age_site)

#write.csv(spec_group, "spec_group.csv")

# get rid of decimal points since they seem to mess up the LMs later
spec_group[,1:6]<-spec_group[,1:6]*100

# round up any remaining decimals
spec_group[,-8] <-round(spec_group[,-8],0)

#re-scale without the UNKN (unknown column)
spec_group<-spec_group[c(1,2,3,4,6,7,8)]
meta<-spec_group[c(6,7)]
spec_group<-spec_group[c(1:5)]
spec_group<- spec_group / rowSums(spec_group) * 100
#get rid of decimal points again
spec_group <-round(spec_group,0)

spec_group<-cbind(meta, spec_group)

#Visualize trends####
#organised by site:

#___ from here tiny things changed

if(1){
  
  AVAI<- filter(spec_group, grepl("Avaiovuna Swamp", Site, ignore.case = TRUE))
  AVAI<-AVAI[-c(2)]
  FINE<- filter(spec_group, grepl("Finemui Swamp", Site, ignore.case = TRUE))
  FINE<-FINE[-c(2)]
  LOTO<-filter(spec_group, grepl("Lotofoa Swamp", Site, ignore.case = TRUE))
  LOTO<-LOTO[-c(2)]
  NGOF<- filter(spec_group, grepl("Ngofe Marsh", Site, ignore.case = TRUE))
  NGOF<-NGOF[-c(2)]
  
  AVAI<-AVAI %>% pivot_longer(cols=c('DIST', 'LITTO', 'MANG', 'RAIN', "WETL"),
                              names_to='Type',
                              values_to='Percentage') 
  FINE<-FINE %>% pivot_longer(cols=c('DIST', 'LITTO', 'MANG', 'RAIN', "WETL"),
                              names_to='Type',
                              values_to='Percentage') 
  LOTO<-LOTO %>% pivot_longer(cols=c('DIST', 'LITTO', 'MANG', 'RAIN', "WETL"),
                              names_to='Type',
                              values_to='Percentage') 
  NGOF<-NGOF %>% pivot_longer(cols=c('DIST', 'LITTO', 'MANG', 'RAIN', "WETL"),
                              names_to='Type',
                              values_to='Percentage') 
  
  AVAI_TRENDS<-ggplot(data = AVAI, aes(x = Cal_yrs_BP,  y = Percentage, color = Type)) +
    geom_point() +
    # geom_smooth(aes(Cal_yrs_BP,Percentage),method = "glm", se = F,
    #             method.args = list(family = "poisson")) +
    geom_smooth(aes(Cal_yrs_BP,Percentage)) +
    xlim(max(AVAI$Cal_yrs_BP), min(AVAI$Cal_yrs_BP)) +
    theme_bw() +
    xlab("Cal. years BP") + ylab("Taxa [%]")
  
  FINE_TRENDS<-ggplot(data = FINE, aes(x = Cal_yrs_BP,  y = Percentage, color = Type)) +
    geom_point() +
    # geom_smooth(aes(Cal_yrs_BP,Percentage),method = "glm", se = F,
    #             method.args = list(family = "poisson")) +
    geom_smooth(aes(Cal_yrs_BP,Percentage)) +
    xlim(max(FINE$Cal_yrs_BP), min(FINE$Cal_yrs_BP)) +
    theme_bw() +
    xlab("Cal. years BP") + ylab("Taxa [%]")
  
  LOTO_TRENDS<-ggplot(data = LOTO, aes(x = Cal_yrs_BP,  y = Percentage, color = Type)) +
    geom_point() +
    # geom_smooth(aes(Cal_yrs_BP,Percentage),method = "glm", se = F,              
    #             method.args = list(family = "poisson")) +
    geom_smooth(aes(Cal_yrs_BP,Percentage)) +
    xlim(max(LOTO$Cal_yrs_BP), min(LOTO$Cal_yrs_BP)) +
    theme_bw() +
    xlab("Cal. years BP") + ylab("Taxa [%]")
  
  NGOF_TRENDS<-ggplot(data = NGOF, aes(x = Cal_yrs_BP,  y = Percentage, color = Type)) +
    geom_point() +
    # geom_smooth(aes(Cal_yrs_BP,Percentage),method = "glm", se = F,              
    #             method.args = list(family = "poisson")) +
    geom_smooth(aes(Cal_yrs_BP,Percentage)) +
    xlim(max(NGOF$Cal_yrs_BP), min(NGOF$Cal_yrs_BP)) +
    theme_bw() +
    xlab("Cal. years BP") + ylab("Taxa [%]")
  
  ggarrange(NGOF_TRENDS, AVAI_TRENDS, LOTO_TRENDS, FINE_TRENDS, common.legend = TRUE)
  
}

#___ what changed here is to plot polynomials and to change "taxa" to upper case

# organised by vegetation type:
if(0){
DIST <-spec_group[c(1:3)]
LITTO <-spec_group[c(1,2,4)]
MANG <-spec_group[c(1,2,5)]
RAIN <-spec_group[c(1:2,6)]
WETL <-spec_group[c(1:2,7)]

DIST<-ggplot(data = spec_group, aes(x = Cal_yrs_BP,  y = DIST, color = Site)) +
  geom_point() +
  geom_smooth(aes(Cal_yrs_BP,DIST),method = "glm", se = F,              
              method.args = list(family = "poisson")) +
  xlim(max(DIST$Cal_yrs_BP), min(DIST$Cal_yrs_BP)) +
  theme_bw() +
  xlab("Cal. years BP") + ylab("% Disturbance taxa")

LITTO<-ggplot(data = spec_group, aes(x = Cal_yrs_BP,  y = LITTO, color = Site)) +
  geom_point() +
  geom_smooth(aes(Cal_yrs_BP,LITTO),method = "glm", se = F,              
              method.args = list(family = "poisson")) +
  xlim(max(LITTO$Cal_yrs_BP), min(LITTO$Cal_yrs_BP)) +
  theme_bw() +
  xlab("Cal. years BP") + ylab("% Littoral taxa")

MANG<-ggplot(data = spec_group, aes(x = Cal_yrs_BP,  y = MANG, color = Site)) +
  geom_point() +
  geom_smooth(aes(Cal_yrs_BP,MANG),method = "glm", se = F,              
              method.args = list(family = "poisson")) +
  xlim(max(MANG$Cal_yrs_BP), min(MANG$Cal_yrs_BP)) +
  theme_bw() +
  xlab("Cal. years BP") + ylab("% Mangrove taxa")

RAIN<-ggplot(data = spec_group, aes(x = Cal_yrs_BP,  y = RAIN, color = Site)) +
  geom_point() +
  geom_smooth(aes(Cal_yrs_BP,RAIN),method = "glm", se = F,              
              method.args = list(family = "poisson")) +
  xlim(max(RAIN$Cal_yrs_BP), min(RAIN$Cal_yrs_BP)) +
  theme_bw() +
  xlab("Cal. years BP") + ylab("% Rainforest taxa")

WETL<-ggplot(data = spec_group, aes(x = Cal_yrs_BP,  y = WETL, color = Site)) +
  geom_point() +
  geom_smooth(aes(Cal_yrs_BP,WETL),method = "glm", se = F,              
              method.args = list(family = "poisson")) +
  xlim(max(WETL$Cal_yrs_BP), min(WETL$Cal_yrs_BP)) +
  theme_bw() +
  xlab("Cal. years BP") + ylab("% Wetland taxa")

ggarrange(DIST, LITTO, MANG, RAIN, WETL, common.legend = TRUE) 
}

#Explanatory variables####

rsl<- read.csv("sea_level_interpolated.csv")
spec_group<-merge(spec_group, rsl, by="Cal_yrs_BP")

ava <- filter(spec_group, grepl("Avaiovuna Swamp", Site, ignore.case = TRUE))
fin <- filter(spec_group, grepl("Finemui Swamp", Site, ignore.case = TRUE))
lot <- filter(spec_group, grepl("Lotofoa Swamp", Site, ignore.case = TRUE))
ngo <- filter(spec_group, grepl("Ngofe Marsh", Site, ignore.case = TRUE))

fin_tephra<-read.csv("fin_tephra.csv")
fin<-merge(fin_tephra, fin, by="Cal_yrs_BP")

lot_tephra<-read.csv("lot_tephra.csv")
lot<-merge(lot_tephra, lot, by="Cal_yrs_BP")

ngo_tephra<-read.csv("ngo_tephra.csv")
ngo<-merge(ngo_tephra, ngo, by="Cal_yrs_BP")

ava<-merge(ngo_tephra, ava, by="Cal_yrs_BP")

#GLMS####
#Disturbance

#___ time was removed
#___ we looked at the distribution of each response variable for each
#___ island and vegetation type

#___ Manuel and I changed the following: we log-transformed the response
#___ variables and defined Tephra as a factor.
#___ we also changed the glms to lms (as it is essentially the same,
#___ because GLMs with gaussian distributions are basically lms)
#___ as we now have lms we can report R² values that tell us more
#___ about the relationship of the response and explanatory variables

hist(ava$DIST)
ava_dist<-lm(log1p(DIST) ~ RSL + as.factor(Tephra), data = ava)
summary(ava_dist)
hist(ava_dist$residuals)
#___ no significant variables

hist(fin$DIST)
fin_dist<-lm(log1p(DIST) ~ RSL + as.factor(Tephra), data = fin)
summary(fin_dist)
hist(fin_dist$residuals)
#___ see level rise is significant, R² = 0.3 (not too bad)

hist(lot$DIST)
lot_dist<-lm(log1p(DIST) ~ RSL + as.factor(Tephra), data = lot)
summary(lot_dist)
hist(lot_dist$residuals)
#___ no significant variables, see level rise is almost significant.

hist(ngo$DIST)
ngo_dist<-lm(log1p(DIST) ~ RSL + as.factor(Tephra), data = ngo)
summary(ngo_dist)
hist(ngo_dist$residuals)
#___ no significant variables

#Littoral####

hist(ava$LITTO)
ava_litto<-lm(log1p(LITTO) ~ RSL + as.factor(Tephra), data = ava)
summary(ava_litto)
hist(ava_litto$residuals)
#___ no significant variables

hist(fin$LITTO)
fin_litto<-lm(log1p(LITTO) ~ RSL + as.factor(Tephra), data = fin)
summary(fin_litto)
hist(fin_litto$residuals)
#___ sea level rise is significant. Makes sense for littoral vegetation, right?
#___ R² = 0.3

hist(lot$LITTO)
lot_litto<-lm(log1p(LITTO) ~ RSL + as.factor(Tephra), data = lot)
summary(lot_litto)
hist(lot_litto$residuals)
#___ no significant variables

hist(ngo$LITTO)
ngo_litto<-lm(log1p(LITTO) ~ RSL + as.factor(Tephra), data = ngo)
summary(ngo_litto)
hist(ngo_litto$residuals)
#___ sea level rise is significant. Makes sense for littoral vegetation, right?
#___ R² = 0.3

#Mangrove####

hist(ava$MANG)
ava_mang<-lm(log1p(MANG) ~ RSL + as.factor(Tephra), data = ava)
summary(ava_mang)
hist(ava_mang$residuals)
#___ no significant variables

hist(fin$MANG)
fin_mang<-lm(log1p(MANG) ~ RSL + as.factor(Tephra), data = fin)
summary(fin_mang)
hist(fin_mang$residuals)
#___ sea level rise is significant. Makes sense for mangroves, right?
#___ Very high R2 of 0.7!

hist(lot$MANG)
lot_mang<-lm(log1p(MANG) ~ RSL + as.factor(Tephra), data = lot)
summary(lot_mang)
hist(lot_mang$residuals)
#___ sea level rise is significant. R² = 0.4

hist(ngo$MANG)
ngo_mang<-lm(log1p(MANG) ~ RSL + as.factor(Tephra), data = ngo)
summary(ngo_mang)
hist(ngo_mang$residuals)
#___ no significant variables

#Rainforest####
hist(ava$RAIN)
ava_rain<-lm(log1p(RAIN) ~ RSL + as.factor(Tephra), data = ava)
summary(ava_rain)
hist(ava_rain$residuals)
#___ no significant variables

hist(fin$RAIN)
fin_rain<-lm(log1p(RAIN) ~ RSL + as.factor(Tephra), data = fin)
summary(fin_rain)
hist(fin_rain$residuals)
#___ no significant variables

hist(lot$RAIN)
lot_rain<-lm(log1p(RAIN) ~ RSL + as.factor(Tephra), data = lot)
summary(lot_rain)
hist(lot_rain$residuals)
#___ no significant variables

hist(ngo$RAIN)
ngo_rain<-lm(log1p(RAIN) ~ RSL + as.factor(Tephra), data = ngo)
summary(ngo_rain)
hist(ngo_rain$residuals)
#___ no significant variables

#Wetland####

hist(ava$WETL)
ava_wetl<-lm(log1p(WETL) ~ RSL + as.factor(Tephra), data = ava)
summary(ava_wetl)
hist(ava_wetl$residuals)
#___ no significant variables

hist(fin$WETL)
fin_wetl<-lm(log1p(WETL) ~ RSL + as.factor(Tephra), data = fin)
summary(fin_wetl)
hist(fin_wetl$residuals)
#___ no significant variables

hist(lot$WETL)
lot_wetl<-lm(log1p(WETL) ~ RSL + as.factor(Tephra), data = lot)
summary(lot_wetl)
hist(lot_wetl$residuals)
#___ sea level rise is significant with R² = 0.4. Tephra is almost significant

hist(ngo$WETL)
ngo_wetl<-lm(log1p(WETL) ~ RSL + as.factor(Tephra), data = ngo)
summary(ngo_wetl)
hist(ngo_wetl$residuals)
#___ sea level rise is significant with R² = 0.4.

export_summs(ava_dist, ava_litto, ava_mang, ava_rain, ava_wetl,
               fin_dist, fin_litto, fin_mang, fin_rain, fin_wetl,
               lot_dist, lot_litto, lot_mang, lot_rain,
               ngo_dist, ngo_litto, ngo_mang, ngo_rain, ngo_wetl, scale = TRUE, to.file = "xlsx", file.name = "LM_results.xlsx")

#___ In addition to the results table the models could be reported in the appendix of the manuscript
#___ The following package and function could help:
library(report)
as.data.frame(report(ava_dist)) # this is an example and could be done for all models
