# Load packages 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(vegan)
install.packages('ggpubr')
library(ggpubr)

###### BIRDS #######

# Read in bird data sheet
birds<-read.csv('Data/Birds_Transect_Point.csv')

## Species Richness by Sampling Location ##

# create a fixed order of sampling locations 
transect_order<-c('Lower','Intermediate','Upper','Point')

# create a data frame of the number of unique species in each plot by each sampling location
birds_t<- birds %>%
  group_by(Plot, species, Transect = factor(Transect, levels = transect_order)) %>%
  summarize(count = n()) %>%
  group_by(Plot, Transect) %>%
  summarize(total_species = n_distinct(species))

# Create a bar plot of birds_t data frame 
ggplot(birds_t, aes(x = Plot, y = total_species, , fill = Transect))+
  geom_bar(stat = "identity", position='dodge', color='black')+
  scale_fill_manual(values= c('steelblue2', 'indianred2', 'orange1', 'green'))+
  labs(x = "Plot", y = "Species Richness", fill = "Sampling Location")

## Abundance in the northern and southern plot ##

# Create subsets for the northern and southern plots
North<- subset(birds[birds$Plot=='North',])
South<- subset(birds[birds$Plot=='South',])

# create a data frame of species and species counts in the northern and southern plot 
birdsN <- North %>%
  group_by(scientificName) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

birdsS <- South %>%
  group_by(species) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

# create a barplot of species counts in both plots 
BN<-ggplot(birdsN, 
       aes(x=species, y=TotalCount))+
  geom_bar(stat='identity', position='dodge', color='black', fill='steelblue2')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))+
  labs(x='Species', y='Individual Count')+
  ylim(c(0,26))
  
BS<-ggplot(birdsS,
       aes(x=species, y=TotalCount))+
  geom_bar(stat='identity', position='dodge', color='black', fill='indianred2')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))+
  labs(x='Species', y='Individual Count')+
  ylim(c(0,26))

ggarrange(BN,BS,
          labels=c('A','B'),
          nrow=1, ncol=2)

# Calculate species richness
length(unique(birds$species))
  # 17 
length(unique(North$species))
  # 11
length(unique(South$species))
  # 11

# Calculate abundance 
sum(birds$individualCount)
  # 141 
sum(North$individualCount)
  # 81
sum(South$individualCount)
  # 60 

# Calculate Shannon Index

shannon<-diversity(birdsN$TotalCount, index='shannon')
shannon
# 1.82 

shannon<-diversity(birdsS$TotalCount, index='shannon')
shannon
# 2.09

###### FRESHWATER INVERTEBRATES #######

# read in freshwater data set
freshwater<- read.csv('Data/Freshwater.csv')

# create a fixed order of sampling locations 
sampling_location<-c('North Stream', 'South Stream', 'Marsh')

# subset data frame for each sampling location 
SouthStream<-subset(freshwater[freshwater$Sampling.Location=='South Stream',])
NorthStream<-subset(freshwater[freshwater$Sampling.Location=='North Stream',])
Marsh<-subset(freshwater[freshwater$Sampling.Location=='Marsh',])

# create data frames of species and species counts 
finvertS <- SouthStream %>%
  group_by(order) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

finvertN<- NorthStream %>% 
  group_by(order) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

finvertM <- Marsh %>%
  group_by(order) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

# make a bar plot of subsetted data frames 
FPN<-ggplot(finvertN, 
       aes(x=order, y=TotalCount))+
  geom_bar(stat='identity', position='dodge', color='black', fill='steelblue2')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))+
  labs(x='Order', y='Individual Count')

FPS<-ggplot(finvertS, 
       aes(x=order, y=TotalCount))+
  geom_bar(stat='identity', position='dodge', color='black', fill='indianred2')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))+
  labs(x='Order', y='Individual Count')+
  ylim(c(0,18))

FPM<-ggplot(finvertM, 
       aes(x=order, y=TotalCount))+
  geom_bar(stat='identity', position='dodge', color='black', fill='orange1')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))+
  labs(x='Order', y='Individual Count')+
  ylim(c(0,18))

ggarrange(FPN, FPS, FPM,
          labels = c('A,', 'B', 'C',
                     ncol=1, nrow=3))

# Calculate order richness
length(unique(freshwater$order))
# 8
length(unique(finvertN$order))
# 6 
length(unique(finvertS$order))
# 5 
length(unique(finvertM$order))
# 6 

# Calculate abundance 
sum(freshwater$individualCount)
# 90 
sum(NorthStream$individualCount)
# 51
sum(SouthStream$individualCount)
# 19
sum(Marsh$individualCount)
# 20 

# Calculate water quality indices 

# North
BMWP<-5+5+5+4+10+1+10+10+10
ASPT<-BMWP/9
  # 6.6666
  # Excellent

# South
BMWP<-5+5+10+10+1
ASPT<-BMWP/5  
  # 6.2 
  # Excellent 

# Marsh 
BMWP<- 5+5+10+6+1+10
ASPT<- BMWP/6
  # 5.166667
  # Excellent 

###### TREES #######

# read in data set
tree<-read.csv('Data/Tree.csv')

# Create subsets for each plot 
TNorth<- subset(tree[tree$Plot=='North',])
TSouth<- subset(tree[tree$Plot=='South',])

# create data frames of tree species abundance by plot 
treesN <- TNorth %>%
  group_by(species) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

treesS <- TSouth %>%
  group_by(species) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

# make graphs of abundance by plot 
TN<-ggplot(treesN, 
           aes(x=species, y=TotalCount))+
  geom_bar(stat='identity', position='dodge', color='black', fill='steelblue2')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))+
  labs(x='Species', y='Individual Count')+
  scale_y_continuous(breaks = seq(0, 10, by = 2))

TS<-ggplot(treesS,
           aes(x=species, y=TotalCount))+
  geom_bar(stat='identity', position='dodge', color='black', fill='indianred2')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))+
  labs(x='Species', y='Individual Count')

ggarrange(TN, TS,
          labels = c('A', 'B', nrow=2))

## Species richness by plot ##

# Create a fixed order of sampling locations 
transect_order<-c('Lower','Intermediate','Upper')

# create a data frame of tree counts per species by plot and transect
trees_t<- tree %>%
  group_by(Plot, species, Transect = factor(Transect, levels = transect_order)) %>%
  summarize(count = n()) %>%
  group_by(Plot, Transect) %>%
  summarize(total_species = n_distinct(species))


#Plot<-c('North', 'North', 'North', 'South', 'South', 'South')
#Transect<-c('Lower','Intermediate','Upper','Lower','Intermediate','Upper')
#total_species<-c(4,0,0,5,1,3)
#trees_t<-data.frame(Plot, Transect,total_species)
#trees_t<-trees_t %>%
 # group_by(Plot, total_species, Transect=factor(Transect, levels = transect_order))

# Create a bar plot of birds_t data frame 
ggplot(trees_t, aes(x = Plot, y = total_species, , fill = Transect))+
  geom_bar(stat = "identity", position='dodge', color='black')+
  scale_fill_manual(values= c('steelblue2', 'indianred2', 'orange1'))+
  labs(x = "Plot", y = "Species Richness", fill = "Sampling Location")

##### TERRESTRIAL INVERTEBRATES ######

# read in dataset 
terrestrial<-read.csv('Data/Terrestrial.csv')

# create a fixed order of sampling locations 
transect_order<-c('Lower','Intermediate','Upper')

# remove observations not identified to family level
terrestrial_refined_family <- terrestrial[terrestrial$family != "", ]

# create data frame of family richness by plot and transect 
terrestrial_family<- terrestrial_refined_family %>%
  group_by(Plot, family, Transect = factor(Transect, levels = transect_order)) %>%
  summarize(count = n()) %>%
  group_by(Plot, Transect) %>%
  summarize(total_order = n_distinct(family))

# plot the terrestrial_family data frame 
ggplot(terrestrial_family, aes(x = Plot, y = total_order,fill = Transect))+
  geom_bar(stat = "identity", position='dodge', color='black')+
  scale_fill_manual(values= c('steelblue2', 'indianred2', 'orange1'))+
  labs(x = "Plot", y = "Family Richness", fill = "Sampling Location")

# subset the data for northern and southern plot 
south<-subset(terrestrial_refined_family[terrestrial_refined_family$Plot=='South',])
north<-subset(terrestrial_refined_family[terrestrial_refined_family$Plot=='North',])

# create data frames of total number of individuals sampled per family 
southF <- south %>%
  group_by(family) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

northF <- north %>%
  group_by(family) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

# plot abundance per family in both plots 
SF<-ggplot(southF, 
           aes(x=family, y=TotalCount))+
  geom_bar(stat='identity', position='dodge', color='black', fill='indianred2')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))+
  labs(x='Family', y='Individual Count')+
  ylim(c(0,50))

NF<-ggplot(northF, 
           aes(x=family, y=TotalCount))+
  geom_bar(stat='identity', position='dodge', color='black', fill='steelblue2')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))+
  labs(x='Family', y='Individual Count')

ggarrange(NF,SF,
          labels=c('A','B'),
          nrow=2, ncol=1)

## family richness in each plot ##
length(unique(south$family))
# 29
length(unique(north$family))
# 37 

## Shannon index ##
shannon<-diversity(southF$TotalCount, index='shannon')
shannon
  # 2.56
shannon<-diversity(northF$TotalCount, index='shannon')
shannon
  # 2.86

##### BATS #####

# read in dataset 
bats<-read.csv('Data/Bats.csv')

# create fixed order of nights 
night_order<-c(1,2,3,4)

# create data frame of bat species richness by plot and night
bats_s<- bats %>%
  group_by(Plot, scientificName, Night = factor(Night, levels = night_order)) %>%
  summarize(count = n()) %>%
  group_by(Plot, Night) %>%
  summarize(total_species = n_distinct(scientificName))

# plot graph of species richness by plot and night 
ggplot(bats_s, aes(x = Night, y = total_species, fill = Plot))+
  geom_bar(stat = "identity", position='dodge', color='black')+
  scale_fill_manual(values= c('steelblue2', 'indianred2'))+
  labs(x = "Night of Survey", y = "Species Richness", fill = "Plot")

# subset the data for northern and southern plot 
North<- subset(bats[bats$Plot=='North',])
South<- subset(bats[bats$Plot=='South',])

# create data frames of species in each plot 
batsN <- North %>%
  group_by(scientificName) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

batsS <- South %>%
  group_by(scientificName) %>%
  summarize(TotalCount = sum(individualCount, na.rm = TRUE))

# calculate species richness 
length(unique(bats$scientificName))
length(unique(batsN$scientificName))
length(unique(batsS$scientificName))

# Create a presence-absence matrix using the table function

# read in presence absence data set 
bat_mat<-read.csv('Data/Bat matrix.csv')

# convert into matrix 
bat_mat<-as.matrix(bat_mat)

# create row names vector 
row_names <- bat_mat[,1]

# convert into numeric matrix from character matrix 
bat_mat <- apply(bat_mat, 2, as.numeric)

# Add row names into matrix 
rownames(bat_mat)<-row_names
bat_mat<-bat_mat[,-1]
bat_mat

# Plot heat map of presence absence data
pheatmap(bat_mat,
         color=c('black','steelblue1'))

