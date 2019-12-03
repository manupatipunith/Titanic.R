#----Libraries----
library(dplyr)
library(tidyverse)
library(ggplot2)
#---import---
getwd()
setwd("C:/Users/punith.manupati/Desktop/R")
titanic<-read.csv("titanic.csv", header = T)
Titanic<-titanic
View(Titanic)

#---manipulation/self------------------------------------------------------------

#---Select---
Titanics<-Titanic %>% 
  select(2:12)

#---Reorder---
TitanicR<-Titanics[c(3,4,5,9,2,1)]
view(TitanicR)

#---NA work---
#....which(is.na(TitanicR))
#....na.omit(TitanicR)#it will remove entair row which has NA 
#Replacing NA with other values
#....TitanicR[which(is.na(TitanicR))]<-1
#finding NA in a single column and replacing the particular one 
#TitanicR$Age[which(is.na(TitanicR$Age))]<-0
##
#TitanicR[which(is.na(TitanicR))]<-"0"
TitanicR$Age[which(is.na(TitanicR$Age))]<-"0"

#---Filter---
TitanicF<-TitanicR %>% 
  filter(Sex=="female", Survived==0)
#TitanicF<-TitanicR %>% 
  #select(2,6) %>% 
  #filter(Survived==1) %>% group_by("Pclass") 

#---Mutate---

#TT<-TitanicR %>% mutate(ifelse(Survived==1,"YES","NO"))
#TitanicF$Surv.new<-ifelse(TitanicF$Survived==1,"Yes","No")
#TT<-TitanicR %>%
#  mutate(Survived)
#  ifelse("Surv.new"=="Yes","1","0")

#Mutate
TitanicM<-TitanicF %>% 
  mutate(Surv=ifelse(Surv.new=="Yes", "Survived","NOP"))
#trnsmutate
TitanicMt<-TitanicF %>% 
  transmute(Surv=ifelse(Surv.new=="Yes", "Survived","NOP"))
#---summarise---
TitanicSU<-Titanics %>%
  group_by(Pclass) %>% 
  summarise(Survived=mean(Survived, na.rm = TRUE))















#---manipulation/github------------------------------------------------------------

# Add a new feature to the data frame for SurvivedLabel
Titanic$SurvivedLabel <- ifelse(Titanic$Survived == 1, 
                                "Survived",
                                "Died")
# Add a new feature (i.e., column) to the data frame for FamilySize
Titanic$FamilySize <- 1 + titanic$SibSp + titanic$Parch
View(titanic)
# Look at the data types (i.e., R's version of Excel data formatting for cells)
str(titanic)
# Apply a row filter to the Titanic data frame - return only males
males <- Titanic[Titanic$Sex == "male",]
# Create summary statistics for male fares
summary(male$Fare)
var(males$Fare)
sd(males$Fare)
sum(males$Fare)
length(males$Fare)
# Ranges work just like in Excel - pick the first 5 rows of data.
first.five <- titanic[1:5,]
# View the first five columns of the first five rows.
View(first.five[, 1:5])
# Use an R package (i.e., the Excel equivalent of an Add-in) to
# create powerful visualizations easy.
#install.packages("ggplot2")
library(ggplot2)
ggplot(Titanic, aes(x = FamilySize, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 1)
# Use an R package (i.e., the Excel equivalent of an Add-in) to 
# make building data pivots easy.
#install.packages("dplyr")
library(dplyr)
pivot <- Titanic %>%
  group_by(Pclass, Sex, Survived) %>%
  summarize(AvgFamilySize = mean(FamilySize),
            PassengerCount = n()) %>%
  arrange(Pclass, Sex, Survived)
View(pivot)
#-------------Endgithub------------------------------------------------------------
