#SABA CONSERVATION FOUNDATION AND WAGENINGEN MARINE RESEARCH INTERSHIP ~ Jan-May 2025
#Amina Schellekens

#BRUV DATA RED HIND SPAWNING SEASON

# Installing/loading needed packages --------------------------------------
library(dplyr)
library(ggplot2)

install.packages("vegan", dependencies=TRUE) #'no' to compilation question
library(vegan)

install.packages("tidyverse")
library(tidyverse)

library(tidyr)

install.packages("brms")  
library(brms)

library(MASS)

install.packages("emmeans") 
library(emmeans)

library(glmmTMB)

# Prepare data for analysis -----------------------------------------------

#Import data
data <- read.csv("~/Documents/Studie/Master/Earth Sciences jaar 2 semester 1/Internship Saba/Data analysis/MaxN Data Final.csv", sep=";")
data <- MaxN.Data.Final

#Add "spawning season" column
data <- data %>%
  mutate(spawning_season = ifelse(month %in% c("dec", "jan", "feb"), "in season",
                                  ifelse(month %in% c("may", "jun"), "out season", NA)))

#Check how many species have zero observations 
zero_species <- colSums(data[, 11:57]) == 0
sum(zero_species) 

#-----> All species have at least one observation

#Remove rows that have 0 for all species
data <- data[rowSums(data[, 11:(ncol(data) - 1)]) > 0, ]

# PERMANOVA ON SPECIES COMP.  ---------------------------------------------

#1. Species comp - season
#Extract species abundance data
species_matrix <- data[, 11:(ncol(data) - 1)]

#Compute Bray-Curtis dissimilarity
dissimilarity_matrix <- vegdist(species_matrix, method = "bray")
print(as.matrix(dissimilarity_matrix))


#Perform PERMANOVA
permanova_result <- adonis2(dissimilarity_matrix ~ spawning_season, data = data, permutations = 999)
print(permanova_result)      # -----> p =  0.001, Rsquared = 0.01802 
                             # -----> Species comp. differes significantly between spawning and non-spawning season
                             # -----> But low Rsquared indicates 'season' is a bad predictor for species comp.


#Visualize using Principal Coordinates Analysis
pcoa <- cmdscale(dissimilarity_matrix, k = 2, eig = TRUE)

#Convert to a data frame
pcoa_data <- as.data.frame(pcoa$points)
colnames(pcoa_data) <- c("PCoA1", "PCoA2")
pcoa_data$spawning_season <- data$spawning_season

#Plot the PCoA results
ggplot(pcoa_data, aes(x = PCoA1, y = PCoA2, color = spawning_season)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse() +
  theme_minimal() +
  labs(title = "PCoA of BRUV species composition",
       x = "PCoA1", 
       y = "PCoA2",
       color = "Spawning Season")


#2. species comp - month
# Perform PERMANOVA
adonis2(dissimilarity_matrix ~ month, data = data, permutations = 999)


pcoa_data$month <- data$month
ggplot(pcoa_data, aes(x = PCoA1, y = PCoA2, color = month)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse() +
  theme_minimal() +
  labs(title = "PCoA Ordination of species composition per month",
       x = "PCoA1", 
       y = "PCoA2",
       color = "Spawning Season")


#3. testing weather 'location' is a good explainer for the difference in species composition
permanova_location <- adonis2(dissimilarity_matrix ~ location, 
                              data = data, 
                              permutations = 999)
print(permanova_location)

# VISUALISATION -----------------------------------------------------------

#Bar graph to show in/out season abundance for selected species
selected_species <- c("Yellowhead.Wrasse", "Ocean.Surgeonfish", "Coney", 
                      "Red.Hind", "Queen.Triggerfish", "Bluehead.Wrasse", 
                      "Bicolor.Damselfish", "Slippery.Dick", "Bar.Jack")

species_counts <- data %>%
  group_by(spawning_season) %>%  # Group by in/out season
  summarise(across(all_of(selected_species), sum)) %>%  # Sum only selected species
  pivot_longer(cols = -spawning_season, names_to = "species", values_to = "count")


ggplot(species_counts, aes(x = species, y = count, fill = spawning_season)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodged bars for comparison
  theme_minimal() +
  labs(title = "Abundance of selected fished in and out of spawning season",
       x = "Species", 
       y = "Total Count",
       fill = "Spawning Season") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability



# RED HIND ABUNDANCE PER LOCATION -----------------------------------------

#Filter for Red Hind data
red_hind_data <- data[, c("month", "spawning_season", "location", "Red.Hind")]

red_hind_summary <- data %>%
  group_by(location) %>%
  summarise(Total_Abundance = sum(Red.Hind, na.rm = TRUE))

mean(red_hind_data$Red.Hind)
var(red_hind_data$Red.Hind)

#Boxplot of abundance per month
ggplot(data, aes(x = factor(month), y = Red.Hind, fill = spawning_season)) +
  geom_boxplot(color = "black") +  
  scale_fill_manual(values = c("in season" = "#FF69B4",  # Hot Pink for spawning season
                               "out season" = "#00CED1")) +  # Turquoise for non-spawning season
  labs(title = "Red Hind Abundance per Month",
       x = "Month", y = "Abundance", fill = "Spawning Season") +
  theme_minimal()

#red hind abundance in and out of spawning season
ggplot(data, aes(x = factor(spawning_season), y = Red.Hind, fill = spawning_season)) +
  geom_boxplot(color = "black") +  
  scale_fill_manual(values = c("in season" = "#FF69B4",  # Hot Pink for spawning season
                               "out season" = "#00CED1")) +  # Turquoise for non-spawning season
  labs(title = "Red Hind Abundance in/out of spawning season",
       x = "Month", y = "Abundance", fill = "Spawning Season") +
  theme_minimal()


#Histogram showing count frequncy
hist(red_hind_data$Red.Hind, 
     breaks = 30, 
     main = "Red Hind Abundance", 
     col = "lightblue", 
     xlab = "Number of Red Hinds Observed")


#Statistics: MONTH & ABUNDANCE
#Fit a Poisson GLM
poisson_model <- glm(Red.Hind ~ factor(month), data = red_hind_data, family = poisson())
summary(poisson_model) #---> AIC = 511.7

#Check for overdispersion
dispersion_ratio <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
dispersion_ratio  # If >1.5, consider a Negative Binomial model ---> ours is 1.438782
                                 #---> Gray area so run nb model and compare AIC values

#Negative Binomial model - month
nb_model <- glm.nb(Red.Hind ~ factor(month), data = red_hind_data)
summary(nb_model)  #AIC = 505.62 --> lower than poisson model --> this one performs better


#Perform ANOVA to check for month differences
anova(nb_model, test = "Chisq")    #--> output suggest jan and jun differ significantly from baseline month dec
                                   #--> perform post hoc 

#post hoc: TUKEY
posthoc_results <- emmeans(nb_model, pairwise ~ factor(month), adjust = "tukey")
summary(posthoc_results)    #--> no signicant difference between the months


#statistics: SEASON & ABUNDANCE

#Negative Binomial model
nb_model_ss <- glm.nb(Red.Hind ~ spawning_season, data = red_hind_data)
summary(nb_model_ss) #--> p=0.7580 --> no difference


#statistics: MONTH & LOCATION
red_hind_data$location <- as.factor(red_hind_data$location)
red_hind_data$month <- as.factor(red_hind_data$month)


trial <- glm.nb(Red.Hind ~ factor(location), data = red_hind_data)
summary(trial)



# QUEEN TRIGGERFISH -------------------------------------------------------

#Data frame with Red Hind & Queen trigger fish
RH_QT <- data[, c("month", "spawning_season", "location", "Red.Hind", "Queen.Triggerfish")]

#COUNT DATA
RH_QT_count <- RH_QT %>%
  select(month, spawning_season, Red.Hind, Queen.Triggerfish) %>%
  pivot_longer(cols = c(Red.Hind, Queen.Triggerfish), 
               names_to = "Species", values_to = "Count")

#Number of counts per month for both species
ggplot(RH_QT_count, aes(x = factor(month), y = Count, fill = Species)) +   
  geom_bar(stat = "identity", position = "dodge") +   
  labs(title = "Number of observed individuals of Red Hind & Queen Triggerfish per month",
       x = "Month", y = "Number of Individuals") +   
  scale_fill_manual(values = c("Red.Hind" = "#D72638",  
                               "Queen.Triggerfish" = "#3A89C9")) +  
  theme_minimal()

ggplot(RH_QT_count, aes(x = factor(spawning_season), y = Count, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Abundance of Red Hind & Queen Triggerfish in/out of spawning season",
       x = "Month", y = "Number of Individuals") +
  scale_fill_manual(values = c("Red.Hind" = "#D72638", "Queen.Triggerfish" = "#3A89C9")) +
  theme_minimal()

#ABUNDANCE
RH_QT_abundance <- RH_QT %>%
  select(spawning_season, month, Red.Hind, Queen.Triggerfish) %>%
  pivot_longer(cols = c(Red.Hind, Queen.Triggerfish), names_to = "Species", values_to = "Abundance")

ggplot(RH_QT_abundance, aes(x = factor(spawning_season), y = Abundance, fill = spawning_season)) +
  geom_boxplot(color = "black") +  
  facet_wrap(~Species) +  # Separate boxplots for each species
  scale_fill_manual(values = c("in season" = "#FF69B4", 
                               "out season" = "#00CED1")) +  
  labs(title = "Abundance of Red Hind & Queen Triggerfish In/Out of Spawning Season",
       x = "Spawning Season", y = "Abundance", fill = "Spawning Season") +
  theme_minimal()

ggplot(RH_QT_abundance, aes(x = factor(month), y = Abundance, fill = spawning_season)) +
  geom_boxplot(color = "black") +  
  facet_wrap(~Species) +  # Separate boxplots for each species
  scale_fill_manual(values = c("in season" = "#FF69B4",  # Hot Pink for spawning season
                               "out season" = "#00CED1")) +  # Turquoise for non-spawning season
  labs(title = "Abundance of Red Hind & Queen Triggerfish In/Out of Spawning Season",
       x = "Spawning Season", y = "Abundance", fill = "Spawning Season") +
  theme_minimal()

RH_QT_abundance %>%
  filter(Species == "Red.Hind") %>%
  ggplot(aes(x = factor(spawning_season), y = Abundance, fill = spawning_season)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("in season" = "#FF69B4", 
                               "out season" = "#00CED1")) +
  labs(title = "Abundance of Red Hind In/Out of Spawning Season",
       x = "Spawning Season", y = "Abundance", fill = "Spawning Season") +
  theme_minimal()


