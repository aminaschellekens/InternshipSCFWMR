#FISHERIES INTERNSHIP SABA CONSERVATION FOUNDATION ~ January - May 2025
#Amina Schellekens
#FISHERY MONITORING DATA 2021-2025

# INSTALL PACKAGES --------------------------------------------------------

#install needed packages
library(readxl)
library(dplyr)
install.packages("MASS")
library(MASS)
library(AER)  
library(ggplot2)
install.packages("stringr")
library(stringr)
install.packages("gridExtra")
library(gridExtra)
install.packages("broom")
library(broom)
install.packages("writexl")
library(writexl)


# IMPORT AND FILTER DATA --------------------------------------------------

#Import data
short_interview <- read_excel("Documents/Studie/Master/Earth Sciences jaar 2 /Internship Saba/Data analysis/fisherydata2021_2025.xlsx")
onboard_21_23 <- read_excel("Documents/Studie/Master/Earth Sciences jaar 2 /Internship Saba/Data analysis/fisherydata2021_2025.xlsx", sheet = "Length measurements")
onboard_24_25 <- read_excel("Documents/Studie/Master/Earth Sciences jaar 2 /Internship Saba/Data analysis/FisheriesWorkbook July 2024.xlsx")

#preparation to merge the data frames
#Rename mismatched columns in onboard_24_25 to match onboard_21_23
names(onboard_24_25)[names(onboard_24_25) == "Date (DD/MM/YYYY)"] <- "Date (M/DD/Y)"
names(onboard_24_25)[names(onboard_24_25) == "Tarspot"] <- "tarspot"
names(onboard_24_25)[names(onboard_24_25) == "Landed/Discarded"] <- "Landed/discarded"
names(onboard_24_25)[names(onboard_24_25) == "Partial/Whole"] <- "Whole/Part"

# Keep only common columns
common_cols <- intersect(colnames(onboard_21_23), colnames(onboard_24_25))
onboard_21_23_common <- onboard_21_23[, common_cols]
onboard_24_25_common <- onboard_24_25[, common_cols]

onboard_21_25 <- rbind(onboard_21_23_common, onboard_24_25_common)
onboard_21_25 <- onboard_21_25[!is.na(onboard_21_25$`Date (M/DD/Y)`), ]


#Fix boat names
short_interview$Boat_name <- gsub("Sofia Carolina", "Carolina Sofia", short_interview$Boat_name)
short_interview$Boat_name <- tolower(short_interview$Boat_name)
unique_boats <- unique(short_interview$Boat_name)
print(unique_boats)


#fix species names
onboard_21_25$Species_cn <- tolower(onboard_21_25$Species_cn)
onboard$Species_cn <- tolower(onboard$Species_cn)

#Remove onboard measurement trip ID's from short interview data
#2021-2023 data does not have a onboard/port category so all tripID's with length measurements are considered 'onboard'
#Get a list of all trip_id's associated with onboard sampling
onboard_trip_ids <- onboard_21_25 %>%
  distinct(Trip_ID) %>%
  filter(!is.na(Trip_ID)) %>%  # Remove NAs
  pull(Trip_ID)  # Extract as a vector

print(onboard_trip_ids)          # ----> These need to be deleted from the short interview data

matching_rows <- short_interview %>%
  filter(Trip_ID %in% onboard_trip_ids) %>%
  nrow()

print(paste("Number of matching rows:", matching_rows)) #check how many rows will be removed

short_interview  <- short_interview  %>%
  filter(!Trip_ID %in% onboard_trip_ids) #remove the rows 

#Short interview filter foe lobster plots
si_lob_21_25 <- short_interview[short_interview$Fishing_gear == 'LP', ] 
si_red_21_25 <- short_interview[short_interview$Fishing_gear == 'RP', ] 

# LPUE  ---------------------------------------------------

#convert to numeric values
si_lob_21_25 $Lobster_no <- as.numeric(si_lob_21_25 $Lobster_no)
si_lob_21_25 $Traps_no <- as.numeric(si_lob_21_25 $Traps_no)
si_lob_21_25$Soaking_time_days <- as.numeric(si_lob_21_25 $Soaking_time_days)

#Remove NA's
si_lob_21_25 <- si_lob_21_25 %>% 
  filter(!is.na(Year) & !is.na(Lobster_no) & !is.na(Traps_no))

#Remove trips with Traps_no == 0
si_lob_21_25 <- si_lob_21_25 %>%
  filter(Traps_no > 0)

#Remove trips with Lobsters_no == 0 (just for now untill we know how to handle this)
si_lob_21_25 <- si_lob_21_25 %>%
  filter(Lobster_no > 0)

#Markatble sized lobsters
# Calculate mean lobsters landed per trap, SE, and 95% CI per year
si_lob_21_25_LPUE <- si_lob_21_25 %>%
  group_by(Year) %>%
  summarise(
    Total_Landed = sum(Lobster_no, na.rm = TRUE),       # Total lobsters landed
    Total_Effort = sum(Traps_no, na.rm = TRUE),          # Total traps used
    LPUE_SI = Total_Landed / Total_Effort,               # Mean lobsters per trap
    Mean_Lobster_Per_Trip = mean(Lobster_no, na.rm = TRUE), # Mean lobsters per trip
    SD_Landing_Per_Trap = sd(Lobster_no / Traps_no, na.rm = TRUE), # SD of lobsters/trap
    SD_Lobster_Per_Trip = sd(Lobster_no, na.rm = TRUE),  # SD of lobsters/trip
    Num_Trips = n(),                                     # Number of trips
    SE_LPUE = SD_Landing_Per_Trap / sqrt(Num_Trips),     # Standard error for LPUE
    SE_Lobster_Per_Trip = SD_Lobster_Per_Trip / sqrt(Num_Trips), # Standard error for lobsters/trip
    CI_lower_LPUE = LPUE_SI - 1.96 * SE_LPUE,            # 95% CI for LPUE
    CI_upper_LPUE = LPUE_SI + 1.96 * SE_LPUE,
    CI_lower_Lobster_Per_Trip = Mean_Lobster_Per_Trip - 1.96 * SE_Lobster_Per_Trip, # 95% CI for lobsters/trip
    CI_upper_Lobster_Per_Trip = Mean_Lobster_Per_Trip + 1.96 * SE_Lobster_Per_Trip,
    .groups = "drop"
  )


#As bar graph with 95% CI
lpue_l <-ggplot(si_lob_21_25_LPUE, aes(x = Year, y = LPUE_SI)) +
  geom_col(fill = "#FFD700", color = "black") +  
  geom_errorbar(aes(ymin = CI_lower_LPUE, ymax = CI_upper_LPUE), width = 0.2) +  # Error bars
  labs(
    title = "Mean Lobster Landings per Trap (LPUE)",
    x = "Year",
    y = "LPUE"
  ) +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(size = 12))

#Berried lobsters
si_lob_21_25_CPUE_berried <- si_lob_21_25 %>%
  group_by(Year) %>%
  summarise(
    Total_catches = sum(Lobster_berried_no, na.rm = TRUE),       
    Total_Effort = sum(Traps_no, na.rm = TRUE),           
    CPUE_SI = Total_catches / Total_Effort,                
    Mean_Lobster_Per_Trip = mean(Lobster_berried_no, na.rm = TRUE),  
    SD_Landing_Per_Trap = sd(Lobster_berried_no / Traps_no, na.rm = TRUE),  
    SD_Lobster_Per_Trip = sd(Lobster_berried_no, na.rm = TRUE),   
    Num_Trips = n(),                                     
    SE_LPUE = SD_Landing_Per_Trap / sqrt(Num_Trips),      
    SE_Lobster_Per_Trip = SD_Lobster_Per_Trip / sqrt(Num_Trips),
    CI_lower_LPUE = CPUE_SI - 1.96 * SE_LPUE,           
    CI_upper_LPUE = CPUE_SI + 1.96 * SE_LPUE,
    CI_lower_Lobster_Per_Trip = Mean_Lobster_Per_Trip - 1.96 * SE_Lobster_Per_Trip,  
    CI_upper_Lobster_Per_Trip = Mean_Lobster_Per_Trip + 1.96 * SE_Lobster_Per_Trip
  )

lpue_b <- ggplot(si_lob_21_25_CPUE_berried, aes(x = Year, y = CPUE_SI)) +
  geom_col(fill = "#FFD700", color = "black") +  
  geom_errorbar(aes(ymin = CI_lower_LPUE, ymax = CI_upper_LPUE), width = 0.2) +  # Error bars
  labs(
    title = "Mean Berried Lobster Catches per Trap (CPUE)",
    x = "Year",
    y = "CPUE"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 12))

#Undersized lobsters
si_lob_21_25_CPUE_undersized <- si_lob_21_25 %>%
  group_by(Year) %>%
  summarise(
    Total_catches = sum(Lobster_undersized_no, na.rm = TRUE),        
    Total_Effort = sum(Traps_no, na.rm = TRUE),            
    CPUE_SI = Total_catches / Total_Effort,                 
    Mean_Lobster_Per_Trip = mean(Lobster_undersized_no, na.rm = TRUE),   
    SD_Landing_Per_Trap = sd(Lobster_undersized_no / Traps_no, na.rm = TRUE),   
    SD_Lobster_Per_Trip = sd(Lobster_undersized_no, na.rm = TRUE),    
    Num_Trips = n(),                                     
    SE_LPUE = SD_Landing_Per_Trap / sqrt(Num_Trips),       
    SE_Lobster_Per_Trip = SD_Lobster_Per_Trip / sqrt(Num_Trips),
    CI_lower_LPUE = CPUE_SI - 1.96 * SE_LPUE,            
    CI_upper_LPUE = CPUE_SI + 1.96 * SE_LPUE,
    CI_lower_Lobster_Per_Trip = Mean_Lobster_Per_Trip - 1.96 * SE_Lobster_Per_Trip,   
    CI_upper_Lobster_Per_Trip = Mean_Lobster_Per_Trip + 1.96 * SE_Lobster_Per_Trip,
    .groups = "drop"
  )

lpue_s <- ggplot(si_lob_21_25_CPUE_undersized, aes(x = Year, y = CPUE_SI)) +
  geom_col(fill = "#FFD700", color = "black") +  # Dodger blue for a contrast with berried
  geom_errorbar(aes(ymin = CI_lower_LPUE, ymax = CI_upper_LPUE), width = 0.2) +
  labs(
    title = "Mean Undersized Lobster Catches per Trap (CPUE)",
    x = "Year",
    y = "CPUE"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 12)) 

grid.arrange(lpue_l, lpue_b, lpue_s, nrow = 1)

#REDFISH (note that this is total fish and no distinction between redfish and mixed fish was made untill 2024)
#convert to numeric values
# Ensure Lobster_no and Traps_no are numeric in the red data frame
si_red_21_25$Fish_lbs <- as.numeric(si_red_21_25$Fish_lbs)
si_red_21_25$Traps_no <- as.numeric(si_red_21_25$Traps_no)

#Convert Fish_lbs to kilograms
si_red_21_25$Fish_kg <- si_red_21_25$Fish_lbs * 0.453592
#Remove rows where Year is NA
si_red_21_25 <- si_red_21_25 %>%
  filter(!is.na(Year))  

si_red_21_25 <- si_red_21_25 %>%
  filter(Traps_no > 0)

#Calculate mean landings per trap, standard error, and confidence intervals per year
si_red_21_25_LPUE <- si_red_21_25 %>%
  group_by(Year) %>%
  summarise(
    Total_Landed = sum(Fish_kg, na.rm = TRUE),  # Total landed fish (kg) per year
    Total_Effort = sum(Traps_no, na.rm = TRUE),  # Total traps (effort) per year
    Mean_Landing_Per_Trap = Total_Landed / Total_Effort,  # Mean landings per trap
    SD_Landing_Per_Trap = sd(Fish_kg / Traps_no, na.rm = TRUE),  # Standard deviation of landings per trap per trip
    # Calculate the number of trips (observations)
    Num_Trips = n(),  # Number of unique trips (observations)
    # Calculate standard error (SE) based on the number of trips
    SE = SD_Landing_Per_Trap / sqrt(Num_Trips),  # SE = SD / sqrt(n)
    # Calculate the 95% confidence intervals (CI)
    CI_lower = Mean_Landing_Per_Trap - 1.96 * SE,  # Lower bound of 95% CI
    CI_upper = Mean_Landing_Per_Trap + 1.96 * SE,  # Upper bound of 95% CI
    .groups = "drop"  # Ungroup after summarizing
  )

#Plot Mean Landing per Trap with 95% CI
ggplot(si_red_21_25_LPUE, aes(x = Year, y = Mean_Landing_Per_Trap)) +
  geom_col(fill = "#FFD700", color = "black") +  
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +  
  labs(
    title = "Mean Redfish Landings per Trap (LPUE)",
    x = "Year",
    y = "Mean Landing per Trap (kg)"
  ) +
  theme_minimal(base_size = 14)  # Clean theme



# MODELLING Lobster abundance: running the model ---------------------------------------------------

#Deal with trips that visit more than one area per trips --> Split & divide total catch equally among the areas
#Division is done in such a way that no non-integer values are created

# Initialize an empty list to store the new rows
new_rows <- list()

# Loop through each row in si_lob_21_25
for (i in 1:nrow(si_lob_21_25)) {
  
  # Split the Fishing_area column based on "/"
  areas <- unlist(strsplit(as.character(si_lob_21_25$Fishing_area[i]), split = "/"))
  
  # Get total number of lobsters and traps
  total_lobsters <- si_lob_21_25$Lobster_no[i]
  total_traps <- si_lob_21_25$Traps_no[i]
  
  # Check for multi-area trip
  if (length(areas) > 1) {
    
    # Compute base lobsters per area (integer division)
    lobsters_per_area <- floor(total_lobsters / length(areas))
    
    # Compute remainder lobsters
    remainder_lobsters <- total_lobsters %% length(areas)
    
    # Compute base traps per area (integer division)
    traps_per_area <- floor(total_traps / length(areas))
    
    # Compute remainder traps
    remainder_traps <- total_traps %% length(areas)
    
    # Create new rows for each area
    for (j in 1:length(areas)) {
      new_row <- si_lob_21_25[i, ]  # Duplicate the current row
      new_row$Fishing_area <- areas[j]  # Assign the current area
      
      # Distribute remainder lobsters fairly
      if (j <= remainder_lobsters) {
        new_row$Lobster_no <- lobsters_per_area + 1
      } else {
        new_row$Lobster_no <- lobsters_per_area
      }
      
      # Distribute remainder traps fairly
      if (j <= remainder_traps) {
        new_row$Traps_no <- traps_per_area + 1
      } else {
        new_row$Traps_no <- traps_per_area
      }
      
      # Add the new row to the list
      new_rows[[length(new_rows) + 1]] <- new_row
    }
    
  } else {
    # If only one area is visited, just add the current row as is
    new_rows[[length(new_rows) + 1]] <- si_lob_21_25[i, ]
  }
}

# Combine all the new rows into a final dataframe
si_lob_21_25 <- do.call(rbind, new_rows)

#DATA EXPLORATION
# Look at distribution of Soaking days
ggplot(data = si_lob_21_25, aes(x = Soaking_time_days, fill = factor(Month))) +
  geom_density(alpha = 0.5) +
  ggtitle("Distribution of Soaking Time")

# Distribution of Traps per trip
ggplot(data = si_lob_21_25, aes(x = Traps_no, fill = factor(Month))) +
  geom_density(alpha = 0.5) +
  ggtitle("Distribution of Number of Traps per Trip")

# Distribution of Catches per trip
ggplot(data = si_lob_21_25, aes(x = Lobster_no, fill = factor(Month))) +
  geom_density(alpha = 0.5) +
  ggtitle("Distribution of Number of Catches per Trip")

# Add column: number of lobsters per trap (effort)
si_lob_21_25 <- si_lob_21_25 %>%
  mutate(YperTrap = Lobster_no / Traps_no)

#Check dependence of soaking time on number of lobsters per trap
ggplot(data = si_lob_21_25, aes(x = Soaking_time_days, y = YperTrap)) +
  geom_point() +
  ggtitle("Soaking Time vs Lobsters per Trap")

#Plot number of lobsters against number of traps
ggplot(data = si_lob_21_25, aes(x = Traps_no, y = Lobster_no)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1.2) +
  ggtitle("Number of Traps vs Lobsters Caught")

#Table of how often each fishing area was visited
sort(table(si_lob_21_25$Fishing_area, useNA = "always"))

#How many trips per month?
month_levels <- c("January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December")

month_counts <- si_lob_21_25 %>%
  count(Month)

month_counts$Month <- factor(month_counts$Month, levels = month_levels)

#Filter out NA fishing areas and count visits per year and area
year_area_counts <- si_lob_21_25 %>%
  filter(!is.na(Fishing_area)) %>%
  count(Year, Fishing_area)

#Stacked bar chart: visits of fishing area per year
ggplot(year_area_counts, aes(x = factor(Year), y = n, fill = Fishing_area)) +
  geom_bar(stat = "identity") +
  labs(title = "Fishing Area Visits over time",
       x = "Year",
       y = "Number of Visits",
       fill = "Fishing Area") +
  theme_minimal()


ggplot(month_counts, aes(x = Month, y = n, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Lobster Trips per Month", x = "Month", y = "No. of Trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. CHECK FOR OUTLIERS
dotchart(si_lob_21_25$Lobster_no, main = "Lobster Catches per Trip")         # Number of lobsters
dotchart(log(si_lob_21_25$Traps_no + 0.1), main = "Log(Number of Traps)")     # Log number of traps

# Log-transform Lobster_no and Traps_no
si_lob_21_25 <- si_lob_21_25 %>%
  mutate(
    logLob = log(Lobster_no + 0.1),
    logTraps = log(Traps_no + 0.1)
  )

#Remove missing values in predictors
si_lob_21_25 <- si_lob_21_25 %>%
  filter(!is.na(Year) & !is.na(Month) & !is.na(Boat_name) & !is.na(Fishing_area) & !is.na(logTraps))

# 4. CHECK VARIANCE HOMOGENEITY
ggplot(si_lob_21_25, aes(factor(Month), logLob)) +
  geom_boxplot() +
  ggtitle("Log(Lobsters) per Month")

ggplot(si_lob_21_25, aes(factor(Boat_name), logLob)) +
  geom_boxplot() +
  ggtitle("Log(Lobsters) per Boat")

#Histogram of number of lobsters per trip
ggplot(data = si_lob_21_25, aes(x = Lobster_no)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black", boundary = 0) +
  labs(
    title = "Distribution of Lobsters Caught per Trip",
    x = "Number of Lobsters Caught",
    y = "Number of Trips"
  ) +
  theme_minimal(base_size = 14)

# Mean and Variance of Lobster_no
mean(si_lob_21_25$Lobster_no, na.rm = TRUE) # --> Check mean #59.4385
var(si_lob_21_25$Lobster_no, na.rm = TRUE)  # --> Check variance #1398.803
summary(si_lob_21_25$Lobster_no)
summary(si_lob_21_25$Traps_no)

# --> Variance is much larger than mean --> suggests overdispersion

#5. TRY DIFFERENT MODELS AND COMPARE

# M1: Poisson Regression
M1 <- glm(Lobster_no ~ factor(Year) + factor(Month) + factor(Boat_name) + factor(Fishing_area) + logTraps,
          data = si_lob_21_25,
          family = "poisson") #AIC = 11446

summary(M1)  # Check model output
dispersiontest(M1)  # Test for overdispersion --> p-value < 2.2e-16 --> so overdispersed

# M2: Negative Binomial model (since Poisson is likely overdispersed)
M2 <- glm.nb(Lobster_no ~ factor(Year) + factor(Month) + factor(Boat_name) + factor(Fishing_area) + logTraps,
             data = si_lob_21_25)

summary(M2)  # Check model output --> AIC = 6782.1

#Get AIC and log-likelihood for M2
M2_AIC <- AIC(M2)
M2_logLik <- logLik(M2)

# Print AIC and logLik
M2_AIC
M2_logLik

# LOG-LIKELIHOOD TESTS

# Define all predictors
predictors <- c("factor(Year)", "factor(Month)", "factor(Boat_name)", "factor(Fishing_area)", "logTraps")

# Initialize empty results table
M2_results <- data.frame(
  Term_Removed = c("<none (full model)>", predictors),
  Num_Parameters = c(length(coef(M2)), rep(NA, length(predictors))),
  AIC = c(M2_AIC, rep(NA, length(predictors))),
  LogLikelihoodRatio = c(NA, rep(NA, length(predictors))),
  P_Value = c(NA, rep(NA, length(predictors))),
  Significance_Level = c("", rep("", length(predictors)))
)

# Loop to remove each predictor one-by-one
for (i in seq_along(predictors)) {
  pred <- predictors[i]
  
  # Create reduced model formula (drop one predictor)
  reduced_formula <- as.formula(paste("Lobster_no ~", paste(setdiff(predictors, pred), collapse = " + ")))
  
  # Fit reduced model
  reduced_model <- glm.nb(reduced_formula, data = si_lob_21_25)
  
  # Calculate Likelihood Ratio Test (LRT)
  logLik_reduced <- logLik(reduced_model)
  LRT_stat <- -2 * (logLik_reduced - M2_logLik)
  p_value <- pchisq(LRT_stat, df = abs(length(coef(M2)) - length(coef(reduced_model))), lower.tail = FALSE)
  
  # Define significance levels
  sig_level <- ifelse(p_value < 0.001, "***",
                      ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "")))
  
  # Save to results
  M2_results[i + 1, "Num_Parameters"] <- length(coef(reduced_model))
  M2_results[i + 1, "AIC"] <- AIC(reduced_model)
  M2_results[i + 1, "LogLikelihoodRatio"] <- as.numeric(LRT_stat)
  M2_results[i + 1, "P_Value"] <- p_value
  M2_results[i + 1, "Significance_Level"] <- sig_level
}

# View results
print(M2_results)

#Plotting residuals of M2
residuals_M2 <- residuals(M2)
plot(fitted(M2), residuals_M2, main="Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")  # Add a horizontal line at 0

# Q-Q plot to check normality of residuals
qqnorm(residuals_M2)
qqline(residuals_M2, col = "red")

#Save the data frame to an Excel file
write_xlsx(M2_results, "M2_results.xlsx")

# PREDICTIONS OPTION 1 --------
# predict amount of lobster per year based on all the other response variables

#Get predictions and standard errors from the model
predictions <- predict(M2, newdata = si_lob_21_25, type = "response", se.fit = TRUE)

#Extract predicted values and standard errors
si_lob_21_25$pred <- predictions$fit            
si_lob_21_25$se_pred <- predictions$se.fit      

#Calculate the 95% confidence intervals
ci_lower <- si_lob_21_25$pred - 1.96 * si_lob_21_25$se_pred
ci_upper <- si_lob_21_25$pred + 1.96 * si_lob_21_25$se_pred

#Add confidence intervals to dataframe
si_lob_21_25$ci_lower <- ci_lower
si_lob_21_25$ci_upper <- ci_upper

#Summarize the data by year, including the predicted values and confidence intervals
avg_preds <- si_lob_21_25 %>%
  group_by(Year) %>%
  summarise(
    Predicted_Lobsters = mean(pred, na.rm = TRUE),
    CI_Lower = mean(ci_lower, na.rm = TRUE),
    CI_Upper = mean(ci_upper, na.rm = TRUE)
  )

#Plotting the results
ggplot(avg_preds, aes(x = factor(Year), y = Predicted_Lobsters, group = 1)) + 
  # Confidence interval as a shaded ribbon
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "steelblue", alpha = 0.2) + 
  # Add points
  geom_point(color = "steelblue", size = 3) +  
  # Connect the points with a line
  geom_line(color = "steelblue", size = 1) +   
  labs(
    title = "Average Predicted Lobsters per Trip by Year with 95% CI",
    x = "Year",
    y = "Predicted Lobsters per Trip"
  ) + 
  theme_minimal()



# PREDICTIONS OPTION 2 

# PREDICTIONS OPTIONS 2 ---------------------------------------------------
#As done in repport: choose one representative from each variable to make predictions with


# Unique years available
years <- sort(unique(si_lob_21_25$Year))

# Create new data for prediction
#Here you make predictions for the number of lobsters for a speciifc boat, area and month.
#Values cannot be directly translated into absolute numbers of lobster stock however the differences percentual between the year are the 'year effect'

predat_year <- data.frame(
  Year = factor(years),   
  Month = "December",      
  Boat_name = "bridgette", 
  logTraps = mean(si_lob_21_25$logTraps, na.rm = TRUE),  
  Fishing_area = "B3")     

#2 MAKE PREDICTIONS USING THE MODEL
#Use your full model M2
preds_year <- predict(M2, newdata = predat_year, type = "link", se.fit = TRUE)

#Calculate fitted values and confidence intervals
predat_year$fit <- exp(preds_year$fit)  # predicted lobster numbers
predat_year$lb <- exp(preds_year$fit - 2 * preds_year$se.fit)  # lower bound
predat_year$ub <- exp(preds_year$fit + 2 * preds_year$se.fit)  # upper bound


ggplot(data = predat_year) + 
  geom_pointrange(aes(x = Year, y = fit, ymin = lb, ymax = ub), col = "blue") + 
  xlab("Year") +  # Corrected the label for the x-axis
  ylab("Modelled Number of Lobsters per Month") + 
  ggtitle("Modelled Year Effect") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability


#MODDEL MONTH EFFECT
#Define the months as numeric values
#August has no data (2024-2025) so is not included
m <- c("January", "February", "March", "April", "May", "June", "July", "September", "October", "November", "December")

#Convert the Month column in lob_SI_lp to numeric values (1 = January, 2 = February, etc.)
m2 <- lob_SI_lp$Month

m2[lob_SI_lp$Month == "January"] <- 1
m2[lob_SI_lp$Month == "February"] <- 2
m2[lob_SI_lp$Month == "March"] <- 3
m2[lob_SI_lp$Month == "April"] <- 4
m2[lob_SI_lp$Month == "May"] <- 5
m2[lob_SI_lp$Month == "June"] <- 6
m2[lob_SI_lp$Month == "July"] <- 7
m2[lob_SI_lp$Month == "September"] <- 8
m2[lob_SI_lp$Month == "October"] <- 9
m2[lob_SI_lp$Month == "November"] <- 10
m2[lob_SI_lp$Month == "December"] <- 11

m2 <- as.numeric(m2)

# Add the numeric month values to the data frame
lob_SI_lp$m2 <- m2

#Create the predat data frame
predat <- data.frame(
  Month = m,  
  Boat_name = "Rhiannon",
  logTraps = log(mean(lob_SI_lp$Traps_no)), 
  Fishing_area = "B3"
)

#Make predictions using the model M2
preds <- predict(M2, newdata = predat, type = "link", se = TRUE)

# Calculate fitted values, standard errors, and confidence intervals
predat$fit <- exp(preds$fit)
predat$se <- exp(preds$se.fit)
predat$lb <- exp(preds$fit - 2 * preds$se.fit)
predat$ub <- exp(preds$fit + 2 * preds$se.fit)

# Add a numeric month column for plotting
predat$month <- 1:11

# Now you can create the plot
M2_month_effect <- ggplot(data = predat) +
  geom_ribbon(aes(ymin = lb, ymax = ub, x = month), alpha = 0.3) +
  geom_line(aes(x = month, y = fit), col = "blue") +
  xlab("Month") + 
  ylab("Numb. Lobster per standard trip") + 
  ggtitle("Modelled month effect") +
  scale_x_continuous(breaks = 1:11, labels = substr(m, 1, 3))  # Labels for months (e.g., Jan, Feb, Mar, etc.)

print(M2_month_effect)

# Check for missing values in the predictors used in the model
sum(is.na(si_lob_21_25$Year))
sum(is.na(si_lob_21_25$Month))
sum(is.na(si_lob_21_25$Boat_name))
sum(is.na(si_lob_21_25$Fishing_area))
sum(is.na(si_lob_21_25$logTraps))


# VISUALIZE MODEL VS RAW DATA ----------------------------------------------

#Getting the coefficients from the M2 model
model_coefs <- broom::tidy(M2) %>%
  filter(grepl("factor\\(Year\\)", term)) %>%
  mutate(
    Year = as.numeric(gsub("factor\\(Year\\)", "", term)),
    Exp_Effect = exp(estimate),
    CI_low = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  )

#Make 2021 the reference year
reference_year <- data.frame(
  Year = 2021,
  Exp_Effect = 1,
  CI_low = 1,
  CI_high = 1
)

model_effects <- bind_rows(reference_year, model_coefs[, c("Year", "Exp_Effect", "CI_low", "CI_high")]) %>%
  arrange(Year)

#Make LPUE from raw data relative to compare
#Make 2021 reference year as well
ref_lpue <- si_lob_21_25_LPUE$LPUE_SI[si_lob_21_25_LPUE$Year == 2021]

#Add relative LPUE column
si_lob_21_25_LPUE <- si_lob_21_25_LPUE %>%
  mutate(Rel_LPUE = LPUE_SI / ref_lpue) #devide each year LPUE by LPUE of reference year 2021

#Visualize
ggplot() +
  # Model-estimated year effects
  geom_line(data = model_effects, aes(x = Year, y = Exp_Effect, color = "Model relative LPUE"), size = 1.2) +
  geom_point(data = model_effects, aes(x = Year, y = Exp_Effect, color = "Model relative LPUE"), size = 3) +
  
  # Raw relative LPUE from actual data
  geom_line(data = si_lob_21_25_LPUE, aes(x = Year, y = Rel_LPUE, color = "Raw data relative LPUE"), size = 1.2) +
  geom_point(data = si_lob_21_25_LPUE, aes(x = Year, y = Rel_LPUE, color = "Raw data relative LPUE"), size = 3) +
  
  # Labels and theme
  labs(
    title = "Relative LPUE of Caribbean Spiny Lobster:\nRaw data relative LPUE vs Modeled relative LPUE",
    x = "Year",
    y = "Relative LPUE",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Model relative LPUE" = "darkred", "Raw data relative LPUE" = "steelblue")) +
  ylim(0.95, 1.3) +  # Adjust these limits as needed
  theme_minimal()





# LENGTH DATA LOBSTERS ----------------------------------------------------

#Lengths of CSL in 2021, 2022 and 2023 are reported as mm while from 2024 onwards they are reported as cm
#Fix this
onboard_21_25 <- onboard_21_25 %>%
  mutate(Length = ifelse(Species_cn == "caribbean spiny lobster" & year %in% c(2021, 2022, 2023),
                         Length / 10,
                         Length))

#Average size females over the years
avg_female_size <- onboard_21_25 %>%
  filter(Sex == "F", !is.na(Length), !is.na(year)) %>%
  group_by(year) %>%
  summarise(
    Avg_Length = mean(Length, na.rm = TRUE),
    SE = sd(Length, na.rm = TRUE) / sqrt(n())  # Standard error
  )

ggplot(avg_female_size, aes(x = year, y = Avg_Length, group = 1)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  geom_errorbar(aes(ymin = Avg_Length - 1.96 * SE, ymax = Avg_Length + 1.96 * SE),
                width = 0.2, alpha = 0.5) +
  labs(
    title = "Average Length of Female Lobsters",
    x = "Year",
    y = "Average Length (cm)"
  ) +
  theme_minimal()

#Average size males over the years
avg_male_size <- onboard_21_25 %>%
  filter(Sex == "M", !is.na(Length), !is.na(year)) %>%
  group_by(year) %>%
  summarise(
    Avg_Length = mean(Length, na.rm = TRUE),
    SE = sd(Length, na.rm = TRUE) / sqrt(n())  # Standard error
  )

ggplot(avg_male_size, aes(x = year, y = Avg_Length, group = 1)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  geom_errorbar(aes(ymin = Avg_Length - 1.96 * SE, ymax = Avg_Length + 1.96 * SE),
                width = 0.2, alpha = 0.5) +
  labs(
    title = "Average Length of Male Lobsters",
    x = "Year",
    y = "Average Length (cm)"
  ) +
  theme_minimal()

#One graph for male and female data
avg_size_by_sex <- onboard_21_25 %>%
  filter(Sex %in% c("F", "M"), !is.na(Length), !is.na(year)) %>%
  group_by(year, Sex) %>%
  summarise(
    Avg_Length = mean(Length, na.rm = TRUE),
    SE = sd(Length, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(avg_size_by_sex, aes(x = year, y = Avg_Length, color = Sex, group = Sex)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Avg_Length - 1.96 * SE, ymax = Avg_Length + 1.96 * SE),
                width = 0.4, alpha = 1) +
  labs(
    title = "Mean lobster length over time",
    x = "Year",
    y = "Mean Length (cm)",
    color = "Sex"
  ) +
  theme_minimal(base_size = 14)




# FISHING PRESSURE --------------------------------------------------------

#filter onboard data for caribbean spiny lobster

onboard_21_25_lob <- onboard_21_25 %>%
  filter(Species_cn == 'caribbean spiny lobster', !is.na(Length), !is.na(year), !is.na(Sex)) 

mean_length <- onboard_21_25_lob  %>% 
  group_by(Sex, year) %>%  
  summarise(Lmean = mean(Length, na.rm = TRUE), .groups = "drop")

#Create length frequency distribution
length_frequency <- onboard_21_25_lob %>% 
  group_by(year, Sex, Length) %>%  
  summarise(count = n(), .groups = "drop")


#Add modal frequency and define the threshold (50% of modal frequency)
length_frequency_with_modal <- length_frequency %>% 
  group_by(Sex, year) %>%  
  mutate(  
    modal_frequency = max(count, na.rm = TRUE),
    threshold = modal_frequency * 0.5
  )

#Add cumulative frequency
length_frequency_cumulative <- length_frequency_with_modal %>% 
  arrange(Length) %>% 
  group_by(Sex, year) %>%  
  mutate(cumulative_frequency = cumsum(count))

#Calculate Lc 
lc_values <- length_frequency_cumulative %>% 
  filter(cumulative_frequency >= threshold) %>%  #
  group_by(Sex, year) %>%  
  summarise(Lc = min(Length), .groups = "drop")  


#Linfinte data
Linf_males <- 19.9
Linf_females <- 17.2

#Calculate length when population is fished at MSY
fishing_pressure <- lc_values %>%
  mutate(
    Lf = case_when(
      Sex == "M" ~ 0.75 * Lc + 0.25 * Linf_males,
      Sex == "F" ~ 0.75 * Lc + 0.25 * Linf_females
    )
  )

#Calculate fishing pressure
fishing_pressure <- fishing_pressure %>%
  left_join(mean_length , by = c("Sex", "year")) %>%
  mutate(
    FP = Lf / Lmean,
    )

#Visualize

ggplot(fishing_pressure, aes(
  x = year,  
  y = FP, 
  fill = Sex
)) + 
  geom_col(position = position_dodge(width = 0.8)) +  
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1.0) +  
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red", linewidth = 0.7) +  
  geom_hline(yintercept = 1.1, linetype = "dashed", color = "red", linewidth = 0.7) +  
  labs(
    title = "Fishing Pressure over time",
    x = "Year",
    y = "Fishing Pressure"
  ) +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# LENGTH DATA REDFISH -----------------------------------------------------
# Define the species of interest
target_species <- c("yelloweye snapper", "vermillion snapper", "lane snapper", "blackfin snapper")

# Filter and summarize
avg_snapper_length <- onboard_21_25 %>%
  filter(Species_cn %in% target_species, !is.na(Length), !is.na(year)) %>%
  group_by(year, Species_cn) %>%
  summarise(
    Avg_Length = mean(Length, na.rm = TRUE),
    SE = sd(Length, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot
ggplot(avg_snapper_length, aes(x = year, y = Avg_Length, color = Species_cn, group = Species_cn)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Avg_Length - 1.96 * SE, ymax = Avg_Length + 1.96 * SE),
                width = 0.2, alpha = 0.5) +
  labs(
    title = "Average Length of Redfish Species",
    x = "Year",
    y = "Average Length (cm)",
    color = "Species"
  ) +
  theme_minimal(base_size = 14)
