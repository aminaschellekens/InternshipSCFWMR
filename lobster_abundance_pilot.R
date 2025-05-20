#FISHERIES INTERNSHIP SABA CONSERVATION FOUNDATION ~ January - May 2025
#Amina Schellekens

#LOBSTER ABUNDANCE SURVEY PILOT 

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(tidyr)
install.packages("readxl")


#Import data
onboard_sampling_survey <- read_excel("Documents/Studie/Master/Earth Sciences jaar 2 semester 1/Internship Saba/Data analysis/onboard sampling survey.xlsx")
onboard_sampling_survey <- onboard_sampling_survey %>%
  filter(!is.na(`Date deployment`))


#calculate CPUE
effort_by_date <- onboard_sampling_survey %>%
  group_by(`Date deployment`, `Line Name`) %>%
  summarise(unique_traps = n_distinct(`Trap #`), .groups = "drop")

total_effort <- sum(effort_by_date$unique_traps)
tot_lobsters <- sum(onboard_sampling_survey$`Common name` == "Caribbean Spiny Lobster", na.rm = TRUE) #25

cpue <- tot_lobsters / total_effort 
print(cpue) #0.5102041

#MALE/FEMALE RATIO
CSL_all <- onboard_sampling_survey %>%
  filter(`Common name` == "Caribbean Spiny Lobster", !is.na(Sex)) #N=9

CSL_females <- onboard_sampling_survey %>%
  filter(`Sex` == "F", `Common name` == "Caribbean Spiny Lobster") #N=9

CSL_males <- onboard_sampling_survey %>%
  filter(`Sex` == "M", `Common name` == "Caribbean Spiny Lobster") #N=15

#LENGTH DATA
#Calculate mean length 
CSL_all_length <- CSL_all %>%
  filter(!is.na(Length))

mean_length <- CSL_all_length %>%
  group_by(Sex) %>%  # Group by Sex
  summarise(Lmean = mean(Length, na.rm = TRUE), .groups = "drop")  # Calculate mean length

#Length frequency
length_frequency <- CSL_all_length %>%  
  group_by(Length, Sex) %>%  
  summarise(count = n(), .groups = "drop")

#Create a box plot for the length distribution of Caribbean Spiny Lobster by Sex
ggplot(CSL_all_length, aes(x = Sex, y = Length, fill = Sex)) +
  geom_boxplot(color = "black") +  # Box plot with different colors for each sex
  labs(title = "Mean length of CSL per sex", 
       x = "Sex", y = "Length (cm)") + 
  theme_minimal()


hist(CSL_all_length$Length,
     main = "Length Distribution for CSL",
     xlab = "Length (cm)",
     breaks = 4,
     col = "darkred",
     border = "white")

#WEIGTH
CSL_all_weight <- CSL_all %>%
  filter(!is.na(Weight))

ggplot(CSL_all_weight, aes(x = Sex, y = Weight, fill = Sex)) +
  geom_boxplot(color = "black") +  # Box plot with different colors for each sex
  labs(title = "Mean weight of CSL per sex", 
       x = "Sex", y = "Weight (lbs)") + 
  theme_minimal()

#HABITAT
#abundance per habitat
abundance_by_habitat <- onboard_sampling_survey %>%
  filter(`Common name` == "Caribbean Spiny Lobster") %>%
  group_by(`Predicted habitat`) %>%
  summarise(Abundance = n(), .groups = "drop")

print(abundance_by_habitat)

ggplot(abundance_by_habitat, aes(x = `Predicted habitat`, y = Abundance, fill = `Predicted habitat`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Caribbean Spiny Lobster Abundance by Habitat",
       x = "Habitat", y = "Number of Lobsters") +
  theme_minimal() +
  theme(legend.position = "none") 

#Size structure
size_structure <- onboard_sampling_survey %>%
  filter(`Common name` == "Caribbean Spiny Lobster",
         !is.na(Length),
         !is.na(`Predicted habitat`),
         !is.na(Sex),
         Sex != "") %>%
  mutate(Length = as.numeric(Length)) %>%
  group_by(`Predicted habitat`, Sex) %>%
  summarise(
    mean_length = mean(Length, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

print(size_structure)

#Plot size structure data
ggplot(onboard_sampling_survey %>%
         filter(`Common name` == "Caribbean Spiny Lobster", 
                !is.na(Length), 
                !is.na(`Predicted habitat`), 
                !is.na(Sex),
                Sex != ""),
       aes(x = `Predicted habitat`, y = Length, fill = Sex)) +
  geom_boxplot(color = "black", outlier.shape = 16, outlier.colour = "black") +
  labs(title = "Length Distribution of CSL by Predicted Habitat",
       x = "Predicted Habitat", y = "Length (cm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

#Habitat per line
Trial1_habitat <- onboard_sampling_survey %>%
  filter(`Line Name` == "Trial 1") %>%
  select(`Date deployment`, `Predicted habitat`, `Trap #`) %>%
  group_by(`Date deployment`, `Predicted habitat`) %>%
  summarise(Trap_Count = n_distinct(`Trap #`), .groups = 'drop') %>%
  arrange(`Date deployment`)

# Check if any traps have multiple habitats associated (grouped by date)
trap_habitats <- onboard_sampling_survey %>%
  filter(`Line Name` == "Trial 1") %>%
  group_by(`Date deployment`, `Trap #`) %>%  # Group by date and trap to check unique habitats per trap
  summarise(Habitats = unique(`Predicted habitat`), .groups = 'drop') %>%
  filter(length(Habitats) > 1)  # Check if there are traps associated with multiple habitats

Trial2_habitat <- onboard_sampling_survey %>%
  filter(`Line Name` == "Trial 2") %>%
  select(`Date deployment`, `Predicted habitat`, `Trap #`) %>%
  group_by(`Date deployment`, `Predicted habitat`) %>%
  summarise(Trap_Count = n_distinct(`Trap #`), .groups = 'drop') %>%
  arrange(`Date deployment`)

Trial3_habitat <- onboard_sampling_survey %>%
  filter(`Line Name` == "Trial 3") %>%
  select(`Date deployment`, `Predicted habitat`, `Trap #`) %>%
  group_by(`Date deployment`, `Predicted habitat`) %>%
  summarise(Trap_Count = n_distinct(`Trap #`), .groups = 'drop') %>%
  arrange(`Date deployment`)

#plot
habitat_colors <- c(
  "Sargassum field" = "#a6cee3",
  "Reef"     = "#eb6eb5",
  "Macro-algae field"    = "#b2df8a"
)

#Line 1
ggplot(Trial1_habitat, aes(x = factor(`Date deployment`, levels = c("January 29th", "February 5th")),
                           y = Trap_Count, fill = `Predicted habitat`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = habitat_colors) +
  labs(title = "Trap Count per Predicted Habitat (Line 1)",
       x = "Deployment Date",
       y = "Number of Traps",
       fill = "Predicted Habitat") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Line 2
ggplot(Trial2_habitat, aes(x = factor(`Date deployment`, levels = c("January 29th", "February 5th", "February 19th", "February 26th", "March 19th")), 
                           y = Trap_Count, fill = `Predicted habitat`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = habitat_colors) +
  labs(title = "Trap Count per Predicted Habitat (Line 2)",
       x = "Deployment Date",
       y = "Number of Traps",
       fill = "Predicted Habitat") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Line 3
ggplot(Trial3_habitat, aes(x = `Date deployment`, y = Trap_Count, fill = `Predicted habitat`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = habitat_colors) +
  labs(title = "Trap Count per Predicted Habitat (Line 3)",
       x = "Deployment Date",
       y = "Number of Traps",
       fill = "Predicted Habitat") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
