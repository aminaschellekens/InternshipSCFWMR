#FISHERIES INTERNSHIP SABA CONSERVATION FOUNDATION ~ January - May 2025
#Amina Schellekens

#SHORT INTERVIEW INFORMARTION AFTER ONBOARD SAMPLING

#Install/load packages
install.packages("googlesheets4")
library(googlesheets4)
library(dplyr)

#Import data
gs4_auth(email = "amina.schellekens@gmail.com") #Replace with your own email
sheet_url <- "https://docs.google.com/spreadsheets/d/1bPAdnLJkTQPMfCIoFykc_SKyXqrFadk1S81zEupE2OE/edit?gid=1378040648#gid=1378040648"
data <- read_sheet(sheet_url, sheet = 5)

data$Depth_m_max <- as.numeric(data$Depth_m_max)
data$Depth_m_min <- as.numeric(data$Depth_m_min)


#Define the Trip ID
trip_id <- 6039

# Total number of landed lobsters
total_lobsters <- data %>%
  filter(Trip_ID == trip_id, Species_sn == "Panulirus argus", `Landed/Discarded` == "Landed") %>%
  tally() %>%
  pull(n)
print(total_lobsters)

# Total number of berried lobsters
berried_lobsters <- data %>%
  filter(Trip_ID == trip_id, Species_sn == "Panulirus argus", Berried == "Y") %>%
  tally() %>%
  pull(n)
print(berried_lobsters)

# Total number of short lobsters
short_lobsters <- data %>%
  filter(Trip_ID == trip_id, Species_sn == "Panulirus argus", Length < 9.5) %>%
  tally() %>%
  pull(n)
print(short_lobsters)

# Total number of Nurse sharks
total_nurse <- data %>%
  filter(Trip_ID == trip_id, Species_sn == "Ginglymostoma cirratum") %>%
  tally() %>%
  pull(n)
print(total_nurse)

# Total number of Lionfish
total_lion <- data %>%
  filter(Trip_ID == trip_id, Species_sn == "Pterois volitans") %>%
  tally() %>%
  pull(n)
print(total_lion)

# Total number of King crabs (= channel clinging crab)
total_kingcrab <- data %>%
  filter(Trip_ID == trip_id, Species_sn == "Mithrax spinosissimus") %>%
  tally() %>%
  pull(n)
print(total_kingcrab)

# Species list of non-fish species
non_fish_species <- c("Panulirus argus", "Dromia erythropus", "Ginglymostoma cirratum",
                      "Carpilius corallinus", "Portunus spinimanus", "Mithrax spinosissimus",
                      "Callinectes sapidus", "Calappa flammea", "Stenocionops furcatus coelata",
                      "Lobatus costatus", "Strombus gigas", "Dromia erythropus")

# Count number of mixed fish individuals
mixed_fish_count <- data %>%
  filter(Trip_ID == trip_id, Fishing_gear == 'LP', !Species_sn %in% non_fish_species) %>%
  tally() %>%
  pull(n)

# Convert total weight from grams to pounds
total_pounds_mixed_fish <- mixed_fish_count * 0.661
print(total_pounds_mixed_fish)

# Count number of red fish individuals
redfish_count <- data %>%
  filter(Trip_ID == trip_id, Fishing_gear == 'RP', !Species_sn %in% non_fish_species) %>%
  tally() %>%
  pull(n)

# Convert total weight from grams to pounds
total_pounds_red_fish <- redfish_count * 0.661
print(total_pounds_red_fish)

