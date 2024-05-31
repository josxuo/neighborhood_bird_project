#### OCCUPANCY ANALYSIS ####

# Libraries
library(tidyverse)
library(unmarked)

# Load data
dat <- read_csv("Data/NBP_2023-10-02.csv")

# Process data for occupancy analysis. Later need to subset count circles to just those
# that don't overlap with each other. Currently restrict analysis to year 2021, 
# to correspond with year of most recent site-level canopy cover covariate.

df <- dat %>%  # filter undesired years, remove observations not ID'ed to species level, remove hybirds, create unique ID for individual surveys
  select(-notes) %>%
  filter(Year == 2021,
         !grepl("sp.", Species), 
         !grepl(" x ", Species)) %>%
  mutate(SurveyID = paste(Year, Month, UniqueStationID, sep = ""),
         Month = factor(Month, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))

sort(unique(df$Year))  ## Check filtered out undesired years
unique(df$sp) ## Should return just "N"

#### Update taxonomy / nomenclature

df[df$Species == "Northwestern Crow", ]$Species <- "American Crow"  # Northwestern Crow no longer considered unique species, merged with American Crow
df[df$Species == "Pacific-slope Flycatcher", ]$Species <- "Western Flycatcher"
df[df$Species == "Mew Gull", ]$Species <- "Short-billed Gull"
df[df$Species == "Thayer's Gull", ]$Species <- "Iceland Gull"

sort(unique(df$Species))  ## Verify current names in species list

## Let's do an occupancy model for, say, Brown Creeper.
# Create detection / non-detection dataframe for each count circle

det <- df %>%
  group_by(UniqueStationID, Month, Species) %>%
  summarise(Count = sum(Observed)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  ungroup() %>%
  select(UniqueStationID, Month, `Brown Creeper`)

det[is.na(det)] <- 0  ## replace NA values with zero

detf <- det %>%  
  mutate(UniqueStationID = as.character(UniqueStationID)) %>% ## convert to detected / not detected
  mutate_if(is.numeric, ~1 * (. != 0)) %>%
  arrange(Month) %>%
  pivot_wider(names_from = Month, values_from = `Brown Creeper`)

view(detf)  ## Good, data structured correctly.

## Make sure data is a dataframe, otherwise unmarked package will throw errors

detf <- as.data.frame(detf[,-1])

## Create unmarked occupancy frame

UFO <- unmarkedFrameOccu(y = detf)
summary(UFO)  # Print summary. 86 sites with at least one detection. Mean obs = 8.02. Max num per site = 12. 

## We have 206 sites in the dataset. Naive occupancy calculated to be 86/206 = 0.42

## NEED TO ADD COVARIATE DATA, but until then, here are some simple models and occupancy estimates
m <- occu(~1 ~1, data = UFO)
backTransform(m, type = "state") ## Backtransformed occupancy estimate = 0.49
backTransform(m, type = "det")  ## Detection probability for BRCR estimated to be .221. 

## Try with Song Sparrow

det <- df %>%
  group_by(UniqueStationID, Month, Species) %>%
  summarise(Count = sum(Observed)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  ungroup() %>%
  select(UniqueStationID, Month, `Song Sparrow`)

det[is.na(det)] <- 0  ## replace NA values with zero

detf <- det %>%  
  mutate(UniqueStationID = as.character(UniqueStationID)) %>% ## convert to detected / not detected
  mutate_if(is.numeric, ~1 * (. != 0)) %>%
  arrange(Month) %>%
  pivot_wider(names_from = Month, values_from = `Song Sparrow`)

view(detf)  ## Good, data structured correctly.

## Make sure data is a dataframe, otherwise unmarked package will throw errors

detf <- as.data.frame(detf[,-1])

## Create unmarked occupancy frame

UFO <- unmarkedFrameOccu(y = detf)
summary(UFO)  # Print summary. 180 sites with at least one detection. Mean obs = 8.02. Max num per site = 12. 

## We have 206 sites in the dataset. Naive occupancy calculated to be 180/206 = 0.87

## NEED TO ADD COVARIATE DATA, but until then, here are some simple models and occupancy estimates
m <- occu(~1 ~1, data = UFO)
backTransform(m, type = "state") ## Backtransformed occupancy estimate = 0.88
backTransform(m, type = "det")  ## Detection probability for SOSP estimated to be .522. 
