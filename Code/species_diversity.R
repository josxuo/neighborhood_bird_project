# Libraries
library(tidyverse)
library(vegan)
library(ggthemes)


# Load data
dat <- read_csv("Data/NBP_2023-10-02.csv")

# Inspect data
str(dat)

# Tidy data
yr <- c(2005:2019, 2021, 2022) # Exclude years prior to 2005, 2020, and 2023

df <- dat %>%
  select(-notes) %>%
  filter(Year %in% yr,
         !grepl("sp.", Species), 
         !grepl(" x ", Species)) %>%
  mutate(SurveyID = paste(Year, Month, UniqueStationID, sep = ""))

sort(unique(df$Year))  ## Check filtered out undesired years
unique(df$sp) ## Should return just "N"


#### Update taxonomy / nomenclature

df[df$Species == "Northwestern Crow", ]$Species <- "American Crow"  # Northwestern Crow no longer considered unique species, merged with American Crow
df[df$Species == "Pacific-slope Flycatcher", ]$Species <- "Western Flycatcher"
df[df$Species == "Mew Gull", ]$Species <- "Short-billed Gull"
df[df$Species == "Thayer's Gull", ]$Species <- "Iceland Gull"

sort(unique(df$Species))  ## Verify current names in species list

## Species response matrix for each individual survey station by year
srm1 <- df %>%
  group_by(Year, Park, UniqueStationID, Species) %>%
  summarise(Count = sum(Observed)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  ungroup()

srm1[is.na(srm1)] <- 0  # Replace NA values in species response matrix with 0s

## Species response matrix for each park by year
srm2 <- df %>%
  group_by(Year, Park, Species) %>%
  summarise(Count = sum(Observed)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  ungroup()

srm2[is.na(srm2)] <- 0

## Species response matrix for each year
srm3 <- df %>%
  group_by(Year, Species) %>%
  summarise(Count = sum(Observed)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  ungroup()

srm3[is.na(srm3)] <- 0

## Species richness dataframes

S1 <- data.frame(Year = srm1$Year, 
                Park = srm1$Park, 
                UniqueStationID = srm1$UniqueStationID, 
                S = specnumber(srm1[, 4:204]))

S2 <- data.frame(Year = srm2$Year, 
                 Park = srm2$Park, 
                 S = specnumber(srm2[, 3:203]))

S3 <- data.frame(Year = srm3$Year, 
                 S = specnumber(srm3[, 2:202]))


## Plot Species Richness

## Highest level of aggregation first
ggplot(S3, aes(Year, S)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_clean()

## S by Year by Park
ggplot(S2, aes(Year, S, colour = Park)) +
  geom_line() +
  geom_smooth(method = "lm", lty = 2, se = FALSE) +
  theme_clean()

## All survey stations all years
ggplot(S, aes(x = Year, y = S, colour = Park)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_clean()

### Plot is difficult to read.

parks <- sort(unique(S$Park))
nparks <- length(parks)

par(mfrow = c(3, 3))

for(i in 1:nparks){
  x <- S[S$Park == parks[i], ]$Year
  y <- S[S$Park == parks[i], ]$S
  m <- lm(y ~ x)
  plot(x, y, main = parks[i], ylab = "S", xlab = "Year")
  abline(m, col = "red")
}

par(mfrow = c(1, 1))
plot(S[S$Park == parks[1], ]$Year, S[S$Park == parks[1], ]$S, main = parks[1], ylab = "S", xlab = "Year")
lines(predict(lm(S[S$Park == parks[1], ]$S ~ S[S$Park == parks[1], ]$Year), col = "green"))
y <- S[S$Park == parks[1], ]$S
x <- S[S$Park == parks[1], ]$Year

plot(x, y, ylim = c(0, 50))
m <- lm(y ~ x)
abline(m, col = "red")


## Now create a detection / mean abundance per survey dataframe.  
det <- df %>%
  group_by(Year, Park, Month, UniqueStationID, Species) %>%
  summarise(Count = sum(Observed)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  ungroup()

det[is.na(det)] <- 0

det <- det %>%
  pivot_longer(5:205, names_to = "Species", values_to = "Count") %>%
  group_by(Year, Park, Species) %>%
  summarise(Abundance = sum(Count), Det.Count = sum(Count > 0), Surveys = length(UniqueStationID)) %>%
  ungroup() %>%
  mutate(Det.Prop = Det.Count / Surveys, MAPS = Abundance / Surveys)  # MAPS = mean abundance per survey


## Inspect dataframe
str(det)
summary(det)
view(det)
