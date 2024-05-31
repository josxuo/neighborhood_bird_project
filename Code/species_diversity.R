# Libraries
library(tidyverse)
library(vegan)
library(ggthemes)


# Load data
dat <- read_csv("Data/NBP_2023-10-02.csv")

# Inspect data
# str(dat)

# Tidy data
yr <- c(2005:2019, 2021, 2022) # Vector with years to exclude from data

df <- dat %>%  # filter undesired years, remove observations not ID'ed to species level, remove hybirds, create unique ID for individual surveys
 select(-notes) %>%
  filter(Year %in% yr,
         !grepl("sp.", Species), 
         !grepl(" x ", Species)) %>%
  mutate(SurveyID = paste(Year, Month, UniqueStationID, sep = ""))

# sort(unique(df$Year))  ## Check filtered out undesired years
# unique(df$sp) ## Should return just "N"


#### Update taxonomy / nomenclature

df[df$Species == "Northwestern Crow", ]$Species <- "American Crow"  # Northwestern Crow no longer considered unique species, merged with American Crow
df[df$Species == "Pacific-slope Flycatcher", ]$Species <- "Western Flycatcher"
df[df$Species == "Mew Gull", ]$Species <- "Short-billed Gull"
df[df$Species == "Thayer's Gull", ]$Species <- "Iceland Gull"

# sort(unique(df$Species))  ## Verify current names in species list

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

## Diversity data frames

S1 <- data.frame(Year = srm1$Year, 
                Park = srm1$Park, 
                UniqueStationID = srm1$UniqueStationID, 
                S = specnumber(srm1[, 4:204]),
                H = diversity(srm1[, 4:204]), 
                Simp = diversity(srm1[, 4:204], index = "simpson"))

S2 <- data.frame(Year = srm2$Year, 
                 Park = srm2$Park, 
                 S = specnumber(srm2[, 3:203]),
                 H = diversity(srm2[, 3:203]), 
                 Simp = diversity(srm2[, 3:203], index = "simpson"))

S3 <- data.frame(Year = srm3$Year, 
                 S = specnumber(srm3[, 2:202]),
                 H = diversity(srm3[, 2:202]), 
                 Simp = diversity(srm3[, 2:202], index = "simpson"))


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
ggplot(S1, aes(x = Year, y = S, colour = Park)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_clean()

### Last plot is difficult to read.

## Other species richness visualizations 

# Plot species richness for individual count circles over time

parks <- sort(unique(S2$Park))
nparks <- length(parks)

par(mfrow = c(3, 4))

for(i in 1:nparks){
  x <- S1[S1$Park == parks[i], ]$Year
  y <- S1[S1$Park == parks[i], ]$S
  m <- lm(y ~ x)
  plot(x, y, main = parks[i], ylab = "S", xlab = "Year", xlim = c(2005, 2022), ylim = c(0, 50))
  abline(m, col = "red")
}

# Plot richness for parks over time
par(mfrow = c(3, 4))

for(i in 1:nparks){
  x <- S2[S2$Park == parks[i], ]$Year
  y <- S2[S2$Park == parks[i], ]$S
  m <- lm(y ~ x)
  plot(x, y, main = parks[i], ylab = "S", xlab = "Year", xlim = c(2005, 2022), ylim = c(0, 130))
  abline(m, col = "red")
}

# Plot total combined richness over time

par(mfrow = c(1, 1))
plot(S3$Year, S3$S, ylab = "S", xlab = "Year", xlim = c(2005, 2022), ylim = c(0, 200),
     main = "Combined Species Richness")
abline(lm(S3$S ~ S3$Year), col = "red")

#### RARE SPECIES ####

### Are there any species that only appear once in the data? 

## Create a species response matrix for each individual survey
ones <- df %>%
  group_by(SurveyID, Species) %>%
  summarise(Count = sum(Observed)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  ungroup()

ones[is.na(ones)] <- 0  ## replace NA values with zero

ones <- ones[, 2:202] %>%  ## convert to detected / not detected, then calculate column sums
  mutate_if(is.numeric, ~1 * (. != 0)) %>%
  colSums()

ones[ones == 1] ## 22 species appear only once in data set,
ones[ones == 2] ## 17 species appear only twice
ones[ones == 3] ## 6 species appear only three times


## Visualizing diversity indices
## Shannon-Weaver and Simpson Indices evaluate both species richness, abundance, and evennes.

plot(S3$Year, S3$H)
abline(lm(S3$H ~ S3$Year), col = 'red')

plot(S3$Year, S3$Simp)
abline(lm(S3$Simp ~ S3$Year), col = 'red')

## Both diversity indices appear to decreasing over time. 

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


## Inspect data frame
str(det)
summary(det)
view(det)


## Explore proportion of surveys reporting

## Plot every combination of species and park (NOTE: 201 x 10 charts)
## Break it down into two batches

species <- sort(unique(det$Species))
sp1 <- species[1:100]
sp2 <- species[101:201]
parks <- sort(unique(det$Park))
nparks <- length(parks)

# BATCH 1-100
par(mfrow = c(4, 4))

for(i in 1:length(sp1)) {
  d <- det[det$Species == sp1[i], ]
  for(j in 1:nparks) {
    x <- d[d$Park == parks[j], ]$Year
    y <- d[d$Park == parks[j], ]$Det.Prop
    ymax <- case_when(max(y) > 0 ~ max(y), TRUE ~ 0.1)
    plot(x, y, xlab = "Year", ylab = "Percent reporting", main = paste(species[i], parks[j]),
         xlim = c(2005, 2022), ylim = c(0, ymax))
  }
}

# BATCH 101-201

par(mfrow = c(4, 4))

for(i in 1:length(sp2)) {
  d <- det[det$Species == sp2[i], ]
  for(j in 1:nparks) {
    x <- d[d$Park == parks[j], ]$Year
    y <- d[d$Park == parks[j], ]$Det.Prop
    ymax <- case_when(max(y) > 0 ~ max(y), TRUE ~ 0.1)
    plot(x, y, xlab = "Year", ylab = "Percent reporting", main = paste(species[i], parks[j]),
         xlim = c(2005, 2022), ylim = c(0, ymax))
  }
}
