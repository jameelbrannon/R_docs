# Install and load necessary libraries
if (!require("plyr")) install.packages("plyr", dependencies=TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies=TRUE)

library(plyr)
library(ggplot2)
library(gridExtra)

# Read the dataset
storm <- read.csv(bzfile("/Users/jameelbrannon/Desktop/weatherproject/repdata-data-StormData.csv.bz2"))

# Number of unique event types
length(unique(storm$EVTYPE))

# Translate all letters to lowercase and replace punctuation characters with a space
storm$EVTYPE <- gsub("[[:blank:][:punct:]+]", " ", tolower(storm$EVTYPE))

# Number of unique event types after cleaning
length(unique(storm$EVTYPE))

# Summarize fatalities and injuries by event type
casualties <- ddply(storm, .(EVTYPE), summarize,
                    fatalities = sum(FATALITIES),
                    injuries = sum(INJURIES))

# Find events that caused the most deaths and injuries
fatal_events <- head(casualties[order(casualties$fatalities, decreasing = TRUE), ], 10)
injury_events <- head(casualties[order(casualties$injuries, decreasing = TRUE), ], 10)

# Display the top events causing fatalities and injuries
fatal_events[, c("EVTYPE", "fatalities")]
injury_events[, c("EVTYPE", "injuries")]

# Function to transform exponent values
exp_transform <- function(e) {
  switch(tolower(e),
         'h' = 2,
         'k' = 3,
         'm' = 6,
         'b' = 9,
         if (!is.na(as.numeric(e))) as.numeric(e) else 0)
}

# Apply the exponent transformation and calculate property and crop damages
storm$prop_dmg <- storm$PROPDMG * (10 ^ sapply(storm$PROPDMGEXP, FUN = exp_transform))
storm$crop_dmg <- storm$CROPDMG * (10 ^ sapply(storm$CROPDMGEXP, FUN = exp_transform))

# Compute the economic loss by event type
econ_loss <- ddply(storm, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))

# Filter out events that caused no economic loss
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]

# Find events that caused the most property and crop damage
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = TRUE), ], 10)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = TRUE), ], 10)

# Display the top events causing property and crop damage
prop_dmg_events[, c("EVTYPE", "prop_dmg")]
crop_dmg_events[, c("EVTYPE", "crop_dmg")]

# Plotting the results
p1 <- ggplot(data = fatal_events,
             aes(x = reorder(EVTYPE, fatalities), y = fatalities, fill = fatalities)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Total number of fatalities") +
  xlab("Event type") +
  theme(legend.position = "none")

p2 <- ggplot(data = injury_events,
             aes(x = reorder(EVTYPE, injuries), y = injuries, fill = injuries)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  ylab("Total number of injuries") +
  xlab("Event type") +
  theme(legend.position = "none")

grid.arrange(p1, p2, main = "Top deadly weather events in the US (1950-2011)")

p1 <- ggplot(data = prop_dmg_events,
             aes(x = reorder(EVTYPE, prop_dmg), y = log10(prop_dmg), fill = prop_dmg)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Event type") +
  ylab("Property damage in dollars (log-scale)") +
  theme(legend.position = "none")

p2 <- ggplot(data = crop_dmg_events,
             aes(x = reorder(EVTYPE, crop_dmg), y = crop_dmg, fill = crop_dmg)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  xlab("Event type") +
  ylab("Crop damage in dollars") + 
  theme(legend.position = "none")

grid.arrange(p1, p2, main = "Weather costs to the US economy (1950-2011)")

