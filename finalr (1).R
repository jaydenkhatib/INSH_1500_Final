library("dplyr")
install.packages("tibble")
library("tibble")
install.packages("tidyverse")
library(tidyverse)

police_calls <- read.csv("911 Call Type Frequency by CT and Year.csv")
View(police_calls)
income2010 <- read.csv("income2010.csv")
income2014 <- read.csv("income2014.csv")
race2010 <- read.csv("households2010.csv")
race2014 <- read.csv("households2014.csv")
police_calls <- data.frame(police_calls)

# converting the data and creating the columns I need to analyze
income2010 <- as_tibble(income2010)
income2010
income2014 <- as_tibble(income2014)
income2014

race2010 <- as_tibble(race2010)
race2010
race2014 <- as_tibble(race2014)
race2014

names(income2010)[names(income2010) == "S1902_C02_015E"] <- "household_income"
names(income2014)[names(income2014) == "S1902_C02_015E"] <- "household_income"

names(race2010)[names(race2010) == "S2502_C01_010E"] <- "white_non_hispanic"
names(race2014)[names(race2014) == "S2502_C01_010E"] <- "white_non_hispanic"

head(as.numeric(income2010$household_income))
head(as.numeric(income2014$household_income))
head(as.numeric(race2010$white_non_hispanic))
head(as.numeric(race2014$white_non_hispanic))

income2010$household_income <- as.numeric(income2010$household_income)
income2014$household_income <- as.numeric(income2014$household_income)

race2010$white_non_hispanic <- as.numeric(race2010$white_non_hispanic)
race2014$white_non_hispanic <- as.numeric(race2014$white_non_hispanic)

income2014$household_income_change <- income2014$household_income - income2010$household_income
race2014$white_nh_change <- race2014$white_non_hispanic - race2010$white_non_hispanic

add_column(income2014, income2014$household_income_change)
add_column(race2014, race2014$white_nh_change)

race_and_income_change <- merge(x = income2014, y = race2014, by = "GEO_ID")

as.data.frame(race_and_income_change)

income_race_2014 <- race_and_income_change[c("GEO_ID", "household_income_change", "white_nh_change")]
View(income_race_2014)

names(income_race_2014)[names(income_race_2014) == "GEO_ID"] <- "census_tract"

police_calls <- as_tibble(police_calls)
head(as.numeric(police_calls$BBQ_2010))
head(as.numeric(police_calls$BBQ_2014))
head(as.numeric(police_calls$DISTRB_2010))
head(as.numeric(police_calls$DISTRB_2014))
head(as.numeric(police_calls$PARTY_2010))
head(as.numeric(police_calls$PARTY_2014))
head(as.numeric(police_calls$NOISE_2010))
head(as.numeric(police_calls$NOISE_2014))

police_calls$BBQ_2010 <- as.numeric(police_calls$BBQ_2010)
police_calls$BBQ_2014 <- as.numeric(police_calls$BBQ_2014)
police_calls$DISTRB_2010 <- as.numeric(police_calls$DISTRB_2010)
police_calls$DISTRB_2014 <- as.numeric(police_calls$DISTRB_2014)
police_calls$PARTY_2010 <- as.numeric(police_calls$PARTY_2010)
police_calls$PARTY_2014 <- as.numeric(police_calls$PARTY_2014)
police_calls$NOISE_2010 <- as.numeric(police_calls$NOISE_2010)
police_calls$NOISE_2014 <- as.numeric(police_calls$NOISE_2014)

police_calls$BBQ_2014 <- police_calls$BBQ_2014 - police_calls$BBQ_2010
police_calls$DISTRB_2014 <- police_calls$DISTRB_2014 - police_calls$DISTRB_2010
police_calls$PARTY_2014 <- police_calls$PARTY_2014 - police_calls$PARTY_2010
police_calls$NOISE_2014 <- police_calls$NOISE_2014 - police_calls$NOISE_2010

add_column(police_calls, police_calls$BBQ_2014)
add_column(police_calls, police_calls$DISTRB_2014)
add_column(police_calls, police_calls$PARTY_2014)
add_column(police_calls, police_calls$NOISE_2014)

as.data.frame(police_calls)

police_calls <- police_calls[c("CT_ID_10", "BBQ_2014", "DISTRB_2014", "PARTY_2014", "NOISE_2014")]
View(police_calls)

names(police_calls)[names(police_calls) == "CT_ID_10"] <- "census_tract"

income_race_2014$census_tract <- as.character(income_race_2014$census_tract)

income_race_2014$census_tract <- gsub( "1400000US", "", income_race_2014$census_tract)

gentrification_data <- merge(income_race_2014, police_calls, by = "census_tract")

gentrification_data[complete.cases(gentrification_data), ]
View(gentrification_data)

gentrification_data <- as_tibble(gentrification_data)

gentrification_data$total_crime <- gentrification_data$BBQ_2014 + gentrification_data$DISTRB_2014 + gentrification_data$PARTY_2014 + gentrification_data$NOISE_2014

add_column(gentrification_data, gentrification_data$total_crime)

as.data.frame(gentrification_data)

View(gentrification_data)

library(ggplot2)

gentrification_correlation_data <- gentrification_data[c(1:3,8)]

gentrification_correlation_data <-na.omit(gentrification_correlation_data)

cor(gentrification_correlation_data$household_income_change, gentrification_correlation_data$total_crime)
cor(gentrification_correlation_data$white_nh_change,gentrification_correlation_data$total_crime)

positive_changes <- filter(gentrification_correlation_data, household_income_change > 0)
positive_changes <- filter(gentrification_correlation_data, white_nh_change > 0)

high_change <- filter(gentrification_correlation_data, household_income_change > 8000)
high_change <- filter(gentrification_correlation_data, white_nh_change > 4)

cor(positive_changes$household_income_change, positive_changes$total_crime)
cor(positive_changes$white_nh_change, positive_changes$total_crime)

cor(high_change$household_income_change, high_change$total_crime)
cor(high_change$white_nh_change, high_change$total_crime)

plot(gentrification_correlation_data$household_income_change, gentrification_correlation_data$total_crime)
plot(gentrification_correlation_data$white_nh_change, gentrification_correlation_data$total_crime)
