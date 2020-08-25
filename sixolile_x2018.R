#https://www.youtube.com/watch?v=wisqb4BFmEY&t=377s

## iiag and timelines

library(readr)
library(tidyverse)
install.packages("gganimate")
library(gganimate)
library(plotly)
library(googleVis)
library(zoo)
library(dplyr)
library(lubridate)

extrafont::loadfonts(quiet = T) # add extra fonts to R

df_x2018 <- X2018_IIAG_Ranks 

#audit factor

class(df_x2018)

dim(df_x2018)

colnames(df_x2018)

str(df_x2018)

glimpse(df_x2018)

summary(df_x2018) #to get an idea on the idea

summary(df_x2018$overall_governance)

dataset1NAS <- subset(df_x2018, is.na(df_x2018$overall_governance))

head(df_x2018)

head(df_x2018, n = 15)

tail(df_x2018) 

complete.cases(df_x2018)


colnames(df_x2018) <- c("iso2c", "country", "year", "overall_governance",
                     "safety_rl", "ruleoflaw", "trans_acc", "personal_safety",
                     "nat_security", "participation_humanrights", "participation",
                     "rights", "gender", "sustain_eco_op", "public_manage", "business_env",
                     "infrastructure", "rural_sec", "human_dev", "welfare", "education",
                     "health")

cols.num <- c("overall_governance",
              "safety_rl", "ruleoflaw", "trans_acc", "personal_safety",
              "nat_security", "participation_humanrights", "participation",
              "rights", "gender", "sustain_eco_op", "public_manage", "business_env",
              "infrastructure", "rural_sec", "human_dev", "welfare", "education",
              "health")

glimpse(df_x2018)


df_x2018[cols.num] <- sapply(df_x2018[cols.num],as.numeric)

sapply(df_x2018, class)

df_x2018$iso3c <-  countrycode::countrycode(df_x2018$iso2c, origin = "iso2c", destination = "iso3c")

merged <- df_x2018

merged <- df_x2018 %>% 
  left_join(iso2c, by = "iso3c")

## time series
# transparency and acountability

df_trans <- aggregate(trans_acc ~ year, data = df_x2018, mean)

ggplot(df_trans) +
  geom_line(aes(x = year, y = trans_acc))


# overall governance

df_all <- aggregate(overall_governance ~ year, data = df_x2018, mean)

df_all_sub <- aggregate(overall_governance ~ year + col, data = merged, mean)

breaks <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)


ggplot(df_all) +
  geom_line(aes(x = year, y = overall_governance), size = 1) +
  scale_x_continuous(breaks = breaks, label = breaks) +
  labs(x = "Year", y = "IIAG Overall Governance Score", 
       caption = "Source: Mo Ibrahim Foundation, 6 August 2020.
Graphic: sixolile & Monique", 
       title = "Overall governance progress for Africa since 2008",
       subtitle = "The overall governance score reflects the four main pillars of governance: Safety & Rule of Law, Participation & Human Rights, 
Sustainable Economic Opportunity and Human Development.") +
  theme(panel.background = element_blank(),
        text = element_text(family = "Helvetica", size = 13),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_blank(),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        plot.title = element_text(size = 18), plot.title.position = "plot",
        legend.position = "top", legend.title = element_blank()) 


# ###########Notes involved in the project######################

#assign better column names

names(data_set)[column_name_being_changed] <- "date" 

names(data_set)[1] <- "date"

#convert data field to correct datetype

dataset$date <- as.Date(dataset$date, origin = "1899-12-30")

# remove row with bad date

dataset <- subset(dataset, date >= "2016-01=01")

# remove missing values/nulls
#identify NA's with with completed cases function

complete.cases(dataset)

summary(dataset)

#identify rows with NA using na.omit for review
dataset1NAS <- subset(dataset, is.na(dataset$variable))

#replace NA's with 0
dataset[is.na(dataset)] <- 0
summary(dataset)

#convert column from chr to int(integer)

dataset$.variable1.variable2.variable3 <- as.integer(dataset$variable1.variable2.variable3)

# remove the row with NA in variable1.varible2.variable3

dataset2 <- dataset[!is.na(dataset$variable1.variable2.variable3)]
summary(dataset)
