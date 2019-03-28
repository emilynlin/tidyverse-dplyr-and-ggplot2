# Install pacakge "tidyverse" for our tools ggplot and dplyr
install.packages("tidyverse", repos = "https://cran.r-project.org/web/packages/tidyverse/index.html")
install.packages("naniar")

# Load "tidyverse" library
library(tidyverse)
library(naniar)
library(stringr)
library(stringi)

# Dataset origin https://archive.ics.uci.edu/ml/datasets/Adult
# Was only able to download .txt file unparsed without headings

# Set working directory
setwd("~/Documents/Projects & Work/Digital_Projects_Studio/R_tutorial/data")

# Read in method 1
adults <- read.delim("adult-data.txt", header = FALSE, sep = ",", na.strings = c("?"))

# Name all column headers correspondingly
names(adults) <- c("age","workclass","fnlwgt","education","education-num",
                   "marital-status","occupation","relationship","race","sex",
                   "capital-gain","capital-loss","hours-per-week",
                   "native-country", "salary")

# Examine the data
head(adults, 30)

# Replace ? with NA
adults$workclass[str_detect(adults$workclass, "\\?")] <- NA
adults$occupation[str_detect(adults$occupation, "\\?")] <- NA
adults$`native-country`[str_detect(adults$`native-country`, "\\?")] <- NA

# Use select to select columns of interest
adults_select <- select(adults, age, workclass, fnlwgt, education, "education-num", "marital-status", occupation, 
                     race, sex, "hours-per-week", "native-country", salary)

# Use select to drop the columns we don't want by adding a '-' in front
adults_negselect <- select(adults, -relationship, -"capital-gain", -"capital-loss")

adults_educ <- select(adults, starts_with("education"))

# Use arrange to order rows of a data frame using a variable name 
adults_arrange <- arrange(adults_select, age)

# We can see that the age of adults in this dataset ranges from 17-90 years old
# We are interested in taking a closer look at older adults past the age of 
# 30 that originated from the United States, and whom work for a private company.
# Use stringr for columns with text data
adults_filter <- adults_select %>%
  filter(str_detect(`native-country`, 'United-States') & 
           as.integer(age) >= 30 & str_detect(workclass, 'Private'))

# Use filter helper functions to select between values
adults_filter_between <- adults_select %>%
  filter(between(as.integer(age), 30, 50))

# Use mutate to create a new column, scaling fnlwgt into something more visualizable
adults_mutate <- mutate(adults_filter, "scaled_fnlwgt" = as.double(fnlwgt)/10000)

# Use transmute which only keeps the new variables
head(transmute(adults_filter, "scaled_fnlwgt" = as.double(fnlwgt)/10000), 10)

# Use summarize to see difference in average hours per week worked for white males vs. white females
adults_mutate %>%
  filter(str_detect(race, 'White') & str_detect(sex, 'Male')) %>%
      summarize(white_males = mean(as.double(`hours-per-week`)))

adults_mutate %>%
  filter(str_detect(race, 'White') & str_detect(sex, 'Female')) %>%
    summarize(white_females = mean(as.double(`hours-per-week`)))

# Using summarize with group_by is more useful
adults_mutate %>%
  group_by(race, sex) %>%
  summarize(avg_hours = mean(as.double(`hours-per-week`)))

# Use rename to rename a variable name
adults_mutate <- rename(adults_mutate, "scaled-fnlwgt" = scaled_fnlwgt)

# ggplot2 basic geom_point for scatterplot
ggplot(adults_mutate) +
  geom_point(mapping = aes(x = age, y = `scaled-fnlwgt`))

# ggplot2 geom_point with color representing third variable
ggplot(adults_mutate) +
  geom_point(mapping = aes(x = age, y = `scaled-fnlwgt`, color = sex))

# ggplot2 basic geom_bar with male female salary distribution
ggplot(adults_mutate) +
  geom_bar(mapping = aes(x = salary, fill = sex), position = "dodge")

# ggplot2 basic geom_bar with race salary distribution
ggplot(adults_mutate) +
  geom_bar(mapping = aes(x = race, y = `scaled-fnlwgt`, fill = sex), position = "dodge", stat = "identity")

# Counting the number of white and black adults collected in this dataset
adults_mutate %>%
  group_by(race) %>%
  summarize(count = n())

# Count the number of male and female adults collected in this dataset
adults_mutate %>%
  group_by(sex) %>%
  summarize(count = n())

# Using geom_point and geom_smooth together
ggplot(adults_mutate, mapping = aes(x = `age`, y = `scaled-fnlwgt`)) +
  geom_point() +
  geom_smooth()

# Using filter to filter down dataset more
adults_asian_fem <- adults_mutate %>%
  filter(str_detect(sex, 'Female') & str_detect(race, 'Asian-Pac-Islander'))
  
# Use filtered data to use geom_point and geom_smooth
ggplot(adults_asian_fem, mapping = aes(x = `age`, y = `scaled-fnlwgt`)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Facet
adults_mutate %>%
  filter(str_detect(sex, 'Female')) %>%
  ggplot() +
  geom_point(mapping = aes(x = age, y = `scaled-fnlwgt`)) +
  facet_wrap(~race)

adults_mutate %>%
  filter(str_detect(sex, 'Female')) %>%
  ggplot() +
  geom_jitter(mapping = aes(x = age, y = `scaled-fnlwgt`))

### Coordinate Systems
# Coxcomb Chart
bar <- ggplot(adults_mutate) +
  geom_bar(mapping = aes(x = `marital-status`, fill = `marital-status`), width = 1)

bar +
  labs(x = NULL) +
  coord_polar()

# Pie Chart
bar <- ggplot(adults_mutate) +
  geom_bar(mapping = aes(x = factor(1), fill = `marital-status`), width = 1)

bar +
  labs(x = NULL) + # remove the x-axis label "marital-status"
  coord_polar(theta = "y") # change to polar coordinates to get a pie chart

# Bullseye Chart
bar +
  labs(x = NULL) +
  coord_polar()
