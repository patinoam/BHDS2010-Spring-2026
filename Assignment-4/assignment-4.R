#####
##### Load packages
#####

# The lines below are run if the packages were not yet installed.
# Otherwise,they are commented out so the code is not executed.
# install.packages("shiny")

# The shiny package enables the use of the shiny app utilities.
library(shiny)

# The tidyverse package contains functions for data wrangling.
library(tidyverse)

#####
##### Data preparation
#####

# The working directory is retrieved and stored as an object. This assumes that 
# the local repository on the user's machine is used.
wd_path <- getwd()

# The data file path is stored as an object based on where the data is stored 
# in the repository and the file name.
# The data is a copy from a file from the GitHub repository below:
# https://github.com/CSSEGISandData 
# The file is from commit 2b59391dc10d791051261f5ea8ebbece234ed432
data_path <- paste0(wd_path,
                    "/Assignment-4/measles_county_all_updates_detailed.csv")

# The data is loaded into the RStudio environment.
# Since the file has a .csv extension, the function `read.csv` is used.
# An object is created which contains the imported data from the specified 
# file path `data_path`.
all_data <- read.csv(data_path, header=TRUE)

# The column names include the following: location_name, location_id,
# location_type, date, and outcome_type
names(all_data)

# It appears that the column location name stores the county and state in the 
# same string
head(all_data)

# Separate columns are created for county and state
all_data <- all_data %>%
  separate_wider_delim(location_name, delim = ",", names = c("county", "state"))

# The variables value and location are both already numeric
is.numeric(all_data$value)
is.numeric(all_data$location_id)

# Convert county, state, location_type, and outcome_type to factors
if (is.factor(all_data$county) == FALSE) {
  all_data$county <- as.factor(all_data$county)
}
is.factor(all_data$county)

if (is.factor(all_data$state) == FALSE) {
  all_data$state <- as.factor(all_data$state)
}
is.factor(all_data$state)

if (is.factor(all_data$location_type) == FALSE) {
  all_data$location_type <- as.factor(all_data$location_type)
}
is.factor(all_data$location_type)

if (is.factor(all_data$outcome_type) == FALSE) {
  all_data$outcome_type <- as.factor(all_data$outcome_type)
}
is.factor(all_data$outcome_type)

# Convert date to type Date
all_data$date <- as.Date(all_data$date)
inherits(all_data$date, "Date")

# Case counts are summed for each state
cases_by_state_data <- all_data %>% 
  filter(outcome_type == "case_lab-confirmed") %>% 
  group_by(state) %>% 
  summarise(total_cases = sum(value, na.rm = TRUE))