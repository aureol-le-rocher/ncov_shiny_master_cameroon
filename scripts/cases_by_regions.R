################################################################################
################################################################################
# Title : Script for compute indicators by regions
# Author: Aureol Ngako, Georgetown University-Cameroon Country Office

################################################################################

# Load the packages to be used -----------------------------------------------

pacman::p_load(tidyverse, # For data cleaning
               here,      # For file location
               rio,       # Import/Export files
               linelist   # For data cleaning/linelists
)


# Import the data base ---------------------------------------------------------

cooking <- import(here("data","dashboard.xlsx"), which = "Cooking")

# Data cleaning ----------------------------------------------------------------

cooking <- cooking %>% 
  # Clean the variables names
  clean_variable_names() %>% 
  # Recode some variables
  rename(cases = echantillons_positifs,
         deaths = deces)

## Create a dataframe with the cases and deaths by day of reporting ------------

cameroun_cases <- cooking %>% 
  # Select the variables of interest 
  select(region,date,population,cases,deaths,guerison) 


# add data for new cases, new deaths

cameroun_cases$new_cases <-  cameroun_cases$new_deaths <-  NA

cameroun_cases <-  cameroun_cases[order(cameroun_cases$region, cameroun_cases$date),]
region_list <-  unique(cameroun_cases$region)

## Fontion to compute new cases and deaths  by  regions

for (i in 1:length(region_list)) {
  cmr_subset <-  subset(cameroun_cases, region == region_list[i])
  cmr_subset <- cmr_subset[order(cmr_subset$date),]
  
  # add starting level for new cases and deaths
  cmr_subset$new_cases  <- cmr_subset$cases[1]
  cmr_subset$new_deaths <- cmr_subset$deaths[1]
  
  for (j in 2:nrow(cmr_subset)) {
    cmr_subset$new_cases[j]  <-  cmr_subset$cases[j] - cmr_subset$cases[j-1]
    cmr_subset$new_deaths[j] <-  cmr_subset$deaths[j] - cmr_subset$deaths[j-1]
  }
  
  # set negative new case or death counts to 0
  cmr_subset$new_cases[cmr_subset$new_cases<0] <-  0
  cmr_subset$new_deaths[cmr_subset$new_deaths<0] <-  0
  
  # fold into main dataset
  
  cameroun_cases$new_cases[cameroun_cases$region==region_list[i]] = cmr_subset$new_cases
  cameroun_cases$new_deaths[cameroun_cases$region==region_list[i]] = cmr_subset$new_deaths
}

## Write the csv file 

write.csv(cameroun_cases, file = here("data","cases_region.csv"),row.names = FALSE)
rm(list = ls())



