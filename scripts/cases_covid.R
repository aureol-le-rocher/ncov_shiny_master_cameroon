################################################################################
################################################################################
# Title : Script for compute indicators for the overall country
# Author: Aureol Ngako, Georgetown University-Cameroon Country Office

################################################################################

# Load the packages to be used -----------------------------------------------

pacman::p_load(tidyverse, # For data cleaning
               here,      # For file location
               rio,       # Import/Export files
               linelist,  # For data cleaning/linelists
               slider     # For Moving average
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
  
  covid <- cooking %>% 
     # Select the variables of interest 
    select(region,date,population,cases,deaths) 
  
     # Order the dates
    covid <- covid[order(covid$date),]

 total_cases <- sum(covid$cases, na.rm = TRUE)
 
## Load the regions of the country and their coordinates -----------------------
 
 countries <-  import(here("data","coordinates_cameroun.xlsx"))

### Create a data frame with the list of every regions 
 
 region_list <- unique(covid$region)
 
## Function to compute the news cases and new deaths ---------------------------
 
   for (i in c(1:nrow(covid))) {
 # Create the covid_subset and make sure that the date are ordered
     
      covid_subset <- covid[order(covid$date),]
      
       covid_subset$new_cases <- covid_subset$cases[1] 
       covid_subset$new_deaths <- covid_subset$deaths[1]
       
  # Compute the number of new cases and new deaths from the second row in the dataset
       
       for (j in 2:nrow(covid_subset)) {
         covid_subset$new_cases[j] = covid_subset$cases[j] - covid_subset$cases[j-1]
         covid_subset$new_deaths[j] = covid_subset$deaths[j] - covid_subset$deaths[j-1]
       }
       
  # set negative new case or death counts to 0 
       
       covid_subset$new_cases[covid_subset$new_cases<0] <-  0
       covid_subset$new_deaths[covid_subset$new_deaths<0] <-  0
       
  # Fold into the main data set 
      
       covid$new_cases[i]   <-  covid_subset$new_cases[i]
       covid$new_deaths[i] <-  covid_subset$new_deaths[i] 
       
   }
 
 

 
 
 ## Merge the covid_subset with the coordinates of the country ----------------
 
 covid_subset <- covid_subset %>% 
   left_join(countries %>% 
               select(Region,Code_Reg,Pays,latitude,longitude),
             by = c("region" = "Region"))
 
 ## Compute the rolling average ----------------------------------------------
 
 covid_subset <- covid_subset %>% 
   mutate(new_cases_rolling7 = slide_index_dbl(
     new_cases,
     .i = date,
     .f = ~sum(.x,na.rm = TRUE),
     .before = days(6)),
     new_deaths_rolling7 = slide_index_dbl(
       new_deaths,
       .i = date,
       .f = ~sum(.x,na.rm = TRUE),
       .before = days(6))) 
 
# Write the output file
 
 write.csv(covid_subset, file = here("data","covid_cases.csv"))
 
 


