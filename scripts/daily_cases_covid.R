################################################################################
################################################################################
# Title : Script to compute indicators for the overall country
# Author: Aureol Ngako, Georgetown University-Cameroon Country Office

################################################################################

# Load the packages to be used -----------------------------------------------

pacman::p_load(tidyverse, # For data cleaning
               here,      # For file location
               rio,       # Import/Export files
               linelist,  # For data cleaning/linelists
               slider,    # For Moving average
               lubridate,
               tsibble,
               aweek
)

# Import the data base ---------------------------------------------------------   

db_covid <- import(here("data","covid_db_week.xlsx"))

################################################################################
################################################################################

# Data cleaning ----------------------------------------------------------------

db_covid <- db_covid %>% 
             clean_variable_names() %>% 
             mutate(date = ymd(date))

db_covid$population[db_covid$region == "Adamaoua"] = 1345934
db_covid$population[db_covid$region == "Centre"] = 4846002
db_covid$population[db_covid$region == "Est"] = 1146981
db_covid$population[db_covid$region == "Extreme-Nord"] = 4824522
db_covid$population[db_covid$region == "Littoral"] = 3987222
db_covid$population[db_covid$region == "Nord"] = 2964768
db_covid$population[db_covid$region == "Nord-Ouest"] = 2244288
db_covid$population[db_covid$region == "Ouest"] = 2113367
db_covid$population[db_covid$region == "Sud"] = 818190
db_covid$population[db_covid$region == "Sud-Ouest"] = 1862687


cmr_cases <- db_covid


cmr_cases$update <- 1:nrow(cmr_cases)

cmr_cases <- cmr_cases %>% 
               mutate_if(is.numeric,~replace(.,is.na(.),0))

#Order the date
 cmr_cases <- cmr_cases[order(cmr_cases$date),]
 
# Compute the news hospitalizations
 
 
 start_hospi <- cmr_cases$hospitalises[1]

 
 
 cmr_cases$hospitalisations_12 <- c(start_hospi, map2_dbl(cmr_cases$hospitalises[-1],
                                                cmr_cases$hospitalises[-nrow(cmr_cases)],~.x-.y))
 
 
 ## Set 0 to the negatives values
 
 cmr_cases$hospitalisations_12[cmr_cases$hospitalisations_12<0] <-  0
 
 
## New cases on oxygen 
 

# Create an object and keep the first value
 
 start_oxygen <- cmr_cases$patients_02[1]

 cmr_cases$oxygen_new <- c(start_oxygen, map2_dbl(cmr_cases$oxygen_new[-1],
                                                          cmr_cases$oxygen_new[-nrow(cmr_cases)],~.x-.y))
 
################################################################################
# Immunization database
################################################################################
 
vaccine_db <-import(here("data","vaccine_dashboard.xlsx"),sheet = "BD", skip = 5)
 
 # Cleaning
 
 vaccine_db <- vaccine_db %>% 
   clean_variable_names() %>% 
   mutate(date =  ymd(date))
 
 vaccine_db <- vaccine_db %>% 
   group_by(region,date = floor_date(as.Date(date),"week",week_start = 03)) %>% 
   summarise(across(.cols = where(is.numeric),.fns = sum)) 
 
 ## Recoding vaccine_db$region
 vaccine_db$region <- vaccine_db$region %>%
   fct_recode(
     "Nord-Ouest" = "Nord Ouest",
     "Sud-Ouest" = "Sud Ouest",
     "Extreme-Nord" = "Extreme Nord"
   )
 
 
## Merger les deux bases de donnees
 
 cmr_cases <- cmr_cases %>% 
               full_join(vaccine_db,by = c("region","date"))
  
 cmr_cases <- cmr_cases %>% 
               mutate_if(is.numeric,~replace(.,is.na(.),0))
 
 ## create a new column named new_doses_administered using purrrr
 
 start_vaccine_doses <- cmr_cases$dose_administrees[1]
 
 cmr_cases$new_vaccine_doses <- c(start_vaccine_doses, map2_dbl(cmr_cases$dose_administrees[-1],
                                                cmr_cases$dose_administrees[-nrow(cmr_cases)],~.x-.y))
 
 
 cmr_cases$new_vaccine_doses[cmr_cases$new_vaccine_doses<0] <-  0
 

   

################################################################################
################################################################################
################################################################################
 ## Load the regions of the country and their coordinates --------------------
 
 countries <-  import(here("data","coordinates_cameroun.xlsx"))
 
 countries$Region <- countries$Region %>%
   fct_recode(
     "Extreme-Nord" = "Extreme Nord"
   )
 
# Add the coordinates to the dataframe ---------------------------------------
 
 cmr_cases <- cmr_cases %>% 
   left_join(countries %>% 
               select(Region,Code_Reg,Pays,latitude,longitude),
             by = c("region" = "Region"))
 
## Write the output file

write.csv(cmr_cases, file = here("data","covid_cases.csv"))

# Export the second file in Rds

#export(cooking_hospi, file = here("data","cooking_hospi.rds"))

 
 
 



