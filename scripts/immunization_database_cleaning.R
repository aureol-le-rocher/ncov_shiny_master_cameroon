################################################################################
################################################################################

# Charger les packages

pacman::p_load(rio,
               readxlsb,
               here,
               janitor,
               linelist,
               lubridate,
               tidyverse)


################################################################################

# Importer la base de donnees

vaccine_db1 <-import(here("data","vaccine_dashboard.xlsx"),sheet = "BD", skip = 5)

# Cleaning

 vaccine_db1 <- vaccine_db1 %>% 
                clean_variable_names() %>% 
                mutate( date = as.Date(date),
                       date =  ymd(date))

 vaccine_db1 <- vaccine_db1 %>% 
      group_by(region,date,vaccin) %>% 
      summarise(across(.cols = where(is.numeric),.fns = sum)) 
 
 export(vaccine_db1, file = here("data","vaccine_db.rds"))
 
 
 
 
  

