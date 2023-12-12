# Import packages

pacman::p_load(rio,
               here,
               lubridate,
               tidyverse,
               lubridate,
               tsibble
               )


# Import the shapefiles

district_shapefile <- sf::read_sf(here("data","shapefiles","District_sante_2022.shp"))

district_shapefile[68,2] <- "PIT"




# Import the databases

base1 <- import(here("data","base_sitrep1.xlsx"))
base2 <- import(here("data","base_sitrep2.xlsx"))


  base1 <- base1 %>% 
            pivot_longer(cols = 10:52,
                         names_to = "date",
                         values_to = "cases") %>% 
            mutate(date = str_replace_all(date,"_","/"),
                   date = ymd(date))
  
  base2 <- base2 %>% 
    pivot_longer(cols = 10:80,
                 names_to = "date",
                 values_to = "cases") %>% 
    mutate(date = str_replace_all(date,"_","/"),
           date = ymd(date))
  
  
  
   sitrep_db <- bind_rows(base1,base2)
     
   sitrep_db <- sitrep_db %>% 
                 select(-c(...15,...16,...17,...18,...19,PROG...10,PROG...11,PROG...12))
   

    sitrep_db <-  sitrep_db %>% arrange(Nom_Dist,date) %>% select(Nom_Dist,Region,Code_DS,date,cases,Population)
 
   
#   sitrep_db <- sitrep_db %>%
#     group_by(Nom_Dist) %>%
#     mutate(
#       cases_diff_1 = c(0, diff(cases)),
#       cases_diff_2 = cases - lag(cases, 2),
#       cases_diff_3 = cases - lag(cases, 3),
#       cases_diff_4 = cases - lag(cases, 4)) %>% 
#      mutate_if(is.numeric,~replace(.,is.na(.),0)) %>% 
#    mutate(
#       total_cases_last_4_weeks = (cases_diff_1 / lag(cases)) * 100,
#       status = case_when(
#         total_cases_last_4_weeks >= 20 ~ "Résurgence",
#         total_cases_last_4_weeks >= 10 ~ "Alerte",
#         total_cases_last_4_weeks < 10 ~ "Sous contrôle",
#         TRUE ~ "Non classé"),
#       change_7jrs = case_when(
#         cases_diff_1 >= 50 ~ ">=50",
#         cases_diff_1 >= 10 ~ "10 à 50",
#         cases_diff_1 >= -10 ~ "-10 à 10",
#         cases_diff_1 >= -50 ~ "-50 à -10",
#         cases_diff_1 < -50 ~ "Non applicable",
#         TRUE ~ "Non applicable")) %>%
#     ungroup() %>%
#     select(Code_DS,Nom_Dist,Population, date, cases, cases_diff_1, cases_diff_2, cases_diff_3, cases_diff_4, status, change_7jrs) %>% 
#      drop_na(Code_DS)
   
       
################################################################################   
# Base depistage 
################################################################################
    
    db_depistage <- import(here("data","db_depistage.xlsx"))
     
     
     db_depistage <- db_depistage %>% 
                        pivot_longer(
                          cols = 6:97
                        )
     
     
     db_depistage <- db_depistage %>%
                      rename(date = name) %>% 
                      mutate(date =  substr(date,1,8),
                             date = str_replace_all(date,"_","/"),
                             date = ymd(date)) %>% 
                      drop_na(Nom_Dist)%>% 
       select(-c(cumul...4,cumul...5))
################################################################################     
# Base depistage 2     
################################################################################
   
     db_depistage2 <- import(here("data","depistage_db2.xlsx"))
     
     db_depistage2 <- db_depistage2 %>% 
       select(-c(...5,cumul))
     
     db_depistage2 <- db_depistage2 %>% 
       pivot_longer(
         cols = 4:123
       )
     
     
     db_depistage2 <- db_depistage2 %>%
       rename(date = name) %>% 
       mutate(date =  substr(date,1,8),
              date = str_replace_all(date,"_","/"),
              date = ymd(date)) %>% 
       drop_na(Nom_Dist) %>% 
       filter(date <= "2023-01-04")  
     
  db_depistage <- bind_rows(db_depistage,db_depistage2)
     
# Loop


for (i in 2:nrow(db_depistage)) {
  if (db_depistage$date[i-1] == db_depistage$date[i]) {
    db_depistage$type[i-1] <- "Test"
    db_depistage$type[i] <- "Positifs"
  } else {
    
  }
}

# Extract population data   
 
  pop_ds <- sitrep_db %>% 
             filter(date >= "2023-01-01") %>% 
              group_by(Code_DS) %>% 
              summarise(Population = max(Population))
 
 db_depistage <-  db_depistage %>% 
                  pivot_wider(
                    names_from = type,
                    values_from = value) 

 db_depistage <- db_depistage %>% 
         left_join(pop_ds,
                  by = "Code_DS")  
         


db_depistage <- db_depistage %>% 
  mutate(Test = as.character(Test),
         Test = as.numeric(Test),
         Positifs = as.character(Positifs),
         Positifs = as.numeric(Positifs))  
         


sitrep_db <- sitrep_db %>% 
       full_join(
         db_depistage %>% select(-c(Region)),
         by = c("date","Code_DS","Nom_Dist")) %>% 
         mutate(population = case_when(
                  is.na(Population.x) & !is.na(Population.y) ~ Population.y,
                  is.na(Population.y) & !is.na(Population.x) ~ Population.x ,
                  !is.na(Population.y) & !is.na(Population.x) ~ Population.x)) %>% 
         select(-c(Population.x,Population.y)) %>%
        mutate_if(is.numeric,~replace(.,is.na(.),0)) %>% 
        mutate(tx_depistage = ifelse(Test == 0,0,round(Positifs/Test*100,2)),
           depist_pop = round(Test/population*10000,2))

 sitrep_db <- sitrep_db %>% 
          group_by(Code_DS) %>% 
          arrange(Code_DS, date) %>%
          mutate(percentage_evolution = ifelse(lag(cases, default = 0) == 0, 0, (cases - lag(cases)) / lag(cases) * 100))

   
   sitrep_db <- sitrep_db %>%
     group_by(Code_DS) %>%
     mutate(status = case_when(
       between(percentage_evolution, -10, 10) ~ "Sous contrôle",
       between(percentage_evolution, -20, -10) ~ "Alerte",
       percentage_evolution < -20 ~ "Résurgence",
       TRUE ~ "Non classé"),
       change_7jrs = case_when(
         percentage_evolution >= 50 ~ ">=50",
         percentage_evolution >= 10 ~ "10 à 50",
         percentage_evolution >= -10 ~ "-10 à 10",
         percentage_evolution >= -50 ~ "-50 à -10",
         percentage_evolution < -50 ~ "Non applicable",
         TRUE ~ "Non applicable")
     ) 





################################################################################
# db_sitrep database
################################################################################

sitrep_db <- sitrep_db %>% 
  mutate(
    week_num = as.numeric(format(date, format = '%W')),
    yearweek = as.numeric(format(date, format = '%Y')),
    epiweek = yearweek(date),
    date_epiweek = floor_date(date,unit = "week",week_start = 3),
    week = str_remove(as.character(epiweek),"W"),
    week = str_remove(week," "),
    week = as.numeric(week))


##############################################################################
# Add the shapefiles
##############################################################################


sitrep_db <- district_shapefile %>% 
  left_join(
    sitrep_db %>% 
      select(date,Code_DS,status,change_7jrs,week,depist_pop,Test,Positifs,tx_depistage),
    by = "Code_DS" ) %>% 
     mutate(depist_pop_cat = age_categories(depist_pop,
                                            breakers =  c(0,10,20,30,40,50),
                                            ceiling = FALSE),
            Tx_depist_class = case_when(
              is.na(tx_depistage) ~ "Aucun test réalisé",
              tx_depistage == 0 ~ "Aucun cas détecté",
              tx_depistage > 0 & tx_depistage <= 4 ~ "0-5",
              tx_depistage > 4 & tx_depistage <= 9 ~ "5-10",
              tx_depistage > 9 & tx_depistage <= 14 ~ "10-15",
              tx_depistage >15 ~ "15 et plus",
              TRUE ~ "Aucun test réalisé"
            ))

##############################################################################
   
   sitrep_db <- sitrep_db %>% 
                  select(District_S,Code_DS,geometry,date,status,change_7jrs,week,depist_pop,Test,Positifs,tx_depistage,
                         depist_pop_cat,Tx_depist_class)
   
    #sf::st_precision(sitrep_db) <-  0.001
   
   
   
   
##############################################################################

#rio::export(sitrep_db, file = here("data","sitrep_db.rds"))  
     
#sf::st_write(sitrep_db,here("data","sitrep_db.shp"),append = FALSE)
     
  