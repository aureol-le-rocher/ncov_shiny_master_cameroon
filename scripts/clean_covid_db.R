################################################################################
# Nettoyage de la bd pour le rapport des regions
################################################################################


# Toutes les données sont susceptibles d'être vérifiées et modifiées et peuvent être corrigées a posteriori


library(tidyverse)
library(rio)
library(here)
library(zoo)
library(lubridate)

rapports_regions_brut <- import(here("data","rapports_regions_brut.xlsx"), 
                                         sheet = "Cas Covid-19   ", range = "BH1:BS5000")



rapports_regions_brut <- rapports_regions_brut %>% 
                          rename( region = `...1`,
                                  deces =  `Rapports des régions du 25 Novembre 2020`,
                                  cas_pos  =  `...3`,
                                  cas_actifs = `...4`,
                                  guerison   = `...5`,
                                  personnel_sante_pos = `...6`,
                                  personnel_sante_dcd = `...7`,
                                  fec_pos = `...8`,
                                  fec_dcd = `...9`,
                                  hospitalises = `...10`,
                                  ambulatoire = `...11`,
                                  patients_02 =  `...12`) %>%
        mutate(period1 = str_remove_all(region,"Rapports des régions du|Adamaoua|Centre|Est|Extreme-Nord|Littoral|Nord|Nord-Ouest|Ouest|Sud|Sud-Ouest|Total|Région"),
               year =  str_extract(region,"2020|2021|2022|2023|2024|2025"),
               mois =  str_extract(region,"Janvier|Février|Mars|Avril|avril|Mai|Juin|Juillet|Août|Septembre|Octobre|octobre|Novembrer|Décembre|decembre|octobre|Aout|juin|avril|février|décembre|Novembre"),
               day = str_sub(period1,1,3),
               mois_number = case_when(
                 mois == "Janvier"~ 1,
                 mois == "Février" | mois == "février" ~ 2,
                 mois == "Mars" ~ 3,
                 mois == "Avril" | mois == "avril" ~ 4,
                 mois == "Mai" ~ 5,
                 mois == "Juin" | mois == "juin" ~ 6,
                 mois == "Juillet" ~ 7,
                 mois == "Août" | mois == "Aout" ~ 8,
                 mois == "Septembre" ~ 9,
                 mois == "Octobre" | mois == "octobre" ~ 10,
                 mois == "Novembrer" | mois == "Novembre" ~ 11,
                 mois == "Décembre"|mois == "decembre" | mois == "décembre" ~ 12,
                 TRUE ~ NA_integer_),
               date =  ymd(paste(year,mois_number,day, sep = "-")))
        

rapports_regions_brut$date[1] <- ymd(as.Date("2020-11-25"))

################################################################################

## Recoding rapports_regions_brut$region
rapports_regions_brut$region <- rapports_regions_brut$region %>%
  fct_recode(
    NULL = "Rapports des régions du 01 Décembre 2021",
    NULL = "Rapports des régions du 01 février 2023",
    NULL = "Rapports des régions du 01 Juin 2022",
    NULL = "Rapports des régions du 01 Mars 2023",
    NULL = "Rapports des régions du 01 Septembre 2021",
    NULL = "Rapports des régions du 02 Aout 2023",
    NULL = "Rapports des régions du 02 Février 2022",
    NULL = "Rapports des régions du 02 Juin 2021",
    NULL = "Rapports des régions du 02 Mars 2022",
    NULL = "Rapports des régions du 03 Août 2022",
    NULL = "Rapports des régions du 03 Février 2021",
    NULL = "Rapports des régions du 03 Mai  2023",
    NULL = "Rapports des régions du 03 Mars 2021",
    NULL = "Rapports des régions du 03 Novembre 2022",
    NULL = "Rapports des régions du 03 Novembrer 2021",
    NULL = "Rapports des régions du 04 Août 2021",
    NULL = "Rapports des régions du 04 Janvier 2023",
    NULL = "Rapports des régions du 04 Mai 2022",
    NULL = "Rapports des régions du 04 octobre 2023",
    NULL = "Rapports des régions du 05 avril  2023",
    NULL = "Rapports des régions du 05 Janvier 2022",
    NULL = "Rapports des régions du 05 Juillet 2023",
    NULL = "Rapports des régions du 05 Mai 2021",
    NULL = "Rapports des régions du 05 Octobre 2022",
    NULL = "Rapports des régions du 06 Avril 2022",
    NULL = "Rapports des régions du 06 Juillet 2022",
    NULL = "Rapports des régions du 06 Octobre 2021",
    NULL = "Rapports des régions du 06 Septembre 2023",
    NULL = "Rapports des régions du 07 Avril 2021",
    NULL = "Rapports des régions du 07 décembre 2022",
    NULL = "Rapports des régions du 07 Janvier 2021",
    NULL = "Rapports des régions du 07 Juillet 2021",
    NULL = "Rapports des régions du 07 juin 2023",
    NULL = "Rapports des régions du 07 Septembre 2022",
    NULL = "Rapports des régions du 08 decembre 2020",
    NULL = "Rapports des régions du 08 Décembre 2021",
    NULL = "Rapports des régions du 08 février 2023",
    NULL = "Rapports des régions du 08 Juin 2022",
    NULL = "Rapports des régions du 08 Mars 2023",
    NULL = "Rapports des régions du 08 Septembre 2021",
    NULL = "Rapports des régions du 09 Aout 2023",
    NULL = "Rapports des régions du 09 Février 2022",
    NULL = "Rapports des régions du 09 Juin 2021",
    NULL = "Rapports des régions du 09 Mars 2022",
    NULL = "Rapports des régions du 09 Novembre 2022",
    NULL = "Rapports des régions du 10 Août 2022",
    NULL = "Rapports des régions du 10 Février 2021",
    NULL = "Rapports des régions du 10 Mai  2023",
    NULL = "Rapports des régions du 10 Mars 2021",
    NULL = "Rapports des régions du 10 Novembrer 2021",
    NULL = "Rapports des régions du 11 Août 2021",
    NULL = "Rapports des régions du 11 Janvier 2023",
    NULL = "Rapports des régions du 11 Mai 2022",
    NULL = "Rapports des régions du 11 octobre 2023",
    NULL = "Rapports des régions du 12 avril  2023",
    NULL = "Rapports des régions du 12 Janvier 2022",
    NULL = "Rapports des régions du 12 Juillet 2023",
    NULL = "Rapports des régions du 12 Mai 2021",
    NULL = "Rapports des régions du 12 Octobre 2022",
    NULL = "Rapports des régions du 13 Avril 2022",
    NULL = "Rapports des régions du 13 Janvier 2021",
    NULL = "Rapports des régions du 13 Juillet 2022",
    NULL = "Rapports des régions du 13 Octobre 2021",
    NULL = "Rapports des régions du 13 Septembre 2023",
    NULL = "Rapports des régions du 14  juin 2023",
    NULL = "Rapports des régions du 14 Avril 2021",
    NULL = "Rapports des régions du 14 décembre 2022",
    NULL = "Rapports des régions du 14 Juillet 2021",
    NULL = "Rapports des régions du 14 Septembre 2022",
    NULL = "Rapports des régions du 15 Décembre 2021",
    NULL = "Rapports des régions du 15 février 2023",
    NULL = "Rapports des régions du 15 Juin 2022",
    NULL = "Rapports des régions du 15 Mars 2023",
    NULL = "Rapports des régions du 15 Septembre 2021",
    NULL = "Rapports des régions du 16 Aout 2023",
    NULL = "Rapports des régions du 16 decembre 2020",
    NULL = "Rapports des régions du 16 Février 2022",
    NULL = "Rapports des régions du 16 Juin 2021",
    NULL = "Rapports des régions du 16 Mars 2022",
    NULL = "Rapports des régions du 16 Novembre 2022",
    NULL = "Rapports des régions du 17 Août 2022",
    NULL = "Rapports des régions du 17 Février 2021",
    NULL = "Rapports des régions du 17 Mai  2023",
    NULL = "Rapports des régions du 17 Mars 2021",
    NULL = "Rapports des régions du 17 Novembrer 2021",
    NULL = "Rapports des régions du 18 Août 2021",
    NULL = "Rapports des régions du 18 Janvier 2023",
    NULL = "Rapports des régions du 18 Mai 2022",
    NULL = "Rapports des régions du 18 octobre 2023",
    NULL = "Rapports des régions du 19 avril  2023",
    NULL = "Rapports des régions du 19 Janvier 2022",
    NULL = "Rapports des régions du 19 Juillet 2023",
    NULL = "Rapports des régions du 19 Mai 2021",
    NULL = "Rapports des régions du 19 Octobre 2022",
    NULL = "Rapports des régions du 20 Avril 2022",
    NULL = "Rapports des régions du 20 Janvier 2021",
    NULL = "Rapports des régions du 20 Juillet 2022",
    NULL = "Rapports des régions du 20 Octobre 2021",
    NULL = "Rapports des régions du 20 Septembre 2023",
    NULL = "Rapports des régions du 21 Avril 2021",
    NULL = "Rapports des régions du 21 décembre 2022",
    NULL = "Rapports des régions du 21 Juillet 2021",
    NULL = "Rapports des régions du 21 Septembre 2022",
    NULL = "Rapports des régions du 22  juin 2023",
    NULL = "Rapports des régions du 22 Décembre 2021",
    NULL = "Rapports des régions du 22 février 2023",
    NULL = "Rapports des régions du 22 Juin 2022",
    NULL = "Rapports des régions du 22 Mars 2023",
    NULL = "Rapports des régions du 22 Septembre 2021",
    NULL = "Rapports des régions du 23 Aout 2023",
    NULL = "Rapports des régions du 23 decembre 2020",
    NULL = "Rapports des régions du 23 Février 2022",
    NULL = "Rapports des régions du 23 Juin 2021",
    NULL = "Rapports des régions du 23 Mars 2022",
    NULL = "Rapports des régions du 24 Août 2022",
    NULL = "Rapports des régions du 24 Février 2021",
    NULL = "Rapports des régions du 24 Mai  2023",
    NULL = "Rapports des régions du 24 Mars 2021",
    NULL = "Rapports des régions du 24 Novembre 2022",
    NULL = "Rapports des régions du 24 Novembrer 2021",
    NULL = "Rapports des régions du 25 Août 2021",
    NULL = "Rapports des régions du 25 Janvier 2023",
    NULL = "Rapports des régions du 25 Mai 2022",
    NULL = "Rapports des régions du 25 octobre 2023",
    NULL = "Rapports des régions du 26 avril  2023",
    NULL = "Rapports des régions du 26 Janvier 2022",
    NULL = "Rapports des régions du 26 Juillet 2023",
    NULL = "Rapports des régions du 26 Mai 2021",
    NULL = "Rapports des régions du 26 Octobre 2022",
    NULL = "Rapports des régions du 27 Avril 2022",
    NULL = "Rapports des régions du 27 Janvier 2021",
    NULL = "Rapports des régions du 27 Juillet 2022",
    NULL = "Rapports des régions du 27 Octobre 2021",
    NULL = "Rapports des régions du 27 Septembre 2023",
    NULL = "Rapports des régions du 28  juin 2023",
    NULL = "Rapports des régions du 28 Avril 2021",
    NULL = "Rapports des régions du 28 décembre 2022",
    NULL = "Rapports des régions du 28 Juillet 2021",
    NULL = "Rapports des régions du 28 Septembre 2022",
    NULL = "Rapports des régions du 29 Décembre 2021",
    NULL = "Rapports des régions du 29 Juin 2022",
    NULL = "Rapports des régions du 29 Mars 2023",
    NULL = "Rapports des régions du 29 Septembre 2021",
    NULL = "Rapports des régions du 30 Aout 2023",
    NULL = "Rapports des régions du 30 Juin 2021",
    NULL = "Rapports des régions du 30 Mars 2022",
    NULL = "Rapports des régions du 30 Novembre 2022",
    NULL = "Rapports des régions du 31 Août 2022",
    NULL = "Rapports des régions du 31 Mai  2023",
    NULL = "Rapports des régions du 31 Mars 2021",
    NULL = "Rapports des régions du 09 decembre 2020",
    NULL = "Région",
    NULL = "Total"
  )




rapports_regions_brut <- rapports_regions_brut %>%
                              mutate(across(.cols = 2:13, ~str_remove_all(., 
                                              "Décès|`Cas +`|Cas actifs|Guérisons|`Personnel de santé +`|Personnel de santé dcd|`FEC +`|FEC dcd|Hospitalisés|ambulatoires|patients sous O2|actifs|dcd")),
                                     across(.cols = 2:13,~fct_recode(.,NULL = "+")))



rapports_regions_brut$date <- rapports_regions_brut$date %>% 
  na.locf(na.rm = FALSE)

rapports_regions_brut <- rapports_regions_brut %>% 
  drop_na(region) %>% 
  select(date,region,deces,cas_pos,cas_actifs,guerison,personnel_sante_pos,personnel_sante_dcd,fec_pos,fec_dcd,
         hospitalises,ambulatoire,patients_02)
 
 
# Second db

rapports_sitrep <- import(here("data","rapports_regions_brut.xlsx"), 
                                sheet = "Cas Covid-19   ", range = "BW274:CK5000")

rapports_sitrep <- rapports_sitrep %>% 
                    #select(`...1`,`cas14/04/2021`,`Remission14/04/2021`,`Décès14/04/2021`,
                     #      `...12`,`sitrepCas`,`sitrepGuérison`,`SitrepDécès`) %>% 
                    mutate(date = regmatches(`cas14/04/2021`, gregexpr("\\d{2}/\\d{2}/\\d{4}", `cas14/04/2021`)),
                           date = dmy(date))
  
rapports_sitrep$date[1] <- ymd(as.Date("2021/04/14"))

rapports_sitrep <- rapports_sitrep %>% 
                     rename(
                       region =  `...1`,
                       NpersoDcd = `...6`,
                       `Nfec dcd` = `...8`,
                       `cas_actifs_rev` = `...12`,
                       cas_rev = `cas14/04/2021`,
                       Remission_rev = `Remission14/04/2021`,
                       Décès_rev = `Décès14/04/2021`
                     )

rapports_sitrep$date <- rapports_sitrep$date %>% 
  na.locf(na.rm = FALSE)

rapports_sitrep <- rapports_sitrep %>% 
                     drop_na(region) %>% 
                     select(date,everything())

export(rapports_sitrep, "rapports_covid.xlsx")

# db3

db_1 <- import(here("data","rapports_covid_db.xlsx"))

db_1$date <- ymd(db_1$date)

data_final <- db_1 %>% 
                left_join(y = rapports_regions_brut %>%
                                select(date,region,personnel_sante_pos,personnel_sante_dcd,fec_pos,
                                       fec_dcd,hospitalises,ambulatoire,patients_02),
                          by = c("date","region"))


#uu <- anti_join(db_1,rapports_regions_brut,by = c("date","region"))


#
db2 <- import(here("data","covid_cases.csv"))

  db2 <- db2 %>% 
          mutate(date =  ymd(date)) %>% 
           select(date,region,cases,deaths,tdr_realise,pcr_realise,tdr_positif,pcr_positif) %>% 
           group_by(date = floor_date(as.Date(date),"week",week_start = 03),region) %>% 
           summarise(across(.cols = where(is.numeric),.fns = ~sum(.x,na.rm = TRUE)))
  
  
 
          
  
###########################################

 ## Recoding db2$region
db2$region <- db2$region %>%
  fct_recode(
    "Extreme-Nord" = "Extreme Nord")
  
  db3 <- import(here("data","clean_rows","data_check_dates.xlsx"))
  
  db3 <- db3 %>% 
    mutate(date =  ymd(date))
  
############################################  
  
data_to_check  <-   data_final %>% 
                    select(date,region,cas_rev,`Ncas+`,`Npersonnel`,`Ndécès`,`NpersoDcd`,
                           `Décès_rev`,cas_actifs_rev,hospitalises,Nfec,`Nfec dcd`,`Décès_rev`,cas_actifs_rev,
                            fec_pos,fec_dcd,ambulatoire,patients_02,Nguérisons,Remission_rev)
  

  
  #export(uu, here("data","clean_rows","data_to_check.xlsx"))
  #export(db2, here("data","clean_rows","data_to_check_raw.xlsx"))
  
  uu <- anti_join(data_to_check,db3,by = c("date","region"))
  
  
  database_week_covid <- full_join(data_to_check,db3,by = c("date","region"))
  
  
  database_week_covid <- database_week_covid %>% 
                            select(
                              date,region,cases,`Ndécès`,`Nguérisons`,`Npersonnel`,`NpersoDcd`,Nfec,`Nfec dcd`,cas_rev,
                              Remission_rev,`Décès_rev`,cas_actifs_rev,hospitalises,ambulatoire,patients_02,fec_pos,fec_dcd)
  
  
  export(database_week_covid, here("data","clean_rows","data_to_ajust.xlsx"))
  
  #uu1 <- semi_join(data_final,db2,by = c("date","region"))
  
  
   
  
  
  
  uv <- database_week_covid %>% 
        group_by(date) %>% 
         summarise(cases_cum =  sum(cas_rev, na.rm = TRUE)) 
  
  

  database_week_covid <-   database_week_covid %>% 
    full_join(
      db2 %>% select(date,region,tdr_positif,pcr_positif,pcr_realise,tdr_realise),
      by = c("date","region")
    )
  
  
  
  uu <-   database_week_covid %>% 
    anti_join(
      db2 %>% select(date,region,tdr_positif,pcr_positif,pcr_realise,tdr_realise),
      by = c("date","region")
    )
  
  
  
  export(database_week_covid, here("data","clean_rows","testing.xlsx"))
  
  
  
  
  
  
  
  ggplot(data = cmr_cases,
            aes(x = date, y = ndeces))+
            geom_histogram(stat = "identity",position = position_nudge(x = 1), width = 6,fill = "#00B050")+
            scale_x_date(
            labels = scales::date_format("%b %d, %Y"),
           date_breaks = "1 year")+
        scale_fill_brewer(palette = "Set3") +
        theme_minimal()+
        theme(panel.grid.major = element_blank(),  # Supprimer la grille majeure
                        panel.grid.minor = element_blank())+
        labs( fill = "", x = "Date", y = "")
