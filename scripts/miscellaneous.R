# miscellaneous -----

 tdr_pcr <- import(here("data","dashboard.xlsx"),which = "Cooking")


 tdr_pcr <- tdr_pcr %>% 
             janitor::clean_names() %>% 
              select(region,date,tdr_realise,pcr_realise,tdr_positif,pcr_positif)

  tdr_pcr <- tdr_pcr %>% 
              group_by(
                date = floor_date(date,unit = "week",week_start = 3),region) %>%
                summarise(across(.cols = where(is.numeric),.fns =  sum))


export(tdr_pcr,file = here("data","miscellaneous_tdr.xlsx"))  
  
  
  
  