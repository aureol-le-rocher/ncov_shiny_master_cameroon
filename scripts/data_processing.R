################################################################################
################################################################################
# Title : Covid19 shiny Application
# Author : Aureol Ngako, Georgetown University- Cameroon country office

################################################################################
################################################################################


###############################################################################
# Download the packages --------------------------------------------------------
################################################################################

pacman::p_load(
  rio,                             # To import/Export files
  here,                            # For files location
  linelist,
  #shiny,
  #shinyWidgets,
  #shinybusy,
  #shinydashboard,
  #plotly,
  geojsonio,
  sf,
  tidyverse,
  lubridate,
  leaflegend,
  RColorBrewer,
  DT,
  data.table,
  ggrepel,
  scales,
  #highcharter,
  tools,
  wesanderson,
  shinyBS,
  dashboardthemes,
  leaflet.providers,
  leaflet,
  cicerone,
  tsibble,
  flextable,
  shinythemes,
  ggspatial
)

################################################################################
################################################################################
# Sources files

 source(here("scripts","clean_sitrep_db.R"))



###############################################################################
# Import the databases --------------------------------------------------------
###############################################################################

##  Import the daily cases and deaths for the overall country

cv_cases <- import(here("data","covid_cases.csv"))

## Import the cases summarized by countries

#cv_regions <- import(here("data","cases_region.csv"))

## Import the coordinates of the country

country <-  import(here("data","coordinates_cameroun.xlsx"))

cameroon_region <- geojson_read(here("data","shapefiles","Region_Cam.geojson"), what = "sp")

#db_sitrep <- sf::read_sf(here("data","sitrep_db.shp"))

db_sitrep <- sitrep_db

region_shapefile <- sf::read_sf(here("data","shapefiles","Regions_2022.shp"))

cv_vaccine <- import(here("data","vaccine_db.rds"))

# Colors  for the  tracker -----------------------------------------------------

covid_col <- "#418FDE"
covid_other_col <- "#D4E5F7"


###############################################################################
# Map function --------------------------------------------------------------
###############################################################################


## Function to map the cumulative cases by region -----------------------------

cumulative_plot <-  function(cv_aggregated, plot_date) {
  plot_df <-  subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = cumul, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("Cas cumulés(milliers)") +  xlab("Date") + 
    theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "k")})+
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

## Function to map news Covid-19 cases by date ---------------------------------

new_cases_plot <-  function(cv_aggregated, plot_date) {
  plot_df_new <-  subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = cases, colour = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    # geom_bar(position="stack", stat="identity") + 
    ylab("Nouveaux cas milliers") + xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "k")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}


## Fonction to plot cases  by  regions -----------------------------------


country_cases_plot <- function(cv_cases,plot_start_date){
  
  plot_start_date <- as.Date(plot_start_date)
  
  cv_cases <- cv_cases %>% 
               filter(as.Date(date) <= plot_start_date) %>% 
               #mutate(date_week = as.Date(floor_date(date,'week',week_start = 1))) %>% 
               group_by(region,date) %>% 
               summarise(outcome = sum(outcome,na.rm = TRUE))
  
  cv_cases$evolution <- c(0, diff(cv_cases$outcome))
  cv_cases$percentage_change <- (cv_cases$evolution / lag(cv_cases$outcome)) * 100
  
 p <-  ggplot(data = cv_cases,
         aes(x = date, y = outcome,fill = reorder(region, -outcome),
             text =  paste(region,"<br>",
                           scales::date_format("%A, %d %B %Y")(date), "<br>",
                           "Total:", outcome, "<br>",
                           "Evolution:",evolution, "<br>",
                           "% Evolution:",round(percentage_change,1),"%","<br>")))+
    geom_histogram(stat = "identity",position = position_nudge(x = 1), width = 6)+
    scale_x_date(
      labels = scales::date_format("%b %d, %Y"),
      date_breaks = "1 year")+
    scale_fill_brewer(palette = "Set3")+
    theme_minimal()+
    theme(axis.text.y = element_blank(),  # Supprimer les étiquettes de l'axe Y
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),  # Supprimer la grille majeure
          panel.grid.minor = element_blank())+
    labs( fill = "",
          x = "Date")
  
 ggplotly(p, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  
}


country_cases_plot_daily <- function(cv_cases,plot_start_date){
  
  plot_start_date <- as.Date(plot_start_date)
  
  cv_cases <- cv_cases %>% 
    filter(as.Date(date) <= plot_start_date) %>% 
    group_by(region,date) %>% 
    summarise(outcome = sum(outcome,na.rm = TRUE))
  
  cv_cases$evolution <- c(0, diff(cv_cases$outcome))
  cv_cases$percentage_change <- (cv_cases$evolution / lag(cv_cases$outcome)) * 100
  
  p <-  ggplot(data = cv_cases,
               aes(x = date, y = outcome,fill = region,
                   text = paste(region,"<br>",
                                scales::date_format("%A, %d %B %Y")(date), "<br>",
                                "Total:", outcome, "<br>",
                                "Evolution:",evolution, "<br>",
                                "% Evolution:",round(percentage_change,1),"%","<br>")))+
    geom_histogram(stat = "identity",position = position_nudge(x = 1), width = 6)+
    scale_x_date(
      labels = scales::date_format("%b %d, %Y"),
      date_breaks = "1 year")+
    scale_fill_brewer(palette = "Set3")+
    theme_minimal()+
    theme(axis.text.y = element_blank(),  # Supprimer les étiquettes de l'axe Y
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),  # Supprimer la grille majeure
          panel.grid.minor = element_blank())+
    labs( fill = "",
          x = "Date")
  
  ggplotly(p, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  
}


# Cumulative

country_cases_plot1 <- function(cv_cases,plot_start_date){
  
  plot_start_date <- as.Date(plot_start_date)
  
  cv_cases <- cv_cases %>% 
    filter(as.Date(date) <= plot_start_date) %>% 
    #mutate(date_week = as.Date(floor_date(date,'week',week_start = 1))) %>% 
    group_by(region,date) %>% 
    summarise(outcome = sum(outcome,na.rm = TRUE),
              outcome_tot =  outcome_tot) %>% 
    mutate(outcome = cumsum(outcome)) %>% 
    arrange(date, desc(outcome))
  
  
  
  p <-  ggplot(data = cv_cases,
               aes(x = date, y = outcome,fill = reorder(region, -outcome),
                   text =  paste(region,"<br>",
                                 scales::date_format("%A, %d %B %Y")(date), "<br>",
                                 "Cumul:", outcome_tot, "<br>")))+
    geom_histogram(stat = "identity",position = position_nudge(x = 1), width = 6)+
    scale_x_date(
      labels = scales::date_format("%b %d, %Y"),
      date_breaks = "1 year")+
    scale_fill_brewer(palette = "Set3")+
    theme_minimal()+
    theme(axis.text.y = element_blank(),  # Supprimer les étiquettes de l'axe Y
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),  # Supprimer la grille majeure
          panel.grid.minor = element_blank())+
    labs( fill = "",
          x = "Date")
  
  ggplotly(p, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  
}



## Function to plot cumulative cases by regions ------------------------------

country_cases_plot_cumulative <- function(cv_cases,plot_start_date){
  
  plot_start_date <- as.Date(plot_start_date) 
  
  cv_cases <- cv_cases %>% 
    filter(as.Date(date) <= plot_start_date)
    #mutate(new_outcome = cumsum(new_outcome))
  
  g = ggplot(cv_cases, aes(x = date, y = new_outcome , fill = region, group = 1,
                           text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
    #xlim(c(plot_start_date,(current_date+5))) + 
    xlab("Date")
  
  ## Plot
  g1 = g +
    geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Données cumulées") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  
}


country_cases_cumulative <- function(cv_cases,plot_start_date){
  
  cv_cases <- cv_cases %>% 
    filter(as.Date(date) <= plot_start_date) %>% 
    mutate(outcome = cumsum(cv_cases$outcome))
  
  g <-  ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
    #xlim(c(plot_start_date,(current_date+1))) + 
    xlab("Date")
  
  ## Plot
  
  g1 <-  g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Données cumulées") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  
}



################################################################################
# Data processing
################################################################################

if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date <-  format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date <-  as.Date(cv_cases$date, format="%Y-%m-%d") }


cv_cases$date <-  as.Date(cv_cases$date)
cv_min_date <-  as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date <-  as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean <-  format(as.POSIXct(current_date),"%d %B %Y")

# extract key summary variables ------------------------------------------------

cv_cases$epiweek <- yearweek(cv_cases$date)

cv_cases <-  cv_cases[order(cv_cases$date),]

cv_cases <- cv_cases %>% 
  mutate(attack_rate    = as.numeric(format(round(cv_cases$cas_rev/(cv_cases$population)*100000,3),nsmall=1)),
         letality_rate  = as.numeric(format(round(cv_cases$ndeces/cv_cases$cases,3),nsmall=1)),
         Incidence_rate = as.numeric(format(round(cv_cases$cases/(cv_cases$population)*100000,3),nsmall=1)),
         cases_thousand = as.numeric(format(round(cv_cases$cas_rev/(cv_cases$population)*1000,3),nsmall=1)),
         cumulative = cas_rev)

# Créez une colonne pour l'évolution par rapport à la semaine précédente
cv_cases$evolution <- c(0, diff(cv_cases$cases))

# Calcul du pourcentage d'évolution
cv_cases$percentage_change <- (cv_cases$evolution / lag(cv_cases$cases)) * 100

# create variable for today's data

cv_today <-  subset(cv_cases, date==current_date) 
current_case_count <-  sum(cv_today$cases)
current_death_count <-  sum(cv_today$ndeces)

# create subset for regions with at least 1000 cases
cv_today_reduced <-  subset(cv_today, cases>=1000)

# Export the file -----------------------------------

#write.csv(cv_today %>% select(c(region, date,cases, new_cases, deaths, new_deaths,
#                               attack_rate, letality_rate,Incidence_rate)), "data/covid_today.csv")


# select regions for mapping polygons

cv_large_countries <-  cv_cases %>% filter( Code_Reg %in% as.numeric(cameroon_region$Code_Reg))
if (all(cv_large_countries$Code_Reg %in% as.numeric(cameroon_region$Code_Reg))==FALSE) { print("Error: inconsistent country names")}
cv_large_countries <-  cv_large_countries[order(cv_large_countries$Code_Reg),]

# create plotting parameters for map
bins = c(0,5,10,15,20,25,Inf)
cv_pal <- leaflet::colorBin("BuPu", domain = cv_large_countries$cases_per_thousand, bins = bins)

# create cv base map 

library(leaflet)

basemap <-  leaflet(cameroon_region) %>% 
  addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 1020)) %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("COVID-19 (nouveaux cas)", "COVID-19 (Cumul)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("COVID-19 (Cumul)")) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(lng1 = 8.0,lat1 = 1.0,lng2 = 16.0,lat2 = 13.0) %>%
  leaflet::addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$deaths,
            title = "<small>Décès</small>")


## Cv  aggregate --------------------------------------------------------------

cv_aggregated <-   cv_cases %>% 
    group_by(date) %>% 
    summarise(cases = sum(cases),
              cumul = sum(cas_rev))

#cv_aggregated <-  aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
#names(cv_aggregated) = c("date", "cases")


# add variable for new cases in last 7 days
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) {cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1]}
}

cv_aggregated$new[cv_aggregated$new<0] = 0

# add plotting region
cv_aggregated$region = "country"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

cv_aggregated$cumul <- cv_aggregated$cumul

cv_aggregated$cumul[45] <- 24616
cv_aggregated$cumul[47] <- 25679

cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = as.character(unique(cv_cases$region))
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names


## Database for the national and regional levels ------------------------------

 cv_cases <- cv_cases %>% 
               rename(cas_sous_o2_15 = patients_02,
                      gueris = nguerisons,
                      new_cases = cases,
                      new_deaths = ndeces,
                      cases = cas_rev,
                      deaths = deces_rev,
                      guerisons = remission_rev)

cv_cases_national <- cv_cases %>% 
  select(where(is.numeric),Pays,date) %>% 
  #select(cases,new_cases,deaths,new_deaths,guerison,date,Pays,tdr_realise,pcr_realise,tdr_positif,pcr_positif,sous_o2,nouvelles_admissions,hospitalises) %>% 
  group_by(Pays, date) %>% 
  summarise(across(.cols = everything(),.fns = sum)) %>% 
  data.frame()

cv_cases_national <- cv_cases_national %>% 
  mutate(population = 26.153961e6,
         attack_rate    = as.numeric(format(round(cases/(population)*100000,3),nsmall=1)),
         letality_rate  = as.numeric(format(round(new_deaths/cases,3),nsmall=1)),
         Incidence_rate = as.numeric(format(round(new_cases/(population)*100000,3),nsmall=1))
  )



cv_cases_national <- cv_cases_national %>% 
                        mutate(Pays = "Cameroun")


### Regional level -----------------------------------------------------------

cv_cases_regions <- cv_cases %>%
  select(where(is.numeric),date,region) %>% 
  #select(c(cases,new_cases,deaths,new_deaths,guerison,date,region,tdr_realise,pcr_realise,tdr_positif,pcr_positif,sous_o2,nouvelles_admissions,hospitalises)) %>% 
  group_by(region, date) %>% 
  summarise(across(.cols = everything(),.fns = sum)) %>% 
  data.frame()

## Add populations by regions of the country---------------------------------

cv_cases_regions$population[cv_cases_regions$region == "Adamaoua"] = 1345934
cv_cases_regions$population[cv_cases_regions$region == "Centre"] = 4846002
cv_cases_regions$population[cv_cases_regions$region == "Est"] = 1146981
cv_cases_regions$population[cv_cases_regions$region == "Extreme Nord"] = 4824522
cv_cases_regions$population[cv_cases_regions$region == "Littoral"] = 3987222
cv_cases_regions$population[cv_cases_regions$region == "Nord"] = 2964768
cv_cases_regions$population[cv_cases_regions$region == "Nord-Ouest"] = 2244288
cv_cases_regions$population[cv_cases_regions$region == "Ouest"] = 2113367
cv_cases_regions$population[cv_cases_regions$region == "Sud"] = 818190
cv_cases_regions$population[cv_cases_regions$region == "Sud-Ouest"] = 1862687



cv_cases_regions <- cv_cases_regions %>% 
  mutate(attack_rate    = as.numeric(format(round(cases/(population)*100000,3),nsmall=1)),
         letality_rate  = as.numeric(format(round(new_deaths/new_cases,3),nsmall=1)),
         Incidence_rate = as.numeric(format(round(new_cases/(population)*100000,3),nsmall=1))
  )

################################################################################
################################################################################
# Vaccination database
################################################################################
################################################################################


cv_min_date_vaccination <-  as.Date(min(cv_vaccine$date),"%Y-%m-%d")
current_date_vaccination <-  as.Date(max(cv_vaccine$date),"%Y-%m-%d")
cv_max_date_clean_vaccination <-  format(as.POSIXct(current_date_vaccination),"%d %B %Y")
cv_today_vaccination <-  subset(cv_vaccine, date==current_date_vaccination)



cv_vaccine_regional <- cv_vaccine %>% 
                        group_by(date,region,vaccin) %>% 
                        summarise(across(.cols = where(is.numeric),.fns = sum))

cv_vaccine_national <- cv_vaccine %>% 
                        group_by(date,vaccin) %>% 
                         summarise(across(.cols = where(is.numeric),.fns = sum)) %>% 
                         mutate(Pays = "Cameroun")

#########################################################
# Add vaccines population data 

cv_vaccine_national$population_cible[cv_vaccine_national$Pays == "Cameroun"] <- 13758942
cv_vaccine_national$population_totale[cv_vaccine_national$Pays == "Cameroun"] <- 27795843
cv_vaccine_national$population_cible_personnel_sante[cv_vaccine_national$Pays == "Cameroun"] <- 133304
cv_vaccine_national$population_cible_personnes_ages[cv_vaccine_national$Pays == "Cameroun"] <- 2694444
cv_vaccine_national$population_cible_personnes_commorbidite[cv_vaccine_national$Pays == "Cameroun"] <- 2626384

cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Adamaoua"] = 753881
cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Centre"] = 2568486
cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Est"] = 689284
cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Extreme Nord"] = 2526584
cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Littoral"] = 2166869
cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Nord"] = 1533515
cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Nord Ouest"] = 936621
cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Ouest"] = 1167571
cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Sud"] = 450052
cv_vaccine_regional$population_cible[cv_vaccine_regional$region == "Sud Ouest"] = 966080

# Population cible

cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Adamaoua"] = 1522992
cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Centre"] = 5188860
cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Est"] = 1392492
cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Extreme Nord"] = 5104209
cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Littoral"] = 4377513
cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Nord"] = 3098010
cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Nord Ouest"] = 1892164
cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Ouest"] = 2358730
cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Sud"] = 909197
cv_vaccine_regional$population_totale[cv_vaccine_regional$region == "Sud Ouest"] = 1951677

# Cible Personnel de sante

cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Adamaoua"] = 7178
cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Centre"] = 32968
cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Est"] = 8077
cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Extreme Nord"] = 23200
cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Littoral"] = 19135
cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Nord"] = 14228
cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Nord Ouest"] = 9415
cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Ouest"] = 13125
cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Sud"] = 5419
cv_vaccine_regional$population_cible_personnel_sante[cv_vaccine_regional$region == "Sud Ouest"] = 7793

# Cible Personnes agees 

cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Adamaoua"] = 124901
cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Centre"] = 562549
cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Est"] = 134138
cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Extreme Nord"] = 386706
cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Littoral"] = 537165
cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Nord"] = 240792
cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Nord Ouest"] = 174579
cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Ouest"] = 227655
cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Sud"] = 115897
cv_vaccine_regional$population_cible_personnes_ages[cv_vaccine_regional$region == "Sud Ouest"] = 190062

# Cible Personnes commorbidites

cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Adamaoua"] = 135291
cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Centre"] = 528393
cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Est"] = 131052
cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Extreme Nord"] = 376370
cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Littoral"] = 525279
cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Nord"] = 232884
cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Nord Ouest"] = 172352
cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Ouest"] = 224670
cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Sud"] = 114071
cv_vaccine_regional$population_cible_personnes_commorbidite[cv_vaccine_regional$region == "Sud Ouest"] = 185022

#########################################################
# Ressortir les principaux indicateurs vaccination dans un datatable
#########################################################


cv_vaccine_national$vaccin <- fct_recode(cv_vaccine_national$vaccin, "Sinopharm" = "SINOPHARM") 

data_clean_immunization <- cv_vaccine_regional %>% 
  group_by(region,date) %>% 
  summarise(
    pop_cible = sum(unique(population_cible)),
    pop_totale = sum(unique(population_totale)),
    dose1_astra = sum(total_dose1[vaccin == "Astra Zeneca"],na.rm = TRUE),
    dose1_sino =  sum(total_dose1[vaccin == "Sinopharm"],na.rm = TRUE),
    dose1_pfizer = sum(total_dose1[vaccin == "Pfizer"],na.rm = TRUE),
    dose1_janssen = sum(total_dose1[vaccin == "Janssen"],na.rm = TRUE),
    dose2_astra = sum(total_dose2[vaccin == "Astra Zeneca"],na.rm = TRUE),
    dose2_sino =  sum(total_dose2[vaccin == "Sinopharm"],na.rm = TRUE),
    dose2_pfizer = sum(total_dose2[vaccin == "Pfizer"],na.rm = TRUE),
    booster_astra = sum(total_dose3[vaccin == "Astra Zeneca"],na.rm = TRUE),
    booster_sino =  sum(total_dose3[vaccin == "Sinopharm"],na.rm = TRUE),
    booster_pfizer = sum(total_dose3[vaccin == "Pfizer"],na.rm = TRUE),
    booster_janssen = sum(total_dose3[vaccin == "Janssen"],na.rm = TRUE),
    overall_dose1 = dose1_astra+dose1_sino+dose1_pfizer+dose1_janssen,
    overall_dose2 = dose2_astra+dose2_sino+dose2_pfizer,
    overall_booster = booster_astra+booster_sino+booster_pfizer+booster_janssen,
    completement_vaccines = dose2_astra+dose2_sino+dose2_pfizer+dose1_janssen,
    total_administered = overall_dose1+overall_dose2+overall_booster,
    population_cible_personnel_sante = sum(unique(population_cible_personnel_sante), na.rm = TRUE),
    population_cible_personnes_ages = sum(unique(population_cible_personnes_ages), na.rm = TRUE),
    population_cible_personnes_commorbidite = sum(unique(population_cible_personnes_commorbidite), na.rm = TRUE),
    dose2_astra_healthworker = sum(personnel_sante_dose2[vaccin == "Astra Zeneca"], na.rm = TRUE),
    dose2_sino_healthworker = sum(personnel_sante_dose2[vaccin == "Sinopharm"], na.rm = TRUE),
    dose2_pfizer_healthworker = sum(personnel_sante_dose2[vaccin == "Pfizer"], na.rm = TRUE),
    dose2_janssen_healthworker = sum(personnel_sante_dose2[vaccin == "Janssen"], na.rm = TRUE),
    dose1_janssen_healthworker = sum(personnel_sante_dose1[vaccin == "Janssen"], na.rm = TRUE),
    dose2_astra_aged = sum(`50ans_dose2_1`[vaccin == "Astra Zeneca"], na.rm = TRUE),
    dose2_sino_aged = sum(`50ans_dose2_1`[vaccin == "Sinopharm"], na.rm = TRUE),
    dose2_pfizer_aged = sum(`50ans_dose2_1`[vaccin == "Pfizer"], na.rm = TRUE),
    dose1_janssen_aged = sum(`50ans_dose1_1`[vaccin == "Janssen"], na.rm = TRUE),
    dose2_janssen_aged = sum(`50ans_dose2_1`[vaccin == "Janssen"], na.rm = TRUE),
    dose2_astra_commobidite = sum(commrobidite_dose2[vaccin == "Astra Zeneca"], na.rm = TRUE),
    dose2_sino_commobidite = sum(commrobidite_dose2[vaccin == "Sinopharm"], na.rm = TRUE),
    dose2_pfizer_commobidite = sum(commrobidite_dose2[vaccin == "Pfizer"], na.rm = TRUE),
    dose2_janssen_commobidite = sum(commrobidite_dose2[vaccin == "Janssen"], na.rm = TRUE),
    dose1_janssen_commobidite = sum(commrobidite_dose1[vaccin == "Janssen"], na.rm = TRUE),
    complet_health_worker = dose2_astra_healthworker+dose2_sino_healthworker+dose2_pfizer_healthworker+dose2_janssen_healthworker+dose1_janssen_healthworker,
    complet_aged = dose2_astra_aged+dose2_sino_aged+dose2_pfizer_aged+dose1_janssen_aged+dose2_janssen_aged,
    complet_commorbidite = dose2_astra_commobidite+dose2_sino_commobidite+dose2_pfizer_commobidite+dose2_janssen_commobidite+dose1_janssen_commobidite,
    dose1_femme =  sum(femme_dose1, na.rm = TRUE),
    dose1_homme = sum(hommes_dose1, na.rm = TRUE),
    dose2_femme = sum(femme_dose2, na.rm = TRUE),
    dose2_homme = sum(hommes_dose2, na.rm = TRUE),
    booster_femme = sum(femme_dose3,na.rm = TRUE),
    booster_homme =  sum(hommes_dose3,na.rm = TRUE),
    vaccine_femme_jj = sum(femme_dose1[vaccin == "Janssen"], na.rm = TRUE),
    vaccine_homme_jj = sum(hommes_dose1[vaccin == "Janssen"], na.rm = TRUE),
    admin_sinopharm = sum(dose_administrees[vaccin == "Sinopharm"], na.rm = TRUE),
    admin_astra = sum(dose_administrees[vaccin == "Astra Zeneca"], na.rm = TRUE),
    admin_pfizer = sum(dose_administrees[vaccin == "Pfizer"], na.rm = TRUE),
    admin_janssen = sum(dose_administrees[vaccin == "Janssen"], na.rm = TRUE),
    mapi_mineur = sum(mapi_mineur2, na.rm = TRUE),
    mapi_grave  = sum(mapi_grave, na.rm = TRUE),
    mapi_mineur_astra = sum(mapi_mineur[vaccin == "Astra Zeneca"], na.rm = TRUE),
    mapi_mineur_pfizer = sum(mapi_mineur[vaccin == "Pfizer"], na.rm = TRUE),
    mapi_mineur_janssen = sum(mapi_mineur[vaccin == "Janssen"], na.rm = TRUE),
    mapi_mineur_sino = sum(mapi_mineur[vaccin == "Sinopharm"], na.rm = TRUE),
    mapi_grave_astra = sum(mapi_grave[vaccin == "Astra Zeneca"], na.rm = TRUE),
    mapi_grave_pfizer = sum(mapi_grave[vaccin == "Pfizer"], na.rm = TRUE),
    mapi_grave_janssen = sum(mapi_grave[vaccin == "Janssen"], na.rm = TRUE),
    mapi_grave_sino = sum(mapi_grave[vaccin == "Sinopharm"], na.rm = TRUE),
    doses_received = sum(dose_recue, na.rm = TRUE),
    fully_vaccinated_femme = dose2_femme + vaccine_femme_jj,
    fully_vaccinated_homme = dose2_homme + vaccine_homme_jj,
    total_doses_admin_femme = dose1_femme + dose2_femme + booster_femme,
    total_doses_admin_homme = dose1_homme + dose2_homme + booster_homme,
    across(
      .cols = contains("hommes") | contains("femme"),
      .fns = ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(epiweek = yearweek(date),
         week_num = as.numeric(format(date, format = '%W')),
         yearweek = as.numeric(format(date, format = '%Y')),
         date_epiweek = floor_date(date,unit = "week",week_start = 1))


data_clean_immunization$week <- str_remove(as.character(data_clean_immunization$epiweek),"W")
data_clean_immunization$week <- str_remove(data_clean_immunization$week," ")
data_clean_immunization$week <- as.numeric(data_clean_immunization$week)


#export(data_clean_immunization,here("database_vaccination.xlsx"))



## cv_cases_database_arrange


 cv_cases <- cv_cases %>% 
               mutate(
                 epiweek = yearweek(date),
                 week_num = as.numeric(format(date, format = '%W')),
                 yearweek = as.numeric(format(date, format = '%Y')),
                 date_epiweek = floor_date(date,unit = "week",week_start = 1)
               )

# Extract the week
 
 cv_cases$week <- str_remove(as.character(cv_cases$epiweek),"W")
 cv_cases$week <- str_remove(cv_cases$week," ")
 cv_cases$week <- as.numeric(cv_cases$week)



 
 

 
 
