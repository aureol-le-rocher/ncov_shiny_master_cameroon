library(dplyr) 
library(shiny)
library(shinyWidgets)
library(openxlsx)
library(leaflet) 
library(leaflet.extras)
library(plotly)
library(htmlwidgets)
library(shinycssloaders)
library(shinyFeedback)
library(gtools)
library(zoo)
library(here)
library(bs4Dash)
library(shinydashboard)
library(tableHTML)
library(highcharter)
library(DT)
library(officer)
library(shinyjs)
library(janitor)
library(cowplot)
library(formattable)
library(epikit)
library(shinyjs)
library(rio)


source(here("scripts","data_processing.R"))


cmprss <- function(x){
  
  x <- round(x/1000,1)
  x <- paste(x,"k")
  return(x)
  
}

#------------USER INTERFACE (UI)---------------

ui <-tagList(
  useShinyFeedback(),
  useShinydashboard(),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
    tags$style(type = "text/css", "#map {height: calc(100vh - 45px) !important;
               width: 100%;
                position: fixed;
                top: 50px;
                left: 0;}
                                         .jhr{
    display: inline;
    vertical-align: middle;
    padding-left: 10px;
                                         }
                        #down{
                        margin-top:30px;
                        }

               "),
    #tags$style(HTML(".info-box {font-sixe:24px;}")),
    tags$script(type="text/javascript", src = "code.js")
  ), 
  navbarPage( 
    windowTitle = "COVID-19 Tracker",
    title=div(
      tags$span(style="font-size:30px;margin-left:10px;margin-top:-10px","COVID-19 Tracker"),
      img(
        src = "covid.png",
        height = 50,
        width = 70,
        style = "margin-top:-15px"
      )
    ),
    id="navbar",
    selected="Map",
    # tabsetPanel(id="panel",
    tabPanel("Carte",
             div(class="outer",
                 tags$head(includeCSS("css_script.css")),
                 leafletOutput("mymap", width="100%", height="100%")),
             absolutePanel(
               top=60, left = 80, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;background-color:rgba(255,255,255,0.8);
                box-shadow: 0 0 15px rgba(0,0,0,0.2);
                border-radius: 5px;padding: 6px 8px;",
               # titlePanel("Covid-19 au Cameroun"),
               tags$b(tags$span(style="font-size:25px","Covid-19 au Cameroun")),
               br(),
               h4(textOutput("clean_date_reactive"), align = "left"),
               #tags$b(tags$i(tags$span(style="font-size:13px","Cette semaine"))),
               h5(textOutput("reactive_case_count_new"), align = "left",style = "color:#333F4F"),
               h5(textOutput("reactive_death_count_new"), align = "left",style = "color:#333F4F"),
               h5(textOutput("reactive_vaccine_count_new"),align = "left",style = "color:#333F4F"),
               tags$b(tags$i(tags$span(style="font-size:10px","Cumul(Ancien + Nouveaux)"))),
               h3(textOutput("reactive_case_count"), align = "left"),
               h4(textOutput("reactive_death_count"), align = "left",style = "color:#FF0000"),
               h5(textOutput("reactive_active_cases"),align = "left"),
               h5(textOutput("reactive_vaccine_count"),align = "left",style = "color:#418FDE"),
               #h6(textOutput("reactive_active_cases"), align = "left"),
               br(),
               plotOutput("epi_curve", height="130px", width="100%"),
               plotOutput("cumulative_plot", height="130px", width="100%"),
               sliderTextInput("plot_date",
                               label = h5("Choisir la date"),
                               choices = format(unique(cv_cases$date), "%d %b %y"),
                               selected = format(current_date, "%d %b %y"),
                               grid = FALSE,
                               animate=animationOptions(interval = 3000, loop = FALSE)),
               h6("Powered with the technical support of Georgetown University CGHPI")
             )
             
    ),
    tabPanel("Indicateurs",
             wellPanel(
               fluidRow(
                 column(3,
                        pickerInput(
                          inputId = "select_level",
                          label = "Choisir le niveau d'analyse", 
                          choices = c("Niveau pays","Régions"),
                          selected = "Régions",
                          options = list(
                            `live-search` = TRUE)
                        )
                 ),
                 column(2,
                        pickerInput(
                          inputId = "region_select",
                          label = "Pays/Région:", 
                          choices = as.character(cv_today[order(-cv_today$cases),]$region),
                          options = list(`actions-box` = TRUE, `none-selected-text` = "Bien vouloir effectuer une sélection!"),
                          selected=as.character(cv_today[order(-cv_today$cases),]$region)[1:3],
                          multiple = TRUE
                        )
                 ),
                 column(2,
                        pickerInput(
                          inputId = "outcome",
                          label = "Métric", 
                          choices = c("Cas confirmes (Total)", "Décès (total)","Hospitalisation(s)","Sous 02","TDR positif","PCR positif"),
                          selected="Cas confirmes (Total)"
                        )
                 ),
                 column(3,
                        sliderTextInput("minimum_date",
                                        label = h5("Choisir la date"),
                                        choices = format(unique(cv_cases$date), "%d %b %y"),
                                        selected = format(current_date, "%d %b %y"),
                                        grid = FALSE,
                                        animate=animationOptions(interval = 3000, loop = FALSE))
                 )
               ),
               tabsetPanel(
                 id="plot",
                 tabPanel(
                   "Evolution",
                   br(),
                   fluidRow(column(6,
                            plotlyOutput("country_plot",width="100%") %>% withSpinner()),
                            column(6,
                                    box(title ="Resume",status = "success",solidHeader = TRUE,width = 12, 
                                        fluidRow(infoBoxOutput("cases_this_day",width = 6),
                                              infoBoxOutput("attack_rate1",width = 6),
                                              infoBoxOutput("attack_rate2", width = 6),
                                              infoBoxOutput("attack_rate3",width = 6),
                                              infoBoxOutput("attack_rate4",width = 6),
                                              infoBoxOutput("attack_rate5",width = 6)
                                    )
                                  )
                              )
                 )
              ),
                 tabPanel(
                   "Cumul", fluidRow(column(6,
                   plotlyOutput("country_plot1",width="100%") %>% withSpinner()),
                   column(6,
                           box(title = "Resume (Cumul)", status = "success",solidHeader = TRUE,width = 12,
                               fluidRow(
                                 infoBoxOutput("cases_cumulative",width = 6),
                                 infoBoxOutput("indicators_cumulative",width = 6),
                                 infoBoxOutput("indicators_cumulative1", width = 6),
                                 infoBoxOutput("indicators_cumulative2",width = 6),
                                 infoBoxOutput("indicators_cumulative3",width = 6)
                               )
                             )
                          )
                   )
                 )
               )
             )),
    tabPanel("Vaccination",
              sidebarLayout(
               div(
                 sidebarPanel( class = "sidebar_back",
                   tags$style(".sidebar_back {background-color:#FFFFFF;
                                              border:2px solid #7FB92F;}"),
                   pickerInput(
                     inputId = "select_level_vaccination",
                     label = "Choisir le niveau d'analyse", 
                     choices = c("Niveau pays","Régions"),
                     selected = "Régions",
                     options = list(
                       `live-search` = TRUE)
                   ),width = 3,
                   br(),
                   br(),
                   pickerInput(
                     inputId = "region_select_vaccination",
                     label = "Pays/Région:", 
                     choices = as.character(unique(cv_vaccine$region)),
                     options = list(`actions-box` = TRUE, `none-selected-text` = "Bien vouloir effectuer une sélection!"),
                     selected=as.character(unique(cv_vaccine$region))[1:5],
                     multiple = TRUE
                   ),
                   br(),
                   br(),
                   
                   sliderInput("minimum_date_vaccination",
                               "Date:",
                               min = as.Date(cv_min_date_vaccination,"%Y-%m-%d"),
                               max = as.Date(current_date_vaccination,"%Y-%m-%d"),
                               value=as.Date(current_date_vaccination),
                               timeFormat="%d %b"),
                   br(),
                   br(),
                   div(
                     uiOutput("image_output"),
                     uiOutput("image_output1"),
                     style = "display: flex; align-items: center;"
                   )
                 )
                ),
                 mainPanel(
                   tabsetPanel(id = "first_tab",
                             tabPanel(
                               title = "Données par niveau",
                              br(),
                            fluidRow(column(width = 6,
                             box(title = "", status = "success",solidHeader = TRUE,width = 12,
                                 highchartOutput("adminregionBarPlot"))),
                             column(
                               width = 6,
                               box(title = "",status = "success",solidHeader = TRUE,width = 12,
                                   highchartOutput("adminregionCumulative")
                                   )),
                             column(width = 12,
                                box(title = "Principaux indicateurs",status = "success",solidHeader = TRUE,width = 12,
                                    fluidRow(
                                      infoBoxOutput("fully_vaccinated"),
                                      infoBoxOutput("prop_one_dose"),
                                      infoBoxOutput("fully_vaccinated_prop"),
                                      infoBoxOutput("fully_vaccinated_prop_total"),
                                      infoBoxOutput("two_vaccine_doses"),
                                      infoBoxOutput("booster_received"),
                                      dataTableOutput("indicators_table")
                                    )
                                    
                                    )
                                )
                        )
                       ),
                       tabPanel(title = "Données par sexe",
                            br(),
                        fluidRow(
                          column(width = 12,
                              box(title = "",status = "success",solidHeader = TRUE,width = 15,
                                  highchartOutput("genderpiechart"),
                                  dataTableOutput("genderdataTable")
                                  )
                                 )
                          
                        )  
                                
                        ),
                       tabPanel(title = "Couvertures vaccinales",
                                br(),
                            fluidRow(
                              column(width = 6,
                                     box(title = "",status = "success",solidHeader = TRUE,width = 12,
                                         highchartOutput("national_coverage_graph")
                                         )),
                              column(width = 6,
                                    box(title = "",status = "success",solidHeader = TRUE,width = 12,
                                        highchartOutput("national_coverage_priorgraph")
                                        ) 
                                    )
                              
                            )    
                          )
                       
                      )
                  )
                
              )
               
          ),
     tabPanel("Rapports",style = "background-color: #f5f5f5;",
              fluidRow(
                column(12,
                       tags$h3("Générer un rapport", style = "color: #333;"),
                       tags$hr(style = "border-top: 2px solid #333;"),
                       tags$p("Cette fenêtre vous permet de créer un rapport  sur la situation de l'épidémie de la COVID-19 et de la vaccination au Cameroun pour une période spécifique."),
                       tags$ul(tags$li("Les rapports sont produits uniquemenent a titre d'information des opérations"),
                               tags$li("Le nettoyage des données se fait continuellement et dont ces données sont peuvent etre vérifiées et modifiées"),
                               tags$li("Les analyses supplémetaires peuvent etre ajoutées ou enlevées en fonction des besoins opérationnels au moment de la production")),
                       br(),
                       br(),
                       div(class = "center_text",
                           tags$style(".center_text {margin-left: 60px; margin-right: auto; text-align: left;}"),
                           pickerInput("semaine_epi", "Sélectionnez la semaine Epidémiologique :", 
                                   choices = as.character(unique(cv_cases$epiweek)), 
                                   selected = as.character(unique(cv_cases$epiweek))[191],
                                   options = list(`live-search` = TRUE) 
                                   
                              )
                           
                           ),
                       br(),
                       
                       div(class = "downloader",
                           tags$style(".downloader {margin-left: 60px; margin-right: auto; text-align: left;}"),
                           downloadButton("OutputButton_maladie", 
                                        tags$b("Télécharger le rapport sur la maladie",style = "color: blue;"),
                                        icon = icon("download",verify_fa = FALSE)
                                        ),
                           align = "right",
                           style = "margin-bottom:10px;",
                           style = "margin-top:-10px;"
                       ),
                       br(),
                       div(class = "downloader1",
                           tags$style(".downloader1 {margin-left: 60px; margin-right: auto; text-align: left;}"),
                           downloadButton("outputButton_PPT", 
                                        tags$b("Télécharger le rapport de vaccination",style = "color: blue;"),
                                        icon = icon("download",verify_fa = FALSE)
                                        ),
                           align = "right",
                           style = "margin-bottom:10px;",
                           style = "margin-top:-10px;"
                       ),
                       br(),
                       br()
                       
                )
            )
          ),
     tabPanel("Data",
          
              numericInput("maxrows", "Nombre de lignes a afficher",25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadcvs", "Télécharger (csv)"),
              br(),
              br(),
              "Données importées des bases du CCOUSP et du PEV"
              
          ),
     tabPanel("A propos",
              div(class ="about",
                  tags$style(".about {background-color:#FFFFFF;
                                              margin-left:50px;
                                              margin-right:50px}"),
                h4(tags$b("Dernière mise a jour:")),
                str_glue("Les donnéees de surveillance de la COVID-19 sont a jour jusqu'au {unique(format(cv_today$date, format = '%A, %d %B %Y'))} et"),
                str_glue("les données de vaccination contre la COVID-19 sont a jour jusqu'au {unique(format(max(cv_vaccine_regional$date),format = '%A, %d %B %Y'))}."),
                br(),
                "La fréquence de mise a jour des données est hebdommadaire, plusieurs autres outils permettent de visualiser la situation de la COVID-19 dans le monde et au Cameroun a travers",
                tags$a(href = "https://covid19.who.int/","le site de l'Organisation Mondiale de la Santé"),",",
                tags$a(href = "www.ccousp.cm","le site officiel du CCOUSP"),"et",
                tags$a(href = "https://pevcameroon.cm/","le site du Programme Elargi de Vaccination-Cameroun."),
                h4(tags$b("Objectifs:")),
                br(),
                tags$p("Ce dashboard vise a:",
                       tags$p("1- Visualiser les statistiques de l'épidémie :",
                        tags$ul(tags$li("Présenter les statistiques quotidiennes et hebdomadaire des cas de COVID-19, des décès et des guérisons au Cameroun."),
                                tags$li("Permettre aux utilisateurs de filtrer les données par région ou par période de temps.")
                                )),
                       tags$p("2- Suivre la progression de la vaccination :",
                              tags$ul(
                                tags$li("Montrer l'Evolution  des doses de vaccin administrées dans le Temps"),
                                tags$li("Comparer les doses de vaccins administrées par type de vaccin"),
                                tags$li("Indiquer le pourcentage de la population du Cameroun ayant reçu au moins une dose et étant complètement vacciné.")
                              ))),
                h4(tags$b("Background de la COVID-19 au Cameroun")),
                "En Decembre 2019, des cas de maladie respiratoire ont été rapportés dans la ville de Wuhan en Chine.
                Ceci était causé par un nouveau type de coronavirus et cette maladie fait coramment réference a la COVID-19.",br(),
                "Le nombre de cas a augmenté rapidement au milieu du mois de Janvier de l'année 2020 et le virus s'est rapidement propagé au dela des frontieres de la Chine.",br(),
                "Au Cameroon, le premier cas a été confirme le 6 Mars 2020 dans la région du Centre et spécifiquement dans le District de santé de Djoungolo.",br(),
                "La vaccination contre la COVID-19 quant a elle a débutée au Cameroun en Avril 2021.",
                h4(tags$b("Close de Non Responsabilité")),
                tags$p( tags$b("Bienvenue sur notre tableau de bord COVID-19 et de la vaccination pour le Cameroun. Nous nous engageons à vous fournir des informations aussi précises que possible, mais il est essentiel de comprendre les éléments suivants :"),
                        tags$ul(
                          tags$li( tags$b("Mises à jour:"),"Les données et les visualisations sont actualisées régulièrement pour refléter l'évolution en temps réel de la situation de la COVID-19 et de la vaccination au Cameroun. Cela signifie que les chiffres, les graphiques et les informations peuvent changer fréquemment."),
                          tags$li(tags$b("Qualité des Données:"),"Nous faisons tout notre possible pour garantir la qualité et l'exactitude des données que nous présentons. Cependant, des erreurs ou des incohérences peuvent parfois survenir dans les données sources. Nous nous efforçons de les corriger rapidement dès qu'elles sont identifiées."),
                          tags$li(tags$b("Sources de Données:"),"Nous utilisons des sources de données officielles,réputées et validées. Cependant, il peut y avoir des variations dans les données provenant de différentes sources, ce qui peut entraîner des divergences dans les chiffres."),
                          tags$li(tags$b("Visuels:"),"Les graphiques sont basés sur des modèles")
                        )),
                br(),
                h4(tags$b("Auteurs")),
                tags$p("Nous sommes fiers de vous présenter ce tableau de bord, fruit de la collaboration entre le Centre des Opérations d'Urgence de Santé Publique (CCOUSP) et le Partenaire Georgetown University, qui œuvrent conjointement pour améliorer la gestion des données."),
                tags$p("Pour des questions Techniques et des erreurs n'hesitez pas a prendre contact avec:",
                       tags$ul(tags$li(tags$b("Christian Mouague"), "Chef Unité gestion des données au CCOUSP: mouanguec1@gmail.com "),
                               tags$li(tags$b("Aureol Ngako"), "Biostatisticien, GeorgetownUniversity : an977@georgetown.edu")))
                
              )
              
          )
      )
  )
  






#--------------------------------------------------

#------------------SERVER--------------------------

server <- function(input, output,session){
  
  formatted_date <-  reactive({
    format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  
  formatted_date1 <- reactive({
    format(as.Date(input$minimum_date, format = "%d %b %y"),"%Y-%m-%d")
  })
  
  formatted_date_last_week <- reactive({
    format(as.Date(input$minimum_date-7, format = "%d %b %y"),"%Y-%m-%d")
  })
  
  formatted_date2 <- reactive({
    format(as.Date(input$minimum_date1, format = "%d %b %y"),"%Y-%m-%d")
  })
  
  output$date_indicator_pane <- renderText({
    paste("Analyse au"," ",format(as.POSIXct(formatted_date2()),"%d %B %Y"))
  })
  
  output$clean_date_reactive <- renderText({
    paste("Semaine épidémiologique du"," ",format(as.POSIXct(formatted_date()),"%d %B %Y"))
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == formatted_date())
  })
  
  reactive_overall <- reactive({
    cv_cases
  })
  
  reactive_cum = reactive({
    cv_cases %>% filter(date <= formatted_date())
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(Code_Reg %in% as.numeric(cameroon_region$Code_Reg))
    cameroon_region_subset = cameroon_region
    large_countries = cameroon_region
    large_countries
  })
  
  reactive_polygons = reactive({
    cameroon_region})
  
  output$reactive_case_count <- renderText({
  
    paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " Cas")
    
  })
  
  
  output$reactive_case_count_new <- renderText({
    
    data <- reactive_db()
    data <- data %>% 
      filter(date == max(date)) %>%
      summarize(total_cases = sum(new_cases))
    
    total_cases <- prettyNum(data$total_cases, big.mark = ",")
    
    paste0(total_cases," ","Cas")
    
  })
  
  
  output$reactive_death_count <- renderText({
    
    data <- reactive_db()
    data <- data %>% 
      filter(date == max(date)) %>%
      summarize(total_deaths = sum(deaths))
    
    total_deaths <- prettyNum(data$total_deaths, big.mark = ",")
    
    paste0(total_deaths, " Décès")
    
  })
  
  output$reactive_death_count_new <- renderText({
    
    data <- reactive_db()
    data <- data %>% 
      filter(date == max(date)) %>%
      summarize(total_deaths = sum(new_deaths))
    
    total_deaths <- prettyNum(data$total_deaths, big.mark = ",")
    
    paste0(total_deaths," ","Décès")
    
  })
  
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db()))," ", "Région (s) affectée(s)")
  })
  
  output$reactive_vaccine_count <- renderText({
    paste0(prettyNum(sum(reactive_cum()$dose_administrees), big.mark=",")," ", "Dose(s) de vaccin administrée(s)")
  })
  
  output$reactive_vaccine_count_new <- renderText({
    paste0(prettyNum(sum(reactive_db()$new_vaccine_doses), big.mark=",")," ", "Dose(s) de vaccin administrée(s)")
  })
  
  output$reactive_active_cases <- renderText({
    paste0(prettyNum(sum(reactive_db()$cas_actifs_rev), big.mark=",")," ", "Cas Actif(s)")
  })
  
  
  
  output$epi_curve <- renderPlot({
    new_cases_plot(cv_aggregated, formatted_date1())})
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(cv_aggregated, formatted_date1())})
  
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes()%>% 
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~ (cases)^(1/3), 
                       fillOpacity = 0.1, color = covid_col, group = "COVID-19 (Cumul)",
                       label = sprintf("<strong>%s (Cumul des cas cette semaine)</strong> <br/> Cas confirmés: %d <br/> Décès: %d <br/> Taux d'attaque (Pour 100000 hbts): %g <br/> Cas guéris: %g <br/> Doses de vaccin administrées: %g", reactive_db()$region,reactive_db()$cases,reactive_db()$deaths,reactive_db()$attack_rate,reactive_db()$guerisons,reactive_db()$dose_administrees) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>% 
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deaths_per_thousand)) %>% 
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/3.5), 
                       fillOpacity = 0.1, color = covid_col, group = "COVID-19 (nouveaux cas)",
                       label = sprintf("<strong>%s (cas de cette semaine)</strong> <br/> Cas confirmés: %d <br/> Taux d'incidence (Pour 100000 hbts): %g <br/> Décès: %g <br/> Nouvelles doses de vaccin administrées:%g", reactive_db()$region,reactive_db()$new_cases,reactive_db()$Incidence_rate,reactive_db()$new_deaths,reactive_db()$new_vaccine_doses) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) 
  })
  
  ## Import an image
  
  image_pev <- "pev.png"
  image_ccousp <- "ccousp.png"
  
  output$image_output <- renderUI({
    tags$img(src = image_pev, width = "100px", height = "100px")
  })
  
  output$image_output1 <- renderUI({
    tags$img(src = image_ccousp, width = "100px", height = "100px")
  })
  
  
  ## Updates regions selections
  
  observeEvent(input$select_level, {
    
    
    if (input$select_level == "Niveau pays") {
      
      updatePickerInput(session = session,
                        inputId = "region_select",
                        choices = "Cameroun",
                        selected = "Cameroun")
      
    }
    
    if (input$select_level == "Régions") {
      
      updatePickerInput( session = session,
                         inputId = "region_select",
                         choices = as.character(cv_today[order(-cv_today$cases),]$region),
                         selected = as.character(cv_today[order(-cv_today$cases),]$region)[1:5])
    }
    
    
  },
  ignoreInit = TRUE)
  
  ## Create the dataframes 
  
  regions_reactives_db <- reactive({
    
    if (input$select_level == "Niveau pays") {
      db <- cv_cases_national
      db$region <- db$Pays
    }
    
    if (input$select_level == "Régions") {
      db <- cv_cases_regions
      db$region <- db$region
      
    }
    
    if (input$outcome == "Cas confirmes (Total)") {
      
      db$outcome <- db$new_cases
      db$new_outcome <- db$new_cases
      db$value <- 1
      db$outcome_tot <- db$cases
    }
    
    if (input$outcome == "Décès (total)") {
      
      db$outcome <- db$new_deaths
      db$new_outcome <- db$new_deaths
      db$outcome_total <- db %>%  group_by(region,date) %>% summarise(outcome = sum(outcome, na.rm = T)) %>% mutate(outcome = cumsum(outcome))
      db$outcome_tot <- db$outcome_total$outcome
      db$value <- 0.001
    }
    
    if(input$outcome == "Hospitalisation(s)") {
      
      db$outcome <- db$hospitalisations_12
      db$outcome_total <- db %>%  group_by(region,date) %>% summarise(outcome = sum(outcome, na.rm = T)) %>% mutate(outcome = cumsum(outcome))
      db$outcome_tot <- db$outcome_total$outcome
    }
    
    if(input$outcome == "Sous 02") {
      
      db$outcome <- db$cas_sous_o2_15
      db$outcome_total <- db %>%  group_by(region,date) %>% summarise(outcome = sum(outcome, na.rm = T)) %>% mutate(outcome = cumsum(outcome))
      db$outcome_tot <- db$outcome_total$outcome
    }
    
    if(input$outcome == "TDR positif") {
      
      db$outcome <- db$tdr_positif
      db$outcome_total <- db %>%  group_by(region,date) %>% summarise(outcome = sum(outcome, na.rm = T)) %>% mutate(outcome = cumsum(outcome))
      db$outcome_tot <- db$outcome_total$outcome
    }
    
    if(input$outcome == "PCR positif") {
      
      db$outcome <- db$pcr_positif
      db$outcome_total <- db %>%  group_by(region,date) %>% summarise(outcome = sum(outcome, na.rm = T)) %>% mutate(outcome = cumsum(outcome))
      db$outcome_tot <- db$outcome_total$outcome
    }
    
    if(input$outcome == "Doses administrées"){
      
      db$outcome <- db$dose_administrees
    }
    
    db %>% filter(region %in% input$region_select)
    
  })
  
  
  ## Formatted dates 2
  
  output$date_graphe_pane <- renderText({
    paste("SITUATION EN DATE DU"," ",format(as.POSIXct(formatted_date1()),"%d %B %Y"))
  })
   

  output$date_graphe_pane2 <- renderText({
    paste("SITUATION EN DATE DU"," ",format(as.POSIXct(formatted_date1()),"%d %B %Y"))
  })
  
## Create dataframe for the indicator panel
  
  regions_reactives_db1 <- reactive({
    
    db <- cv_cases_national
    db$region <- db$Pays
    
    # Filter the period
    
    db %>% filter(date <= input$minimum_date1)
    
    
  })

###############################################################################
###############################################################################
 
# country-specific plots

  
  shinyjs::useShinyjs()
  
 # Creer le graphique
  
  
  output$country_plot <- renderPlotly({
    country_cases_plot(regions_reactives_db(),plot_start_date =  formatted_date1())})
  
# Cumul
  
 output$country_plot1 <- renderPlotly({
    country_cases_plot1(regions_reactives_db(),plot_start_date =  formatted_date1())
  })
  

 

# Total cases for this day and cumulative 
  
 regions_reactives_fluidbox <- reactive({
   
   if (input$select_level == "Niveau pays") {
     db <- cv_cases_national
     db$region <- db$Pays
   }
   
   if (input$select_level == "Régions") {
     db <- cv_cases_regions
     db$region <- db$region
   }
   
   if (input$outcome == "Cas confirmes (Total)") {
     
     db$outcome <- db$new_cases
     db$new_outcome <- db$cases
     db$title_today <- "Nvx Cas cette semaine"
     db$title_week <- "Deux dernière semaines"
     db$title_cum <- "Cumul des cas"
   }
   
   
     if (input$outcome == "Décès (total)") {
       
       db$outcome <- db$new_deaths
       db$new_outcome <- db$cases
       db$title_today <- "Décès cette semaine"
       db$title_week <- "Deux dernière semaines"
       db$title_cum <- "Cumul des décès"
     }
   
   
   if(input$outcome == "Hospitalisation(s)") {
     
     db$outcome <- db$hospitalisations_12
     db$new_outcome <- db$hospitalisation
     db$title_today <- "Hospi cette semaine"
     db$title_week <- "Deux dernière semaines"
     db$title_cum <- "Cumul Hospitalisations"
   }
   
   if(input$outcome == "Sous 02") {
     
     db$outcome <- db$cas_sous_o2_15
     db$title_today <- "Cas sous 02 cette semaine"
     db$title_week <- "Cas 02 - 7jours"
     db$title_cum <- "Cas sous Oxigiene (Cumul)"
   }
   
   if(input$outcome == "TDR positif") {
     
     db$outcome <- db$tdr_positif
     db$title_today <- "TDR+ cette semaine"
     db$title_week <- "Deux dernière semaines"
     db$title_cum <- "Cumul TDR+"
   }
   
   if(input$outcome == "PCR positif") {
     
     db$outcome <- db$pcr_positif
     db$title_today <- "PCR+ cette semaine"
     db$title_week <- "Deux dernière semaines"
     db$title_cum <- "Cumul des PCR+"
   }
   
   db %>% filter(region %in% input$region_select)
   
 })
 
 population_total <- reactive({
   
   db <- tibble()
   
   if (input$select_level == "Niveau pays") {
     
     db <- cv_cases_national %>% 
       mutate(region = Pays) %>% 
       group_by(region,date) %>% 
       filter(as.Date(date) == formatted_date1())
     
   }
   
   if (input$select_level == "Régions") {
     
     db <- cv_cases_regions %>% 
       mutate(region = region) %>% 
       group_by(region,date) %>% 
       filter(as.Date(date) == formatted_date1())
     
   }
   
   if (input$outcome == "Cas confirmes (Total)") {
     
     db <- db %>% 
       summarise(cases = sum(cases, na.rm = TRUE),
                 new_cases = sum(new_cases,na.rm = TRUE),
                 guerisons = sum(npersonnel, na.rm = TRUE),
                 gueris_total = sum(gueris,na.rm = TRUE),
                 deces = sum(deaths, na.rm = TRUE),
                 fec_total = sum(nfec, na.rm = TRUE),
                 cas_actifs = sum(cas_actifs_rev, na.rm = TRUE),
                 population = sum(unique(population), na.rm = TRUE))
     
     db <-  db %>% filter(region %in% input$region_select)
     
     db$outcome <- round(sum(db$cases, na.rm = TRUE)/sum(unique(db$population))*100000,2)
     
     db$new_outcome <- round(sum(db$new_cases, na.rm = TRUE)/sum(unique(db$population))*100000,2)
     
     db$indicator1 <- sum(db$cas_actifs, na.rm = TRUE)
     
     db$indicator2 <-  sum(db$guerisons, na.rm = TRUE)
     
     db$indicator3 <-  sum(db$gueris_total, na.rm = TRUE)
     
     db$indicator4 <-  sum(db$fec_total, na.rm = TRUE)
     
     db$title <- "Tx d'incidence/100000hbts"
     db$title1 <- "Cas actifs"
     db$title2 <- "Personnel de santé infecté"
     db$title3 <- "Cas guéris"
     db$title4 <- "Femmes enceintes positives"
     
     db$value <- 1
     
   }
   
   if (input$outcome == "Décès (total)") {
     
     db <- db %>% 
       summarise(deaths = sum(deaths, na.rm = TRUE),
                 new_deaths = sum(new_deaths,na.rm = TRUE), 
                 new_cases = sum(new_cases, na.rm = TRUE),
                 deces_personnnel = sum(npersodcd,na.rm = TRUE),
                 deces_fec = sum(nfec_dcd,na.rm = TRUE))
     
     db <-  db %>% filter(region %in% input$region_select)
     
     db$new_outcome <- case_when(is.na(round(sum(db$new_deaths, na.rm = TRUE)/sum(db$new_cases, na.rm = TRUE)*100,2)) ~ 0,
                             is.infinite(round(sum(db$new_deaths, na.rm = TRUE)/sum(db$new_cases, na.rm = TRUE)*100,2)) ~ 0,
                             TRUE ~ round(sum(db$new_deaths, na.rm = TRUE)/sum(db$new_cases, na.rm = TRUE)*100,2))
     
     db$indicator1 <-  sum(db$deces_personnnel, na.rm = TRUE)
     db$indicator2 <-  sum(db$deces_fec, na.rm = TRUE)
     db$indicator3 <-  NA_integer_
     db$indicator4 <-  NA_integer_
     
     
     db$title <- "Tx letalité (%)"
     db$title1 <- "Décès personnel de santé"
     db$title2 <- "Décès femme enceinte"
     db$title3 <-  "-----------------------------------------"
     db$title4 <-  "-----------------------------------------"
     
     db$value <- 1
     
     
   } 
   
   
   if (input$outcome == "Hospitalisation(s)") {
     
     db <- db %>%
       summarise(admissions = sum(hospitalisations_12, na.rm = TRUE),
                 cases = sum(new_cases, na.rm = TRUE)) %>% 
       select(-date)%>% 
       filter(region %in% input$region_select)
     
     
     db$new_outcome <- case_when(is.na(round(sum(db$admissions, na.rm = TRUE)/sum(db$cases, na.rm = TRUE)*100,2)) ~ 0,
                             is.infinite(round(sum(db$admissions, na.rm = TRUE)/sum(db$cases, na.rm = TRUE)*100,2))~0,
                             TRUE ~ round(sum(db$admissions, na.rm = TRUE)/sum(db$cases, na.rm = TRUE)*100,2)) 
     
     
     
     db$title <- "Nouvelles admissions(%)"
     
     db$title1 <-  "-----------------------------------------"
     db$title2 <-  "-----------------------------------------"
     db$title3 <-  "-----------------------------------------"
     db$title4 <-  "-----------------------------------------"
     
     db$indicator1 <-  NA_integer_
     db$indicator2 <-  NA_integer_
     db$indicator3 <-  NA_integer_
     db$indicator4 <-  NA_integer_
     
   }
   
   if (input$outcome == "Sous 02") {
     
     db <- db %>%
       summarise(cas_o2 = sum(cas_sous_o2_15, na.rm = TRUE),
                 admissions = sum(new_cases, na.rm = TRUE)) %>% 
       select(-date)%>% 
       filter(region %in% input$region_select)
     
     
     db$new_outcome <- case_when(is.na(round(sum(db$cas_o2, na.rm = TRUE)/sum(db$admissions, na.rm = TRUE)*100,2)) ~ 0,
                             is.infinite(round(sum(db$cas_o2, na.rm = TRUE)/sum(db$admissions, na.rm = TRUE)*100,2))~0,
                             TRUE ~ round(sum(db$cas_o2, na.rm = TRUE)/sum(db$admissions, na.rm = TRUE)*100,2))
     
     db$title <- "Taux de séverité (%)"
     db$title1 <- "-----------------------------------------"
     db$title2 <- "-----------------------------------------"
     db$title3 <- "-----------------------------------------"
     db$title4 <- "-----------------------------------------"
     db$indicator1 <-  NA_integer_
     db$indicator2 <-  NA_integer_
     db$indicator3 <-  NA_integer_
     db$indicator4 <-  NA_integer_
   }
   
   if(input$outcome == "TDR positif"){
     
     db <- db %>%
       summarise(realises_tdr = sum(tdr_realise, na.rm = TRUE),
                 positif_tdr = sum(tdr_positif, na.rm = TRUE)) %>% 
       select(-date)%>% 
       filter(region %in% input$region_select)
     
     db$new_outcome <- case_when(is.na(round(sum(db$positif_tdr, na.rm = TRUE)/sum(db$realises_tdr, na.rm = TRUE)*100,2)) ~ 0,
                             is.infinite(round(sum(db$positif_tdr, na.rm = TRUE)/sum(db$realises_tdr, na.rm = TRUE)*100,2))~0,
                             TRUE ~ round(sum(db$positif_tdr, na.rm = TRUE)/sum(db$realises_tdr, na.rm = TRUE)*100,2))
     
     db$title <- "Tx de positivité TDR (%)"
     db$title1 <-  "-----------------------------------------"
     db$title2 <-  "-----------------------------------------"
     db$title2 <-  "-----------------------------------------"
     db$title3 <-  "-----------------------------------------"
     db$title4 <-  "-----------------------------------------"
     
     db$indicator1 <-  NA_integer_
     db$indicator2 <-  NA_integer_
     db$indicator3 <-  NA_integer_
     db$indicator4 <-  NA_integer_
   }
   
   if(input$outcome == "PCR positif"){
     
     db <- db %>%
       summarise(realises_pcr = sum(pcr_realise, na.rm = TRUE),
                 positif_pcr = sum(pcr_positif, na.rm = TRUE),
                 realises_tdr = sum(tdr_realise, na.rm = TRUE),
                 population = sum(unique(population, na.rm = TRUE))) %>%
       select(-date)%>% 
       filter(region %in% input$region_select)
     
     db$new_outcome <- case_when(is.na(round(sum(db$positif_pcr, na.rm = TRUE)/sum(db$realises_pcr, na.rm = TRUE)*100,2)) ~ 0,
                             is.infinite(round(sum(db$positif_pcr, na.rm = TRUE)/sum(db$realises_pcr, na.rm = TRUE)*100,2))~0,
                             TRUE ~ round(sum(db$positif_pcr, na.rm = TRUE)/sum(db$realises_pcr, na.rm = TRUE)*100,2))
     
     db$indicator1 <- round((sum(db$realises_pcr, na.rm = TRUE) + sum(db$realises_tdr, na.rm = TRUE))/sum(unique(db$population))*100000,3)
     
     db$title <- "Tx de positivité PCR (%)"
     db$title1 <- "Dépistage (PCR+TDR)/100000hbts"
     db$title2 <-  "-------------------------------"
     db$title3 <-  "-------------------------------"
     db$title4 <-  "-------------------------------"
     
     db$indicator2 <-  NA_integer_
     db$indicator3 <-  NA_integer_
     db$indicator4 <-  NA_integer_
   }
   
   db
   
 })
 
# Cumulative
 
 regions_reactives_fluidbox_cum <- reactive({
   
   if (input$select_level == "Niveau pays") {
     db <- cv_cases_national
     db$region <- db$Pays
     
   }
   
   if (input$select_level == "Régions") {
     db <- cv_cases_regions
     db$region <- db$region
   
   }
   
   if (input$outcome == "Cas confirmes (Total)") {
     
     db$outcome_cumulative <- db$cases
     db$outcome_cumulative1 <- db$guerisons
     db$outcome_cumulative2 <- db$fec_pos
     db$outcome_cumulative3 <- db$npersonnel
     db$title_today <- "Cumul des cas"
     db$title1 <- "Total guerisons"
     db$title_cum <- "Cumul des cas"
     db$title_cum1 <- "Femmes  enceintes infectees"
     db$title_cum2 <- "Personnels de sante infectes"
   }
   
   
   if (input$outcome == "Décès (total)") {
     
     db$cases <- db$cases
     db$outcome_cumulative <- db$deaths
     db$outcome_cumulative1 <- db$fec_dcd
     db$outcome_cumulative3 <- db$npersonnel
     outcome_cumulative2 <- db %>% 
                                filter(region %in% input$region_select) %>% 
                                  group_by(region,date) %>% 
                                  summarise(deaths = sum(deaths),
                                  cases = sum(cases)) %>% 
                                  filter(date == max(formatted_date1())) %>% 
                                  ungroup() %>% 
                                  mutate(deaths =  sum(deaths),
                                  cases =  sum(cases),
                                  letality_rate = round(deaths/cases*100,2)) %>% 
                                  select(letality_rate)
     
     db$outcome_cumulative2 <- unique(outcome_cumulative2$letality_rate)
     
     
     
     db$title_today <- "Cumul des Décès"
     db$title1 <- "Femmes  enceintes décédées"
     db$title_cum <- "Cumul des décès"
     db$title_cum1 <- "Tx de létalité global"
     
   }
   
   if(input$outcome == "Hospitalisation(s)") {
     
    # db %>% filter(region %in% input$region_select)
     
     db$outcome_cumulative <- NA_integer_
     db$outcome_cumulative1 <- NA_integer_
     db$outcome_cumulative2 <- NA_integer_
     db$title_today <- "----------------------------------"
     db$title1 <- "----------------------------------"
     db$title_cum <- "----------------------------------"
     db$title_cum1 <- "----------------------------------"
   }
   
   if(input$outcome == "Sous 02") {
     
     #db %>% filter(region %in% input$region_select)
     
     db$outcome_cumulative <- NA_integer_
     db$outcome_cumulative1 <- NA_integer_
     db$outcome_cumulative2 <- NA_integer_
     db$title_today <- "----------------------------------"
     db$title1 <- "----------------------------------"
     db$title_cum <- "----------------------------------"
     db$title_cum1 <- "----------------------------------"
     
   }
   
   if(input$outcome == "TDR positif") {
     
     #db %>% filter(region %in% input$region_select)
     db$outcome_cumulative <- NA_integer_
     db$outcome_cumulative1 <- NA_integer_
     db$outcome_cumulative2 <- NA_integer_
     db$title_today <- "----------------------------------"
     db$title1 <- "----------------------------------"
     db$title_cum <- "----------------------------------"
     db$title_cum1 <- "----------------------------------"
   }
   
   if(input$outcome == "PCR positif") {
     
    
     db$outcome_cumulative <- NA_integer_
     db$outcome_cumulative1 <- NA_integer_
     db$outcome_cumulative2 <- NA_integer_
     db$title_today <- "----------------------------------"
     db$title1 <- "----------------------------------"
     db$title_cum <- "----------------------------------"
     db$title_cum1 <- "----------------------------------"
   }
   
   db %>% filter(region %in% input$region_select)
   
 })
 
 regions_reactives_indicators_cum <- reactive({
   
  if (input$select_level == "Niveau pays") {
     db <- cv_cases_national
     db$region <- db$Pays}
   
   
   if (input$select_level == "Régions") {
     db <- cv_cases_regions
     db$region <- db$region}
   
   
   if (input$outcome == "Cas confirmes (Total)") {
     
     db$outcome1 <- db$npersonnel
     db$title1 <- "Personnel de santé Infecté"
     
   }
   
   if (input$outcome == "Décès (total)") {
     
     db$outcome1 <- db$npersodcd 
     db$title1 <- "Personnel de santé Décédé"
     
   }
   
   if(input$outcome == "Hospitalisation(s)") {
     db$outcome1 <- NA_integer_
     db$title1 <- "----------------------------------"
   }
   
   if(input$outcome == "Sous 02") {
     db$outcome1 <- NA_integer_
     db$title1 <- "----------------------------------"
   }
   
   if(input$outcome == "TDR positif") {
     db$outcome1 <- NA_integer_
     db$title1 <- "----------------------------------"
   }
   
   if(input$outcome == "PCR positif") {
     db$outcome1 <- NA_integer_
     db$title1 <- "----------------------------------"
   }
   
   db %>% filter(region %in% input$region_select)
   
 })


 # Fluidbox

 
 output$cases_this_day <- renderInfoBox({
   infoBox(tags$p(paste0(unique(regions_reactives_fluidbox()$title_today)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(sum(regions_reactives_fluidbox()$outcome[regions_reactives_fluidbox()$date == formatted_date1()], na.rm = TRUE))),style = "font-size: 201%;"),
           icon = shiny::icon("virus-covid"),
           width = 10,color = "green", fill = TRUE)})
 
 
 output$attack_rate1 <- renderInfoBox({
   infoBox(tags$p(paste0(unique(population_total()$title)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(unique(population_total()$new_outcome))),style = "font-size: 201%;"),
           icon = shiny::icon("hospital-user"),
           width = 10,color = "green", fill = TRUE)})
 


 output$attack_rate2 <- renderInfoBox({
   infoBox(tags$p(paste0(unique(population_total()$title1)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(unique(population_total()$indicator1))),style = "font-size: 201%;"),
           icon = shiny::icon("mask-face"),
           width = 10,color = "green", fill = TRUE)})


 output$attack_rate3 <- renderInfoBox({
   infoBox(tags$p(paste0(unique(population_total()$title2)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(unique(population_total()$indicator2))),style = "font-size: 201%;"),
           icon = shiny::icon("prescription-bottle"),
           width = 10,color = "green", fill = TRUE)})
 
 
 output$attack_rate4 <- renderInfoBox({
   infoBox(tags$p(paste0(unique(population_total()$title3)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(unique(population_total()$indicator3))),style = "font-size: 201%;"),
           icon = shiny::icon("pump-medical"),
           width = 10,color = "green", fill = TRUE)})
 
 output$attack_rate5 <- renderInfoBox({
   infoBox(tags$p(paste0(unique(population_total()$title4)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(unique(population_total()$indicator4))),style = "font-size: 201%;"),
           icon = shiny::icon("shield-virus"),
           width = 10,color = "green", fill = TRUE)})
 
## Cumulative 
 
 output$cases_cumulative <- renderInfoBox({
   infoBox(tags$p(paste0(unique(regions_reactives_fluidbox_cum()$title_today)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(sum(regions_reactives_fluidbox_cum()$outcome_cumulative[regions_reactives_fluidbox_cum()$date == formatted_date1()]))),style = "font-size: 201%;"),
           icon = shiny::icon("virus-covid"),
           width = 10,color = "green", fill = TRUE)})
 
 
 output$indicators_cumulative1 <- renderInfoBox({
   infoBox(tags$p(paste0(unique(regions_reactives_fluidbox_cum()$title1)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(sum(regions_reactives_fluidbox_cum()$outcome_cumulative1[regions_reactives_fluidbox_cum()$date == formatted_date1()]))),style = "font-size: 201%;"),
           icon = shiny::icon("hospital-user"),
           width = 10,color = "green", fill = TRUE)})
 
 
 output$indicators_cumulative2 <- renderInfoBox({
   infoBox(tags$p(paste0(unique(regions_reactives_fluidbox_cum()$title_cum1)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(sum(regions_reactives_fluidbox_cum()$outcome_cumulative2[regions_reactives_fluidbox_cum()$date == formatted_date1()]))),style = "font-size: 201%;"),
           icon = shiny::icon("pump-medical"),
           width = 10,color = "green", fill = TRUE)})
 
 output$indicators_cumulative3 <- renderInfoBox({
   infoBox(tags$p(paste0(unique(regions_reactives_indicators_cum()$title1)),style = "font-size: 75%;"),tags$p(paste0(prettyNum(sum(regions_reactives_indicators_cum()$outcome1[regions_reactives_indicators_cum()$date <= formatted_date1()]))),style = "font-size: 201%;"),
           icon = shiny::icon("shield-virus"),
           width = 10,color = "green", fill = TRUE)})
 

 
 

 
  
#############################################################################
# Vaccination  
#############################################################################
  

  observeEvent(input$select_level_vaccination, {
    
    
    if (input$select_level_vaccination == "Niveau pays") {
      
      updatePickerInput(session = session,
                        inputId = "region_select_vaccination",
                        choices = "Cameroun",
                        selected = "Cameroun")
      
    }
    
    if (input$select_level_vaccination == "Régions") {
      
      updatePickerInput( session = session,
                         inputId = "region_select_vaccination",
                         choices = as.character(unique(cv_vaccine$region)),
                         selected = as.character(unique(cv_vaccine$region)[1:5]))
    }
    
    
  },
  ignoreInit = TRUE)
  
 vaccine_reactive_db <- reactive({
   
   db <- tibble()
   
    if (input$select_level_vaccination == "Niveau pays") {
      
      db <- cv_vaccine_national
      db$region <- db$Pays
      db <-   db %>%  
               filter(date <= input$minimum_date_vaccination)
      
    }
   
    if (input$select_level_vaccination == "Régions") {
     db <- cv_vaccine_regional
     db <-   db %>% 
       filter(region %in% input$region_select_vaccination) %>% 
       filter(date <= input$minimum_date_vaccination)
   }
   
   
  return(db)
   
 })
 
output$adminregionBarPlot <- renderHighchart({
  
     data <- vaccine_reactive_db()
     
     data %>% 
       group_by(region,date) %>% 
       summarise(doses_admin = sum(dose_administrees)) %>% 
       hchart(type = "line",
              hcaes(x = date, y = doses_admin,group = region)) %>% 
       hc_title(text = "Doses administrées par Niveau") %>% 
       hc_xAxis(title = list(text = "Date")) %>% 
       hc_yAxis(title = list(text = "Total doses administrees"))
     
     
})


output$adminregionCumulative <- renderHighchart({
  
  data <- vaccine_reactive_db()
  
  data %>%
    mutate(vaccin = fct_recode(vaccin,"Sinopharm" = "SINOPHARM")) %>% 
    select(region, vaccin, date,dose_administrees) %>% 
    group_by(vaccin,date) %>% 
    summarize(doses_admin = sum(dose_administrees)) %>% 
    arrange(date,vaccin) %>% 
    mutate(cumulative_doses = cumsum(doses_admin)) %>% 
    hchart(
      type = "line",
      hcaes(x = date, y = cumulative_doses, group = vaccin)
    ) %>%
    hc_title(text = "Doses administrées par Niveau et par vaccin") %>%
    hc_xAxis(title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Total doses administrées"))
  
  
})
 
indicator <- reactive({
  data <- tibble()
  data <- vaccine_reactive_db()
  
 data <-  data %>%
    filter(region %in% input$region_select_vaccination) %>% 
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
      completement_vaccines = dose2_astra+dose2_sino+dose2_pfizer+dose1_janssen
    )
 
 return(data)
  
})

output$genderdataTable <- renderDataTable({
  
  data <- vaccine_reactive_db()
    
 data %>% 
    summarise(
      across(
        .cols = contains("homme") | contains("femme"),
        .fns = ~ sum(.x, na.rm = TRUE))) %>% 
    mutate(total_hommes = hommes_dose1+hommes_dose1_50+hommes_dose1_50_1+hommes_dose2+hommes_dose3,
           total_femmes = femme_dose1+femme_dose1_50+femme_dose1_50_1+femme_dose2+femme_dose3) %>% 
    select(date,total_hommes,total_femmes) %>% 
    summarise(across(c(total_hommes,total_femmes),~sum(.x,na.rm = TRUE))) %>%
    rename(Hommes = total_hommes,
           Femmes = total_femmes) %>% 
    datatable(caption = htmltools::tags$caption(style = 'caption-side: top;
          text-align: left;color: #81C784; font-size:80%;',"Doses administrees par sexe"),
          options = list(pageLength = 10, scrollX = TRUE))
           
  
})

output$genderpiechart <- renderHighchart({
  
   data <- tibble()
   data <- vaccine_reactive_db()
   
   data %>% 
     summarise(
       across(
         .cols = contains("homme") | contains("femme"),
         .fns = ~ sum(.x, na.rm = TRUE))) %>% 
     mutate(total_hommes = hommes_dose1+hommes_dose1_50+hommes_dose1_50_1+hommes_dose2+hommes_dose3,
            total_femmes = femme_dose1+femme_dose1_50+femme_dose1_50_1+femme_dose2+femme_dose3) %>% 
     select(date,total_hommes,total_femmes) %>% 
     summarise(across(c(total_hommes,total_femmes),~sum(.x,na.rm = TRUE))) %>%
     rename(Hommes = total_hommes,
            Femmes = total_femmes) %>% 
     pivot_longer(cols = c(Hommes,Femmes)) %>% 
     group_by(name) %>% 
     summarise(value = sum(value)) %>%
     hchart(type = "pie",hcaes(name,value)) %>% 
     hc_title(text = "Doses administrées Hommes vs Femmes") %>% 
     hc_tooltip(pointFormat = "<b>Valeur:</b> {point.y} <br>
                               <b>Pourcentages </b> {point.percentage:,.2f}%")
  
})


output$indicators_table <- renderDataTable({
  
  data <- vaccine_reactive_db()
  
  data_summary <-  data %>% 
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
      completement_vaccines = dose2_astra+dose2_sino+dose2_pfizer+dose1_janssen)
 
 colnames(data_summary) <- c(
   "Région", "Date", "Population cible", "Population totale",
   "Dose 1 Astra Zeneca", "Dose 1 Sinopharm", "Dose 1 Pfizer", "Dose 1 Janssen",
   "Dose 2 Astra Zeneca", "Dose 2 Sinopharm", "Dose 2 Pfizer",
   "Booster Astra Zeneca", "Booster Sinopharm", "Booster Pfizer", "Booster Janssen",
   "Total Dose 1", "Total Dose 2", "Total Booster", "Complètement vaccinés"
 )
   
   
   
   
    datatable(data_summary,
           caption = htmltools::tags$caption(style = 'caption-side: top;
          text-align: left;color: #81C784; font-size:150%;',"Doses administrees par niveau"),
              options = list(pageLength = 10, scrollX = TRUE))
  
})


coverage_plot <- reactive({
  
  data <- tibble()
  data <- indicator()
  
  population <- data %>% filter(region %in% input$region_select_vaccination) %>% select(region,pop_cible,pop_totale)
  population <- population %>%  distinct(region, pop_cible, pop_totale)
  
 data <-  data %>% 
    group_by(date = as.Date(floor_date(date,'month')),region) %>% 
    summarise(#across(c(overall_dose1,completement_vaccines),.fns = ~max(.x,na.rm = TRUE)),
      across(c(overall_dose1,completement_vaccines),.fns = ~sum(.x,na.rm = TRUE)),
      population_cible = sum(population$pop_cible, na.rm = TRUE)) %>%
    pivot_longer(cols = -c(date,region,population_cible)) %>%  
    group_by(date, name) %>% 
    summarise(total_doses = sum(value, na.rm = TRUE),
              pop = sum(population$pop_cible,na.rm = TRUE)) %>% 
    group_by(name) %>% 
    arrange(date) %>% 
    mutate(cumul = cumsum(total_doses),
           prop = round(cumul/pop*100,2))
 
data$name <-  data$name %>%
   fct_recode(
     "% Completement vaccines" = "completement_vaccines",
     "% Dose 1" = "overall_dose1"
   )
 
 return(data)
  
})

output$national_coverage_graph <- renderHighchart({
  
   data <- coverage_plot()
  
   data %>%
     hchart("line",hcaes(x = date,y = prop,group = name)) %>% 
              hc_title(text = "Couverture vaccinale") %>% 
              hc_xAxis(title = list(text = "Date")) %>%
              hc_yAxis(title = list(text = "Couverture vaccinale"))
   
})


output$national_coverage_priorgraph <- renderHighchart({
  
  data <- vaccine_reactive_db()
  
  population <- data %>% 
    ungroup() %>% 
    filter(region %in% input$region_select_vaccination) %>% 
    select(region,population_cible_personnel_sante,population_cible_personnes_ages,
           population_cible_personnes_commorbidite)
  
  population <- population %>%  distinct(region,population_cible_personnel_sante,population_cible_personnes_ages,population_cible_personnes_commorbidite)
  
  data <-  data %>% 
    filter(region %in% input$region_select_vaccination) %>%
    group_by(region,date) %>% 
    summarise(population_cible_personnel_sante = sum(unique(population_cible_personnel_sante), na.rm = TRUE),
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
              complet_commorbidite = dose2_astra_commobidite+dose2_sino_commobidite+dose2_pfizer_commobidite+dose2_janssen_commobidite+dose1_janssen_commobidite) %>% 
    select(
      region, date,complet_health_worker, complet_aged, complet_commorbidite
    ) %>% 
    group_by(date = as.Date(floor_date(date, 'month')), region) %>% 
    summarise(
      complet_health_worker = sum(complet_health_worker, na.rm = TRUE),
      complet_aged = sum(complet_aged, na.rm = TRUE),
      complet_commorbidite = sum(complet_commorbidite, na.rm = TRUE),
      pc_healtworker = sum(population$population_cible_personnel_sante, na.rm = TRUE),
      pc_aged = sum(population$population_cible_personnes_ages, na.rm = TRUE),
      pc_commor = sum(population$population_cible_personnes_commorbidite, na.rm = TRUE)) %>% 
    pivot_longer(cols = -c(date, region, pc_healtworker, pc_aged, pc_commor)) %>%  
    group_by(date, name) %>% 
    summarise(
      total_doses = sum(value, na.rm = TRUE),
      pc_healtworker = sum(unique(pc_healtworker), na.rm = TRUE),
      pc_aged = sum(unique(pc_aged), na.rm = TRUE),
      pc_commor = sum(unique(pc_commor), na.rm = TRUE)) %>% 
    group_by(name) %>% 
    arrange(date) %>% 
    mutate(cumul = cumsum(total_doses))
 
 data <- data %>% 
   group_by(name) %>% 
   mutate(
     coverage =round(cumsum(total_doses) / ifelse(name == "complet_health_worker", pc_healtworker, 
                                                  ifelse(name == "complet_aged", pc_aged, pc_commor)) * 100,2)) %>%
   ungroup()
 
 data$name <- data$name %>%
   fct_recode(
     "Personnes Agees" = "complet_aged",
     "Personnes avec commorbidites" = "complet_commorbidite",
     "Personnels de sante" = "complet_health_worker"
   )
 
 data %>% 
   hchart("line", hcaes(x = date, y = coverage, group = name)) %>% 
   hc_title(text = "Couverture vaccinale groupes prioritaires") %>% 
   hc_xAxis(title = list(text = "Date")) %>%
   hc_yAxis(title = list(text = "Couverture vaccinale(%)"))
  
})



## Fluidbox

output$fully_vaccinated <- renderInfoBox({
  infoBox(title = tags$p(paste0("# Complètement vaccinés"),style = "font-size: 75%;"),tags$p(paste0(prettyNum(sum(indicator()$completement_vaccines))),style = "font-size: 201%;"),
          icon = shiny::icon("syringe"),
          width = 10,color = "green", fill = TRUE)})


output$prop_one_dose <- renderInfoBox({
  infoBox(title = tags$p(paste0("% CV Dose 1"),style = "font-size: 75%;"),tags$p(paste0(prettyNum(round(sum(indicator()$overall_dose1)/sum(unique(indicator()$pop_cible))*100,1)),"%"),style = "font-size: 201%;"),
          icon = shiny::icon("syringe"),
          width = 10,color = "green", fill = TRUE)})



output$fully_vaccinated_prop <- renderInfoBox({
  infoBox(title = tags$p(paste0("% Pop cible complètement vaccinée"),style = "font-size: 75%;"),tags$p(paste0(prettyNum(round(sum(indicator()$completement_vaccines)/sum(unique(indicator()$pop_cible))*100,1)),"%"),style = "font-size: 201%;"),
          icon = shiny::icon("syringe"),
          width = 10,color = "green", fill = TRUE)})


output$fully_vaccinated_prop_total <- renderInfoBox({
  infoBox(title = tags$p(paste0("% Pop totale complètement vaccinée"),style = "font-size: 75%;"),tags$p(paste0(prettyNum(round(sum(indicator()$completement_vaccines)/sum(unique(indicator()$pop_totale))*100,1)),"%"),style = "font-size: 201%;"),
          icon = shiny::icon("syringe"),
          width = 10,color = "green", fill = TRUE)})


output$two_vaccine_doses <- renderInfoBox({
  infoBox(title = tags$p(paste0("## Deux doses de vaccins"),style = "font-size: 75%;"),tags$p(paste0(prettyNum(sum(indicator()$overall_dose2))),style = "font-size: 201%;"),
          icon = shiny::icon("syringe"),
          width = 10,color = "green", fill = TRUE)})

output$booster_received <- renderInfoBox({
  infoBox(title = tags$p(paste0("## Doses de rappel"),style = "font-size: 75%;"),tags$p(paste0(prettyNum(sum(indicator()$overall_booster))),style = "font-size: 201%;"),
          icon = shiny::icon("syringe"),
          width = 10,color = "green", fill = TRUE)})


################################################################################
#Produire le rapport de vaccination
################################################################################

rv <- reactiveValues()



# File for analysis

immunization_data <- reactive({
  
   selected_period <- as.character(input$semaine_epi)
   week_select <- str_remove(as.character(selected_period),"W")
   week_select <- str_remove(week_select," ")
   week_select <- as.numeric(week_select)
   
   
  db <- tibble()
   
  db <-  data_clean_immunization 
  
  db <- db %>% 
         filter(week <= week_select)
  
  return(db)
  
})


cases_data <- reactive({
  
  selected_period <- as.character(input$semaine_epi)
  week_select <- str_remove(as.character(selected_period),"W")
  week_select <- str_remove(week_select," ")
  week_select <- as.numeric(week_select)
  
  db <- tibble()
  
  db <- cv_cases
  
  db <- db %>% 
    filter(week <= week_select)
  
  return(db)
  
})

resurgence_data <- reactive({
  
  selected_period <- as.character(input$semaine_epi)
  week_select <- str_remove(as.character(selected_period),"W")
  week_select <- str_remove(week_select," ")
  week_select <- as.numeric(week_select)
  
  db <- tibble()
  
  db <- db_sitrep
  
  db <- db %>% 
    filter(week == week_select)
  
  return(db)
  
})

shapefile_region <- reactive({
  
  db <- region_shapefile
  return(db)
  
})


# Rapport PowerPoint vaccination COVID-19

 observe({
   
   rv$data <- immunization_data()
   rv$db1 <- cases_data()
   rv$db2 <- resurgence_data()
   rv$region_shapefile <- shapefile_region()
   
   
   if (nrow(rv$data) == 0 | nrow(rv$db1) == 0) {
     
     showModal(
       modalDialog(
         title = "Aucune donnée disponible",
         "Aucune donnée n'est disponible pour la semaine épidemiologique choisie.",
         easyClose = TRUE,
         footer = NULL
       )
     )
   } else {
   
     #1
     
   rv$tab1 <- rv$data %>% 
     select(region, date,dose1_sino,dose1_astra,dose1_janssen,dose1_pfizer,
            dose2_astra,dose2_sino,dose2_pfizer,dose3_astra = booster_astra,dose3_sino = booster_sino,dose3_pfizer = booster_pfizer,dose3_janssen = booster_janssen,total_administered) %>% 
     group_by(region) %>% 
     summarise(across(where(is.numeric) ,~sum(.x,na.rm = TRUE))) %>% 
     pivot_longer(cols = dose1_sino:dose3_janssen,
                  names_to = c("dose","vaccin"),
                  names_pattern = "(.*)_(.*)") %>% 
     pivot_wider(names_from = dose, values_from = value) %>% 
     group_by(vaccin) %>% 
     summarise(across(.cols = starts_with("dose"),.fns = ~sum(.x, na.rm = TRUE))) %>% 
     mutate(vaccin =  fct_recode(vaccin,
                                 "ASTRA ZENECA" = "astra",
                                 "JANSSEN" = "janssen",
                                 "PFIZER" = "pfizer",
                                 "SINOPHARM" = "sino"))
   
   # 2 
   rv$doses100hbts <- rv$data %>% 
     select(region, date,total_administered,pop_cible) %>% 
     group_by(region) %>% 
     summarise(popul_cible = sum(unique(pop_cible),na.rm = TRUE),
               across(total_administered,~sum(.x,na.rm = TRUE))) %>% 
     mutate(administered_100hbts = round((total_administered/popul_cible)*100,0))
   
   #3
   
   rv$population_cible <- sum(unique(rv$data$pop_cible), na.rm = TRUE)
   rv$population_totale <- sum(unique(rv$data$pop_totale), na.rm = TRUE)
   rv$date_max <- max(as.Date(floor_date(rv$data$date,'month')))
   rv$date_min <- rv$date_max - months(12)
   
   rv$administered_cible_totale <- rv$data %>% 
     group_by(date = as.Date(floor_date(date,'month'))) %>%
     summarise(completement_vaccines =  sum(completement_vaccines, na.rm = TRUE),
               overall_dose1 = sum(overall_dose1, na.rm = TRUE),
               pop_cible =  rv$population_cible) %>% 
     arrange(date) %>% 
     mutate(cumul_complete = cumsum(completement_vaccines),
            cumul_dose1_overall = cumsum(overall_dose1),
            cv_complete = round(cumul_complete/pop_cible*100,2),
            cv_dose1 = round(cumul_dose1_overall/pop_cible*100,2)) %>% 
     select(date,cv_complete,cv_dose1) %>%
     #filter(date >= max(rv$data$date) - months(12)) %>% 
     filter(date >= rv$date_min & date <= rv$date_max) %>%
     pivot_longer(cols =  c(cv_complete,cv_dose1))
   
   #4
   
   rv$administered_cv_totale <- rv$data %>% 
     group_by(date = as.Date(floor_date(date,'month'))) %>%
     summarise(completement_vaccines =  sum(completement_vaccines, na.rm = TRUE),
               overall_dose1 = sum(overall_dose1, na.rm = TRUE),
               pop_cible =  rv$population_totale) %>% 
     arrange(date) %>% 
     mutate(cumul_complete = cumsum(completement_vaccines),
            cumul_dose1_overall = cumsum(overall_dose1),
            cv_complete = round(cumul_complete/pop_cible*100,2),
            cv_dose1 = round(cumul_dose1_overall/pop_cible*100,2)) %>% 
     select(date,cv_complete,cv_dose1) %>% 
     filter(date >= rv$date_min & date <= rv$date_max) %>%  
     #filter(date <= max(rv$data$date) & date >= max(rv$data$date)-months(12)) %>%
     pivot_longer(cols =  c(cv_complete,cv_dose1))
   
   #5
   
   rv$administered_doses_5weeks <-   rv$data %>% 
     filter(epiweek <= max(rv$data$epiweek) & epiweek >= max(rv$data$epiweek)-4) %>% 
     group_by(epiweek) %>% 
     summarise(total_administered = sum(total_administered, na.rm = TRUE))
   
   # 6 
   
   rv$dose_admin_mois <-   rv$data %>% 
     filter(date <= max(data_clean_immunization$date) & date >= max(data_clean_immunization$date)-months(24)) %>% 
     group_by(date = as.Date(floor_date(date,unit = "month"))) %>% 
     summarise(doses_admin = sum(total_administered, na.rm = TRUE))
   
   # 7
   
   rv$admin_evolution <- rv$data %>% 
     group_by(epiweek) %>% 
     summarise(doses_admin = sum(total_administered))
   
   # 8 
   
   rv$cv_popcible_poptotal <- rv$data %>% 
     group_by(region) %>% 
     summarise(
       pop_cible = sum(unique(pop_cible)),
       pop_totale = sum(unique(pop_totale)),
       completement_vaccines = sum(completement_vaccines, na.rm = TRUE),
       cv_cible = round(completement_vaccines/pop_cible*100,2),
       cv_totale = round(completement_vaccines/pop_totale*100,2)) %>% 
     select(region,cv_cible,cv_totale) %>% 
     pivot_longer(cols = starts_with("cv"))
   
   # 9
   
   rv$cv_regional <- rv$data %>% 
     group_by(region) %>% 
     summarise(cible = sum(unique(pop_cible), na.rm = TRUE),
               premiere_dose = sum(overall_dose1, na.rm = TRUE),
               deuxieme_dose = sum(overall_dose2, na.rm = TRUE),
               booster = sum(overall_booster,na.rm = TRUE),
               completement_vaccines = sum(completement_vaccines, na.rm = TRUE)) %>% 
     adorn_totals("row") %>% 
     mutate(cv_dose_1 = round(premiere_dose/cible*100,2),
            cv_complete = round(completement_vaccines/cible*100,2))
   
   #10
   
   rv$fully_region_series <- rv$data %>% 
     group_by(region) %>% 
     summarise(pop_cible =  sum(unique(pop_totale), na.rm = TRUE),
               completement_vaccines = sum(completement_vaccines,na.rm = TRUE),
               overall_booster = sum(completement_vaccines,na.rm = TRUE),
               dose1_janssen = sum(dose1_janssen, na.rm = TRUE),
               complete_plus_booster = completement_vaccines+overall_booster,
               overall_dose1 = sum(overall_dose1,na.rm = TRUE),
               overall_dose_first = overall_dose1-dose1_janssen) %>% 
     mutate(complete_primary1 = round(complete_plus_booster/pop_cible*100,),
            complete_primary2 = round(completement_vaccines/pop_cible*100,1)) %>% 
     select(region,complete_primary1, complete_primary2) %>% 
     pivot_longer(-region)
   
   #11
   
   rv$prioritaire_prioritaire_cv <- rv$data %>% 
     group_by(region) %>% 
     summarize(
       population_cible_personnel_sante = sum(unique(population_cible_personnel_sante), na.rm = TRUE),
       population_cible_personnes_ages  = sum(unique(population_cible_personnes_ages), na.rm = TRUE),
       population_cible_personnes_commorbidite = sum(unique(population_cible_personnes_commorbidite), na.rm = TRUE),
       complet_health_worker = sum(complet_health_worker, na.rm = TRUE),
       complet_aged = sum(complet_aged, na.rm = TRUE),
       complet_commorbidite = sum(complet_commorbidite, na.rm = TRUE)) %>%
     adorn_totals(where = "row") %>% 
     mutate(cv_health_worker = round(complet_health_worker/population_cible_personnel_sante*100,2),
            cv_personnes_ages = round(complet_aged/population_cible_personnes_ages*100,2),
            cv_commorbidites =  round(complet_commorbidite/population_cible_personnes_commorbidite*100,2)) %>% 
     select(region,population_cible_personnel_sante,complet_health_worker,cv_health_worker,population_cible_personnes_ages,complet_aged,cv_personnes_ages,
            population_cible_personnes_commorbidite,complet_commorbidite,cv_commorbidites)
   
   #12
   
   rv$cv_hcw_poptot <-  rv$data %>% 
     group_by(region) %>% 
     summarize(
       population_cible_personnel_sante = sum(unique(population_cible_personnel_sante), na.rm = TRUE),
       complet_health_worker = sum(complet_health_worker, na.rm = TRUE),
       pop_totale = sum(unique(pop_totale), na.rm = TRUE),
       completement_vaccines = sum(completement_vaccines, na.rm = TRUE)
     ) %>% 
     mutate(cv_hcw = complet_health_worker/population_cible_personnel_sante*100,
            cv_pop_tot = completement_vaccines/pop_totale*100) %>% 
     select(region,starts_with("cv"))
   
   # 13
   
   rv$graph_gender <-   rv$data %>% 
     group_by(region) %>% 
     summarise(
       fully_vaccinated_femme = sum(fully_vaccinated_femme, na.rm = TRUE),
       fully_vaccinated_homme = sum(fully_vaccinated_homme, na.rm = TRUE),
       total_doses_admin_femme = sum(total_doses_admin_femme, na.rm = TRUE),
       total_doses_admin_homme = sum(total_doses_admin_homme, na.rm = TRUE),
       pop_cible = sum(unique(pop_cible), na.rm = TRUE),
       hommes = round(total_doses_admin_homme/pop_cible*100,2),
       femmes = round(total_doses_admin_femme/pop_cible*100,2)
     )
   
   #14
   
   rv$tab_vaccine_type <-  rv$data %>% 
     group_by(region) %>% 
     summarize(
       admin_sino = sum(admin_sinopharm, na.rm = TRUE),
       admin_astra = sum(admin_astra, na.rm = TRUE),
       admin_pfizer = sum(admin_pfizer, na.rm = TRUE),
       admin_janssen = sum(admin_janssen, na.rm = TRUE))
   #15
   
   rv$gap_admin_received <- rv$data %>% 
     filter(date <= max(data_clean_immunization$date) & date >= max(data_clean_immunization$date)-months(24)) %>%
     group_by(date = as.Date(floor_date(date, 'month'))) %>% 
     summarize(doses_received = sum(doses_received, na.rm = TRUE),
               doses_administered = sum(total_administered, na.rm = TRUE)) %>% 
     mutate(cumul_doses_received = cumsum(doses_received),
            cumul_doses_admin = cumsum(doses_administered),
            prop_admin = cumul_doses_admin/cumul_doses_received*100) %>%
     select(-starts_with("doses"))%>% 
     pivot_longer(cols = starts_with("cumul"))
   
   ############################################################################
   # Calcul partie maladie
   ############################################################################
   
   rv$reportingcovid <- max(as.Date(rv$db1$date))
   
   # 1
   rv$epicurve <- rv$db1
   
   rv$reportcases <- rv$db1 %>% 
     filter(date == max(date)) %>% 
     group_by(date) %>% summarise(tot_cases = sum(cases)) %>% select(tot_cases)
   
   rv$reportdeaths <- rv$db1 %>% 
     filter(date == max(date)) %>% 
     group_by(date) %>% summarise(tot_deaths = sum(deaths)) %>% select(tot_deaths)
   
    # 2
   
   rv$db_pos <-  rv$db1 %>% 
     select(date,pcr_realise,tdr_realise,pcr_positif,tdr_positif) %>%  
     group_by(date) %>% 
     summarise(pcr_r = sum(pcr_realise,na.rm = TRUE),
               tdr_r = sum(tdr_realise, na.rm = TRUE),
               pcr_p = sum(pcr_positif,na.rm = TRUE),
               tdr_p = sum(tdr_positif,na.rm = TRUE),
               total_realise = pcr_r + tdr_r,
               total_positif = pcr_p + tdr_p) %>% 
     mutate(tx_positivite = total_positif/total_realise) %>% 
     select(date,tdr_r,pcr_r) %>% 
     pivot_longer(cols = -date)
   
   
   # 3 
   
   rv$db_positivite <-  rv$db1 %>% 
     select(date,pcr_realise,tdr_realise,pcr_positif,tdr_positif) %>%  
     group_by(date) %>% 
     summarise(pcr_r = sum(pcr_realise,na.rm = TRUE),
               tdr_r = sum(tdr_realise, na.rm = TRUE),
               pcr_p = sum(pcr_positif,na.rm = TRUE),
               tdr_p = sum(tdr_positif,na.rm = TRUE),
               total_realise = pcr_r + tdr_r,
               total_positif = pcr_p + tdr_p) %>% 
     mutate(tx_positivite = total_positif/total_realise)
   
 # 7
   
   rv$indicator_table <- rv$db1 %>% 
     select(region,date,tdr_realise,pcr_realise,new_cases,npersonnel,
            cas_actifs_rev,hospitalises,cas_sous_o2_15,new_deaths,dose_administrees) %>% 
     group_by(date) %>% 
     summarise(across(where(is.numeric),~sum(.x,na.rm = TRUE))) %>% 
     mutate(tx_letalite = round(new_deaths/new_cases*100,2),
            tx_severity = round(cas_sous_o2_15/new_cases*100,2),
            test_realises = tdr_realise+pcr_realise) %>% 
     select(-c(tdr_realise,pcr_realise)) %>% 
     tail(10) %>% 
     pivot_longer(cols = where(is.numeric)) %>% 
     pivot_wider(names_from = date) %>% 
     mutate(name = fct_recode(name,
                              "Cas Actifs" = "cas_actifs_rev",
                              "Cas sous O2" = "cas_sous_o2_15",
                              "Vaccination" = "dose_administrees",
                              "Hospitalisations" = "hospitalises",
                              "Nouveaux cas" = "new_cases",
                              "Décès" = "new_deaths",
                              "PS infectés" = "npersonnel",
                              "Testing" = "test_realises",
                              "Taux de létalité" = "tx_letalite",
                              "Taux de sévérité" = "tx_severity")) %>% 
            rename(`Intitulés` =  name)
  
   
   # 8 
   
   rv$indicator_region <- rv$db1 %>%
     select(date, region, new_cases, cases, gueris, guerisons, nfec, fec_pos, nfec_dcd, fec_dcd, npersonnel, npersodcd, new_deaths, deaths) %>%
     filter(date == max(date)) %>%
     left_join(
       rv$db1 %>%
         select(date, region, npersonnel, npersodcd) %>%
         group_by(region) %>%
         summarise(npersonnel_cum = sum(npersonnel), npersodcd_cum = sum(npersodcd)),
       by = "region"
     ) %>%
     select(
       c(region, new_cases, cases, gueris, guerisons, nfec, fec_pos, nfec_dcd, fec_dcd, npersonnel, npersonnel_cum, npersodcd, npersodcd_cum, new_deaths, deaths)
     ) %>%
     adorn_totals(where = "row", name = "Cameroun")
   
   }
  
    
 })
 
 
 
 
 
 
 observe({
   
   req(rv$tab1,rv$doses100hbts,rv$administered_cible_totale,rv$cv_popcible_poptotal,rv$administered_cv_totale,
       rv$administered_doses_5weeks,rv$dose_admin_mois,rv$admin_evolution,rv$cv_regional,rv$fully_region_series,
       rv$cv_hcw_poptot,rv$graph_gender,rv$gap_admin_received,rv$reportingcovid,rv$epicurve,rv$reportcases,rv$reportdeaths,rv$db_pos,
       rv$db_positivite,rv$db2,rv$indicator_table,rv$indicator_region,rv$region_shapefile)
   
   comma <- function(x){
     x <-  paste(x,"%")
     return(x)
   }
   
   epiweek_select <- as.character(max(rv$data$epiweek, na.rm = TRUE))
   week_select <- as.numeric(substr(epiweek_select, nchar(epiweek_select) - 1, nchar(epiweek_select)))
   year_select <- as.numeric(substr(epiweek_select, 1, 4))
   
   
   
   p1 <- ggplot(rv$doses100hbts,
                mapping = aes(x =  region, y = administered_100hbts,label = paste0(prettyNum(administered_100hbts))))+
     geom_bar(stat = "identity",fill = ifelse(rv$doses100hbts$administered_100hbts<10,"brown","darkgreen"))+
     geom_text(size = 3,fontface = "bold",colour = "black", hjust =0.5,vjust = -0.2)+
     geom_hline(yintercept = 5, linetype = "dashed",col = "red",size = 0.7)+
     geom_hline(yintercept = 10, linetype = "dashed",col =  "red",size = 0.7)+
     geom_hline(yintercept = 20, linetype = "dashed",col =  "red",size = 0.7)+
     scale_x_discrete(limits = rv$doses100hbts$region[order(rv$doses100hbts$administered_100hbts,decreasing =  FALSE)],
                      expand = c(0,0))+
     theme_classic()+
     theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = .5,size = 15))+
     theme(axis.title.x = element_text(colour = "black",size = 12,face = "bold"))+
     theme(axis.title.y = element_text(colour = "black",size = 12,face = "bold"))+
     theme(legend.title = element_text(colour = "black",size = 12),
           legend.text = element_text(colour = "black", size = 12))+
     labs(title = "Total de doses administrées pour 100 habitants",
          x ="Région",
          y = "doses administrées pour 100hbts")
   
   
   p2 <-   ggplot(data = rv$administered_cible_totale,
                  aes(x=date,y = value,fill = name, label = paste0(value,"%")))+
     geom_bar(position = position_dodge(),stat = "identity",alpha = 0.8)+
     scale_fill_manual( values = c("#FF8A65","#355C7D") ,
                        labels = c("% Complètement vaccinés","% Vaccinés dose 1"))+
     scale_x_date(date_breaks = '1 month', 
                  date_minor_breaks = '1 day', 
                  date_labels = '%b %Y',
                  expand = c(0,0))+
     scale_y_continuous(labels = comma, expand = c(0,0))+
     geom_text(group = rv$administered_cible_totale$name, position = position_dodge(width = 28),vjust = -0.3,size = 2.5)+                ylim(0,100)+
     labs( fill =  "",
           y = "% Personnes vaccinées")+
     theme(legend.position = "bottom",
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black"),
           plot.title = element_text(hjust = 0.5),
           axis.text = element_text(size = 12),
           axis.title = element_text(size = 12, face = "bold"),
           axis.text.x = element_text(angle = 0,vjust = .5,hjust = .5,size = 10),
           axis.title.y = element_text(color = "black", size = 12, face = "bold"))
   
   
   p3 <-   ggplot(data = rv$administered_cv_totale,
                  aes(x=date,y = value,fill = name, label = paste0(value,"%")))+
     geom_bar(position = position_dodge(),stat = "identity",alpha = 0.8)+
     scale_fill_manual( values = c("#FF8A65","#355C7D") ,
                        labels = c("% Complètement vaccinés","% Vaccinés dose 1"))+
     scale_x_date(date_breaks = '1 month', 
                  date_minor_breaks = '1 day', 
                  date_labels = '%b %Y',
                  expand = c(0,0))+
     scale_y_continuous(labels = comma, expand = c(0,0))+
     geom_text(group = rv$administered_cv_totale$name, position = position_dodge(width = 28),vjust = -0.3, size = 2.5)+
     ylim(0,100)+
     labs( fill = "",
           y = "% Personnes vaccinées")+
     theme(legend.position = "bottom",
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black"),
           plot.title = element_text(hjust = 0.5),
           axis.text = element_text(size = 12),
           axis.title = element_text(size = 12, face = "bold"),
           axis.text.x = element_text(angle = 0,vjust = .5,hjust = .5,size = 10),
           axis.title.y = element_text(color = "black", size = 12, face = "bold"))
   
   p4 <-   ggplot(rv$administered_doses_5weeks,
                  mapping = aes(x = epiweek,y = total_administered,label = prettyNum(paste(total_administered))))+
     geom_line(color =  "blue", size = 1.1)+
     geom_text(size = 3,fontface = "bold",colour = "black", hjust =0.8,vjust = -0.5)+
     theme_classic()+
     labs(x = "",y = "")
   
   
   p5 <-   ggplot(data = rv$dose_admin_mois,
                  aes(x = date, y = doses_admin, label = paste(cmprss(doses_admin)))) +
     geom_bar(stat = "identity",fill = "darkcyan")+
     geom_text(size = 2,fontface = "bold",colour = "black", hjust =0.5,vjust = -0.5)+
     scale_y_continuous(labels = cmprss)+
     scale_x_date(date_breaks = '1 month',
                  date_minor_breaks = '1 day')+
     theme(legend.position = "bottom",
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black"),
           plot.title = element_text(hjust = 0.5),
           axis.text = element_text(size = 12),
           axis.title = element_text(size = 12, face = "bold"),
           axis.text.x = element_text(angle = 90,vjust = .5,hjust = .5,size = 10),
           axis.title.y = element_text(color = "black", size = 12, face = "bold"))+
     labs(x = "date",
          y = "# doses administrées (Milliers)")+
     labs(x = "Date",
          y = "Nombre de doses administrées (en milliers)") 
   
   p6 <-    ggplot(rv$admin_evolution,
                   mapping = aes(x = epiweek, y =  doses_admin))+
     geom_density(stat = "identity", fill = "blue", color = "blue")+
     scale_y_continuous(labels = cmprss)+
     theme_classic()+
     theme(axis.title.x = element_blank())+
     labs(y = "Doses administrées (milliers")
   
   rv$cv_popcible_poptotal <- rv$cv_popcible_poptotal[order(rv$cv_popcible_poptotal$value, decreasing = TRUE), ]
   rv$cv_popcible_poptotal$region <- fct_reorder(rv$cv_popcible_poptotal$region, rv$cv_popcible_poptotal$value, .desc = TRUE)
   
   p7 <- ggplot(data = rv$cv_popcible_poptotal,
                aes(x = region, y = value, fill = name, label = paste0(value)))+
     geom_bar(stat = "identity", position = position_dodge(1),alpha = 0.8)+
     geom_text(position = position_dodge(width = 0.8), aes(group = name), hjust = 0.5, vjust = -0.5, size = 3) +
     scale_fill_manual(values = c("blue4","#27A187"),
                       labels = c("cv_cible" = "Population cible",
                                  "cv_totale" = "Population générale"))+
     scale_x_discrete()+
     ylim(0,60)+
     labs(fill =  "Type de population",
          x = "Région",
          y = "Pourcentage")+
     theme(legend.position = "bottom",
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black"),
           plot.title = element_text(hjust = 0.5),
           axis.text = element_text(size = 12),
           axis.title = element_text(size = 12, face = "bold"),
           axis.text.x = element_text(angle = 0,vjust = .5,hjust = .5,size = 10),
           axis.title.y = element_text(color = "black", size = 12, face = "bold"))
   
   rv$fully_region_series <- rv$fully_region_series[order(rv$fully_region_series$value, decreasing = TRUE), ]
   rv$fully_region_series$region <- fct_reorder(rv$fully_region_series$region, rv$fully_region_series$value, .desc = TRUE)
   
  p8 <-  ggplot(rv$fully_region_series, aes(x = region, y = value, fill = name, label = paste(value))) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = c("blue4","#27A187"),
                      labels = c("complete_primary1" = "Série complète de vaccination + booster",
                                 "complete_primary2" = "Série complète de vaccination sans booster"))+
    scale_x_discrete() +
    geom_text(position = position_stack(reverse = TRUE,vjust = 0.7),size = 3, color = "white")+
    ylim(0,60)+
    theme_minimal() +
    labs(x = "Région", y = "Valeur")+
    theme(legend.position = "top",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(angle = 90,vjust = .5,hjust = .5,size = 10),
          axis.title.y = element_text(color = "black", size = 12, face = "bold"))+
    labs(fill = "",
         y = "% population totale")
  
  
  rv$trsup <- data.frame(x=c(0,0,100),y=c(0,100,100))
  rv$trinf <- data.frame(x=c(0,100,100),y=c(0,0,100))
  
  p9 <-  ggplot(data=rv$cv_hcw_poptot, aes(x=cv_pop_tot, y=cv_hcw))+
    geom_polygon(aes(x=x, y=y), data=rv$trsup, fill="cadetblue",alpha = 0.5) +
    geom_polygon(aes(x=x, y=y), data=rv$trinf, fill="#D3BBFB",alpha = 0.5)+
    geom_point(color = "red") +
    geom_text_repel(aes(label = region), hjust = .5, vjust = -0.6, color = "blue3",size = 2.5) +
    scale_x_continuous(labels = scales::percent_format(scale = 1))+
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    geom_abline(intercept = 0, slope = 1,alpha = 0)+
    geom_vline(xintercept = 25,linetype = "dashed",color =  "black")+
    geom_hline(yintercept = 25, linetype = "dashed", color = "black")+
    theme_classic()+
    labs(x = "% des personnes avec une série primaire complete dans la population totale",
         y = "% des personnels de santé avec une série primaire complete")
  
  
  p10 <- rv$graph_gender %>% 
         select(region,hommes,femmes) %>% 
         pivot_longer(cols = c(hommes,femmes)) %>% 
         mutate(region = as.factor(region),
           name =as.factor(name),
           name = fct_relevel(name,"hommes","femmes")) %>% 
    apyramid::age_pyramid(region,split_by = name, count = value, show_midpoint = FALSE) +
    scale_fill_manual(values = c("#61D6FF","#FFCCFF"))+
    labs(fill = "",
         x = "",
         y = "")+
    theme_classic()+
    theme(legend.position = "bottom")
  
  rv$cammenberg_gender <- rv$graph_gender %>% 
    summarise(across(.cols = c(fully_vaccinated_femme,fully_vaccinated_homme),.fns= ~sum(.x, na.rm = TRUE))) %>% 
    mutate(total = fully_vaccinated_femme+fully_vaccinated_homme,
           homme = round(fully_vaccinated_homme/total*100,2),
           femme = round(fully_vaccinated_femme/total*100,2)) %>% 
    select(homme,femme) %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(name = fct_relevel(name,"homme"))
  
  
 
  
 p11 <-  ggplot(rv$cammenberg_gender, aes(x="", y=value, fill=name, label = paste0(value,"%"))) +
    geom_bar(stat="identity", width=1) +
    geom_text(position = position_stack(vjust = 0.5))+
    scale_fill_manual(values = c("#61D6FF","#FFCCFF"))+
    coord_polar("y", start=0)+
    theme_void()+
    theme(legend.position = "bottom")+
    labs(fill = "")
 
 p12 <- ggplot(data = rv$gap_admin_received,
               mapping = aes(x = date, y =  value, fill = name))+
   geom_col()+
   scale_fill_manual(values = c("#5FC9D7","#F06D6A"),
                     labels = c(cumul_doses_admin = "Doses administrées",
                                cumul_doses_received = "Doses disponibles"))+
   scale_y_continuous(labels = cmprss)+
   scale_x_date(date_breaks = '1 month',
                date_minor_breaks = '1 day')+
   theme(legend.position = "bottom",
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black"),
         plot.title = element_text(hjust = 0.5),
         axis.text = element_text(size = 12),
         axis.title = element_text(size = 12, face = "bold"),
         axis.text.x = element_text(angle = 90,vjust = .5,hjust = .5,size = 10),
         axis.title.y = element_text(color = "black", size = 12, face = "bold"))+
   labs(fill = "",
        x = "date",
        y = "Doses en Milliers")
 
 # Plot rapport maladie
 
 g1 <-  ggplot(data = rv$epicurve,
                    aes(x = date, y = new_cases))+
   geom_histogram(stat = "identity",position = position_nudge(x = 1), width = 6,fill = "#00B050")+
   scale_x_date(
     labels = scales::date_format("%b %d, \n %Y"),
     date_breaks = "1 year")+
   scale_fill_brewer(palette = "Set3")+
   theme_minimal()+
   theme(panel.grid.major = element_blank(),  # Supprimer la grille majeure
         panel.grid.minor = element_blank())+
   labs( fill = "", x = "Date", y = "")
 
 
 g2 <-  ggplot(data = rv$db_pos,
               aes(x = date, y = value, fill = name))+
   geom_histogram(stat = "identity",position = position_nudge(x = 1), width = 6)+
   scale_x_date(
     labels = scales::date_format("%b %d, %Y"),
     date_breaks = "1 year")+
   scale_fill_manual(values = c("#FFC000","#00B050"),
                     labels = c(pcr_r = "PCR", tdr_r = "RDT"))+
   theme_cowplot()+
   theme(legend.position =  "top",
         panel.grid.major = element_blank(),  # Supprimer la grille majeure
         panel.grid.minor = element_blank())+
   labs( fill = "", x = "Date", y = "")
 
 g3 <- ggplot(data = rv$db_positivite,
              aes(x = date, y = tx_positivite, color = "red"))+
   geom_line(size = 1, linetype = "dashed") +
   scale_y_continuous(position = "right", labels = percent) +
   scale_color_manual(values = "red", labels = c(red = "Positivity rate")) +
   theme_cowplot() +
   theme(legend.position = "top",
         legend.text = element_text(hjust = 0.5),
         axis.text.x = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.margin = margin(l = 105)) + # Ajuster la marge gauche de la légende
   labs(color = "", x = "", y = "(%)")
 
 aligned_plots <- cowplot::align_plots(g2, g3, align="hv", axis="tblr")
 
 # Plot a inserer dans le PPTX
 
 g4 <- plot_posrate <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
 
 #Plot 5 
 
 color <-c("Sous contrôle"= "#11FF11","Alerte" = "#FFFF00","Résurgence" = "#FF0000","Non classé" = "white")
 g5 <- ggplot(data = rv$db2, aes(fill = status)) +
   geom_sf(color = "#808080", size = 1) +
   geom_sf(data = rv$region_shapefile, fill = NA, size = 3, color = "black") +
   geom_sf_text(aes(label = District_S),size = 1.5) +
   scale_fill_manual(values = color) +
   labs(fill = "Niveau de résurgence des districts") +
   theme_void() +
   theme(
     plot.background = element_rect(fill = "white", color = "black", size = 1, linetype = "solid"),
     plot.title = element_text(hjust = 0, vjust = 1, size = 14),  # Ajuste le titre
     legend.position = c(0.30, 0.9),  # Déplace la légende au coin supérieur gauche
     legend.box.background = element_blank(),  # Enlève le cadre de la légende
     legend.text = element_text(size = 12),  # Ajuste la taille du texte de la légende
     legend.title = element_text(margin = margin(0, 0, 5, 0), size = 12))  # Ajuste la marge du titre de la légende
     
     g5 <- g5 +
       ggspatial::annotation_scale(
         location = "bl",  # Déplace l'échelle en bas à gauche
         bar_cols = c("grey60", "white"),
         text_family = "ArcherPro Book"
       ) +
       ggspatial::annotation_north_arrow(
         location = "tr",  # Garde la flèche du nord en haut à droite
         which_north = "true",
         pad_x = unit(0.4, "in"),  # Ajuste l'espace en x
         pad_y = unit(0.4, "in"),  # Ajuste l'espace en y
         style = ggspatial::north_arrow_nautical(
           fill = c("grey40", "white"),
           line_col = "grey20",
           text_family = "ArcherPro Book"
         )
       )
     
     
     #Plot 6
     
     g6 <-  ggplot(data = rv$db2, aes(fill = depist_pop_cat)) +
       geom_sf(color = "#808080", size = 1) +
       geom_sf(data = rv$region_shapefile, fill = NA, size = 3, color = "black")+
       geom_sf_text(aes(label = District_S),size = 1.5) + 
       scale_fill_brewer(palette = "Reds")+
       labs(fill = "Taux de dépistage pour 10.000 habitants")+
       theme_void() +
       theme(
         plot.background = element_rect(fill = "white", color = "black", size = 1, linetype = "solid"),
         plot.title = element_text(hjust = 0, vjust = 1, size = 14),  # Ajuste le titre
         legend.position = c(0.33, 0.85),  # Déplace la légende au coin supérieur gauche
         legend.box.background = element_blank(),  # Enlève le cadre de la légende
         legend.text = element_text(size = 12),  # Ajuste la taille du texte de la légende
         legend.title = element_text(margin = margin(0, 0, 5, 0), size = 12))  # Ajuste la marge du titre de la légende
     
     g6 <-  g6 +
       ggspatial::annotation_scale(
         location = "bl",  # Déplace l'échelle en bas à gauche
         bar_cols = c("grey60", "white"),
         text_family = "ArcherPro Book"
       ) +
       ggspatial::annotation_north_arrow(
         location = "tr",  # Garde la flèche du nord en haut à droite
         which_north = "true",
         pad_x = unit(0.4, "in"),  # Ajuste l'espace en x
         pad_y = unit(0.4, "in"),  # Ajuste l'espace en y
         style = ggspatial::north_arrow_nautical(
           fill = c("grey40", "white"),
           line_col = "grey20",
           text_family = "ArcherPro Book"
         )
       )
     
     
     cols <- c("0-5" = "#FFFF00","5-10" = "#fee0d2","10-15" = "#fc9272", "15 et plus" = "#cb181d",
               "Aucun test réalisé" = "grey","Aucun cas détecté" = "#18cb3f" )
     
     
     ## Reordering db_week1$Tx_dps_
     rv$db2$Tx_depist_class <- rv$db2$Tx_depist_class %>%
       fct_relevel(
         "Aucun cas détecté", "Aucun test réalisé", "0-5", "5-10",
         "10-15", "15 et plus"
       )
     
     
     
     
     
     
     g7 <-  ggplot(data = rv$db2, aes(fill = Tx_depist_class)) +
       geom_sf(color = "#808080", size = 1) +
       geom_sf(data = rv$region_shapefile, fill = NA, size = 3, color = "black")+
       geom_sf_text(aes(label = District_S),size = 1.5) + 
       scale_fill_manual(values = cols) +
       labs(fill = "Taux de Positivité des échantillons testés") +
       theme_void() +
       theme(
         plot.background = element_rect(fill = "white", color = "black", size = 1, linetype = "solid"),
         plot.title = element_text(hjust = 0.5, vjust = 1, size = 14),  # Ajuste le titre
         legend.position = c(0.33, 0.85),  # Déplace la légende au coin supérieur gauche
         legend.box.background = element_blank(),  # Enlève le cadre de la légende
         legend.text = element_text(size = 12),  # Ajuste la taille du texte de la légende
         legend.title = element_text(margin = margin(0, 0, 5, 0), size = 12))  # Ajuste la marge du titre de la légende
     
     
     g7 <-  g7 +
       ggspatial::annotation_scale(
         location = "bl",  # Déplace l'échelle en bas à gauche
         bar_cols = c("grey60", "white"),
         text_family = "ArcherPro Book"
       ) +
       ggspatial::annotation_north_arrow(
         location = "tr",  # Garde la flèche du nord en haut à droite
         which_north = "true",
         pad_x = unit(0.4, "in"),  # Ajuste l'espace en x
         pad_y = unit(0.4, "in"),  # Ajuste l'espace en y
         style = ggspatial::north_arrow_nautical(
           fill = c("grey40", "white"),
           line_col = "grey20",
           text_family = "ArcherPro Book"
         )
       )
     
     
 
 
################################################################################ 
 
  rv$list <- lst(week_select,year_select,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,g1,g4,g5,g6,g7)
   
 })
 

 
output$outputButton_PPT <- downloadHandler(
  
   filename = function() paste0("Situation Vaccination Covid-19 Cameroun","_",input$semaine_epi,".pptx"),
   content = function(file) {
     
     withProgress(
       message = 'Production du PowerPoint...',
       detail = 'Ceci peut prendre quelques minutes...',
       value = 0,
       {
         # Simulate progress
         for (i in 1:10) {
           Sys.sleep(1)  # Simulating a time-consuming task
           incProgress(1/10, detail = sprintf('Step %d', i))
         }
     
       
     # Generate the report
     
     
     propertiesTitle <- fp_text(color = "white", font.size = 54, bold = TRUE, font.family = "Baloo Tammudu 2")
     cameroun_font <- fp_text(color = "#FFC000", font.size = 54, bold = TRUE, font.family = "Baloo Tammudu 2")
     subtitle <- fp_text(color = "#1F4E79", font.size = 24, bold = TRUE, font.family = "Maiandra GD")
     subtitle1 <- fp_text(color = "white", font.size = 32, bold = TRUE, font.family = "Baloo Tammudu 2")
     subtitle2 <- fp_text(color = "#FFFF00", font.size = 32, bold = TRUE, font.family = "Baloo Tammudu 2")
     subtitle3 <- fp_text(color = "#FFC000", font.size = 32, bold = TRUE, font.family = "Baloo Tammudu 2")
     propertiesDate <- fp_text(color = "black",font.size = 18,bold = F)
     propertiesFootNote <- fp_text(color = "grey",font.size = 11,bold = F)
     #reportingDate <- max(as.Date(data_clean_immunization$date))
     cameroun_text <- toupper("Cameroun")
     propertiestitledia <- fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")
     font_text0 <- fp_text(color = "black",font.size = 80,bold = T,font.family = "Gill Sans MT (Headings)")
     
     
     template <- here("rev_temp.pptx") 
     
     template_doc <- read_pptx(template)
     # Access the values from rv$list
     week_select <- rv$list[[1]]
     year_select <- rv$list[[2]]
     
     # Personnaliser la première page (Page de titre)
     template_doc %>%
       remove_slide(index = 1) %>% 
       remove_slide(index = 2) %>%
       remove_slide(index = 1) %>%
       add_slide(layout = "Diapositive de titre", master = "Thème Office") %>% 
       ph_with(value = fpar(ftext(paste("Situation de la vaccination Covid-19"),propertiesTitle), 
                            ftext(paste(" ",cameroun_text),cameroun_font),
                            fp_p = fp_par(text.align = "right")), 
               location = ph_location(left = 0.15, top = 0.93, width = 8.64, height =3 )) %>% 
       ph_with(value = fpar(ftext(paste("Date de lancement"),subtitle1),
                            ftext(paste("|12 Avril 2021"),subtitle2),
                            fp_p = fp_par(text.align = "right")), 
               location = ph_location(left = 0.33,top = 4.03,width = 6.34 ,height = 0.54)) %>% 
       ph_with(value = fpar(ftext(paste("Semaine"),subtitle1),
                            ftext(paste("|"),subtitle2),
                            ftext(paste(week_select),subtitle3),
                            ftext(paste(" ",year_select),subtitle2),
                            fp_p = fp_par(text.align = "right")), 
               location = ph_location(left = -0.18,top = 4.57,width = 4.12 ,height = 0.54)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Doses administrées par type de vaccin", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$tab1 %>% 
                 adorn_totals(where = c("row", "col"),
                              name = c("Total","Total doses administrees")) %>% 
                 purrr::set_names("Vaccin","Dose 1","Dose 2","Booster", "Total doses administrées") %>% 
                 flextable::flextable() %>%
                 color(
                   i = ~Vaccin == "Total",
                   j =1:5,
                   color = "black") %>% 
                 bg(j = 1:5, bg = "#FFC000",i = ~Vaccin == "Total") %>% 
                 bold(j = 1:5,i = ~Vaccin == "Total") %>% 
                 bold(j = 1:5, part = "header") %>% 
                 fontsize(3) %>% align(align = "center",part = "all") %>% 
                width(width = 1.5),
               location = ph_location(left = 2.63,top = 2.02)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Doses administrées pour 100 hts", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p1,
               location = ph_location(left = 1.03,top = 1.61,width = 10.92 ,height = 5.03)) %>% 
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Couverture population cible", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p2,
               location = ph_location(left = 0.71,top = 1.64,width = 11.37 ,height = 5)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Couverture population totale", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p3,
               location = ph_location(left = 0.71,top = 1.64,width = 11.37 ,height = 5)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Tendances Doses administrées des 5 dernières semaines", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p4,
               location = ph_location(left = 1.62,top = 1.97,width = 10.1 ,height = 4.58)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Evolution hebdomadaire et campagnes des doses administrées", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p5,
               location = ph_location(left = 0.1,top = 1.98,width = 7.13 ,height = 4.66)) %>%
       ph_with(value = rv$list$p6,
               location = ph_location(left = 7.56,top = 1.85,width = 5.67 ,height = 4.66)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Couverture vaccinale par Régions", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p7,
               location = ph_location(left = 2.05,top = 1.75,width = 9.43 ,height = 4.66)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Couverture vaccinale par Régions", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$cv_regional %>% 
                 set_names("Région","Population cible","# Première dose","# Deuxième dose","# Dose de rapppel","# Complètement vaccinés","Couverture première dose","Couverture vaccinale complète") %>% 
                 flextable() %>% 
                 theme_zebra(odd_header = "#00B0F0", odd_body = "#BDD7EE") %>% 
                 color(
                   i = ~Région == "Total",
                   j =1:8,
                   color = "black"
                 ) %>% 
                 #bg(j = 1:8, bg = "#FFC000",i = ~Région == "Total") %>% 
                 bold(j = 1:8,i = ~Région == "Total") %>% 
                 bold(j = 1:8, part = "header") %>% 
                 fontsize(3) %>% align(align = "center",part = "all") %>% width(width = 1.5),
               location = ph_location(left = 0.42,top = 1.93,width = 12.49 ,height = 4.66)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Couverture vaccinale par Régions", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p8,
               location = ph_location(left = 2.17,top = 1.7,width = 8.69 ,height = 4.66)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Couverture vaccinale des cibles prioritaires par Régions", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$prioritaire_prioritaire_cv %>% 
                 set_names("Région","Cible","complètement vaccinés1","Couverture vaccinale(%)","Cibles2","complètement vaccinés2","Couverture vaccinale2(%)","Cibles3",
                           "complètement vaccinés3","Couverture vaccinale3(%)") %>% 
                 flextable() %>% color(
                   i = ~Région == "Total",
                   j =1:10,
                   color = "black"
                 ) %>% 
                 #bg(j = 1:8, bg = "#FFC000",i = ~Région == "Total") %>% 
                 bold(j = 1:10,i = ~Région == "Total") %>% 
                 bold(j = 1:10, part = "header") %>% 
                 fontsize(3) %>% align(align = "center",part = "all") %>% 
                 add_header_row(top = TRUE,
                                values = c(
                                  "Région",
                                  "Personnel de santé",
                                  "",
                                  "",
                                  "Personnes agées",
                                  "",
                                  "",
                                  "Personnes avec comorbidites",
                                  "",
                                  "" )) %>% 
                 set_header_labels(Région = "",
                                   Cible = "Cibles",
                                   `complètement vaccinés1` = "complètement vaccinés",
                                   Cibles2 = "Cibles",
                                   `complètement vaccinés2` = "complètement vaccinés",
                                   `Couverture vaccinale2(%)` = "Couverture vaccinale(%)",
                                   Cibles3 = "Cibles",
                                   `complètement vaccinés3` = "complètement vaccinés",
                                   `Couverture vaccinale3(%)` = "Couverture vaccinale(%)") %>% 
                 merge_at(i = 1, j = 2:4, part = "header") %>% 
                 merge_at(i = 1, j = 5:7, part = "header") %>% 
                 merge_at(i = 1, j = 8:10, part = "header") %>%  
                 merge_at(i = 1:2, j = 1, part = "header") %>% 
                 flextable::align(align = "center", j = c(2:10), part = "all") %>% 
                 fontsize(i = 1, size = 12, part = "header") %>% 
                 theme_zebra(odd_header = "#00B0F0", odd_body = "#BDD7EE") %>% 
                 bold(i = 1, bold = TRUE, part = "header") %>% 
                  width(width = 1.2),
               location = ph_location(left = 0.42,top = 1.93,width = 12.49 ,height = 4.66)) %>% 
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Couverture vaccinale des personnels de santé comparée a celle de la population totale", 
                          prop = fp_text(color = "#1F4E79", font.size = 40, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p9,
               location = ph_location(left = 0.32,top = 1.7,width = 12.6 ,height = 4.97)) %>% 
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Analyse suivant le genre", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p10,
               location = ph_location(left = 0.23,top = 2.67,width = 6.31 ,height = 4.16)) %>% 
       ph_with(value = rv$list$p11,
               location = ph_location(left = 6.67,top = 2.39,width = 6.31 ,height = 4.16)) %>%
       ph_with(value = fpar(ftext(paste("CV par rapport à la Pop. Cible"),subtitle), 
                            fp_p = fp_par(text.align = "right")), 
               location = ph_location(left = 0.76,top = 0.5, width = 5.65)) %>% 
       ph_with(value = fpar(ftext(paste("Proportion par rapport à la \n cible complétement vaccinées"),subtitle), 
                            fp_p = fp_par(text.align = "right")), 
               location = ph_location(left = 6.8,top = 0.5, width = 5.65)) %>%
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Doses administrées par type de vaccin", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$tab_vaccine_type %>% 
                        adorn_totals(where = c("row","col")) %>% 
                 set_names("Région","Sinopharm","AstraZeneca","PFIZER","Johnson & Johnson","Total") %>% 
                 flextable() %>% 
                 color(
                   i = ~Région == "Total",
                   j =1:6,
                   color = "black") %>% 
                 bold(j = 1:6,i = ~Région == "Total") %>% 
                 bold(j = 1:6, part = "header") %>% 
                 fontsize(3) %>% align(align = "center",part = "all") %>% 
                 theme_zebra(odd_header = "#00B0F0", odd_body = "#BDD7EE") %>% 
                        width(width = 1.5),
               location = ph_location(left = 2.17,top = 1.96,width = 12.49 ,height = 4.66)) %>% 
       add_slide(layout = "Deux contenus",master = "Thème Office") %>%
       ph_with(fpar(ftext("Gap entre les doses reçues et administrées", 
                          prop = fp_text(color = "#1F4E79", font.size = 44, bold = TRUE, font.family = "Maiandra GD")), fp_p = fp_par(text.align = "center")),
               location = ph_location_type(type = "title")) %>%  
       ph_with(value = rv$list$p12,
               location = ph_location(left = 1.34,top = 1.85,width = 10.12 ,height = 4.66)) %>%
       add_slide(layout = "Vide", master = "Thème Office") %>%
       ph_with(value = external_img(src = here("www","img3.png")),
               location = ph_location(left = 0.69 ,top = 2.5 ,width = 4.38 ,height = 1.93)) %>%
       ph_with(value = fpar(ftext(paste("Thank   You!!"),font_text0)),
               location = ph_location(left = 6.17,top = 2.5,width = 6.84 ,height = 4.03)) %>%
       
       print(target = "immunization_template.pptx")
     
     
     
     file.copy("immunization_template.pptx",file)
     
   }
  
)
     
   }
)
 
################################################################################
# Produire et generer le rapport sur la maladie 
################################################################################

# ecrire le PPTX

output$OutputButton_maladie <- downloadHandler(   
  
  filename = function() paste0("Situation de la Covid-19 au Cameroun","_",input$semaine_epi,".pptx"),
  content = function(file) {
    
    withProgress(
      message = 'Production du PowerPoint...',
      detail = 'Ceci peut prendre quelques minutes...',
      value = 0,
      {
        # Simulate progress
        for (i in 1:10) {
          Sys.sleep(1)  
          incProgress(1/10, detail = sprintf('Step %d', i))
        }
  
    ################################################################################
    # Production des PPTX pour le rapport sur la maladie
    ################################################################################
    
    
    cameroun_font <- fp_text(color = "#000000", font.size = 48, bold = TRUE, font.family = "Arial Narrow")
    title_font <- fp_text(color = "white", font.size = 36, bold = TRUE, font.family = "Arial Narrow")
    end_font <- fp_text(color = "#000000", font.size = 44, bold = TRUE, font.family = "Calibri Light (Headings)")
    datefont <- fp_text(color = "#000000", font.size = 28, bold = TRUE, font.family = "Arial")
    propertietext <- fp_text(color = "#000000", font.size = 22, bold = TRUE, font.family = "Arial")
    propertietext1 <- fp_text(color = "white", font.size = 20, bold = TRUE, font.family = "Arial")
    font_cases <- fp_text(color = "#548235", font.size = 30, bold = TRUE, font.family = "Arial Narrow")
    font_deaths <- fp_text(color = "#FF0000", font.size = 30, bold = TRUE, font.family = "Arial Narrow")
    font_text <- fp_text(color = "#000000", font.size = 32, bold = FALSE, font.family = "Arial Narrow")
    
   
    rv$reportcasestext <- str_glue("{rv$reportcases} cas confirmés")
    rv$reportdeathstext <- str_glue("{rv$reportdeaths} Décès")
    
    rv$district_resurgence <- if (sum(rv$db2$status == "Résurgence") == 0) {
      rv$comment <- "Aucun DS n’a franchi le seuil de résurgence au courant de la semaine"} 
    else {
      rv$comment <- str_glue("{fmt_count(rv$db2, status == 'Résurgence')} des DS ont franchi le seuil de résurgence au courant de la semaine")
      
    }
    
    rv$district_control <- str_glue("L’épidémie est sous contrôle dans {epikit::fmt_count(rv$db2,status == 'Sous contrôle')} district(s)")
    
    #rv$district_control <- str_glue("L’épidémie est sous contrôle dans {epikit::fmt_count(rv$db2,status == 'Sous contrôle')} district(s)")
    rv$district_tdr <- str_glue("{epikit::fmt_count(rv$db2,Test >1)} Districts ont effectué au moins 1 TDR.")
    rv$district_normes <- str_glue("{epikit::fmt_count(rv$db2, depist_pop >= 10)} DS ont/a atteint(s) la norme selon OMS qui est d’effectuer au moins 10 tests pour 10.000 habitants en une semaine;")
    rv$nom_district <- rv$db2$District_S[rv$db2$depist_pop >= 10]
    rv$nom_district1 <- paste(rv$nom_district,collapse = ", ")
    rv$nom_district2 <- str_glue("Il s’agit des DS {rv$nom_district1}")
    rv$depist_positifs <- str_glue("Sur les {epikit::fmt_count(rv$db2,Test >1)} DS ayant réalisés les TDR,{epikit::fmt_count(rv$db2,Positifs >1)} ont détecté des cas positif")
   
    rv$index_max <- which.max(rv$db2$Positifs / rv$db2$Test)             
    rv$max_district <- rv$db2$District_S[rv$index_max]
    
    rv$ratios <- rv$db2$Positifs / rv$db2$Test
    rv$second_position <- order(rv$ratios, decreasing = TRUE)[2]
    rv$second_position1 <- rv$db2$District_S[rv$second_position]
    
    rv$max_district1 <- str_glue("Le DS avec le plus haut taux de positivité est celui de {rv$max_district} suivi de {rv$second_position1}")
    
    
    ################################################################################
    ################################################################################
    
    template_1 <- here::here("template_maladie.pptx") 
    
    template_doc_disease <- read_pptx(template_1)  
    
    
    template_doc_disease %>% 
      remove_slide(index = 1) %>% 
      remove_slide(index = 2) %>%
      remove_slide(index = 1) %>% 
      add_slide(layout = "Cover slide layout", master = "Thème Office") %>%
      ph_with(value = fpar(ftext(paste("Analyses des données COVID-19 Au Cameroun"),cameroun_font),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 1.43, top = 2.31, width = 9.39, height =1.72 )) %>% 
      ph_with(value = fpar(ftext(paste(rv$reportingcovid),datefont),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 6.88, top = 4.47, width = 5.17, height =0.57)) %>% 
      ph_with(value = fpar(ftext(paste("PREPAREE PAR:"),propertietext),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 3.51, top = 5.67, width = 2.82, height =0.43)) %>% 
      ph_with(value = external_img(src = here("www","img1.png")),
              location = ph_location(left = 6.64 ,top = 5.57 ,width = 5.92 ,height = 0.54)) %>% 
      ph_with(value = fpar(ftext(paste("UGD / CCOUSP"),propertietext1),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 6.64 ,top = 5.57 ,width = 5.92 ,height = 0.54)) %>%
      add_slide(layout = "1_Contents slide layout", master = "Thème Office") %>%
      ph_with(value = fpar(ftext(paste("COURBE EPIDEMIOLOGIQUE AVEC LES VAGUES"),title_font),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 0.69, top = 0.26, width = 11.31, height =0.88)) %>%
      ph_with(value = rv$list$g1,
              location = ph_location(left = 0.31,top = 1.33,width = 12.69 ,height = 5.91)) %>%
      ph_with(value = fpar(ftext(paste(rv$reportcasestext),font_cases)),
              location = ph_location(left = 8.1,top = 1.65,width = 4.27 ,height = 0.6)) %>%
      ph_with(value = fpar(ftext(paste(rv$reportdeathstext),font_deaths)),
              location = ph_location(left = 8.1,top = 2.25,width = 4.27 ,height = 0.6)) %>%
      add_slide(layout = "1_Contents slide layout", master = "Thème Office") %>%
      ph_with(value = fpar(ftext(paste("COURBE DE POSITIVITÉ TDR/PCR"),title_font),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 0.69, top = 0.26, width = 11.31, height =0.88)) %>%
      ph_with(value = rv$list$g4,
              location = ph_location(left = 0.31,top = 1.33,width = 12.69 ,height = 5.91)) %>%
      
      add_slide(layout = "Contents slide layout", master = "Thème Office") %>%
      ph_with(value = fpar(ftext(paste("SEUIL DE RESURGENCE PAR DISTRICT"),title_font),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 5.03, top = 0, width = 7.3, height =1.25)) %>%
      ph_with(value = rv$list$g5,
              location = ph_location(left = -0.15,top = 0,width = 5.3 ,height = 7.5)) %>%
      ph_with(value = external_img(src = here("www","img2.png")),
              location = ph_location(left = 5.31 ,top = 4.91 ,width = 7.87 ,height = 2.53)) %>%
      ph_with(value = fpar(ftext(paste(rv$district_resurgence),font_text)),
              location = ph_location(left = 5.15,top = 1.54,width = 7.87 ,height = 1.25)) %>%
      ph_with(value = fpar(ftext(paste(rv$district_control),font_text)),
              location = ph_location(left = 5.15,top = 2.98,width = 7.87 ,height = 1.25)) %>% 
      
      
      add_slide(layout = "Contents slide layout", master = "Thème Office") %>%
      ph_with(value = fpar(ftext(paste("TAUX DE DÉPISTAGE PAR DISTRICT"),title_font),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 5.03, top = 0, width = 7.3, height =1.25)) %>%
      ph_with(value = rv$list$g6,
              location = ph_location(left = -0.15,top = 0,width = 5.3 ,height = 7.5)) %>%
      ph_with(value = fpar(ftext(paste(rv$district_tdr),font_text)),
              location = ph_location(left = 5.15,top = 1.54,width = 7.87 ,height = 1.25)) %>%
      ph_with(value = fpar(ftext(paste(rv$district_normes),font_text)),
              location = ph_location(left = 5.15,top = 2.98,width = 7.87 ,height = 1.25)) %>%
      ph_with(value = fpar(ftext(paste(rv$nom_district2),font_text)),
              location = ph_location(left = 5.15,top = 4.95,width = 7.87 ,height = 1.25)) %>%
      
      add_slide(layout = "Contents slide layout", master = "Thème Office") %>%
      ph_with(value = fpar(ftext(paste("TAUX DE POSITIVITE PAR DISTRICT"),title_font),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 5.03, top = 0, width = 7.3, height =1.25)) %>%
      ph_with(value = rv$list$g7,
              location = ph_location(left = -0.15,top = 0,width = 5.3 ,height = 7.5)) %>%
      ph_with(value = fpar(ftext(paste(rv$depist_positifs),font_text)),
              location = ph_location(left = 5.15,top = 1.54,width = 7.87 ,height = 1.25)) %>%
      ph_with(value = fpar(ftext(paste(rv$max_district1),font_text)),
              location = ph_location(left = 5.15,top = 2.98,width = 7.87 ,height = 1.25)) %>%
      ph_with(value = fpar(ftext(paste(rv$max_district1),font_text)),
              location = ph_location(left = 5.15,top = 2.98,width = 7.87 ,height = 1.25)) %>%
      
      add_slide(layout = "1_Contents slide layout", master = "Thème Office") %>%
      ph_with(value = fpar(ftext(paste("EVOLUTION DES PRINCIPAUX INDICATEURS"),title_font),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 0.69, top = 0.26, width = 11.31, height =0.88)) %>%
      ph_with(value = rv$indicator_table %>% flextable() %>% 
                set_table_properties(align = "center") %>% 
                autofit() %>% width(width = 1.13),
              location = ph_location(left = 0.41,top = 1.49)) %>%
      add_slide(layout = "1_Contents slide layout", master = "Thème Office") %>%
      ph_with(value = fpar(ftext(paste("SITUATION EPIDEMIOLOGIQUE DE LA SEMAINE PAR REGION"),title_font),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 0, top = 0, width = 11.31, height =0.88)) %>%
      ph_with(value = rv$indicator_region %>% 
                flextable() %>% 
                color(
                  i = ~region == "Cameroun",
                  j =1:15,
                  color = "black") %>% 
                bold(j = 1:15,i = ~region == "Cameroun") %>% 
                bold(j = 1:15, part = "header") %>% 
                fontsize(3) %>% align(align = "center",part = "all") %>% 
                add_header_row(top = TRUE,
                               values = c(
                                 "Région",
                                 " Cas positifs",
                                 "",
                                 "Cas guéris",
                                 "",
                                 "Femmes Enceintes infectées",
                                 "",
                                 "Femmes enceintes décédées",
                                 "",
                                 "Personnels de santé infectés",
                                 "",
                                 "Personnels de santé décédés",
                                 "",
                                 "Décès",
                                 ""
                               )) %>% 
                set_header_labels(
                  region = "",
                  new_cases = "Semaine",
                  cases = "Cumul",
                  gueris = "Semaine",
                  guerisons = "Cumul",
                  nfec = "Semaine",
                  fec_pos = "Cumul",
                  nfec_dcd = "Semaine",
                  fec_dcd = "Cumul",
                  npersonnel = "Semaine",
                  npersonnel_cum = "Cumul",
                  npersodcd = "Semaine",
                  npersodcd_cum = "Cumul",
                  new_deaths = "Semaine",
                  deaths = "Cumul") %>% 
                merge_at(i = 1, j = 2:3, part = "header") %>% 
                merge_at(i = 1, j = 4:5, part = "header") %>% 
                merge_at(i = 1, j = 6:7, part = "header") %>% 
                merge_at(i = 1, j = 8:9, part = "header") %>% 
                merge_at(i = 1, j = 10:11, part = "header") %>% 
                merge_at(i = 1, j = 12:13, part = "header") %>% 
                merge_at(i = 1, j = 14:15, part = "header") %>% 
                merge_at(i = 1:2, j = 1, part = "header") %>% 
                flextable::theme_box() %>% 
                flextable::align(align = "center", j = c(2:15), part = "all") %>% 
                fontsize(i = 1, size = 12, part = "header") %>% width(width = 0.84),
              location = ph_location(left = 0.41,top = 1.49)) %>% 
      add_slide(layout = "1_Contents slide layout", master = "Thème Office") %>%
      ph_with(value = fpar(ftext(paste("MERCI POUR VOTRE AIMABLE ATTENTION"),end_font),
                           fp_p = fp_par(text.align = "center")), 
              location = ph_location(left = 0.86, top = 3.31, width = 11.31, height =0.88)) %>%
       
      
      print(target = "disease_template.pptx")
    
    file.copy("disease_template.pptx",file)
 
    
  }
  
)
  }
)


###############################################################################
# Telecharger le jeu de donnees feuille data
###############################################################################

output$rawtable <- renderPrint({
 cv_cases_regions_subset <-   cv_cases_regions %>% 
   select(date,region,population,cases,new_cases,deaths, 
          new_deaths,nfec,nfec_dcd,hospitalises,guerisons,cas_sous_o2_15,tdr_realise,tdr_positif,pcr_realise,pcr_positif,dose_administrees,new_vaccine_doses) %>% 
   rename(cas_02 = cas_sous_o2_15,
          femme_enceinte = nfec,
          femme_enceinte_dcd = nfec_dcd)
   
   orig <- options(width = 900)
   print(tail(cv_cases_regions_subset,input$maxrows),row.names = FALSE)
   options(orig)
 })

output$downloadcvs <- downloadHandler(
  
  filename = function(){
      paste("data_covid_", cv_today$date[1], ".csv", sep = "")},
  content = function(file){
    
    cv_cases_regions_subset <-   cv_cases_regions %>% 
      select(date,region,latitude,longitude,population,cases,new_cases,deaths, new_deaths,tdr_realise,tdr_positif,pcr_realise,pcr_positif,
             hospitalises,cas_sous_o2_15,dose_administrees,new_vaccine_doses,mapi_mineur2,mapi_grave)
    
    write.csv(cv_cases_regions_subset,file)
    
  })


}


 
 




# Run the APP  ---------------------------------------------------------------

shinyApp(ui, server)



################################################################################
################################################################################
# Deploy the App

#rsconnect::setAccountInfo(name='pheoc-cameroon', 
 #                         token='2315B2CABD3F76180AF4C087D90731BD', 
  #                        secret='5N1DZ8VFY6sB7VC+nMisuDDNqXvUQOK0nJmhJ6+J')


#rsconnect::deployApp('D:/ncov_shiny_master_camer')



#rsconnect::setAccountInfo(name='bdasna-aureol0le0rocher-ngako0njiacheu',
#                          token='37AFAC2EF6D5918BC4809C910D0E5D7E',
#                          secret='K63X/lqjbwlt0Uh9EwegTkC4znliW5Dfk3195Rof')

#rsconnect::deployApp('D:/ncov_shiny_master_camer')

#rsconnect::configureApp('ncov_shiny_master_camer',size = 8192)



