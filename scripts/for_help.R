
# Load the packages to be used -----------------------------------------------

pacman::p_load (tidyverse,here,rio,reprex)



# Generate the data without covid database -----------------------


demo_data <-   data.frame(
  region = c("Adamaoua",
             "Centre","Est",
             "Extreme Nord",
             "Littoral","Nord",
             "Nord-Ouest","Ouest","Sud",
             "Sud-Ouest",
             "Adamaoua","Centre",
             "Est","Extreme Nord",
             "Littoral","Nord",
             "Nord-Ouest",
             "Ouest","Sud",
             "Sud-Ouest","Adamaoua",
             "Centre","Est",
             "Extreme Nord","Littoral",
             "Nord","Nord-Ouest",
             "Ouest","Sud",
             "Sud-Ouest",
             "Adamaoua","Centre","Est",
             "Extreme Nord",
             "Littoral","Nord",
             "Nord-Ouest","Ouest",
             "Sud","Sud-Ouest",
             "Adamaoua","Centre",
             "Est",
             "Extreme Nord","Littoral",
             "Nord","Nord-Ouest",
             "Ouest","Sud",
             "Sud-Ouest"),
  date = c("2021-01-02",
           "2021-01-02",
           "2021-01-02",
           "2021-01-02","2021-01-02",
           "2021-01-02",
           "2021-01-02","2021-01-02",
           "2021-01-02",
           "2021-01-02","2021-01-03",
           "2021-01-03",
           "2021-01-03",
           "2021-01-03","2021-01-03",
           "2021-01-03",
           "2021-01-03","2021-01-03",
           "2021-01-03",
           "2021-01-03","2021-01-04",
           "2021-01-04",
           "2021-01-04",
           "2021-01-04","2021-01-04",
           "2021-01-04",
           "2021-01-04","2021-01-04",                                                                                                 "2021-01-04",
           "2021-01-04","2021-01-05",                                                                                                        "2021-01-05",
           "2021-01-05",                                                                                                        "2021-01-05","2021-01-05",
           "2021-01-05",
           "2021-01-05","2021-01-05",
           "2021-01-05",
           "2021-01-05","2021-01-06",
           "2021-01-06",
           "2021-01-06",
           "2021-01-06","2021-01-06",
           "2021-01-06",
           "2021-01-06","2021-01-06",
           "2021-01-06",
           "2021-01-06"),
  population = c(1345934,
                 4846001.81701392,
                 1146981,4824522.05380686,
                 3987222,
                 2964767.81627007,
                 2244288.13462958,2113367,
                 818190,1862687,1345934,
                 4846001.81701392,
                 1146981,
                 4824522.05380686,3987222,
                 2964767.81627007,
                 2244288.13462958,2113367,
                 818190,1862687,
                 1518189,4965861,
                 1360451,4967788,4277464,
                 2996271,1868031,
                 2327807,894878,
                 1899941,1518189,
                 4965861,1360451,4967788,
                 4277464,2996271,
                 1868031,2327807,
                 894878,1899941,
                 1518189,4965861,1360451,
                 4967788,4277464,
                 2996271,1868031,
                 2327807,894878,
                 1899941),
  cases = c(2,181,0,2,
            20,10,0,0,0,0,0,
            9,0,0,100,83,
            0,0,110,0,2,107,
            0,3,0,0,0,0,0,
            0,0,82,3,8,20,
            0,0,0,0,0,4,
            105,0,1,63,16,0,
            2,0,8),
  deaths = c(0,20,0,1,0,
             0,0,1,0,0,0,
             0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,
             0,0,0,0,0,0,
             0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,
             0,0,0,0,0)
)


## Function to compute the news cases and new deaths ---------------------------

for (i in c(1:nrow(demo_data))) {
  # Create the covid_subset and make sure that the date are ordered
  
  covid_subset <- demo_data[order(demo_data$date),]
  
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
  
  demo_data$new_cases[i]   <-  covid_subset$new_cases[i]
  demo_data$new_deaths[i] <-  covid_subset$new_deaths[i] 
  
}

