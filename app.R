#---------------------------------
#Load needed libraries COVIDMETRICS
#---------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(dashboardthemes)
library(fresh)
library(htmlwidgets)

library(RCurl)
library(jsonlite)

library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)
library(TTR)

library(plotly)
library(sf)
library(albersusa)
library(leaflet.extras)

#---------------------------------
#Read data
#---------------------------------

state_data <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
nyt_state <- read.csv(text = state_data)
covid_testing<- fromJSON("https://covidtracking.com/api/states/daily")
population_state_all <- read_csv("nst-est2019-alldata.csv", na = "--")
population_state_all <- clean_names(population_state_all)
county_data <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
nyt_county <- read.csv(text = county_data)
population_county_all <- read_csv("co-est2019-alldata.csv", na = "--")
population_county_all <- clean_names(population_county_all)
svi <- read_csv("SVI2018_US_COUNTY.csv", na = "--")
svi <- clean_names(svi)

#---------------------------------
#SVI Data Wrangling
#---------------------------------

svi_select <- svi%>%
  select(fips, rpl_themes, county)%>%
  rename(fips = fips, svi_score = rpl_themes, county = county)%>%
  mutate(county_fips = as.numeric(fips))%>%
  filter(svi_score >= 0)%>%
  mutate(svi_score_map = cut(svi_score, breaks = 4, labels = c("0-0.25", "0.25-0.5", "0.5-0.75", "0.75-1")))

#---------------------------------
#State Level Data Wrangling
#---------------------------------

nyt_formated <- nyt_state%>%
  mutate(state = as.character(state))

ustotal <- nyt_state%>%
  group_by(date)%>%
  summarize(
    state = as.character("United States"),
    fips = 00,
    cases = sum(cases),
    deaths = sum(deaths))

nyt_state <- bind_rows(nyt_formated, ustotal)

population_state <- population_state_all %>%
  filter(state != "72")%>%
  filter(name != "Northeast Region")%>%
  filter(name != "Midwest Region") %>%
  filter(name != "South Region") %>%
  filter(name != "West Region")%>%
  select(state, name, popestimate2019) %>%
  mutate(fips = as.numeric(state))%>%
  rename(pop = popestimate2019)

covid_state <- merge(nyt_state, population_state, by = "fips")%>%
  select(date = date, fips = fips, name = state.x, cases = cases, deaths = deaths, pop = pop)

#metric calculations
doubling_fun <- function(r, t){log(2)/(log(r/lag(r, order_by = t)))}

covid_state <- covid_state %>%
  mutate(deaths_case = (deaths/cases)*100)%>%
  mutate(deaths_pop = (deaths/pop)*100) %>%
  group_by(name)%>%
  mutate(doubling_time = as.numeric(doubling_fun(cases, date)))

covid_state$doubling_time[which(!is.finite(covid_state$doubling_time))] <- 0
covid_state[is.na(covid_state)] <-0

covid_state <- covid_state %>%
  group_by(name)%>%
  arrange(date)%>%
  mutate(doubling_ave = SMA(doubling_time, n =3))%>%
  arrange(fips)

#testing data
covid_testing_select <- covid_testing%>%
  mutate(datecorrect = ymd(as.Date(dateChecked)))%>%
  mutate(fips = as.numeric(fips))%>%
  select(datecorrect, state, fips, totalTestResults)

covidtesting_us <- covid_testing_select%>%
  group_by(datecorrect)%>%
  summarize(
    state = "US",
    fips = 00,
    totalTestResults = sum(totalTestResults)
  )

testing_us <- bind_rows(covidtesting_us, covid_testing_select)

testing_state <- merge(testing_us, population_state, by = "fips")%>%
  mutate(testing_pop = (totalTestResults/pop)*1000)%>%
  select(datecorrect = datecorrect, fips = fips, name = name, state = state.x, testing_pop = testing_pop)

#---------------------------------
#County Level Data Wrangling
#---------------------------------

nyt_county <- nyt_county%>%
  mutate(state = as.character(state))%>%
  mutate(county = as.character(county))

population_county <- population_county_all %>%
  filter(county != "000")%>%
  select(state, county, stname, ctyname, popestimate2019) %>%
  unite(fips, c(state, county), sep = "")%>%
  mutate(fips = as.numeric(fips))%>%
  rename(pop = popestimate2019)

covid_county <- merge(nyt_county, population_county, by = "fips")%>%
  select(date = date, fips = fips, name = stname, county = county, cases = cases, deaths = deaths, pop = pop)

#metric calculations
#doubling_fun <- function(r, t){log(2)/(log(r/lag(r, order_by = t)))}

covid_county <- covid_county %>%
  mutate(deaths_case = (deaths/cases)*100)%>%
  mutate(deaths_pop = (deaths/pop)*100) %>%
  group_by(fips)%>%
  arrange(date)%>%
  mutate(doubling_time = doubling_fun(cases, date))%>%
  arrange(fips)

covid_county$doubling_time[which(!is.finite(covid_county$doubling_time))] <- 0
covid_county[is.na(covid_county)] <-0

#---------------------------------
#Set dates
#---------------------------------
covid_state$date = as.Date(covid_state$date)
covid_county$date = as.Date(covid_county$date)
cv_min_date = as.Date(min(testing_state$datecorrect),"%Y-%m-%d")
current_date = as.Date(max(covid_state$date),"%Y-%m-%d")

current_nyt_date = max(covid_state$date)
current_ctp_date = max(testing_state$datecorrect)
reported_date = as.Date("2020-02-01","%Y-%m-%d")
plots_date = as.Date("2020-03-01","%Y-%m-%d")

#---------------------------------
#UI
#---------------------------------

ui <- navbarPage(
  title = strong("COVID-19: Evaluating Confirmed vs. Actual Cases"),
  windowTitle = "COVID-19 Dashboards",
  
  header = tagList(
    useShinydashboard(),
    setSliderColor(c("LightSalmon", "LightSalmon", "LightSalmon", "LightSalmon", "LightSalmon", "LightSalmon"), c(1, 2, 3, 4, 5, 6)),
    chooseSliderSkin("Flat"),
    use_theme(
      create_theme(
        theme = "default",
        bs_vars_navbar(
          default_bg = "#343E48",
          default_color = "#CDCDCD",
          default_link_color = "#CDCDCD",
          default_link_active_color = "#2F3740",
          default_link_active_bg = "#CDCDCD",
          default_link_hover_color = "CDCDCD"
        ),
        output_file = NULL
      )
    ),
    shinyDashboardThemes(theme = "grey_dark"),
    setBackgroundColor(color = "#2F3740")
  ),
  
  #---------------------------------
  #State Page
  #---------------------------------
  
  tabPanel(
    title = strong("State Dashboard"),
    fluidRow(
      box(width = 12,
          "This dashboard is intended to provide a set of potentially useful indicators related to the difference between “confirmed case” figures for COVID-19 in the United States and the actual number of infections. We examine four metrics that provide warning signs that actual infection rates substantially exceed the number of confirmed cases. Because each metric evaluates a different aspect of the spread of the disease, we conclude that states with more of these warning signs are likely to have a substantially wider gap between confirmed cases and actual infections than states with fewer.")),
    fluidRow(
      box(width = 3, height = 100,
          tags$style(".selectize-dropdown {background-color:#cdcdcd;}"),
          selectInput("state_select",
                      "Select a State:",
                      unique(covid_state$name),
                      selectize=TRUE,
                      selected = "United States"),
          status = "primary", solidHeader = FALSE
      ),
      box(width = 3, height = 100,
          descriptionBlock(
            header = textOutput("totalpostivecases"),
            text = strong("Confirmed Cases"), 
            right_border = FALSE,
            margin_bottom = TRUE)
      ),
      box(width = 3, height = 100,
          descriptionBlock(
            header = textOutput("totaldeaths"), 
            text = strong("Deaths"), 
            right_border = FALSE,
            margin_bottom = TRUE)
      ),
      box(width = 3, height = 100,
          sliderInput("plot_date",
                      "Mapping Date (def. current)",
                      min = as.Date(cv_min_date,"%Y-%m-%d"),
                      max = as.Date(current_date,"%Y-%m-%d"),
                      value = as.Date(current_date),
                      timeFormat = "%d %b")
      )
    ),
    
    fluidRow(
      column(width = 3,
             box(height = 400,
                 title = strong("Reported Confirmed Cases"), width = 12, solidHeader = TRUE,
                 plotlyOutput("positiveplot", 
                              height = 340
                 )),
             box(h6(em("Confirmed case numbers only reflect the number of patients who have tested positive for COVID-19; they do not include patients who have not been tested (or patients who are unaware they have the disease).")), width = 12),
             box(height = 400,
                 title = strong("Reported Deaths"), width = 12, solidHeader = TRUE,
                 plotlyOutput("deathsplot", 
                              height = 340
                 )),
             box(h6(em("Reported deaths only include patients who are reported to have died of COVID-19. These figures are generally assumed to be more accurate, although they may not reflect all deaths from the disease.")), width = 12)
      ),
      
      column(width = 6,
             fluidRow(
               box(width = 12, height = 500,
                   title = strong("Potential for Infection Rate in Excess of Confirmed Cases"), solidHeader = FALSE,
                   column(width = 12, leafletOutput("rankingplot", height = 440))),
               box(h6(em("Map colors indicate the number of warning sign metrics that are exceeded for each state (see explanation of warning signs in “Metrics Description” below).")), width = 12)
             ),
             fluidRow(
               column(width = 6,
                      box(width = 12, height = 300,
                          title = strong("Deaths as Percentage of Population"), solidHeader = TRUE, 
                          plotlyOutput("deathpopplot", height = 240)),
                      box(h6(em("The number of deaths as a percentage of the population. Because of the substantial lag time that typically exists between infection and the death of an infected patient, a higher fraction of deaths indicates that a higher percentage of the population was infected several weeks ago, and that infections may have spread more widely in the intervening period.")), width = 12)),
               column(width = 6,
                      box(width = 12, height = 300,
                          title = strong("Testing Effort in Proportion to Population"), solidHeader = TRUE,
                          plotlyOutput("testingpopplot", height = 240)),
                      box(h6(em("The number of tests conducted in the state as a proportion of state population. These numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient, but a higher level of testing effort would be expected to detect a larger number of confirmed cases as compared to states with limited testing.")), width = 12))
             )
      ),
      column(width = 3,
             box(title = strong("Confirmed Case Doubling Period"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 plotlyOutput("doublingrateplot",
                              height = 235)),
             box(h6(em("The time period for confimed cases to double (3-day moving average). Where confirmed case numbers are closer to actual infection numbers, the trend line would be expected to decline towards the expected value.")), width = 12),
             box(title = strong("Apparent Case Fatality Ratio"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 plotlyOutput("mortalityrateplot", 
                              height = 235)
             ),
             box(h6(em("The current ratio of total deaths to confirmed cases. If confirmed case numbers are closer to actual infection numbers, the trend line should approach the expected case fatality ratio over time.")), width = 12),
             box(height = 210, 
                 width = 12,
                 sliderInput("mortalityrate",
                             "Expected case fatality ratio (def. 0.9%)",
                             min = 0.5,
                             max = 4.1,
                             value = 0.9),
                 sliderInput("doublingrate",
                             "Expected doubling rate (def. 12 days)",
                             min = 5,
                             max = 15,
                             value = 12))
      )
    ),
    
    fluidRow(
      box(width = 5, 
          strong("Overview"), br(),
          br(),
          "The purpose of the dashboard is to provide a different view of existing, available data on the COVID-19 epidemic in the United States. At the time of the creation of this dashboard (March 29, 2020), the vast majority of public reporting on the COVID-19 pandemic in the United States has focused primarily on two values: (1) the number of confirmed cases; and (2) the number of confirmed deaths (which are displayed here for the convenience of the user). However, given the initial and ongoing limitations on testing for COVID-19 within the United States, together with the likely presence of large numbers of asymptomatic patients, it is clear that actual infection rates greatly exceed the number of confirmed cases.", br(),
          br(),
          "Just as importantly, state-by-state variation in levels of testing, testing availability and targeting, and test processing timelines mean that the relative distribution of COVID-19 infections within the United States almost certainly differs significantly from that suggested by confirmed case numbers. In many areas, health officials and professionals have been targeting scarce testing resources on patients with more severe symptoms, which will result in significant undercounting of infections in the community. Also, multiple tests are frequently conducted on a single patient, further limiting the number of infections that are detected.", br(),
          br(),
          "This dashboard is intended to point to several simple metrics that could suggest the presence (or absence) of infection rates that exceed the currently reported and confirmed cases in hopes of increasing the information available to local decision makers and the public. Where confirmed case data is substantially underestimating actual rates of infection, continued reliance on confirmed case data alone appears inadvisable when assessing the appropriateness of imposing or continuing local mitigation or suppression efforts.", br(),
          br(),
          "In addition to the reported number of confirmed cases and confirmed deaths, which are widely reported and available from other sources (see graphs on left), this dashboard provides four indicators or metrics, each with an associated “threshold value” (see graphs on right). These are provided at both the state and the county level (see tabs at top). If the threshold value is exceeded, this is tallied as a potential warning sign that the actual infection rate in either the state or county could significantly exceed the confirmed cases. The more thresholds that the state or county exceeds, the more warnings it accumulates (up to a total of four), as reflected in the color-coded map. Trend lines are also applied to some of the presented data to provide a sense of the direction of change of those metrics.", br(),
          br(),
          strong(textOutput("dataupdatetext"))
      ),
      box(width = 7,
          strong("Metrics Description"), br(),
          br(),
          strong("Confirmed Case Doubling Period:"), "The number of days it takes for the number of confirmed cases to double. Given the currently documented reproductive rate of SARS-Cov-19, epidemiological models suggest an expected doubling period of approximately 6 days in the absence of any mitigation or suppression efforts. Since most areas have been undertaking some level of mitigation/suppression for several weeks, the default expected doubling period has been set at 12 days in an effort to reflect a reasonable reduction in transmission rates. The slide bar provided allows users to select a different doubling period if desired.", em(strong("A doubling period that is faster than the expected value"))," (above the line on the graph, which has a reversed Y-axis)", em(strong("is a warning sign that the actual number of infections substaintially exceeds the number of confirmed cases.")), "Where the local doubling period is faster than expected, it could mean that: (a) testing is being restricted or targeted primarily at patients with a higher probability of infection (such as severe cases); or (b) the population being tested otherwise already has a high rate of infection; in either case, the number of actual cases is likely to be much higher than the number of confirmed cases. A doubling rate lower than the expected value suggests that the number of confirmed cases may be closer to the actual number of cases present. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the doubling period). A downward trend could reflect a more robust testing effort and/or beneficial effects of mitigation/suppression measures in limiting the growth in new cases. ", br(),
          br(),
          strong("Apparent Case Fatality Ratio:"), "The ratio of deaths attributed to COVID-19 to total confirmed cases. Current estimates of the case fatality ratio (CFR) for COVID-19 infections range widely but have been estimated by various experts at around 0.9 – 1.2% of infected patients, with an average period from infection-to-death at around 31 days. Because infections tend to spread during this intervening period, during the early phase of an epidemic the apparent CFR would normally be expected to be lower than the actual CFR, converging on the expected CFR over time.", em(strong("Accordingly a ratio higher than the expected CFR is a warning sign that the number of actual infections substantially exceeds the number of confirmed cases.")), "It should be noted, of course, that local conditions, such as the presence of relatively more vulnerable populations, limitations on medical care, or similar factors, may increase the CFR for a given population. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the apparent case fatality ratio).", br(),
          br(),
          strong("Deaths as Percentage of Population:")," The number of deaths as a percentage of state population, as compared to the national mean value.", em(strong("A death rate in the population that exceeds the national mean by at least one standard deviation is a warning sign that the actual number of infections significantly exceeds the number of confirmed cases.")), "Because of the substantial lag time that typically exists between initial infection and the death of an infected patient, a higher fraction of deaths likely indicates that a higher relative percentage of the local population was infected several weeks ago in comparison to other areas, and that infections may thus have spread more widely in the intervening period in the absence of effective suppression.", br(), 
          br(), 
          strong("Relative Testing Effort as Proportion of Population:"), "The cumulative number of tests conducted in a particular state as a proportion of the state population (county level testing data was unavailable). The U.S. testing effort has substantially lagged in comparison to other developed nations; in addition, there are currently broad differences between states in the availability of testing supplies and testing capacity, and the degree to which available tests are being targeted or restricted to particular patients or populations (many areas initially restricted and/or are continuing to target testing to patients with severe symptoms). Here,", em(strong("the warning sign is a testing rate that is lower than the national mean.")), "Lower testing rates, particularly in the absence of efforts to back-trace infections and the absence of broad mitigation and suppression controls, mean that local infection rates are likely to be far more widespread than confirmed case counts would suggest. Just as importantly, lower testing rates in one state as compared to others can substantially skew the apparent distribution of infections, since states with relatively high levels of testing effort (e.g. New York) will by definition result in a higher number of confirmed cases. Note that these numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient. However, a higher level of testing effort would be expected to detect a larger number of confirmed cases as compared to states with limited testing."
      )
    )
  ),
  
  #---------------------------------
  #County Page
  #---------------------------------
  
  tabPanel(
    title = strong("County Dashboard"),
    fluidRow(
      box(width = 12,
          "This dashboard is intended to provide a set of potentially useful indicators related to the difference between “confirmed case” figures for COVID-19 in the United States and the actual number of infections. We examine four metrics that provide warning signs that actual infection rates substantially exceed the number of confirmed cases. Because each metric evaluates a different aspect of the spread of COVID-19, we conclude that states with more of these warning signs are likely to have a substantially wider gap between confirmed cases and actual infections than states with fewer.")),
    fluidRow(
      box(width = 2, height = 100,
          htmlOutput("state_selector"),
          status = "primary", solidHeader = FALSE
      ),
      box(width = 2, height = 100,
          htmlOutput("county_selector"),
          status = "primary", solidHeader = FALSE
      ),
      box(width = 3, height = 100,
          descriptionBlock(
            header = textOutput("totalpostivecasescounty"),
            text = strong("Confirmed Cases"), 
            right_border = FALSE,
            margin_bottom = TRUE)
      ),
      box(width = 2, height = 100,
          descriptionBlock(
            header = textOutput("totaldeathscounty"), 
            text = strong("Deaths"), 
            right_border = FALSE,
            margin_bottom = TRUE)
      ),
      box(width = 3, height = 100,
          sliderInput("plot_date_county",
                      "Mapping Date (def. current)",
                      min = as.Date(cv_min_date,"%Y-%m-%d"),
                      max = as.Date(current_date,"%Y-%m-%d"),
                      value = as.Date(current_date),
                      timeFormat = "%d %b")
      )
      
    ),
    
    fluidRow(
      column(width = 3,
             box(height = 400,
                 title = strong("Reported Confirmed Cases"), width = 12, solidHeader = TRUE,
                 withSpinner(plotlyOutput("positiveplotcounty", 
                                          height = 340
                 ), type = 1, color = "#CDCDCD")),
             box(h6(em("Confirmed case numbers only reflect the number of patients who have tested positive for COVID-19; they do not include patients who have not been tested (or patients who are unaware they have the disease).")), width = 12),
             box(height = 400,
                 title = strong("Reported Deaths"), width = 12, solidHeader = TRUE,
                 withSpinner(plotlyOutput("deathsplotcounty", 
                                          height = 340), 
                             type = 1, color = "#CDCDCD")),
             box(h6(em("Reported deaths only include patients who are reported to have died of COVID-19. These figures are generally assumed to be more accurate, although they may not reflect all deaths from the disease.")), width = 12)
             
      ),
      column(width = 6,
             fluidRow(
               box(width = 12, height = 500,
                   title = strong("Potential for Infection Rate in Excess of Confirmed Cases/Social Vulnerability Index"),
                   tags$style(".leaflet-control-layers-expanded{background: #2F3740}"),
                   withSpinner(leafletOutput("rankingplotcounty", height = 440), 
                               type = 1, color = "#CDCDCD")),
               box(h6(em("Map colors indicate the number of warning sign metrics that are exceeded for each county (see explanation of warning signs in “Metrics Description” below)."), strong("Please note that some counties currently have inadequate data to be reliably evaluated."), em("By selecting “SVI Score,” the map will instead display the Social Vulnerability Index rating for each county, as calculated by CDC (the Social Vulnerability Index ranks counties on 15 social factors, including poverty, lack of vehicle access, and crowded housing conditions to identify populations that tend to be uniquely vulnerable to disease outbreaks and other emergency events).")), width = 12)),
             fluidRow(
               column(width = 6,
                      box(width = 12, height = 300,
                          title = strong("Deaths as Percentage of Population"), solidHeader = TRUE, 
                          withSpinner(plotlyOutput("deathpopplotcounty", height = 240),
                                      type = 1, color = "#CDCDCD")),
                      box(h6(em("The number of deaths as a percentage of the population. Because of the substantial lag time that typically exists between infection and the death of an infected patient, a higher fraction of deaths indicates that a higher percentage of the population was infected several weeks ago, and that infections may have spread more widely in the intervening period.")), width = 12)),
               column(width = 6,
                      box(width = 12, height = 300,
                          title = strong("Testing Effort in Proportion to Population"), solidHeader = TRUE,
                          withSpinner(plotlyOutput("testingpopplotcounty", height = 240),
                                      type = 1, color = "#CDCDCD")),
                      box(h6(em("The number of tests conducted in the state as a proportion of state population (county level testing data was unavailable). These numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient, but a higher level of testing effort would be expected to detect a larger number of confirmed cases as compared to states with limited testing.")), width = 12))
             )
      ),
      column(width = 3,
             box(title = strong("Confirmed Case Doubling Period"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 withSpinner(plotlyOutput("doubling_timeplotcounty", height = 235),
                             type = 1, color = "#CDCDCD")),
             box(h6(em("The time period for confimed cases to double (daily). Where confirmed case numbers are closer to actual infection numbers, the trend line would be expected to decline towards the expected value.")), width = 12),
             box(title = strong("Apparent Case Fatality Ratio"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 withSpinner(plotlyOutput("mortalityrateplotcounty", height = 235),
                             type = 1, color = "#CDCDCD")),
             box(h6(em("The current ratio of total deaths to confirmed cases. If confirmed case numbers are closer to actual infection numbers, the trend line should approach the expected case fatality ratio over time.")), width = 12),
             box(height = 210, 
                 width = 12,
                 sliderInput("mortalityratecounty",
                             "Expected case fatality ratio (def. 0.9%)",
                             min = 0.5,
                             max = 4.1,
                             value = 0.9),
                 sliderInput("doublingratecounty",
                             "Expected doubling rate (def. 12 days)",
                             min = 5,
                             max = 15,
                             value = 12)))
    ),
    
    fluidRow(
      box(width = 5, 
          strong("Overview"), br(),
          br(),
          "The purpose of the dashboard is to provide a different view of existing, available data on the COVID-19 epidemic in the United States. At the time of the creation of this dashboard (March 29, 2020), the vast majority of public reporting on the COVID-19 pandemic in the United States has focused primarily on two values: (1) the number of confirmed cases; and (2) the number of confirmed deaths (which are displayed here for the convenience of the user). However, given the initial and ongoing limitations on testing for COVID-19 within the United States, together with the likely presence of large numbers of asymptomatic patients, it is clear that actual infection rates greatly exceed the number of confirmed cases.", br(),
          br(),
          "Just as importantly, state-by-state variation in levels of testing, testing availability and targeting, and test processing timelines mean that the relative distribution of COVID-19 infections within the United States almost certainly differs significantly from that suggested by confirmed case numbers. In many areas, health officials and professionals have been targeting scarce testing resources on patients with more severe symptoms, which will result in significant undercounting of infections in the community. Also, multiple tests are frequently conducted on a single patient, further limiting the number of infections that are detected.", br(),
          br(),
          "This dashboard is intended to point to several simple metrics that could suggest the presence (or absence) of infection rates that exceed the currently reported and confirmed cases in hopes of increasing the information available to local decision makers and the public. Where confirmed case data is substantially underestimating actual rates of infection, continued reliance on confirmed case data alone appears inadvisable when assessing the appropriateness of imposing or continuing local mitigation or suppression efforts.", br(),
          br(),
          "In addition to the reported number of confirmed cases and confirmed deaths, which are widely reported and available from other sources (see graphs on left), this dashboard provides four indicators or metrics, each with an associated “threshold value” (see graphs on right). These are provided at both the state and the county level (see tabs at top). If the threshold value is exceeded, this is tallied as a potential warning sign that the actual infection rate in either the state or county could significantly exceed the confirmed cases. The more thresholds that the state or county exceeds, the more warnings it accumulates (up to a total of four), as reflected in the color-coded map. Trend lines are also applied to some of the presented data to provide a sense of the direction of change of those metrics.", br(),
          br()
      ),
      box(width = 7,
          strong("Metrics Description"), br(),
          br(),
          strong("Confirmed Case Doubling Period:"), "The number of days it takes for the number of confirmed cases to double. Given the currently documented reproductive rate of SARS-Cov-19, epidemiological models suggest an expected doubling period of approximately 6 days in the absence of any mitigation or suppression efforts. Since most areas have been undertaking some level of mitigation/suppression for several weeks, the default expected doubling period has been set at 12 days in an effort to reflect a reasonable reduction in transmission rates. The slide bar provided allows users to select a different doubling period if desired.", em(strong("A doubling period that is faster than the expected value"))," (above the line on the graph, which has a reversed Y-axis)", em(strong("is a warning sign that the actual number of infections substaintially exceeds the number of confirmed cases.")), "Where the local doubling period is faster than expected, it could mean that: (a) testing is being restricted or targeted primarily at patients with a higher probability of infection (such as severe cases); or (b) the population being tested otherwise already has a high rate of infection; in either case, the number of actual cases is likely to be much higher than the number of confirmed cases. A doubling rate lower than the expected value suggests that the number of confirmed cases may be closer to the actual number of cases present. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the doubling period). A downward trend could reflect a more robust testing effort and/or beneficial effects of mitigation/suppression measures in limiting the growth in new cases. ", br(),
          br(),
          strong("Apparent Case Fatality Ratio:"), "The ratio of deaths attributed to COVID-19 to total confirmed cases. Current estimates of the case fatality ratio (CFR) for COVID-19 infections range widely but have been estimated by various experts at around 0.9 – 1.2% of infected patients, with an average period from infection-to-death at around 31 days. Because infections tend to spread during this intervening period, during the early phase of an epidemic the apparent CFR would normally be expected to be lower than the actual CFR, converging on the expected CFR over time.", em(strong("Accordingly a ratio higher than the expected CFR is a warning sign that the number of actual infections substantially exceeds the number of confirmed cases.")), "It should be noted, of course, that local conditions, such as the presence of relatively more vulnerable populations, limitations on medical care, or similar factors, may increase the CFR for a given population. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the apparent case fatality ratio).", br(),
          br(),
          strong("Deaths as Percentage of Population:")," The number of deaths as a percentage of state population, as compared to the national mean value.", em(strong("A death rate in the population that exceeds the national mean by at least one standard deviation is a warning sign that the actual number of infections significantly exceeds the number of confirmed cases.")), "Because of the substantial lag time that typically exists between initial infection and the death of an infected patient, a higher fraction of deaths likely indicates that a higher relative percentage of the local population was infected several weeks ago in comparison to other areas, and that infections may thus have spread more widely in the intervening period in the absence of effective suppression.", br(), 
          br(), 
          strong("Relative Testing Effort as Proportion of Population:"), "The cumulative number of tests conducted in a particular state as a proportion of the state population (county level testing data was unavailable). The U.S. testing effort has substantially lagged in comparison to other developed nations; in addition, there are currently broad differences between states in the availability of testing supplies and testing capacity, and the degree to which available tests are being targeted or restricted to particular patients or populations (many areas initially restricted and/or are continuing to target testing to patients with severe symptoms). Here,", em(strong("the warning sign is a testing rate that is lower than the national mean.")), "Lower testing rates, particularly in the absence of efforts to back-trace infections and the absence of broad mitigation and suppression controls, mean that local infection rates are likely to be far more widespread than confirmed case counts would suggest. Just as importantly, lower testing rates in one state as compared to others can substantially skew the apparent distribution of infections, since states with relatively high levels of testing effort (e.g. New York) will by definition result in a higher number of confirmed cases. Note that these numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient. However, a higher level of testing effort would be expected to detect a larger number of confirmed cases as compared to states with limited testing."
      )
    )
  ),
  
  tabPanel(
    title = strong("About"),
    fluidRow(
      column(width = 8,
             box(width = 12,
                 "This dashboard has been produced by professionals with experience in data visualization and scenario planning for public policy analysis and decision making.", em(strong("We do not have medical expertise or formal training in epidemiology.")), "The information provided here is based solely on a mathematical analysis of data from other sources,", em(strong("is not intended to replace or supersede authoritative information from any official or medical source.")), br(),
                 br(),
                 "We have created this dashboard based on a growing concern about the nature of the information being used to guide public attitudes, behaviors, and state and local responses to the COVID-19 epidemic within the United States. In an effort to increase the transparency of current data about the likely extent of COVID-19 infections, this dashboard undertakes some simple mathematical analysis of available data about confirmed COVID-19 cases, testing, and death rates in the U.S., and presents this information together with data from the Centers for Disease Control’s “Social Vulnerability Index.” As noted in the “Overview” section on each dashboard page, the metrics used are intended to identify potential “warning signs” that the number of infections in a particular state or county are substantially greater than the current “confirmed case” count might suggest.", br(),
                 br(),
                 "As of the date of creation of this dashboard (April 4, 2020),", em(strong("it has been clear for many weeks that the rate of COVID-19 infection in the United States is substantially greater than reflected in the “confirmed case” counts")), "that are the most widely reported measure for tracking the course of the pandemic in the U.S.  There are many reasons for this, including the likely presence of large numbers of asymptomatic patients in the population. However, the largest single cause is undoubtedly the initial unavailability and continued inadequacy of testing in the United States. In most areas, given the limited number of tests and the very long wait times for test results, health officials and medical professionals have directed (and in most cases are continuing to direct) the limited testing available to diagnose only relatively severe suspected cases of COVID-19.", br(),
                 br(),
                 "An inevitable consequence of the lack of available testing, the narrow targeting of testing resources, and the presence of asymptomatic carriers is that", em(strong("the “confirmed case” count in the United States is currently failing to capture many, if not the vast majority, of actual infections.")), "Many areas that currently show only a small number of “confirmed cases” (or no cases) are thus likely to have many more cases than they are presently aware of.", br(),
                 br(),
                 "Nevertheless, many decision makers and members of the public are continuing to focus on reported “confirmed cases” as a benchmark for understanding the severity of the problem they face in their communities. This, in turn, may be guiding the urgency, nature, and extent of local mitigation/suppression efforts – such as social distancing behaviors and requirements, closure of public venues, and required or voluntary business closures and telework mandates – and the level of effort being exerted to prepare local health care facilities to handle COVID-19 patients.", em(strong("In this context, reliance on “confirmed case” information is potentially dangerous, and may result in those areas suffering substantially higher rates of disease and death over the coming weeks and months.")), br(),
                 br(),
                 "There are also many examples of communities attempting to (officially or unofficially) exclude or “quarantine” people traveling from “hot spots” elsewhere in the United States. In some cases, these measures may be informed by a belief that there are currently low levels of local infection, and excluding outsiders will therefore prevent further spread of COVID-19. Setting aside any legal or moral implications, since the local “confirmed case” data is likely undercounting actual rates of infection, these measures may be misguided and create a false sense of security. Available information indicates that COVID-19 has been circulating in the United States for much longer than initially reported, and that the infection is already extraordinarily widespread in the United States.", br(),
                 br(),
                 "At present, the national response policy of the United States appears to depend almost entirely on the implementation of state and local response measures. To the extent that the implementation of these state and local measures are being guided by the number of confirmed cases in particular areas,", em(strong("the failure to understand the difference between “confirmed cases” and the actual potential extent of infection is likely to lead to failure of both those local responses and the U.S. national response as a whole.")), br(),
                 br(),
                 "As noted above, we have no expertise in epidemiology and this is not intended to replace any official source of information. However, our intent in creating this dashboard was to provide a different perspective on existing data in a manner that could better inform local understanding about the potential undercounting of COVID-19 infections. We welcome any feedback from users – and particularly experts and decision makers – on how this tool could be improved, and whether it is useful. "
             )
      ),
      column(width = 4,
             box(width = 12,
                 strong("Data Sources"), br(),
                 br(),
                 strong("COVID-19 Confirmed Cases and Deaths "), br(),
                 "Data on confirmed cases and deaths was obtained from:", br(),
                 a(href = "https://github.com/nytimes/covid-19-data", "The New York Times"),br(),
                 "Updated on:", current_nyt_date, br(),
                 br(),
                 
                 strong("COVID-19 Testing"), br(),
                 "Data on COVID-19 testing was obtained from:", br(),
                 a(href = "https://covidtracking.com/", "The COVID Tracking Project"),br(),
                 "Updated on:", current_ctp_date, br(),
                 br(),
                 
                 strong("State Population"), br(),
                 "State 2019 estimated poulation was obtained from:", br(),
                 a(href = "https://www.census.gov/newsroom/press-kits/2020/pop-estimates-county-metro.html", "United States Census: County Population Estimates"),br(),
                 em("County Population Totals: 2010-2019; Datasets, Population, Population Change, and Estimated Components of Population Change: April 1, 2010 to July 1, 2019 (CO-EST2019-alldata.csv)"), br(),
                 "Accessed on: April 4, 2020", br(),
                 br(),
                 
                 strong("County Population"), br(),
                 "County 2019 estimated poulation was obtained from:", br(),
                 a(href = "https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage", "United States Census: State Population Estimates"),br(),
                 em("Nation, States, and Puerto Rico Population; Datasets; Population, Population Change, and Estimated Components of Population Change: April 1, 2010 to July 1, 2019 (NST-EST2019-alldata.csv)"), br(),
                 "Accessed on: April 4, 2020", br(),
                 br(),
                 
                 strong("Social Vulnerability Index"), br(),
                 "CDC Social Vulnerability Index data was obtained from:", br(),
                 a(href = "https://svi.cdc.gov/data-and-tools-download.html", "Social Vulnerability Index 2018 Database US"), br(),
                 em("Centers for Disease Control and Prevention/ Agency for Toxic Substances and Disease Registry/ Geospatial Research, Analysis, and Services Program."),br(),
                 "Accessed on: April 4, 2020"
             ),
             box(width = 12,
                 strong("About the Authors"), br(),
                 h6("The COVID Infection Metrics analysis was developed by Season Martin and Peter Culp. RShiny application was developed by Season Martin. For inquries about this dashboard, the methods or intended use please", a(href="mailto:covidmetrics@gmail.com?subject=COVID-19 Metrics", "contact us."), "Code avaliable on:", a(href="https://github.com/seasonlmartin/covidmetrics", "GitHub") 
                 ))
      )
    )
  )
)


#---------------------------------
#Server
#---------------------------------

server <- function(input, output) {
  
  cvpaperbgcolor <- 'rgb(52,62,72)'
  cvplotbgcolor <- cvpaperbgcolor
  cvlinecolor <- 'rgb(205,205,205)'
  cvplotfont <- list(color = cvlinecolor)
  
  #---------------------------------
  #State Graphics
  #---------------------------------
  
  state_selected <- reactive({
    covid_state%>%
      filter(name==input$state_select)%>%
      arrange(date)
  })
  
  summary_numbers <- reactive({
    state_selected()%>%
      filter(date==input$plot_date)
  })
  
  output$totalpostivecases <- renderText({
    paste(format(summary_numbers()$cases,
                 big.mark = ",", trim = TRUE, scientific = FALSE))
  })
  
  output$totaldeaths <- renderText({
    paste(format(summary_numbers()$deaths,
                 big.mark = ",", trim = TRUE, scientific = FALSE))
  })
  
  output$dataupdatetext <- renderText({ 
    paste("Data Souces outlined on the About page. Updated:", current_nyt_date)
  })
  
  testing_selected <- reactive({
    testing_state%>%
      filter(name==input$state_select)
  })
  
  output$positiveplot <- renderPlotly({
    
    plot_ly(state_selected(), 
            x=~date, 
            y=~cases, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = "%{y:,.0f}<extra>%{text}</extra>",
            line = list(color = '#93648D', width = 3),
            fill = 'tozeroy',
            fillcolor = '#93648D')%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          zeroline = FALSE,
          range = c(reported_date, current_date)),
        yaxis = list(
          title = 'Confirmed Cases',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE),
        paper_bgcolor = cvpaperbgcolor,
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont)
  })
  
  output$deathsplot <- renderPlotly({
    
    plot_ly(state_selected(), 
            x=~date, 
            y=~deaths, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = '#FFC65D', width = 3),
            fill = 'tozeroy',
            fillcolor = '#FFC65D')%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          zeroline = FALSE,
          range = c(reported_date, current_date)),
        yaxis = list(
          title = 'Deaths',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont)
  })
  
  output$doublingrateplot <- renderPlotly({
    
    state_selected_doubling <- state_selected()%>%
      filter(doubling_ave != 0)
    
    dr_lm = lm(doubling_ave~date, state_selected_doubling)
    
    plot_ly(data = state_selected_doubling,
            type = 'scatter',
            mode = 'markers')%>%
      add_markers(x=~date, 
                  y=~as.numeric(doubling_ave),
                  name = "Doubling Period (moving average)",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.2f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = '#7BC8A4'))%>%
      add_lines(x=~date,
                y=~predict(dr_lm),
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.2f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#7BC8A4'))%>%
      add_lines(x=~date,
                y=input$doublingrate,
                name = "Expected Value (should be below)",
                text = "Expected value",
                hovertemplate = paste("%{x}: %{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          zeroline = FALSE,
          range = c(plots_date, current_date)),
        yaxis = list(
          title = 'Doubling period <br> (Days)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(15, 0)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))
  })
  
  output$mortalityrateplot <- renderPlotly({
    
    state_selected_mortality <- state_selected()%>%
      filter(deaths_case != 0)
    
    mr_smooth = supsmu(state_selected()$date, state_selected()$deaths_case)
    
    plot_ly(data = state_selected_mortality,
            type = 'scatter',
            mode = 'markers')%>%
      add_markers(x=~date, 
                  y=~deaths_case,
                  name = "Fatality Ratio",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.2f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = '#4cc3d9'))%>%
      add_lines(data = state_selected(),
                x=~date,
                y=~mr_smooth$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.2f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#4cc3d9'))%>%
      add_lines(x=~date,
                y=input$mortalityrate,
                name = "Expected Value (should be below)",
                text = "Expected value",
                hovertemplate = paste("%{x}: %{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          zeroline = FALSE,
          range = c(plots_date, current_date)),
        yaxis = list(
          title = 'Fatalities',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))
  })
  
  current_death_pop <- reactive({
    covid_state%>%
      filter(date==input$plot_date)
  })
  
  death_stats <- reactive({
    covid_state%>%
      group_by(date)%>%
      summarise(
        meandeath = mean(deaths),
        sddeath = sd(deaths),
        meandeath_pop = mean(deaths_pop),
        sddeath_pop = sd(deaths_pop))%>%
      filter(date==input$plot_date)
  })
  
  deathpopselect <- reactive({
    covid_state%>%
      filter(date==input$plot_date)%>%
      filter(name==input$state_select)
  })
  
  output$deathpopplot <- renderPlotly({
    
    plot_ly(data = covid_state,
            type = "scatter",
            mode = "lines")%>%
      add_lines(
        x=~date, 
        y=~deaths_pop,
        name = "Other States",
        text = ~name,
        hovertemplate = paste("%{x}: %{y:,.5f}",
                              "<extra>%{text}</extra>"),
        line = list(color = '#bfbfbf'))%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          range = c(plots_date, current_date),
          zeroline = FALSE),
        yaxis = list(
          title = 'Deaths <br> (% of Population)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))%>%
      add_lines(data = state_selected(),
                x=~date, 
                y=~deaths_pop, 
                name = "Selected State",
                text = ~name,
                hovertemplate = paste("%{x}: %{y:,.5f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = 'F16745')
      )
    
  })
  
  output$testingpopplot <- renderPlotly({
    
    testmax <- max(testing_state$testing_pop)
    
    testing_pop_stats <- 
      testing_state%>%
      group_by(datecorrect)%>%
      summarise(
        meantesting_pop = mean(testing_pop),
        sdtesting_pop = sd(testing_pop))
    
    plot_ly(data = testing_pop_stats,
            type = "scatter",
            mode = "lines")%>%
      add_lines(
        x=~datecorrect, 
        y=~meantesting_pop,
        name = "Mean (national)",
        text = "Mean",
        hovertemplate = paste("%{x}: %{y:,.4f}",
                              "<extra>%{text}</extra>"),
        line = list(color = '#bfbfbf'))%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          range = c(plots_date, current_date),
          zeroline = FALSE),
        yaxis = list(
          title = 'Tests <br> (Per 1000 Population)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0,testmax)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))%>%
      add_lines(data = testing_selected(),
                x=~datecorrect, 
                y=~testing_pop, 
                name = "Selected State",
                text = ~name,
                hovertemplate = paste("%{x}: %{y:,.4f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = 'F16745')
      )
    
  })
  
  testing_stats <- reactive({
    testing_state%>%
      group_by(datecorrect)%>%
      summarise(
        meantesting_pop = mean(testing_pop),
        sdtesting_pop = sd(testing_pop))%>%
      filter(datecorrect==input$plot_date)
  })
  
  ranking <- reactive({
    death_pop_mean <- death_stats()$meandeath_pop
    death_pop_sd <- death_stats()$sddeath_pop
    testing_pop_mean <- testing_stats()$meantesting_pop
    testing_pop_sd <- testing_stats()$sdtesting_pop
    
    covid_rank <- covid_state%>%
      filter(date==input$plot_date) %>%
      summarize(
        exceeddoubling = ifelse(doubling_time<=input$doublingrate,1,0),
        exceedmortality = ifelse(deaths_case>=input$mortalityrate,1,0),
        exceedpopulation = ifelse(deaths_pop>=(death_pop_mean+death_pop_sd),1,0),
        rank = exceeddoubling+exceedmortality+exceedpopulation)
    
    testing_rank <- testing_state%>%
      filter(datecorrect==input$plot_date) %>%
      mutate(
        exceedtesting = ifelse(testing_pop<=(testing_pop_mean),1,0))%>%
      select(name, state, exceedtesting)
    
    merge(covid_rank, testing_rank, by = "name")%>%
      mutate(rank = exceeddoubling+exceedmortality+exceedpopulation+exceedtesting)%>%
      filter(state != "US")%>%
      mutate(state_name = name)%>%
      mutate(rank = paste("Warnings:", rank))%>%
      mutate(rank = as.factor(rank))
    
  })    
  
  output$rankingplot <- renderLeaflet({
    
    spdf <- usa_sf()
    
    ranking_state <- ranking() %>%
      mutate(name = as.factor(name))
    
    state_leaf_rank <- merge(spdf, ranking_state, by = "name")%>%
      select(name, rank)%>%
      st_transform(crs= 4326)
    
    pal_red <- colorFactor(
      palette = c("#ffc65d", "#de9942", "#bb6d2b", "#964417", "#701b04"),
      levels = c("Warnings: 0", "Warnings: 1", "Warnings: 2", "Warnings: 3", "Warnings: 4"),
    )
    
    fitbounds <- c(-120, 24, -69, 43)
    maxbounds <- c(-130, 18, -55, 45)
    
    leaflet(
      options =
        leafletOptions(
          worldCopyJump = FALSE,
          crs = leafletCRS(
            crsClass = "L.Proj.CRS",
            code = "EPSG:4326",
            proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
            resolutions = c(65536, 32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128)
          ))
    ) %>%
      fitBounds(fitbounds[1], fitbounds[2], fitbounds[3], fitbounds[4]) %>%
      setMaxBounds(maxbounds[1], maxbounds[2], maxbounds[3], maxbounds[4]) %>%
      addPolygons(data = spdf, 
                  stroke = TRUE, 
                  weight = 1, 
                  color = "#ffffff", 
                  opacity = 1,
                  fill = TRUE, 
                  label = ~stringr::str_c(name),
                  labelOptions = labelOptions(direction = "auto")) %>%
      addPolygons(data = state_leaf_rank, 
                  stroke = TRUE, 
                  weight = 1, 
                  color = "#ffffff", 
                  opacity = 1,
                  fill = TRUE, 
                  fillColor = ~pal_red(rank), 
                  fillOpacity = 1,
                  label = ~stringr::str_c(name),
                  labelOptions = labelOptions(direction = "auto")
      ) %>%
      setMapWidgetStyle(list(background= "#343E48"))%>%
      addLegend(data = state_leaf_rank, pal = pal_red, values = ~rank, opacity = 1, "bottomright", title = "Warnings")
    
  })
  
  #---------------------------------
  #County Graphics
  #---------------------------------
  
  output$state_selector = renderUI({ 
    selectInput(inputId = "state", 
                label = "Select a State:", 
                choices = unique(covid_county$name),
                selected = "Alabama") 
  })
  
  output$county_selector = renderUI({
    
    data_available = covid_county[covid_county$name == input$state, "county"]
    
    selectInput(inputId = "county", #name of input
                label = "Select a County:", #label displayed in ui
                choices = unique(data_available),
                selected = unique(data_available)[1])
  })
  
  county_selected <- reactive({
    covid_county%>%
      filter(name ==input$state)%>%
      filter(county==input$county)%>%
      arrange(date)
  })
  
  county_state_selected <- reactive({
    covid_county%>%
      filter(name==input$state)%>%
      arrange(date)
  })
  
  summary_numbers_county <- reactive({
    county_selected()%>%
      filter(date==input$plot_date_county)
  })
  
  output$totalpostivecasescounty <- renderText({
    paste(format(summary_numbers_county()$cases,
                 big.mark = ",", trim = TRUE, scientific = FALSE))
  })
  
  output$totaldeathscounty <- renderText({
    paste(format(summary_numbers_county()$deaths,
                 big.mark = ",", trim = TRUE, scientific = FALSE))
  })
  
  output$dataupdatetextcounty <- renderText({ 
    paste("Data Souces outlined on the About page. Updated:", current_nyt_date)
  })
  
  testing_selected_county <- reactive({
    testing_state%>%
      filter(name==input$state)
  })
  
  output$positiveplotcounty <- renderPlotly({
    
    plot_ly(county_selected(), 
            x=~date, 
            y=~cases, 
            type = 'scatter',
            mode = 'lines',
            text = ~county,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = '#93648D', width = 3),
            fill = 'tozeroy',
            fillcolor = '#93648D')%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          zeroline = FALSE,
          range = c(reported_date,current_date)),
        yaxis = list(
          title = 'Confirmed Cases',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE),
        paper_bgcolor = cvpaperbgcolor,
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont)
  })
  
  output$deathsplotcounty <- renderPlotly({
    
    plot_ly(county_selected(), 
            x=~date, 
            y=~deaths, 
            type = 'scatter',
            mode = 'lines',
            text = ~county,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = '#FFC65D', width = 3),
            fill = 'tozeroy',
            fillcolor = '#FFC65D')%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          zeroline = FALSE,
          range = c(reported_date, current_date)),
        yaxis = list(
          title = 'Deaths',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont)
  })
  
  output$doubling_timeplotcounty <- renderPlotly({
    
    county_selected_doubling <- county_selected()%>%
      filter(doubling_time > 0)

    dr_smooth_county = supsmu(county_selected()$date, county_selected()$doubling_time)
    
    plot_ly(data = county_selected_doubling,
            type = 'scatter',
            mode = 'markers')%>%
      add_markers(x=~date, 
                  y=~doubling_time,
                  name = "Doubling Period (daily)",
                  text = ~county,
                  hovertemplate = paste("%{x}: %{y:,.2f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = '#7BC8A4'))%>%
      add_lines(data = county_selected(),
                x=~date,
                y=~dr_smooth_county$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.2f}",
                                      "<extra>%{text}</extra>"),
                
                line = list(color = '#7BC8A4'))%>%
      add_lines(x=~date,
                y=input$doublingratecounty,
                name = "Expected Value (should be below)",
                text = "Expected value",
                hovertemplate = paste("%{x}: %{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          zeroline = FALSE,
          range = c(plots_date, current_date)),
        yaxis = list(
          title = 'Doubling period <br> (Days)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(15, 0)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))
  })
  
  output$mortalityrateplotcounty <- renderPlotly({
    
    county_selected_mortality <- county_selected()%>%
      filter(deaths_case != 0)

    mr_smooth_county = supsmu(county_selected()$date, county_selected()$deaths_case)
    
    plot_ly(data = county_selected_mortality,
            type = 'scatter',
            mode = 'markers')%>%
      add_markers(x=~date, 
                  y=~deaths_case,
                  name = "Fatality Ratio",
                  text = ~county,
                  hovertemplate = paste("%{x}: %{y:,.2f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = '#4cc3d9'))%>%
      add_lines(data = county_selected(),
                x=~date,
                y=~mr_smooth_county$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.2f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#4cc3d9'))%>%
      add_lines(x=~date,
                y=input$mortalityratecounty,
                name = "Expected Value (should be below)",
                text = "Expected value",
                hovertemplate = paste("%{x}: %{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          zeroline = FALSE,
          range = c(plots_date, current_date)),
        yaxis = list(
          title = 'Fatalities',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))
  })
  
  output$deathpopplotcounty <- renderPlotly({
    
    plot_ly(data = county_state_selected(),
            type = "scatter",
            mode = "lines")%>%
      add_lines(
        x=~date, 
        y=~deaths_pop,
        name = "Other Counties",
        text = ~county,
        hovertemplate = paste("%{x}: %{y:,.5f}",
                              "<extra>%{text}</extra>"),
        line = list(color = '#bfbfbf'))%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          range = c(as.Date("2020-03-01"), current_date),
          zeroline = FALSE,
          range = c(plots_date, current_date)),
        yaxis = list(
          title = 'Deaths <br> (% of Population)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))%>%
      add_lines(data = county_selected(),
                x=~date, 
                y=~deaths_pop, 
                name = "Selected County",
                text = ~county,
                hovertemplate = paste("%{x}: %{y:,.5f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = 'F16745')
      )
    
  })
  
  output$testingpopplotcounty <- renderPlotly({
    
    testmax <- max(testing_state$testing_pop)
    
    testing_pop_stats <- 
      testing_state%>%
      group_by(datecorrect)%>%
      summarise(
        meantesting_pop = mean(testing_pop),
        sdtesting_pop = sd(testing_pop))
    
    plot_ly(data = testing_pop_stats,
            type = "scatter",
            mode = "lines")%>%
      add_lines(
        x=~datecorrect, 
        y=~meantesting_pop,
        name = "Mean (national)",
        text = "Mean",
        hovertemplate = paste("%{x}: %{y:,.4f}",
                              "<extra>%{text}</extra>"),
        line = list(color = '#bfbfbf'))%>%
      layout(
        xaxis = list(
          title = '',
          type = 'date',
          tickformat = '%m/%d',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          tickangle = 0,
          nticks = 4,
          range = c(plots_date, current_date),
          zeroline = FALSE),
        yaxis = list(
          title = 'Tests <br> (Per 1000 Population)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0,testmax)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))%>%
      add_lines(data = testing_selected_county(),
                x=~datecorrect, 
                y=~testing_pop, 
                name = "Selected State (county data not avaliable)",
                text = ~name,
                hovertemplate = paste("%{x}: %{y:,.4f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = 'F16745')
      )
    
  })
  
  death_statscounty <- reactive({
    covid_county%>%
      group_by(date)%>%
      summarise(
        meandeath = mean(deaths),
        sddeath = sd(deaths),
        meandeath_pop = mean(deaths_pop),
        sddeath_pop = sd(deaths_pop))%>%
      filter(date==input$plot_date_county)
  })
  
  rankingcounty <- reactive({
    death_pop_mean_county <- death_statscounty()$meandeath_pop
    death_pop_sd_county <- death_statscounty()$sddeath_pop
    testing_pop_mean <- testing_stats()$meantesting_pop
    testing_pop_sd <- testing_stats()$sdtesting_pop
    
    covid_rank_county <- covid_county%>%
      filter(date==input$plot_date_county) %>%
      mutate(
        exceeddoubling = ifelse(doubling_time<=input$doublingrate,1,0),
        exceedmortality = ifelse(deaths_case>=input$mortalityrate,1,0),
        exceedpopulation = ifelse(deaths_pop>=(death_pop_mean_county+death_pop_sd_county),1,0))%>%
      select(name, county, fips, exceeddoubling, exceedmortality, exceedpopulation)
    
    testing_rank_county <- testing_state%>%
      filter(datecorrect==input$plot_date_county) %>%
      mutate(
        exceedtesting = ifelse(testing_pop<=(testing_pop_mean),1,0))%>%
      select(name, state, exceedtesting)
    
    merge(covid_rank_county, testing_rank_county, by = "name")%>%
      mutate(rank = exceeddoubling+exceedmortality+exceedpopulation+exceedtesting)%>%
      filter(state != "US")%>%
      mutate(county_fips = fips)%>%
      mutate(rank = paste("Warnings: ", rank))%>%
      mutate(rank = as.factor(rank))
    
  })    
  
  output$rankingplotcounty <- renderLeaflet({
    
    state <- usa_sf()
    ctdf <- counties_sf()
    
    county <- ctdf%>%
      select(name, fips)%>%
      mutate(fips_2 = as.character(fips))%>%
      mutate(county_fips = as.numeric(fips_2))%>%
      mutate(county_name = as.character(name))%>%
      filter(county_fips != 35013)
    
    county_rank <- left_join(county, rankingcounty(), by = "county_fips")
    
    county_svi_map <- left_join(county, svi_select, by = "county_fips")
    
    pal_red_county <- colorFactor(
      palette = c("#ffc65d", "#de9942", "#bb6d2b", "#964417", "#701b04"),
      levels = c("Warnings:  0", "Warnings:  1", "Warnings:  2", "Warnings:  3", "Warnings:  4")
    )
    
    pal_svi <- colorFactor(
      palette = c("#EBEBC2", "#A1C8AA", "#61A9B5", "#365C99"),
      levels = c("0-0.25", "0.25-0.5", "0.5-0.75", "0.75-1")
    )
    
    fitbounds <- c(-120, 24, -69, 43)
    maxbounds <- c(-130, 18, -55, 45)
    
    leaflet(
      options =
        leafletOptions(
          worldCopyJump = FALSE,
          crs = leafletCRS(
            crsClass = "L.Proj.CRS",
            code = "EPSG:4326",
            proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
            resolutions = c(65536, 32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128)
          ),
          zoomControl = FALSE)) %>%
      fitBounds(fitbounds[1], fitbounds[2], fitbounds[3], fitbounds[4]) %>%
      setMaxBounds(maxbounds[1], maxbounds[2], maxbounds[3], maxbounds[4]) %>%
      addPolygons( 
        data = county_rank, 
        stroke = TRUE, 
        weight = .2, 
        color = "#ffffff", 
        opacity = 1,
        fill = TRUE, 
        fillColor = ~pal_red_county(rank), 
        fillOpacity = 1,
        label = ~stringr::str_c(county_name),
        labelOptions = labelOptions(direction = "auto"),
        group = "Warnings"
      ) %>%
      addPolygons(
        data = county_svi_map, 
        stroke = TRUE, 
        weight = .2, 
        color = "#ffffff", 
        opacity = 1,
        fill = TRUE, 
        fillColor = ~pal_svi(svi_score_map), 
        fillOpacity = 1,
        label = ~stringr::str_c(county),
        labelOptions = labelOptions(direction = "auto"),
        group = "SVI Score"
      ) %>%
      addPolygons(data = state, 
                  stroke = TRUE, 
                  weight = 1, 
                  color = "#ffffff", 
                  opacity = 1,
                  fill = FALSE,
                  group = "States")%>%
      setMapWidgetStyle(list(background= "#343E48"))%>%
      addLegend(data = county_rank, 
                pal = pal_red_county, 
                values = ~rank, 
                opacity = 1, 
                "bottomright", 
                title = "Warnings", 
                group = "Warnings") %>%
      addLegend(data = county_svi_map, 
                pal = pal_svi, 
                values = ~svi_score_map, 
                opacity = 1, "bottomright", 
                title = "SVI Score", 
                group = "SVI Score") %>%
      addLayersControl(
        baseGroups = c("Warnings", "SVI Score"),
        options = layersControlOptions(collapsed = FALSE),
        overlayGroups = "States",
        position = c("bottomright"))%>%
      htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
        }
                            ")%>%
    htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }")

  })
  
}

#---------------------------------
#Run Application
#---------------------------------
shinyApp(ui = ui, server = server)

