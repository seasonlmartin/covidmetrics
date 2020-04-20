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
library(leaftime)

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
  group_by(name)%>%
  mutate(doubling_time = as.numeric(doubling_fun(cases, date)))

covid_state$doubling_time[which(!is.finite(covid_state$doubling_time))] <- 0
covid_state[is.na(covid_state)] <-0

covid_state <- covid_state %>%
  group_by(name)%>%
  arrange(date)%>%
  mutate(doubling_ave = SMA(doubling_time, n =3))%>%
  arrange(fips)

covid_state[is.na(covid_state)] <-0

#testing data
covid_testing_select <- covid_testing%>%
  mutate(datecorrect = ymd(as.Date(dateChecked)))%>%
  mutate(fips = as.numeric(fips))%>%
  select(datecorrect, state, fips, totalTestResults, positive, totalTestResultsIncrease, positiveIncrease)
covid_testing_select[is.na(covid_testing_select)] <-0

covidtesting_us <- covid_testing_select%>%
  group_by(datecorrect)%>%
  summarize(
    state = "US",
    fips = 00,
    totalTestResults = sum(totalTestResults),
    positive = sum(positive),
    totalTestResultsIncrease = sum(totalTestResultsIncrease),
    positiveIncrease = sum(positiveIncrease)
  )

testing_us <- bind_rows(covidtesting_us, covid_testing_select)

testing_state <- merge(testing_us, population_state, by = "fips")%>%
  mutate(testing_daily_pop = (totalTestResultsIncrease/pop)*100000)%>%
  mutate(pos_rate_daily = (positiveIncrease/totalTestResultsIncrease)*100)%>%
  select(datecorrect = datecorrect, fips = fips, name = name, state = state.x, testing_daily_pop = testing_daily_pop, pos_rate_daily = pos_rate_daily)

testing_state[is.na(testing_state)] <-0

testing_state <- testing_state%>%
  group_by(name)%>%
  arrange(datecorrect)%>%
  mutate(pos_rate_daily_ma = SMA(pos_rate_daily, n =3))%>%
  arrange(fips)

testing_state[is.na(testing_state)] <-0

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

covid_county <- left_join(population_county, nyt_county, by = "fips")%>%
  mutate(state = stname)%>%
  mutate(county = ctyname)%>%
  select(date = date, fips = fips, name = stname, county = county, cases = cases, deaths = deaths, pop = pop)


#metric calculations
#doubling_fun <- function(r, t){log(2)/(log(r/lag(r, order_by = t)))}

covid_county <- covid_county %>%
  mutate(deaths_case = (deaths/cases)*100)%>%
  group_by(fips)%>%
  arrange(date)%>%
  mutate(doubling_time = doubling_fun(cases, date))%>%
  arrange(fips)

covid_county$doubling_time[which(!is.finite(covid_county$doubling_time))] <- 0

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
testing_date = as.Date("2020-03-15","%Y-%m-%d")

dates_graph <- covid_state %>%
  select(date, name)%>%
  filter(name == "United States")

positivethreshold = 10
testingthreshold = 152

#---------------------------------
#UI
#---------------------------------

ui <- navbarPage(
  title = strong("COVID-19: Evaluating “Confirmed Cases” vs. Actual Number of Infections"),
  windowTitle = "COVID-19 Dashboards",
  
  header = tagList(
    useShinydashboard(),
    setSliderColor(c("#7c7e80", "#7c7e80", "#7c7e80", "#7c7e80", "#7c7e80", "#7c7e80", "#7c7e80", "#7c7e80", "#7c7e80"), c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
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
          strong("From the outset of the COVID-19 epidemic, it has been clear that the number of actual infections in the United States has greatly exceeded the official “confirmed case” count."), "This dashboard is intended to provide a set of potentially useful indicators relating to the", strong("degree of difference between “confirmed case” figures for COVID-19 and the likely actual number of infections."), "We examine four metrics that provide warning signs that the “confirmed case” figures may not be providing an accurate picture of the extent or trend of the epidemic in a particular area. Because each metric evaluates the available data from a different perspective, we conclude that states with more of these warning signs should be less reliant on the official confirmed case count when evaluating the severity of the epidemic in their region.")),
    fluidRow(
      box(width = 2, height = 100,
          boxPad(
            descriptionBlock(
              header = strong("Confirmed Cases"), 
              right_border = FALSE),
            descriptionBlock(
              text = strong(textOutput("totalpostivecases")),
              right_border = FALSE))
      ),
      box(width = 1, height = 100,
          boxPad(
            descriptionBlock(
              header = strong("Deaths"), 
              right_border = FALSE),
            descriptionBlock(
              text = strong(textOutput("totaldeaths")), 
              right_border = FALSE))
      ),
      box(width = 6, height = 100,
          status = "primary",
          tags$head(tags$style(type='text/css', ".description-percentage { font-size: 16px; font-weight: 600 }")),
          title = em(strong("Click on the map below to select a state!")),
            column(
              width = 6,
              descriptionBlock(header = "Selected State")),
            column(
              width = 6,
              descriptionBlock(number = textOutput("stateselected"),
                               number_color = "yellow",
                               right_border = FALSE))
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
             tabBox(height = 400, id = "tabset1", width = 12,
                    tabPanel(strong("Linear"), 
                             plotlyOutput("positiveplot", 
                                          height = 340)),
                    tabPanel(strong("Logarithmic"),
                             plotlyOutput("positiveplotlog", 
                                          height = 340))),
             box(h6(em("Confirmed case numbers only reflect the number of patients who have tested positive for COVID-19; they do not include patients who have not been tested (or patients who are unaware they have the disease).")), width = 12),
             tabBox(height = 400, id = "tabset1", width = 12,
                    tabPanel(strong("Linear"),
                             plotlyOutput("deathsplot",
                                          height = 340)),
                    tabPanel(strong("Logarithmic"),
                             plotlyOutput("deathsplotlog",
                                          height = 340))),
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
                          title = strong("Percentage of Positive Tests"), solidHeader = TRUE, 
                          plotlyOutput("positivity", height = 240)),
                      box(h6(em("The number of tests that were positive for COVID-19 as proportion of the total tests conducted (3-day moving average). Where the positivity rate is high, the rate of testing is likely to be exerting a more significant influence on the confirmed case rate, making it a less reliable indicator of the actual number of cases.")), width = 12)),
               column(width = 6,
                      box(width = 12, height = 300,
                          title = strong("Relative Testing Rate"), solidHeader = TRUE,
                          plotlyOutput("testperday", height = 240)),
                      box(h6(em("The number of tests conducted daily in the state per 100,000 residents. These numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient, but a higher testing rate would be expected to increase the relative accuracy of confirmed case counts and the ability of a state to reliably assess the extent of the epidemic and the rate of spread.")), width = 12))
             )
      ),
      column(width = 3,
             box(title = strong("Confirmed Case Doubling Period"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 plotlyOutput("doublingrateplot",
                              height = 235)),
             box(h6(em("The time period for confirmed cases to double (3-day moving average).", strong("Assuming that the rate of testing is adequate,"), "the trend line would be expected to increase and then remain above the expected value (a doubling rate consistent with an effective mitigation/suppression effort).")), width = 12),
             box(title = strong("Apparent Case Fatality Ratio"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 plotlyOutput("mortalityrateplot", 
                              height = 235)
             ),
             box(h6(em("The current ratio of total deaths to confirmed cases. The degree to which this exceeds the expected value suggests the degree to which actual infections might exceed confirmed cases (although it can also indicate higher death rates due to vulnerable populations).")), width = 12),
             box(height = 210, 
                 width = 12,
                 sliderInput("mortalityrate",
                             "Expected case fatality ratio (def. 0.9%)",
                             min = 0.5,
                             max = 4.1,
                             value = 0.9),
                 sliderInput("doublingrate",
                             "Expected doubling rate (def. 15 days)",
                             min = 5,
                             max = 25,
                             value = 15))
      )
    ),
    
    fluidRow(
      box(width = 5, 
          strong("Overview"), br(),
          br(),
          "The purpose of the dashboard is to provide a different view of existing, available data on the COVID-19 epidemic in the United States. At the time of the last update of this dashboard (April 18, 2020), the vast majority of public reporting on the COVID-19 pandemic in the United States continues to focus primarily on two values: (1) the number of confirmed cases; and (2) the number of confirmed deaths (which are displayed here for the convenience of the user). However, given the initial and ongoing limitations on testing for COVID-19 within the United States, together with the likely presence of large numbers of asymptomatic patients, it is clear that actual infection rates greatly exceed the number of confirmed cases.", br(),
          br(),
          "Just as importantly, state-by-state variation in levels of testing, testing availability and targeting, and test processing timelines mean that the relative distribution of COVID-19 infections within the United States almost certainly differs significantly from that suggested by confirmed case numbers. In many areas, health officials and professionals have been targeting scarce testing resources on patients with more severe symptoms, which will result in significant undercounting of infections in the community. Also, multiple tests are frequently conducted on a single patient, further limiting the number of infections that are detected.", br(),
          br(),
          "As states and localities work to evaluate whether current measures to mitigate/suppress the spread of COVID-19 are effective, and to consider how and when these measures can be relaxed, it seems increasingly critical that this disconnect between the “confirmed case” numbers be understood. Where confirmed case data is substantially underestimating actual rates of infection, continued reliance on “confirmed case” data alone – whether used as a means of counting total infections, the distribution of infections within a region, or the current severity of the epidemic and rate of spread -- appears inadvisable.", br(),
          br(),
          "To that end, this dashboard is intended to point to several simple metrics that may suggest the degree to which actual infection rates exceed the currently reported and confirmed cases -- in hopes of increasing the information available to local decision makers and the public. In addition to the reported number of confirmed cases and confirmed deaths, which are widely reported and available from other sources (see graphs to the right and below the map), this dashboard provides four additional indicators or metrics that may help to evaluate the degree of difference between actual infection rates and official “confirmed case” counts. These metrics are provided at both a state and county level (see tabs at top).", br(),
          br(),
          "For each metric, we have also provided an associated “threshold value”. If the threshold value is exceeded, this is tallied as a potential “warning sign” that the actual infection rate in either the state or county could significantly exceed the confirmed cases. (Since there is uncertainty about the correct threshold values, the user can change some of these values using the slider bars.) The more thresholds that the state or county exceeds, the more warnings it accumulates (up to a total of four), as reflected in the color-coded map. Trend lines are also applied to some of the presented data to provide a sense of the direction of change in the metrics.", br(),
          br(),
          strong(textOutput("dataupdatetext"))
      ),
      box(width = 7,
          strong("Metrics Description"), br(),
          br(),
          strong("Confirmed Case Doubling Period:"), "The number of days it takes for the number of confirmed cases to double. Given the currently documented reproductive rate of SARS-Cov-19, epidemiological models suggest an expected doubling period of approximately 6 days in the absence of any mitigation or suppression efforts. Since most areas have been undertaking some level of mitigation/suppression for several weeks, the default expected doubling period has been set at 15 days in an effort to reflect a reasonable reduction in transmission rates. The slide bar provided allows users to select a different doubling period if desired.", em(strong("A doubling period that is faster than the expected value"))," (below the line on the graph)", em(strong("is a warning sign that the actual number of infections substantially exceeds the number of confirmed cases.")), "Where the local doubling period is faster than expected, it could mean that: (a) testing is being restricted or targeted primarily at patients with a higher probability of infection (such as severe cases); or (b) the population being tested otherwise already has a high rate of infection; in either case, the number of actual cases is likely to be much higher than the number of confirmed cases. A doubling rate lower than the expected value suggests that the number of confirmed cases may be more accurately tracking to infection rates. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the doubling period).", em(strong("A upward trend could reflect a more robust testing effort and/or beneficial effects of mitigation/suppression measures")), "in limiting the growth in new cases.", em(strong("Assuming that the rate of testing is adequate,")), "the trend line would be expected to increase and then remain above the expected value (i.e. a doubling rate consistent with an effective mitigation/suppression effort).", br(),
          br(),
          strong("Apparent Case Fatality Ratio:"), "The ratio of deaths attributed to COVID-19 to total confirmed cases. Current estimates of the case fatality ratio (CFR) for COVID-19 infections range widely but have been estimated by various experts at around 0.9 – 1.2% of infected patients, with an average period from infection-to-death at around 31 days. Because infections tend to spread during this intervening period, during the early phase of an epidemic the apparent CFR would normally be expected to be lower than the actual CFR, converging on the expected CFR over time.", em(strong("Accordingly a ratio higher than the expected CFR is a warning sign that the number of actual infections substantially exceeds the number of confirmed cases,")), "and the degree to which this exceeds the expected value suggests the degree to which actual infections might exceed confirmed cases. It should be noted, of course, that local conditions, such as the presence of relatively more vulnerable populations, limitations on medical care, or similar factors, may increase the CFR for a given population. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the apparent case fatality ratio).", br(),
          br(),
          strong("Percentage of Positive Tests:"),"The number of tests that were positive for COVID-19 as proportion of the total tests conducted. (Because reliable county level data was not available throughout the United States, this information is provided at the state level only.) The higher this percentage is, the more likely it is that (a) testing is being restricted to patients with a higher probability of infection (such as hospitalized patients); or (b) the population being tested already has a high underlying rate of infection. When this rate is high, there is also a substantial risk that observed changes in the number of confirmed cases are closely linked to the number of tests that are being conducted -- rather than to changes in the number of people that are actually infected. For example, if limitations in testing are restricting tests only to people with more severe symptoms, and the number of tests being conducted each day is not increasing, the rate of new confirmed cases could appear to stabilize in a particular area even if the number of actual cases was continuing to increase. As such,", em(strong("a high positivity rate is a warning sign that the confirmed case rate is not a reliable indicator of the actual number of cases in the area.")), "The default threshold value has been established at 10 percent, the maximum positivity rate recommended by the World Health Organization. Importantly, however, this rate is substantially above (i.e. worse) than the levels being observed in most other developed nations’ testing programs.", br(), 
          br(), 
          strong("Relative Testing Rate:"), "The number of tests that were conducted each day per 100,000 residents. (Because reliable county level data was not available throughout the United States, this information is provided at the state level only.) The U.S. testing effort has substantially lagged in comparison to other developed nations; in addition, there are currently broad differences between states in the availability of testing supplies and testing capacity, and the degree to which available tests are being targeted or restricted to particular patients or populations (many areas initially restricted and/or are continuing to target testing to patients with severe symptoms).", em(strong("Here, the warning sign is a testing rate that is lower than 152 per 100,000,")), "which is the rate recently recommended by researchers at the Harvard Global Health Institute to successfully identify the majority of infected patients. Lower testing rates, particularly in the absence of efforts to back-trace infections and the absence of broad mitigation and suppression controls, mean that local infection rates are likely to be far more widespread than confirmed case counts would suggest. Just as importantly, lower testing rates in one state as compared to others can substantially skew the apparent distribution of infections, since states with relatively high levels of testing effort (e.g. New York) will by definition result in a higher number of confirmed cases. These numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient. However, a higher testing rate would be expected to increase the relative accuracy of confirmed case counts and the ability of a state to reliably assess the extent of the epidemic and the rate of spread."
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
          strong("From the outset of the COVID-19 epidemic, it has been clear that the number of actual infections in the United States has greatly exceeded the official “confirmed case” count."), "This dashboard is intended to provide a set of potentially useful indicators relating to the", strong("degree of difference between “confirmed case” figures for COVID-19 and the likely actual number of infections."), "We examine four metrics that provide warning signs that the “confirmed case” figures may not be providing an accurate picture of the extent or trend of the epidemic in a particular area. Because each metric evaluates the available data from a different perspective, we conclude that states with more of these warning signs should be less reliant on the official confirmed case count when evaluating the severity of the epidemic in their region.")),
    fluidRow(
      box(width = 2, height = 100,
          boxPad(
            descriptionBlock(
              header = strong("Confirmed Cases"), 
              right_border = FALSE),
            descriptionBlock(
              text = strong(textOutput("totalpostivecasescounty")),
              right_border = FALSE))
      ),
      box(width = 1, height = 100,
          boxPad(
            descriptionBlock(
              header = strong("Deaths"), 
              right_border = FALSE),
            descriptionBlock(
              text = strong(textOutput("totaldeathscounty")), 
              right_border = FALSE))
      ),
      box(width = 6, height = 100,
          status = "primary",
          tags$head(tags$style(type='text/css', ".description-percentage { font-size: 16px; font-weight: 600 }")),
          title = em(strong("Click on the map below to select a county!")),
          column(
            width = 4,
            descriptionBlock(header = strong("Selected County"))),
          column(
            width = 8,
            descriptionBlock(number = textOutput("titlecountystate"),
                             number_color = "yellow",
                             right_border = FALSE))
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
             tabBox(height = 400, id = "tabset1", width = 12,
                    tabPanel(strong("Linear"), 
                             withSpinner(plotlyOutput("positiveplotcounty", 
                                          height = 340), type = 1, color = "#CDCDCD")),
                    tabPanel(strong("Logarithmic"),
                             withSpinner(plotlyOutput("positiveplotcountylog", 
                                          height = 340), type = 1, color = "#CDCDCD"))),
             box(h6(em("Confirmed case numbers only reflect the number of patients who have tested positive for COVID-19; they do not include patients who have not been tested (or patients who are unaware they have the disease).")), width = 12),
             tabBox(height = 400, id = "tabset1", width = 12,
                    tabPanel(strong("Linear"), 
                             withSpinner(plotlyOutput("deathsplotcounty", 
                                                      height = 340), type = 1, color = "#CDCDCD")),
                    tabPanel(strong("Logarithmic"),
                             withSpinner(plotlyOutput("deathsplotcountylog", 
                                                      height = 340), type = 1, color = "#CDCDCD"))),
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
                          title = strong("Percentage of Positive Tests (State)"), solidHeader = TRUE, 
                          withSpinner(plotlyOutput("positivitycounty", height = 240),
                                      type = 1, color = "#CDCDCD")),
                      box(h6(em("The number of tests that were positive for COVID-19 as proportion of the total tests conducted (3-day moving average). Where the positivity rate is high, the rate of testing is likely to be exerting a more significant influence on the confirmed case rate, making it a less reliable indicator of the actual number of cases.")), width = 12)),
               column(width = 6,
                      box(width = 12, height = 300,
                          title = strong("Relative Testing Rate (State)"), solidHeader = TRUE,
                          withSpinner(plotlyOutput("testperdaycounty", height = 240),
                                      type = 1, color = "#CDCDCD")),
                      box(h6(em("The number of tests conducted daily in the state per 100,000 residents. These numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient, but a higher testing rate would be expected to increase the relative accuracy of confirmed case counts and the ability of a state to reliably assess the extent of the epidemic and the rate of spread.")), width = 12))
             )
      ),
      column(width = 3,
             box(title = strong("Confirmed Case Doubling Period"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 withSpinner(plotlyOutput("doubling_timeplotcounty", height = 235),
                             type = 1, color = "#CDCDCD")),
             box(h6(em("The time period for confirmed cases to double (daily).", strong("Assuming that the rate of testing is adequate,"), "the trend line would be expected to increase and then remain above the expected value (a doubling rate consistent with an effective mitigation/suppression effort).")), width = 12),
             box(title = strong("Apparent Case Fatality Ratio"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 withSpinner(plotlyOutput("mortalityrateplotcounty", height = 235),
                             type = 1, color = "#CDCDCD")),
             box(h6(em("The current ratio of total deaths to confirmed cases. The degree to which this exceeds the expected value suggests the degree to which actual infections might exceed confirmed cases (although it can also indicate higher death rates due to vulnerable populations).")), width = 12),
             box(height = 210, 
                 width = 12,
                 sliderInput("mortalityratecounty",
                             "Expected case fatality ratio (def. 0.9%)",
                             min = 0.5,
                             max = 4.1,
                             value = 0.9),
                 sliderInput("doublingratecounty",
                             "Expected doubling rate (def. 15 days)",
                             min = 5,
                             max = 25,
                             value = 15)))
    ),
    
    fluidRow(
      box(width = 5, 
          strong("Overview"), br(),
          br(),
          "The purpose of the dashboard is to provide a different view of existing, available data on the COVID-19 epidemic in the United States. At the time of the last update of this dashboard (April 18, 2020), the vast majority of public reporting on the COVID-19 pandemic in the United States continues to focus primarily on two values: (1) the number of confirmed cases; and (2) the number of confirmed deaths (which are displayed here for the convenience of the user). However, given the initial and ongoing limitations on testing for COVID-19 within the United States, together with the likely presence of large numbers of asymptomatic patients, it is clear that actual infection rates greatly exceed the number of confirmed cases.", br(),
          br(),
          "Just as importantly, state-by-state variation in levels of testing, testing availability and targeting, and test processing timelines mean that the relative distribution of COVID-19 infections within the United States almost certainly differs significantly from that suggested by confirmed case numbers. In many areas, health officials and professionals have been targeting scarce testing resources on patients with more severe symptoms, which will result in significant undercounting of infections in the community. Also, multiple tests are frequently conducted on a single patient, further limiting the number of infections that are detected.", br(),
          br(),
          "As states and localities work to evaluate whether current measures to mitigate/suppress the spread of COVID-19 are effective, and to consider how and when these measures can be relaxed, it seems increasingly critical that this disconnect between the “confirmed case” numbers be understood. Where confirmed case data is substantially underestimating actual rates of infection, continued reliance on “confirmed case” data alone – whether used as a means of counting total infections, the distribution of infections within a region, or the current severity of the epidemic and rate of spread -- appears inadvisable.", br(),
          br(),
          "To that end, this dashboard is intended to point to several simple metrics that may suggest the degree to which actual infection rates exceed the currently reported and confirmed cases -- in hopes of increasing the information available to local decision makers and the public. In addition to the reported number of confirmed cases and confirmed deaths, which are widely reported and available from other sources (see graphs to the right and below the map), this dashboard provides four additional indicators or metrics that may help to evaluate the degree of difference between actual infection rates and official “confirmed case” counts. These metrics are provided at both a state and county level (see tabs at top).", br(),
          br(),
          "For each metric, we have also provided an associated “threshold value”. If the threshold value is exceeded, this is tallied as a potential “warning sign” that the actual infection rate in either the state or county could significantly exceed the confirmed cases. (Since there is uncertainty about the correct threshold values, the user can change some of these values using the slider bars.) The more thresholds that the state or county exceeds, the more warnings it accumulates (up to a total of four), as reflected in the color-coded map. Trend lines are also applied to some of the presented data to provide a sense of the direction of change in the metrics.", br(),
          br(),
          strong(textOutput("dataupdatetextcounty"))
      ),
      box(width = 7,
          strong("Metrics Description"), br(),
          br(),
          strong("Confirmed Case Doubling Period:"), "The number of days it takes for the number of confirmed cases to double. Given the currently documented reproductive rate of SARS-Cov-19, epidemiological models suggest an expected doubling period of approximately 6 days in the absence of any mitigation or suppression efforts. Since most areas have been undertaking some level of mitigation/suppression for several weeks, the default expected doubling period has been set at 15 days in an effort to reflect a reasonable reduction in transmission rates. The slide bar provided allows users to select a different doubling period if desired.", em(strong("A doubling period that is faster than the expected value"))," (below the line on the graph)", em(strong("is a warning sign that the actual number of infections substantially exceeds the number of confirmed cases.")), "Where the local doubling period is faster than expected, it could mean that: (a) testing is being restricted or targeted primarily at patients with a higher probability of infection (such as severe cases); or (b) the population being tested otherwise already has a high rate of infection; in either case, the number of actual cases is likely to be much higher than the number of confirmed cases. A doubling rate lower than the expected value suggests that the number of confirmed cases may be more accurately tracking to infection rates. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the doubling period).", em(strong("A upward trend could reflect a more robust testing effort and/or beneficial effects of mitigation/suppression measures")), "in limiting the growth in new cases.", em(strong("Assuming that the rate of testing is adequate,")), "the trend line would be expected to increase and then remain above the expected value (i.e. a doubling rate consistent with an effective mitigation/suppression effort).", br(),
          br(),
          strong("Apparent Case Fatality Ratio:"), "The ratio of deaths attributed to COVID-19 to total confirmed cases. Current estimates of the case fatality ratio (CFR) for COVID-19 infections range widely but have been estimated by various experts at around 0.9 – 1.2% of infected patients, with an average period from infection-to-death at around 31 days. Because infections tend to spread during this intervening period, during the early phase of an epidemic the apparent CFR would normally be expected to be lower than the actual CFR, converging on the expected CFR over time.", em(strong("Accordingly a ratio higher than the expected CFR is a warning sign that the number of actual infections substantially exceeds the number of confirmed cases,")), "and the degree to which this exceeds the expected value suggests the degree to which actual infections might exceed confirmed cases. It should be noted, of course, that local conditions, such as the presence of relatively more vulnerable populations, limitations on medical care, or similar factors, may increase the CFR for a given population. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the apparent case fatality ratio).", br(),
          br(),
          strong("Percentage of Positive Tests:"),"The number of tests that were positive for COVID-19 as proportion of the total tests conducted. (Because reliable county level data was not available throughout the United States, this information is provided at the state level only.) The higher this percentage is, the more likely it is that (a) testing is being restricted to patients with a higher probability of infection (such as hospitalized patients); or (b) the population being tested already has a high underlying rate of infection. When this rate is high, there is also a substantial risk that observed changes in the number of confirmed cases are closely linked to the number of tests that are being conducted -- rather than to changes in the number of people that are actually infected. For example, if limitations in testing are restricting tests only to people with more severe symptoms, and the number of tests being conducted each day is not increasing, the rate of new confirmed cases could appear to stabilize in a particular area even if the number of actual cases was continuing to increase. As such,", em(strong("a high positivity rate is a warning sign that the confirmed case rate is not a reliable indicator of the actual number of cases in the area.")), "The default threshold value has been established at 10 percent, the maximum positivity rate recommended by the World Health Organization. Importantly, however, this rate is substantially above (i.e. worse) than the levels being observed in most other developed nations’ testing programs.", br(), 
          br(), 
          strong("Relative Testing Rate:"), "The number of tests that were conducted each day per 100,000 residents. (Because reliable county level data was not available throughout the United States, this information is provided at the state level only.) The U.S. testing effort has substantially lagged in comparison to other developed nations; in addition, there are currently broad differences between states in the availability of testing supplies and testing capacity, and the degree to which available tests are being targeted or restricted to particular patients or populations (many areas initially restricted and/or are continuing to target testing to patients with severe symptoms).", em(strong("Here, the warning sign is a testing rate that is lower than 152 per 100,000,")), "which is the rate recently recommended by researchers at the Harvard Global Health Institute to successfully identify the majority of infected patients. Lower testing rates, particularly in the absence of efforts to back-trace infections and the absence of broad mitigation and suppression controls, mean that local infection rates are likely to be far more widespread than confirmed case counts would suggest. Just as importantly, lower testing rates in one state as compared to others can substantially skew the apparent distribution of infections, since states with relatively high levels of testing effort (e.g. New York) will by definition result in a higher number of confirmed cases. These numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient. However, a higher testing rate would be expected to increase the relative accuracy of confirmed case counts and the ability of a state to reliably assess the extent of the epidemic and the rate of spread."
      )
    )
  ),
  
  #---------------------------------
  #United States Page
  #---------------------------------
  
  tabPanel(
    title = strong("National Dashboard"),
    fluidRow(
      box(width = 12,
          strong("From the outset of the COVID-19 epidemic, it has been clear that the number of actual infections in the United States has greatly exceeded the official “confirmed case” count."), "This dashboard is intended to provide a set of potentially useful indicators relating to the", strong("degree of difference between “confirmed case” figures for COVID-19 and the likely actual number of infections."), "We examine four metrics that provide warning signs that the “confirmed case” figures may not be providing an accurate picture of the extent or trend of the epidemic in a particular area. Because each metric evaluates the available data from a different perspective, we conclude that states with more of these warning signs should be less reliant on the official confirmed case count when evaluating the severity of the epidemic in their region.")),
    fluidRow(
      box(width = 2, height = 100,
          boxPad(
            descriptionBlock(
              header = strong("Confirmed Cases"), 
              right_border = FALSE),
            descriptionBlock(
              text = strong(textOutput("totalpostivecasesus")),
              right_border = FALSE))
      ),
      box(width = 1, height = 100,
          boxPad(
            descriptionBlock(
              header = strong("Deaths"), 
              right_border = FALSE),
            descriptionBlock(
              text = strong(textOutput("totaldeathsus")), 
              right_border = FALSE))
      ),
      box(width = 6, height = 100,
          status = "primary",
          tags$head(tags$style(type='text/css', ".shiny-text-ouput .shiny-bound-output  { font-size: 16px, font-weight: 600 }")),
          title = em(strong("State and county details found on respective dashboards pages.")),
          column(
            width = 6,
            descriptionBlock(header = strong("Selected Area"))),
          column(
            width = 6,
            descriptionBlock(number = textOutput("titlecountystateus"),
                             number_color = "yellow",
                             right_border = FALSE))
          ),
      box(width = 3, height = 100,
          sliderInput("plot_date_us",
                      "Mapping Date (def. current)",
                      min = as.Date(cv_min_date,"%Y-%m-%d"),
                      max = as.Date(current_date,"%Y-%m-%d"),
                      value = as.Date(current_date),
                      timeFormat = "%d %b")
          )
    ),
    
    fluidRow(
      column(width = 3,
             tabBox(height = 400, id = "tabset1", width = 12,
                    tabPanel(strong("Linear"), 
                             withSpinner(plotlyOutput("positiveplotus", 
                                                      height = 340), type = 1, color = "#CDCDCD")),
                    tabPanel(strong("Logarithmic"),
                             withSpinner(plotlyOutput("positiveplotuslog", 
                                                      height = 340), type = 1, color = "#CDCDCD"))),
             box(h6(em("Confirmed case numbers only reflect the number of patients who have tested positive for COVID-19; they do not include patients who have not been tested (or patients who are unaware they have the disease).")), width = 12),
             tabBox(height = 400, id = "tabset1", width = 12,
                    tabPanel(strong("Linear"), 
                             withSpinner(plotlyOutput("deathsplotus", 
                                                      height = 340), type = 1, color = "#CDCDCD")),
                    tabPanel(strong("Logarithmic"),
                             withSpinner(plotlyOutput("deathsplotuslog", 
                                                      height = 340), type = 1, color = "#CDCDCD"))),
             box(h6(em("Reported deaths only include patients who are reported to have died of COVID-19. These figures are generally assumed to be more accurate, although they may not reflect all deaths from the disease.")), width = 12)
             
      ),
      column(width = 6,
             fluidRow(
               box(width = 12, height = 500,
                   tags$style(".leaflet-control-layers-expanded{background: #2F3740}"),
                   leafletOutput("rankingplotus", height = 440), 
                               ),
               box(h6(em("Map colors indicate the number of warning sign metrics that are exceeded for each county (see explanation of warning signs in “Metrics Description” below)."), strong("Please note that some counties currently have inadequate data to be reliably evaluated."), em("By selecting “SVI Score,” the map will instead display the Social Vulnerability Index rating for each county, as calculated by CDC (the Social Vulnerability Index ranks counties on 15 social factors, including poverty, lack of vehicle access, and crowded housing conditions to identify populations that tend to be uniquely vulnerable to disease outbreaks and other emergency events).")), width = 12)),
             fluidRow(
               column(width = 6,
                      box(width = 12, height = 300,
                          title = strong("Percentage of Positive Tests (State)"), solidHeader = TRUE, 
                          withSpinner(plotlyOutput("positivityus", height = 240),
                                      type = 1, color = "#CDCDCD")),
                      box(h6(em("The number of tests that were positive for COVID-19 as proportion of the total tests conducted (3-day moving average). Where the positivity rate is high, the rate of testing is likely to be exerting a more significant influence on the confirmed case rate, making it a less reliable indicator of the actual number of cases.")), width = 12)),
               column(width = 6,
                      box(width = 12, height = 300,
                          title = strong("Relative Testing Rate (State)"), solidHeader = TRUE,
                          withSpinner(plotlyOutput("testperdayus", height = 240),
                                      type = 1, color = "#CDCDCD")),
                      box(h6(em("The number of tests conducted daily in the state per 100,000 residents. These numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient, but a higher testing rate would be expected to increase the relative accuracy of confirmed case counts and the ability of a state to reliably assess the extent of the epidemic and the rate of spread.")), width = 12))
             )
      ),
      column(width = 3,
             box(title = strong("Confirmed Case Doubling Period"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 withSpinner(plotlyOutput("doubling_timeplotus", height = 235),
                             type = 1, color = "#CDCDCD")),
             box(h6(em("The time period for confirmed cases to double (daily).", strong("Assuming that the rate of testing is adequate,"), "the trend line would be expected to increase and then remain above the expected value (a doubling rate consistent with an effective mitigation/suppression effort).")), width = 12),
             box(title = strong("Apparent Case Fatality Ratio"),
                 width = 12,
                 height = 285,
                 solidHeader = TRUE,
                 withSpinner(plotlyOutput("mortalityrateplotus", height = 235),
                             type = 1, color = "#CDCDCD")),
             box(h6(em("The current ratio of total deaths to confirmed cases. The degree to which this exceeds the expected value suggests the degree to which actual infections might exceed confirmed cases (although it can also indicate higher death rates due to vulnerable populations).")), width = 12),
             box(height = 210, 
                 width = 12,
                 sliderInput("mortalityrateus",
                             "Expected case fatality ratio (def. 0.9%)",
                             min = 0.5,
                             max = 4.1,
                             value = 0.9),
                 sliderInput("doublingrateus",
                             "Expected doubling rate (def. 15 days)",
                             min = 5,
                             max = 25,
                             value = 15)))
    ),
    
    fluidRow(
      box(width = 5, 
          strong("Overview"), br(),
          br(),
          "The purpose of the dashboard is to provide a different view of existing, available data on the COVID-19 epidemic in the United States. At the time of the last update of this dashboard (April 18, 2020), the vast majority of public reporting on the COVID-19 pandemic in the United States continues to focus primarily on two values: (1) the number of confirmed cases; and (2) the number of confirmed deaths (which are displayed here for the convenience of the user). However, given the initial and ongoing limitations on testing for COVID-19 within the United States, together with the likely presence of large numbers of asymptomatic patients, it is clear that actual infection rates greatly exceed the number of confirmed cases.", br(),
          br(),
          "Just as importantly, state-by-state variation in levels of testing, testing availability and targeting, and test processing timelines mean that the relative distribution of COVID-19 infections within the United States almost certainly differs significantly from that suggested by confirmed case numbers. In many areas, health officials and professionals have been targeting scarce testing resources on patients with more severe symptoms, which will result in significant undercounting of infections in the community. Also, multiple tests are frequently conducted on a single patient, further limiting the number of infections that are detected.", br(),
          br(),
          "As states and localities work to evaluate whether current measures to mitigate/suppress the spread of COVID-19 are effective, and to consider how and when these measures can be relaxed, it seems increasingly critical that this disconnect between the “confirmed case” numbers be understood. Where confirmed case data is substantially underestimating actual rates of infection, continued reliance on “confirmed case” data alone – whether used as a means of counting total infections, the distribution of infections within a region, or the current severity of the epidemic and rate of spread -- appears inadvisable.", br(),
          br(),
          "To that end, this dashboard is intended to point to several simple metrics that may suggest the degree to which actual infection rates exceed the currently reported and confirmed cases -- in hopes of increasing the information available to local decision makers and the public. In addition to the reported number of confirmed cases and confirmed deaths, which are widely reported and available from other sources (see graphs to the right and below the map), this dashboard provides four additional indicators or metrics that may help to evaluate the degree of difference between actual infection rates and official “confirmed case” counts. These metrics are provided at both a state and county level (see tabs at top).", br(),
          br(),
          "For each metric, we have also provided an associated “threshold value”. If the threshold value is exceeded, this is tallied as a potential “warning sign” that the actual infection rate in either the state or county could significantly exceed the confirmed cases. (Since there is uncertainty about the correct threshold values, the user can change some of these values using the slider bars.) The more thresholds that the state or county exceeds, the more warnings it accumulates (up to a total of four), as reflected in the color-coded map. Trend lines are also applied to some of the presented data to provide a sense of the direction of change in the metrics.", br(),
          br(),
          strong(textOutput("dataupdatetextus"))
      ),
      box(width = 7,
          strong("Metrics Description"), br(),
          br(),
          strong("Confirmed Case Doubling Period:"), "The number of days it takes for the number of confirmed cases to double. Given the currently documented reproductive rate of SARS-Cov-19, epidemiological models suggest an expected doubling period of approximately 6 days in the absence of any mitigation or suppression efforts. Since most areas have been undertaking some level of mitigation/suppression for several weeks, the default expected doubling period has been set at 15 days in an effort to reflect a reasonable reduction in transmission rates. The slide bar provided allows users to select a different doubling period if desired.", em(strong("A doubling period that is faster than the expected value"))," (below the line on the graph)", em(strong("is a warning sign that the actual number of infections substantially exceeds the number of confirmed cases.")), "Where the local doubling period is faster than expected, it could mean that: (a) testing is being restricted or targeted primarily at patients with a higher probability of infection (such as severe cases); or (b) the population being tested otherwise already has a high rate of infection; in either case, the number of actual cases is likely to be much higher than the number of confirmed cases. A doubling rate lower than the expected value suggests that the number of confirmed cases may be more accurately tracking to infection rates. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the doubling period).", em(strong("A upward trend could reflect a more robust testing effort and/or beneficial effects of mitigation/suppression measures")), "in limiting the growth in new cases.", em(strong("Assuming that the rate of testing is adequate,")), "the trend line would be expected to increase and then remain above the expected value (i.e. a doubling rate consistent with an effective mitigation/suppression effort).", br(),
          br(),
          strong("Apparent Case Fatality Ratio:"), "The ratio of deaths attributed to COVID-19 to total confirmed cases. Current estimates of the case fatality ratio (CFR) for COVID-19 infections range widely but have been estimated by various experts at around 0.9 – 1.2% of infected patients, with an average period from infection-to-death at around 31 days. Because infections tend to spread during this intervening period, during the early phase of an epidemic the apparent CFR would normally be expected to be lower than the actual CFR, converging on the expected CFR over time.", em(strong("Accordingly a ratio higher than the expected CFR is a warning sign that the number of actual infections substantially exceeds the number of confirmed cases,")), "and the degree to which this exceeds the expected value suggests the degree to which actual infections might exceed confirmed cases. It should be noted, of course, that local conditions, such as the presence of relatively more vulnerable populations, limitations on medical care, or similar factors, may increase the CFR for a given population. The trend line indicates the direction of change (i.e. a trend towards the expected value indicates an improvement in the apparent case fatality ratio).", br(),
          br(),
          strong("Percentage of Positive Tests:"),"The number of tests that were positive for COVID-19 as proportion of the total tests conducted. (Because reliable county level data was not available throughout the United States, this information is provided at the state level only.) The higher this percentage is, the more likely it is that (a) testing is being restricted to patients with a higher probability of infection (such as hospitalized patients); or (b) the population being tested already has a high underlying rate of infection. When this rate is high, there is also a substantial risk that observed changes in the number of confirmed cases are closely linked to the number of tests that are being conducted -- rather than to changes in the number of people that are actually infected. For example, if limitations in testing are restricting tests only to people with more severe symptoms, and the number of tests being conducted each day is not increasing, the rate of new confirmed cases could appear to stabilize in a particular area even if the number of actual cases was continuing to increase. As such,", em(strong("a high positivity rate is a warning sign that the confirmed case rate is not a reliable indicator of the actual number of cases in the area.")), "The default threshold value has been established at 10 percent, the maximum positivity rate recommended by the World Health Organization. Importantly, however, this rate is substantially above (i.e. worse) than the levels being observed in most other developed nations’ testing programs.", br(), 
          br(), 
          strong("Relative Testing Rate:"), "The number of tests that were conducted each day per 100,000 residents. (Because reliable county level data was not available throughout the United States, this information is provided at the state level only.) The U.S. testing effort has substantially lagged in comparison to other developed nations; in addition, there are currently broad differences between states in the availability of testing supplies and testing capacity, and the degree to which available tests are being targeted or restricted to particular patients or populations (many areas initially restricted and/or are continuing to target testing to patients with severe symptoms).", em(strong("Here, the warning sign is a testing rate that is lower than 152 per 100,000,")), "which is the rate recently recommended by researchers at the Harvard Global Health Institute to successfully identify the majority of infected patients. Lower testing rates, particularly in the absence of efforts to back-trace infections and the absence of broad mitigation and suppression controls, mean that local infection rates are likely to be far more widespread than confirmed case counts would suggest. Just as importantly, lower testing rates in one state as compared to others can substantially skew the apparent distribution of infections, since states with relatively high levels of testing effort (e.g. New York) will by definition result in a higher number of confirmed cases. These numbers do not necessarily reflect the number of people tested, since multiple tests may be conducted on the same patient. However, a higher testing rate would be expected to increase the relative accuracy of confirmed case counts and the ability of a state to reliably assess the extent of the epidemic and the rate of spread."
      )
    )
  ),
  
  #---------------------------------
  #ABOUT Page
  #---------------------------------
  
  tabPanel(
    title = strong("About"),
    fluidRow(
      column(width = 8,
             box(width = 12,
                 "This dashboard has been produced by professionals with experience in data visualization and scenario planning for public policy analysis and decision making.", em(strong("We do not have medical expertise or formal training in epidemiology.")), "The information provided here is based solely on a mathematical analysis of data from other sources, and is not intended to replace or supersede authoritative information from any official or medical source.", br(),
                 br(),
                 "We have created this dashboard based on a continuing concern about the nature of the information being used to guide public attitudes, behaviors, and state and local responses to the COVID-19 epidemic within the United States. In an effort to increase the transparency of current data about the likely extent of COVID-19 infections, this dashboard undertakes some simple mathematical analysis of available data about confirmed COVID-19 cases, testing, and death rates in the U.S., and presents this information together with data from the Centers for Disease Control’s “Social Vulnerability Index.” As noted in the “Overview” section on each dashboard page, the metrics used are intended to identify potential “warning signs” that the number of infections in a particular state or county are substantially greater than the current “confirmed case” count might suggest, and to provide users a means to visualize – at least to some extent -- the degree to which this may be the case.", br(),
                 br(),
                 "From the outset of the COVID-19 pandemic, it has been clear that", em(strong("the rate of COVID-19 infection in the United States is substantially greater than reflected in the “confirmed case” counts")), "that have been the most widely reported measure for tracking the course of the pandemic. There are many reasons for this, including the likely presence of large numbers of asymptomatic patients in the population. However, the largest single cause is undoubtedly the initial unavailability and continued inadequacy of testing in the United States. In most areas, given the limited number of tests and the very long wait times for test results, health officials and medical professionals have directed (and in most cases are continuing to direct) the limited testing available to diagnose only relatively severe suspected cases of COVID-19.", br(),
                 br(),
                 "An inevitable consequence of the lack of available testing, the narrow targeting of testing resources, and the presence of asymptomatic carriers is that", em(strong("the “confirmed case” count in the United States is currently failing to capture many, if not the vast majority, of actual infections.")), "Many areas that currently show only a small number of “confirmed cases” (or no cases) are thus likely to have many more cases than they are presently aware of – and are likely to be experiencing rates of spread that are not reflected in this data.", br(),
                 br(),
                 "Nevertheless, many decision makers and members of the public are continuing to focus on reported “confirmed cases” as a benchmark for understanding the severity of the problem they face in their communities. This, in turn, may be guiding the urgency, nature, and extent of local mitigation/suppression measures – such as social distancing behaviors and requirements, closure of public venues, and required or voluntary business closures and telework mandates. Perhaps more critically, the daily changes in “confirmed case” counts are being used as a means to track changes in the rate of spread of the infection in their communities, and evaluate when these measures could potentially be relaxed.", em(strong("In this context, reliance on “confirmed case” information is potentially dangerous, and may result in those areas suffering substantially higher rates of disease and death over the coming weeks and months.")), br(),
                 br(),
                 "At present, the national response policy of the United States appears to depend almost entirely on the implementation of state and local response measures. To the extent that the implementation of these state and local measures or evaluations of their effectiveness are being guided by the number of confirmed cases in particular areas,", em(strong("the failure to understand the difference between “confirmed cases” and the actual potential extent of infection is likely to lead to failure of both those local responses and the U.S. national response as a whole.")), br(),
                 br(),
                 "As noted above, we have no expertise in epidemiology and this is not intended to replace any official source of information. However, our intent in creating this dashboard was to provide a different perspective on existing data in a manner that could better inform local understanding about the potential undercounting of COVID-19 infections. We welcome any feedback from users – and particularly experts and decision makers – on how this tool could be improved, and whether it is useful."
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

server <- function(input, output,session) {
  
  cvpaperbgcolor <- 'rgb(52,62,72)'
  cvplotbgcolor <- cvpaperbgcolor
  cvlinecolor <- 'rgb(205,205,205)'
  cvplotfont <- list(color = cvlinecolor)
  colwarning <- 'rgba(112,27,4, 0.6)'
  colconfirmedcases <- '#ffe2af'
  coldeaths <- '#ffe2af'
  coldoublingperiod <- '#ffe2af'
  colcasefatalityratio <- '#ffe2af'
  colpositivetest <- '#ffe2af'
  coltestingrate <- '#ffe2af'
  
  #---------------------------------
  #State Graphics
  #---------------------------------
  
  clicked_state<- eventReactive(input$rankingplot_shape_click, {
    return(input$rankingplot_shape_click$id)
  })
  
  output$stateselected <- renderText({
    paste(clicked_state())
  })
  
  state_selected <- reactive({
    covid_state%>%
      filter(name==clicked_state())%>%
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
    paste("Data sources are outlined on the About page. Updated:", current_nyt_date)
  })
  
  testing_selected <- reactive({
    testing_state%>%
      filter(name==clicked_state())%>%
      arrange(datecorrect)
  })
  
  output$positiveplot <- renderPlotly({
    
    plot_ly(state_selected(), 
            x=~date, 
            y=~cases, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = colconfirmedcases, width = 3))%>%
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
          title = 'Reported Confirmed Cases',
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
  
  output$positiveplotlog <- renderPlotly({
    
    plot_ly(state_selected(), 
            x=~date, 
            y=~cases, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = colconfirmedcases, width = 3))%>%
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
          title = 'Reported Confirmed Cases',
          type = "log",
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
            line = list(color = coldeaths, width = 3))%>%
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
          title = 'Reported Deaths',
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
  
  output$deathsplotlog <- renderPlotly({
    
    plot_ly(state_selected(), 
            x=~date, 
            y=~deaths, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = coldeaths, width = 3))%>%
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
          title = 'Reported Deaths',
          type = "log",
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
    
    dr_smooth = supsmu(state_selected()$date, state_selected()$doubling_ave)
    
    plot_ly(data = state_selected(),
            type = 'scatter',
            mode = 'markers')%>%
      add_lines(data = dates_graph,
                x=~date,
                y=input$doublingrate,
                name = "Warning Area (should be above)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tozeroy",
                fillcolor = colwarning)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=input$doublingrate,
                name = "Expected Value (should be above)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot')
      )%>%
      add_markers(data = state_selected_doubling,
                  x=~date, 
                  y=~as.numeric(doubling_ave),
                  name = "Doubling Period (moving average)",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.1f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = coldoublingperiod))%>%
      add_lines(data = state_selected(),
                x=~date,
                y=~dr_smooth$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = coldoublingperiod))%>%
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
          range = c(testing_date, current_date)),
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
          range = c(0, 30)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)',
                      traceorder = "reversed"))
  })
  
  output$mortalityrateplot <- renderPlotly({
    
    state_selected_mortality <- state_selected()%>%
      filter(deaths_case != 0)
    
    mr_smooth = supsmu(state_selected()$date, state_selected()$deaths_case)
    
    plot_ly(data = state_selected_mortality,
            type = 'scatter',
            mode = 'markers')%>%
      add_lines(x=~date,
                y=40,
                line = list(color = '#bfbfbf', dash = 'dot'),
                showlegend = FALSE)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=input$mortalityrate,
                name = "Warning Area (should be below)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tonexty",
                fillcolor = colwarning)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=input$mortalityrate,
                name = "Expected Value (should be below)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot')
                )%>%
      add_markers(data = state_selected_mortality,
                  x=~date, 
                  y=~deaths_case,
                  name = "Fatality Ratio",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.1f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = colcasefatalityratio))%>%
      add_lines(data = state_selected(),
                x=~date,
                y=~mr_smooth$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = colcasefatalityratio))%>%
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
          range = c(testing_date, current_date)),
        yaxis = list(
          title = 'Fatality Ratio <br> (% Deaths)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0, 8)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)',
                      traceorder = "reversed"))
  })
  
  output$positivity <- renderPlotly({
    
    testing_selected_mortality <- testing_selected()%>%
      filter(pos_rate_daily_ma != 0)
    
    pos_smooth = supsmu(testing_selected()$datecorrect, testing_selected()$pos_rate_daily_ma)
    
    plot_ly(data = testing_selected_mortality,
            type = "scatter",
            mode = "markers")%>%
      add_lines(data = dates_graph,
                x=~date,
                y=60,
                line = list(color = 'rgba(255, 255, 255, 0)'),
                showlegend = FALSE)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=positivethreshold,
                name = "Warning Area (should be below)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tonexty",
                fillcolor = colwarning)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=positivethreshold,
                name = "Expected Value (should be below)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      add_markers(data = testing_selected_mortality,
        x=~datecorrect, 
        y=~pos_rate_daily_ma,
        name = "% Positive Test (moving average)",
        text = ~name,
        hovertemplate = paste("%{x}: %{y:,.0f}",
                              "<extra>%{text}</extra>"),
        marker = list(color = colpositivetest))%>%
      add_lines(data = testing_selected(),
                x=~datecorrect,
                y=~pos_smooth$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = colpositivetest))%>%
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
          range = c(testing_date, current_date),
          zeroline = FALSE),
        yaxis = list(
          title = 'Positivity Rate <br> (Daily %)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0, 50)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))
     

  })
  
  output$testperday <- renderPlotly({
    
    testing_selected_rate<- testing_selected()%>%
      filter(pos_rate_daily_ma != 0)
    
    testrate_smooth = supsmu(testing_selected()$datecorrect, testing_selected()$testing_daily_pop)
    
    plot_ly(data = testing_selected(),
            type = "scatter",
            mode = "lines")%>%
      add_lines(data = dates_graph,
                x=~date,
                y=testingthreshold,
                name = "Warning Area (should be above)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tozeroy",
                fillcolor = colwarning)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=testingthreshold,
                name = "Expected Value (should be above)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      add_markers(data = testing_selected_rate,
                  x=~datecorrect,
                  y=~testing_daily_pop,
                  name = "Tests Conducted per Day",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.0f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(color = coltestingrate))%>%
      add_lines(data = testing_selected(),
                x=~datecorrect,
                y=~testrate_smooth$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = coltestingrate))%>%
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
          range = c(testing_date, current_date),
          zeroline = FALSE),
        yaxis = list(
          title = 'Daily Testing <br> (Per 100k Population)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0, 200)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)',
                      traceorder = "reversed"))
  })
  
  ranking <- reactive({
  
    covid_rank <- covid_state%>%
      filter(date==input$plot_date) %>%
      summarize(
        exceeddoubling = ifelse(doubling_ave<input$doublingrate,1,0), #comparing 3 day moving average
        exceedmortality = ifelse(deaths_case>input$mortalityrate,1,0))
    
    testing_rank <- testing_state%>%
      filter(datecorrect==input$plot_date) %>%
      mutate(exceedtestingdaily = ifelse(testing_daily_pop<(testingthreshold),1,0))%>%
      mutate(exceedposrate = ifelse(pos_rate_daily_ma>(positivethreshold),1,0))%>% #comparing 3 day moving average
      replace_na(list(exceedposrate = 0))%>%
      select(name, state, exceedtestingdaily, exceedposrate)
    
    merge(covid_rank, testing_rank, by = "name")%>%
      mutate(rank = exceeddoubling+exceedmortality+exceedtestingdaily+exceedposrate)%>%
      filter(state != "US")%>%
      mutate(state_name = name)%>%
      mutate(rank = paste("Warnings:", rank))%>%
      mutate(rank = as.factor(rank))%>%
      mutate(wdp = ifelse(exceeddoubling==1,"Warning","No Warning"))%>%
      mutate(wfr = ifelse(exceedmortality==1,"Warning","No Warning"))%>%
      mutate(wpr = ifelse(exceedposrate==1,"Warning","No Warning"))%>%
      mutate(wtr = ifelse(exceedtestingdaily==1,"Warning","No Warning"))
    
  })    
  
  output$rankingplot <- renderLeaflet({
    
    spdf <- usa_sf()
    
    ranking_state <- ranking() %>%
      mutate(name = as.factor(name))
    
    state_leaf_rank <- merge(spdf, ranking_state, by = "name")%>%
      st_transform(crs= 4326)
    
    statePopup <- paste0(
      state_leaf_rank$name, br(),
      state_leaf_rank$rank, br(),
      "Doubling Period: ", state_leaf_rank$wdp, br(),
      "Fatality Ratio: ", state_leaf_rank$wfr, br(),
      "Positivity Rate: ", state_leaf_rank$wpr, br(),
      "Testing Rate: ", state_leaf_rank$wtr)
    
    pal_red <- colorFactor(
      palette = c("#ffffff", "#FFC65D", "#d38a3a", "#a3521d", "#701b04"),
      levels = c("Warnings: 0", "Warnings: 1", "Warnings: 2", "Warnings: 3", "Warnings: 4")
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
                  layerId = ~name,
                  stroke = TRUE, 
                  weight = 1, 
                  color = "#ffffff", 
                  opacity = 1,
                  fill = TRUE, 
                  fillColor = ~pal_red(rank), 
                  fillOpacity = 1,
                  popup = statePopup
      )%>%
      setMapWidgetStyle(list(background= "#343E48"))%>%
      addLegend(data = state_leaf_rank, pal = pal_red, values = ~rank, opacity = 1, "bottomright", title = "Warnings")
    
  })
  
  #---------------------------------
  #County Graphics
  #---------------------------------
  
  clicked_county <- eventReactive(input$rankingplotcounty_shape_click, {
    return(input$rankingplotcounty_shape_click$id)
  })
  
  county_selected <- reactive({
    covid_county%>%
      filter(fips == clicked_county())%>%
      arrange(date)
  })
  
  state_county_selected <- reactive({
    dplyr::first(county_selected()$name)
  })
  
  state_county_title <- reactive({
    dplyr::first(county_selected()$county)
  })
  
  output$titlecountystate <- renderText({ 
    paste(state_county_title(), "- ", state_county_selected())
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
    paste("Data sources are outlined on the About page. Updated:", current_nyt_date)
  })
  
  testing_selected_county <- reactive({
    testing_state%>%
      filter(name==state_county_selected())
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
            line = list(color = colconfirmedcases, width = 3))%>%
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
  
  output$positiveplotcountylog <- renderPlotly({
    
    plot_ly(county_selected(), 
            x=~date, 
            y=~cases, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = colconfirmedcases, width = 3))%>%
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
          title = 'Reported Confirmed Cases',
          type = "log",
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
            line = list(color = coldeaths, width = 3))%>%
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
  
  output$deathsplotcountylog <- renderPlotly({
    
    plot_ly(county_selected(), 
            x=~date, 
            y=~deaths, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = coldeaths, width = 3))%>%
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
          title = 'Reported Deaths',
          type = "log",
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
      add_lines(data = dates_graph,
                x=~date,
                y=input$doublingratecounty,
                name = "Warning Area (should be above)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tozeroy",
                fillcolor = colwarning)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=input$doublingratecounty,
                name = "Expected Value (should be above)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      add_markers(data = county_selected_doubling,
                  x=~date, 
                  y=~doubling_time,
                  name = "Doubling Period (daily)",
                  text = ~county,
                  hovertemplate = paste("%{x}: %{y:,.1f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = coldoublingperiod))%>%
      add_lines(data = county_selected(),
                x=~date,
                y=~dr_smooth_county$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                
                line = list(color = coldoublingperiod))%>%
      
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
          range = c(testing_date, current_date)),
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
          range = c(0, 30)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)',
                      traceorder = "reversed"))
  })
  
  output$mortalityrateplotcounty <- renderPlotly({
    
    county_selected_mortality <- county_selected()%>%
      filter(deaths_case != 0)

    mr_smooth_county = supsmu(county_selected()$date, county_selected()$deaths_case)
    
    plot_ly(data = county_selected_mortality,
            type = 'scatter',
            mode = 'markers')%>%
      add_lines(data = dates_graph,
                x=~date,
                y=40,
                line = list(color = '#bfbfbf', dash = 'dot'),
                showlegend = FALSE)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=input$mortalityratecounty,
                name = "Warning Area (should be below)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tonexty",
                fillcolor = colwarning)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=input$mortalityratecounty,
                name = "Expected Value (should be below)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      add_markers(data = county_selected_mortality,
                  x=~date, 
                  y=~deaths_case,
                  name = "Fatality Ratio",
                  text = ~county,
                  hovertemplate = paste("%{x}: %{y:,.1f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = colcasefatalityratio))%>%
      add_lines(data = county_selected(),
                x=~date,
                y=~mr_smooth_county$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = colcasefatalityratio))%>%
      
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
          range = c(testing_date, current_date)),
        yaxis = list(
          title = 'Fatality Ratio <br> (% Deaths)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0, 8)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)',
                      traceorder = "reversed"))
  })
  
  output$positivitycounty <- renderPlotly({
    
    testing_selected_county_positive <- testing_selected_county()%>%
      filter(pos_rate_daily_ma != 0)
    
    pos_smooth = supsmu(testing_selected_county()$datecorrect, testing_selected_county()$pos_rate_daily_ma)
    
    plot_ly(data = testing_selected_county_positive,
            type = "scatter",
            mode = "markers")%>%
      add_lines(data = dates_graph,
                x=~date,
                y=60,
                line = list(color = 'rgba(255, 255, 255, 0)'),
                showlegend = FALSE)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=positivethreshold,
                name = "Warning Area (should be below)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tonexty",
                fillcolor = colwarning)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=positivethreshold,
                name = "Expected Value (should be below)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      add_markers(data = testing_selected_county_positive,
                  x=~datecorrect, 
                  y=~pos_rate_daily_ma,
                  name = "% Positive Test (moving average)",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.0f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(color = colpositivetest))%>%
      add_lines(data = testing_selected_county(),
                x=~datecorrect,
                y=~pos_smooth$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = colpositivetest))%>%
      
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
          range = c(testing_date, current_date),
          zeroline = FALSE),
        yaxis = list(
          title = 'Positivity Rate <br> (Daily %)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0, 50)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))
  })
  
  output$testperdaycounty <- renderPlotly({
    
    testing_selected_county_rate <- testing_selected_county()%>%
      filter(testing_daily_pop != 0)
    
    testingcounty_smooth = supsmu(testing_selected_county()$datecorrect, testing_selected_county()$testing_daily_pop)
    
    plot_ly(testing_selected_county(),
            type = "scatter",
            mode = "lines")%>%
      add_lines(data = dates_graph,
                x=~date,
                y=testingthreshold,
                name = "Warning Area (should be above)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tozeroy",
                fillcolor = colwarning)%>%
      add_lines(data = dates_graph,
                x=~date,
                y=testingthreshold,
                name = "Expected Value (should be above)",
                showlegend = FALSE,
                line = list(color = '#bfbfbf', dash = 'dot'),
                text = "Expected value",
                hovertemplate = paste("%{y:,.0f}",
                                      "<extra>%{text}</extra>"))%>%
      add_markers(data = testing_selected_county(),
                  x=~datecorrect, 
                  y=~testing_daily_pop,
                  name = "Tests Conducted per Day",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.0f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(color = coltestingrate))%>%
      add_lines(data = testing_selected_county(),
                x=~datecorrect,
                y=~testingcounty_smooth$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = coltestingrate))%>%
      plotly::layout(
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
          range = c(testing_date, current_date),
          zeroline = FALSE),
        yaxis = list(
          title = 'Daily Testing <br> (Per 100k Population)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0, 200)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)',
                      traceorder = "reversed"))
  })
  
  rankingcounty <- reactive({
    
    covid_rank_county <- covid_county%>%
      filter(date==input$plot_date_county) %>%
      mutate(
        exceeddoubling = ifelse(doubling_time<input$doublingrate,1,0),
        exceedmortality = ifelse(deaths_case>input$mortalityrate,1,0))%>%
      select(name, county, fips, exceeddoubling, exceedmortality)
    
    testing_rank_county <- testing_state%>%
      filter(datecorrect==input$plot_date_county) %>%
      mutate(exceedtestingdaily = ifelse(testing_daily_pop<(testingthreshold),1,0))%>%
      mutate(exceedposrate = ifelse(pos_rate_daily_ma>(positivethreshold),1,0))%>% #comparing 3 day moving average
      replace_na(list(exceedposrate = 0))%>%
      select(name, state, exceedtestingdaily, exceedposrate)
    
    merge(covid_rank_county, testing_rank_county, by = "name")%>%
      mutate(rank = exceeddoubling+exceedmortality+exceedtestingdaily+exceedposrate)%>%
      filter(state != "US")%>%
      mutate(county_fips = fips)%>%
      mutate(rank = paste("Warnings: ", rank))%>%
      mutate(rank = as.factor(rank))%>%
      mutate(wdp = ifelse(exceeddoubling==1,"Warning","No Warning"))%>%
      mutate(wfr = ifelse(exceedmortality==1,"Warning","No Warning"))%>%
      mutate(wpr = ifelse(exceedposrate==1,"Warning","No Warning"))%>%
      mutate(wtr = ifelse(exceedtestingdaily==1,"Warning","No Warning"))
    
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
    
    county_rank <- left_join(county, rankingcounty(), by = "county_fips")%>%
      mutate(rank = as.character(rank))
    county_rank[is.na(county_rank)] <-"No Data"
    
    county_rank <- county_rank%>%
      mutate(rank = as.factor(rank))
    
    county_svi_map <- left_join(county, svi_select, by = "county_fips")
    
    pal_red_county <- colorFactor(
      palette = c("#ffffff", "#FFC65D", "#d38a3a", "#a3521d", "#701b04", "#808080"),
      levels = c("Warnings:  0", "Warnings:  1", "Warnings:  2", "Warnings:  3", "Warnings:  4", "No Data")
    )
    
    pal_svi <- colorFactor(
      palette = c("#EBEBC2", "#A1C8AA", "#61A9B5", "#365C99"),
      levels = c("0-0.25", "0.25-0.5", "0.5-0.75", "0.75-1")
    )
    
    fitbounds <- c(-120, 24, -69, 43)
    maxbounds <- c(-130, 18, -55, 45)
    
    countyPopup <- paste0(
      county_rank$county_name," County, ", county_rank$state, br(),
      county_rank$rank, br(),
      "Doubling Period: ", county_rank$wdp, br(),
      "Fatality Ratio: ", county_rank$wfr, br(),
      "Positivity Rate: ", county_rank$wpr, br(),
      "Testing Rate: ", county_rank$wtr)
    
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
        layerId = ~county_fips,
        stroke = TRUE, 
        weight = .2, 
        color = "#ffffff", 
        opacity = 1,
        fill = TRUE, 
        fillColor = ~pal_red_county(rank), 
        fillOpacity = 1,
        popup = countyPopup,
        group = "Warnings"
      ) %>%
      addPolygons(
        data = county_svi_map, 
        layerId = ~county_fips,
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
  
  #---------------------------------
  #United States Graphics
  #---------------------------------

  us_selected <- covid_state%>%
    filter(name == "United States")
  
  us_selected_title <- reactive({
    dplyr::first(us_selected$name)
  })
  
  output$titlecountystateus <- renderText({
    paste(us_selected_title())
  })
  
  summary_numbers_us <- reactive({
    us_selected%>%
      filter(date==input$plot_date_us)
  })
  
  output$totalpostivecasesus <- renderText({
    paste(format(summary_numbers_us()$cases,
                 big.mark = ",", trim = TRUE, scientific = FALSE))
  })
  
  output$totaldeathsus <- renderText({
    paste(format(summary_numbers_us()$deaths,
                 big.mark = ",", trim = TRUE, scientific = FALSE))
  })
  
  output$dataupdatetextus <- renderText({ 
    paste("Data sources are outlined on the About page. Updated:", current_nyt_date)
  })
  
  testing_selected_us <- 
    testing_state%>%
      filter(name=="United States")

  output$positiveplotus <- renderPlotly({
    
    plot_ly(us_selected, 
            x=~date, 
            y=~cases, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = colconfirmedcases, width = 3))%>%
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
  
  output$positiveplotuslog <- renderPlotly({
    
    plot_ly(us_selected, 
            x=~date, 
            y=~cases, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = colconfirmedcases, width = 3))%>%
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
          title = 'Reported Confirmed Cases',
          type = "log",
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
  
  output$deathsplotus <- renderPlotly({
    
    plot_ly(us_selected, 
            x=~date, 
            y=~deaths, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = coldeaths, width = 3))%>%
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
  
  output$deathsplotuslog <- renderPlotly({
    
    plot_ly(us_selected, 
            x=~date, 
            y=~deaths, 
            type = 'scatter',
            mode = 'lines',
            text = ~name,
            hovertemplate = paste("%{x}: %{y:,.0f}",
                                  "<extra>%{text}</extra>"),
            line = list(color = coldeaths, width = 3))%>%
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
          title = 'Reported Deaths',
          type = "log",
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
  
  output$doubling_timeplotus <- renderPlotly({
    
    us_selected_doubling <- us_selected%>%
      filter(doubling_ave > 0)
    
    dr_smooth_us = supsmu(us_selected$date, us_selected$doubling_ave)
    
    plot_ly(data = us_selected_doubling,
            type = 'scatter',
            mode = 'markers')%>%
      add_lines(x=~date,
                y=input$doublingrateus,
                name = "Warning Area (should be above)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tozeroy",
                fillcolor = colwarning)%>%
      add_lines(x=~date,
                y=input$doublingrateus,
                name = "Expected Value (should be above)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      add_markers(x=~date, 
                  y=~doubling_ave,
                  name = "Doubling Period (daily)",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.1f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = coldoublingperiod))%>%
      add_lines(data = us_selected,
                x=~date,
                y=~dr_smooth_us$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                
                line = list(color = coldoublingperiod))%>%
      
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
          range = c(testing_date, current_date)),
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
          range = c(0, 30)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)',
                      traceorder = "reversed"))
  })
  
  output$mortalityrateplotus <- renderPlotly({
    
    us_selected_mortality <- us_selected%>%
      filter(deaths_case != 0)
    
    mr_smooth_us = supsmu(us_selected$date, us_selected$deaths_case)
    
    plot_ly(data = us_selected_mortality,
            type = 'scatter',
            mode = 'markers')%>%
      add_lines(
        # data = covid_county,
        x=~date,
        y=40,
        line = list(color = '#bfbfbf', dash = 'dot'),
        showlegend = FALSE)%>%
      add_lines(x=~date,
                y=input$mortalityrateus,
                name = "Warning Area (should be below)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tonexty",
                fillcolor = colwarning)%>%
      add_lines(x=~date,
                y=input$mortalityrateus,
                name = "Expected Value (should be below)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      add_markers(x=~date, 
                  y=~deaths_case,
                  name = "Fatality Ratio",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.1f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(size = 8,
                                color = colcasefatalityratio))%>%
      add_lines(data = us_selected,
                x=~date,
                y=~mr_smooth_us$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.1f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = colcasefatalityratio))%>%
      
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
          range = c(testing_date, current_date)),
        yaxis = list(
          title = 'Fatality Ratio <br> (% Deaths)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0, 8)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)',
                      traceorder = "reversed"))
  })
  
  output$positivityus <- renderPlotly({
    
    testing_selected_us_positive <- testing_selected_us%>%
      filter(pos_rate_daily_ma != 0)
    
    pos_smooth_us = supsmu(testing_selected_us$datecorrect, testing_selected_us$pos_rate_daily_ma)
    
    plot_ly(data = testing_selected_us_positive,
            type = "scatter",
            mode = "markers")%>%
      add_lines(x=~datecorrect,
                y=60,
                line = list(color = 'rgba(255, 255, 255, 0)'),
                showlegend = FALSE)%>%
      add_lines(x=~datecorrect,
                y=positivethreshold,
                name = "Warning Area (should be below)",
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fill = "tonexty",
                fillcolor = colwarning)%>%
      add_lines(x=~datecorrect,
                y=positivethreshold,
                name = "Expected Value (should be below)",
                text = "Expected value",
                showlegend = FALSE,
                hovertemplate = paste("%{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = '#bfbfbf', dash = 'dot'))%>%
      add_markers(
        x=~datecorrect, 
        y=~pos_rate_daily_ma,
        name = "% Positive Test (moving average)",
        text = ~name,
        hovertemplate = paste("%{x}: %{y:,.0f}",
                              "<extra>%{text}</extra>"),
        marker = list(color = colpositivetest))%>%
      add_lines(data = testing_selected_us,
                x=~datecorrect,
                y=~pos_smooth_us$y,
                name = "Trend",
                text = "Trend",
                hovertemplate = paste("%{x}: %{y:,.0f}",
                                      "<extra>%{text}</extra>"),
                line = list(color = colpositivetest))%>%
      
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
          range = c(testing_date, current_date),
          zeroline = FALSE),
        yaxis = list(
          title = 'Positivity Rate <br> (Daily %)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0, 50)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)'))
  })
  
  output$testperdayus <- renderPlotly({
    
    plot_ly(testing_selected_us,
            type = "scatter",
            mode = "lines")%>%
      add_lines(
        x=~datecorrect,
        y=testingthreshold,
        name = "Warning Area (should be above)",
        line = list(color = 'rgba(255, 255, 255, 0)'),
        fill = "tozeroy",
        fillcolor = colwarning)%>%
      add_lines(
        x=~datecorrect,
        y=testingthreshold,
        name = "Expected Value (should be above)",
        showlegend = FALSE,
        line = list(color = '#bfbfbf', dash = 'dot'),
        text = "Expected value",
        hovertemplate = paste("%{y:,.0f}",
                              "<extra>%{text}</extra>"))%>%
      add_markers(data = testing_selected_us,
                  x=~datecorrect, 
                  y=~testing_daily_pop,
                  name = "Tests Conducted per Day",
                  text = ~name,
                  hovertemplate = paste("%{x}: %{y:,.0f}",
                                        "<extra>%{text}</extra>"),
                  marker = list(color = coltestingrate))%>%
      plotly::layout(
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
          range = c(testing_date, current_date),
          zeroline = FALSE),
        yaxis = list(
          title = 'Daily Testing <br> (Per 100k Population)',
          gridcolor = cvlinecolor,
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          linecolor = cvlinecolor,
          tickcolor = cvlinecolor,
          ticks = 'inside',
          zeroline = FALSE,
          range = c(0, 200)),
        paper_bgcolor = cvpaperbgcolor, 
        plot_bgcolor = cvplotbgcolor,
        font = cvplotfont,
        legend = list(orientation = 'h',
                      bgcolor = 'rgba(0,0,0,0)',
                      traceorder = "reversed"))
  })
  
  rankingus <- reactive({
    
    covid_rank_county <- covid_county%>%
      filter(date==input$plot_date_county) %>%
      mutate(
        exceeddoubling = ifelse(doubling_time<input$doublingrate,1,0),
        exceedmortality = ifelse(deaths_case>input$mortalityrate,1,0))%>%
      select(name, county, fips, exceeddoubling, exceedmortality)
    
    testing_rank_county <- testing_state%>%
      filter(datecorrect==input$plot_date_county) %>%
      mutate(exceedtestingdaily = ifelse(testing_daily_pop<(testingthreshold),1,0))%>%
      mutate(exceedposrate = ifelse(pos_rate_daily_ma>(positivethreshold),1,0))%>% #comparing 3 day moving average
      replace_na(list(exceedposrate = 0))%>%
      select(name, state, exceedtestingdaily, exceedposrate)
    
    merge(covid_rank_county, testing_rank_county, by = "name")%>%
      mutate(rank = exceeddoubling+exceedmortality+exceedtestingdaily+exceedposrate)%>%
      filter(state != "US")%>%
      mutate(county_fips = fips)%>%
      mutate(rank = paste("Warnings: ", rank))%>%
      mutate(rank = as.factor(rank))%>%
      mutate(wdp = ifelse(exceeddoubling==1,"Warning","No Warning"))%>%
      mutate(wfr = ifelse(exceedmortality==1,"Warning","No Warning"))%>%
      mutate(wpr = ifelse(exceedposrate==1,"Warning","No Warning"))%>%
      mutate(wtr = ifelse(exceedtestingdaily==1,"Warning","No Warning"))
    
  })    
  
  output$rankingplotus <- renderLeaflet({

    state <- usa_sf()
    ctdf <- counties_sf()
    
    county <- ctdf%>%
      select(name, fips)%>%
      mutate(fips_2 = as.character(fips))%>%
      mutate(county_fips = as.numeric(fips_2))%>%
      mutate(county_name = as.character(name))%>%
      filter(county_fips != 35013)
    
    county_rank <- left_join(county, rankingcounty(), by = "county_fips")%>%
      mutate(rank = as.character(rank))
    county_rank[is.na(county_rank)] <-"No Data"
    
    county_rank <- county_rank%>%
      mutate(rank = as.factor(rank))
    pal_red_county <- colorFactor(
      palette = c("#ffffff", "#FFC65D", "#d38a3a", "#a3521d", "#701b04", "#808080"),
      levels = c("Warnings:  0", "Warnings:  1", "Warnings:  2", "Warnings:  3", "Warnings:  4", "No Data")
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
        layerId = ~county_fips,
        stroke = TRUE, 
        weight = .2, 
        color = "#ffffff", 
        opacity = 1,
        fill = TRUE, 
        fillColor = ~pal_red_county(rank), 
        fillOpacity = 1,
        group = "Warnings"
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

