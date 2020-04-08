library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(datasets)
library(markdown) 

source("000_integrate_county_and_coc_data.R")
 

homeless_counts <- all_communities %>%
  select(community, community_type, unshelteredhomeless, sheltered) %>%
  gather(variable, value, 3:4)
  
# Create parameters to be used later
# Create vector of CoC names
community_names <- as.vector(distinct(all_communities, community))

# Create hospitalization, ICU admission and fatality rate parameters--
# these are calculated as ratio of hospitalizations, icu admissions and fatalities
# to total infected cases based on information in Randall Kuhn's spreadsheet
# "COVID Homeless Forecast Clean" from April 1, 2020 calculated by the following
# formulae in that spreadhseet
# Hospitalization rate among infected = I39 / (L39 * D3) * .75
# ICU rate among infected = J39 / (L39 * D3) * .75
# Fatality rate among infected = K3 / (K39 * D3) * .75

# Or alternatively, these can be equivalently calculated
# based on number of infections from report 197363 = (.40 * 493408)
# and number of hospitalizatoins, ICU admissions and fatalities, respctively 
hospitalization_rate <- 21295 / 197363
icu_admissions <- 7145 / 197363
fatality <- 3454 / 197363


ui <- fluidPage(
  
  titlePanel("Community-level COVID-19 Homelessness Planning & Response Dashboard"),
  
  sidebarLayout(
    sidebarPanel("Select values to customize",
                 
                 
                 selectInput("community_type", "Select type of community", 
                             choices = c("HUD Continuum of Care (CoC)", 
                                         "County",
                                         "United States, nationwide")), 
                 
                 selectInput("community", "Name", choices = community_names),
                 br(),
                 
                 sliderInput("unshel_undercount", "% undercount of unsheltered population",
                             min = 0, max = 100, value = 40),
                 
                 sliderInput("infection", "Infection rate in homeless population, %", 
                             min = 0, max = 100, value = 40),
                 
                 sliderInput("high_risk", "% of homeless population with  high risk of medical complications",
                             min = 0, max = 100, value = 50),
                 
                 sliderInput("turnover_rate", "Annual turnover rate in sheltered homeless population",
                             min = 1, max = 20, value = 0, step = 0.1),
                 
                 checkboxGroupInput("population", "Groups used to estimate capacity",
                                    
                                   choices = list("COVID 19 Positive & High Risk" = 1,
                                    "COVID 19 Positive & Low Risk"  = 2,
                                    "COVID 19 Negative & High Risk" = 3,
                                    "COVID 19 Negative & Low Risk"  = 4),
                                   selected = c(1,2,3,4))
                 ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
              tabPanel("About", 
                       includeMarkdown("about.md")
                       ),
              
              tabPanel("Size of Homeless Population",
                       
                       h2("Estimated number of single homeless adults"),
                       tableOutput("count_table"),
                       
                       h2("Estimated number of single homeless adults, by medical risk status"),
                       tableOutput("count_table2")),
              
              tabPanel("Infections, Hospitalizations, ICU admissions & Fatalities",
                       tableOutput("impact_table")),
              
              #
              #tabPanel("Capacity Needed, Total", 
              #         tableOutput("capacity_table")),
              
              tabPanel("Capacity Needed, by risk group", 
                       
                       
                       h2(textOutput("capacity_needed")),
                       
                       plotOutput("covid_plot"))
    
  )
  
)
)
)
 
server <- function(input, output, session){
  
  observe({
    
    x <- input$community_type
    y <- filter(all_communities, community_type == x)
    
    community_choices <- distinct(y, community)
    
    if (is.null(x))
      
      x <- character(0)
    
    updateSelectInput(session, "community",
                      choices = community_choices)
    
  
  
  output$count_table <- renderTable({
    
    data <- subset(all_communities, community == input$community)
    
    data2 <- data %>%
      mutate(pct_undercount = input$unshel_undercount / 100,
             `Sheltered homeless individuals (2019 HUD Point-in-Time)` = sheltered,
             `Unsheltered homeless individuals (2019 HUD Point-in-Time)` = unshelteredhomeless,
             `Adjusted unsheltered homeless individuals` = round(unshelteredhomeless * (1 + pct_undercount), 0),
             `Annualized sheltered homeless individuals (adjusted for turnover)` = round(sheltered * input$turnover_rate, 0),
             `Annualized total homeless individuals (with adjustments)` =  round(`Annualized sheltered homeless individuals (adjusted for turnover)` + `Adjusted unsheltered homeless individuals`,0),
             `Annualized total homeless individuals in high risk category (with adjustments)` =
               round(`Annualized total homeless individuals (with adjustments)` * ((100 - input$high_risk) /100), 0)) %>%
      select(community, 
             `Sheltered homeless individuals (2019 HUD Point-in-Time)`,
             `Unsheltered homeless individuals (2019 HUD Point-in-Time)`,
             `Adjusted unsheltered homeless individuals`,
             `Annualized sheltered homeless individuals (adjusted for turnover)`,
             `Annualized total homeless individuals (with adjustments)`)
    data2
    
  })
  
  
  output$count_table2 <- renderTable({
    
    data3 <- subset(all_communities, community == input$community)
    
    data4 <- data3 %>%
      mutate(pct_undercount = input$unshel_undercount / 100,
             `Sheltered homeless individuals (2019 HUD Point-in-Time)` = sheltered,
             `Unsheltered homeless individuals (2019 HUD Point-in-Time)` = unshelteredhomeless,
             `Adjusted unsheltered homeless individuals` = round(unshelteredhomeless * (1 + pct_undercount), 0),
             `Annualized sheltered homeless individuals (adjusted for turnover)` = round(sheltered * input$turnover_rate, 0),
             `Annualized total homeless individuals (with adjustments)` =  round(`Annualized sheltered homeless individuals (adjusted for turnover)` + `Adjusted unsheltered homeless individuals`,0),
             `Annualized total homeless individuals in high risk category (with adjustments)` =
               round(`Annualized total homeless individuals (with adjustments)` * ((input$high_risk) /100), 0),
             `Annualized total homeless individuals in low risk category (with adjustments` =
               round(`Annualized total homeless individuals (with adjustments)` * ((100 - input$high_risk) /100), 0)
             
             ) %>%
      select(community, 
             `Annualized total homeless individuals in high risk category (with adjustments)`,
             `Annualized total homeless individuals in low risk category (with adjustments`,
             `Annualized total homeless individuals (with adjustments)`
             )
    
    data4
    
  })
  
  
  output$impact_table <- renderTable({
    
    impact <- subset(all_communities, community == input$community)
    
    impact2 <- impact %>%
      mutate(pct_undercount = input$unshel_undercount / 100,
             `Sheltered homeless individuals (2019 HUD Point-in-Time)` = sheltered,
             `Unsheltered homeless individuals (2019 HUD Point-in-Time)` = unshelteredhomeless,
             `Adjusted unsheltered homeless individuals` = round(unshelteredhomeless * (1 + pct_undercount), 0),
             `Annualized sheltered homeless individuals (adjusted for turnover)` = round(sheltered * input$turnover_rate, 0),
             `Annualized total homeless individuals (with adjustments)` =  round(`Annualized sheltered homeless individuals (adjusted for turnover)` + `Adjusted unsheltered homeless individuals`,0),
             `Annualized total homeless individuals in high risk category (with adjustments)` =
               round(`Annualized total homeless individuals (with adjustments)` * ((input$high_risk) /100), 0),
             `Annualized total homeless individuals in low risk category (with adjustments` =
               round(`Annualized total homeless individuals (with adjustments)` * ((100 - input$high_risk) /100), 0),
             
             Infections = round(`Annualized total homeless individuals (with adjustments)` * (input$infection / 100), 0),
             Hospitalizations = round(Infections * hospitalization_rate,0),
             `ICU Admissions` = round(Infections * icu_admissions,0),
             Fatalities = round(Infections * fatality,0)
             
      ) %>%
      select(community,
             `Annualized total homeless individuals (with adjustments)`,
             Infections,
             Hospitalizations,
             `ICU Admissions`,
             Fatalities
      )
    
    
    impact2
    
  })
  
  
  
  output$capacity_needed <- renderText({
    
    covid <- subset(all_communities, community == input$community)
    
    covid <- covid %>%
      mutate(pct_undercount = input$unshel_undercount / 100,
             `Sheltered homeless individuals (2019 HUD Point-in-Time)` = sheltered,
             `Unsheltered homeless individuals (2019 HUD Point-in-Time)` = unshelteredhomeless,
             `Adjusted unsheltered homeless individuals` = round(unshelteredhomeless * (1 + pct_undercount), 0),
             `Annualized sheltered homeless individuals (adjusted for turnover)` = round(sheltered * input$turnover_rate, 0),
             tot_homeless_undercount_only = `Sheltered homeless individuals (2019 HUD Point-in-Time)` +  `Adjusted unsheltered homeless individuals`,
             
             `Annualized total homeless individuals (with adjustments)` =  round(`Annualized sheltered homeless individuals (adjusted for turnover)` + `Adjusted unsheltered homeless individuals`,0),
             `Annualized total homeless individuals in high risk category (with adjustments)` =
               round(`Annualized total homeless individuals (with adjustments)` * ((input$high_risk) /100), 0),
             `Annualized total homeless individuals in low risk category (with adjustments` =
               round(`Annualized total homeless individuals (with adjustments)` * ((100 - input$high_risk) /100), 0)) %>%
      mutate(total_homeless = tot_homeless_undercount_only,
             pct_infected = input$infection / 100,
             pct_high_risk = input$high_risk /100,
             covid_neg_high_risk = total_homeless * (1 - pct_infected)  * pct_high_risk,
             covid_neg_low_risk = total_homeless * (1 - pct_infected) * (1 - pct_high_risk),
             covid_pos_high_risk = total_homeless *  pct_infected  * pct_high_risk,
             covid_pos_low_risk =  total_homeless *  pct_infected  * (1 -pct_high_risk)) %>%
      gather(variable, value, total_homeless:covid_pos_low_risk) %>%
      filter(!(variable %in%  c("total_homeless", "pct_infected", "pct_high_risk"))) %>%
      mutate(`COVID status` = ifelse(variable %in% c("covid_neg_high_risk", 
                                                     "covid_neg_low_risk"), "COVID Negative", "COVID Positive"),
             `Risk status` = ifelse(variable %in% c("covid_neg_high_risk", 
                                                    "covid_pos_high_risk"), "In High risk pop", "Not in high risk pop"),
             group = ifelse(variable == "covid_pos_high_risk", 1,
                            ifelse(variable == "covid_pos_low_risk", 2,
                                   ifelse(variable == "covid_neg_high_risk", 3,4))))
    
    
    p1 <- filter(covid, group %in% c(input$population) )
    
    cap <- sum(p1$value)
    
    print(paste("Total Capacity needed:", cap))
    
    
    
  })
  
    output$covid_plot <- renderPlot({
      covid <- subset(all_communities, community == input$community)
      
      covid <- covid %>%
        mutate(pct_undercount = input$unshel_undercount / 100,
               `Sheltered homeless individuals (2019 HUD Point-in-Time)` = sheltered,
               `Unsheltered homeless individuals (2019 HUD Point-in-Time)` = unshelteredhomeless,
               `Adjusted unsheltered homeless individuals` = round(unshelteredhomeless * (1 + pct_undercount), 0),
               `Annualized sheltered homeless individuals (adjusted for turnover)` = round(sheltered * input$turnover_rate, 0),
               tot_homeless_undercount_only = `Sheltered homeless individuals (2019 HUD Point-in-Time)` +  `Adjusted unsheltered homeless individuals`,
               
               `Annualized total homeless individuals (with adjustments)` =  round(`Annualized sheltered homeless individuals (adjusted for turnover)` + `Adjusted unsheltered homeless individuals`,0),
               `Annualized total homeless individuals in high risk category (with adjustments)` =
                 round(`Annualized total homeless individuals (with adjustments)` * ((input$high_risk) /100), 0),
               `Annualized total homeless individuals in low risk category (with adjustments` =
                 round(`Annualized total homeless individuals (with adjustments)` * ((100 - input$high_risk) /100), 0)) %>%
        mutate(total_homeless = tot_homeless_undercount_only,
               pct_infected = input$infection / 100,
               pct_high_risk = input$high_risk /100,
               covid_neg_high_risk = total_homeless * (1 - pct_infected)  * pct_high_risk,
               covid_neg_low_risk = total_homeless * (1 - pct_infected) * (1 - pct_high_risk),
               covid_pos_high_risk = total_homeless *  pct_infected  * pct_high_risk,
               covid_pos_low_risk =  total_homeless *  pct_infected  * (1 -pct_high_risk)) %>%
               gather(variable, value, total_homeless:covid_pos_low_risk) %>%
        filter(!(variable %in%  c("total_homeless", "pct_infected", "pct_high_risk"))) %>%
        mutate(`COVID status` = ifelse(variable %in% c("covid_neg_high_risk", 
                                                       "covid_neg_low_risk"), "COVID Negative", "COVID Positive"),
               `Risk status` = ifelse(variable %in% c("covid_neg_high_risk", 
                                                      "covid_pos_high_risk"), "In High risk pop", "Not in high risk pop"),
               group = ifelse(variable == "covid_pos_high_risk", 1,
                              ifelse(variable == "covid_pos_low_risk", 2,
                                     ifelse(variable == "covid_neg_high_risk", 3,4))))


        p1 <- filter(covid, group %in% c(input$population) )
        ggplot(data = p1,
               aes(x = `Risk status`, y = value, label = round(value, 1))) +
          geom_bar(stat = "identity")+
          facet_wrap(~`COVID status`)+
          geom_label()+
          theme_bw()+
          labs(title = "Projected Capacity Needed by
             COVID-19 Status and Membership in High Risk Group")

      
  })
    
    output$capacity_table <- renderTable({
      
      capacity <- subset(all_communities, community == input$community)
      
    
      
      capacity2 <- capacity %>%
        mutate(pct_undercount = input$unshel_undercount / 100,
               `Sheltered homeless individuals (2019 HUD Point-in-Time)` = sheltered,
               `Unsheltered homeless individuals (2019 HUD Point-in-Time)` = unshelteredhomeless,
               `Adjusted unsheltered homeless individuals` = round(unshelteredhomeless * (1 + pct_undercount), 0),
               `Annualized total homeless individuals (with adjustments)` =  round(`Sheltered homeless individuals (2019 HUD Point-in-Time)` + `Adjusted unsheltered homeless individuals`,0)) %>%
        mutate(total_homeless = `Annualized total homeless individuals (with adjustments)`,
               pct_infected = input$infection / 100,
               `Total units needed` = `Annualized total homeless individuals (with adjustments)`,
               `Density reduction need` = round(`Sheltered homeless individuals (2019 HUD Point-in-Time)` * .5, 0),
               `Total new units required`  = `Density reduction need` + `Adjusted unsheltered homeless individuals`,
               `Quarantine units required`  = round(`Annualized total homeless individuals (with adjustments)` * pct_infected,0)
               ) %>%
        select(`Adjusted unsheltered homeless individuals`,
               `Sheltered homeless individuals (2019 HUD Point-in-Time)`,
               `Density reduction need`,
               `Total new units required`,
               `Quarantine units required`)
      
      capacity2
    })      
        
}) 
}

shinyApp(ui = ui, server = server)
