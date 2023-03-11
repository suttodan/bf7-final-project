library(shiny)
library(tidyverse)

covid <- read_csv("data/covid_by_zip_code.csv")

# Note: We decided to replace NA values with 0 for technical reasons related to
#   our implementation.
covid[is.na(covid)] <- 0

covid_alt_column_titles = data.frame(
  "selected" = c("Zip Code",
                 "Ethnicity",
                 "Population",
                 "Case Count",
                 "Hospitalized Count",
                 "Death Count",
                 "Incidental Hospitalizations",
                 "PCR Test Count",
                 "Positive PCR Test Count",
                 "Positive PCR Test Percentage"),
  "actual" = colnames(covid))

ui <- shinyUI(
  
  ####
  ## Header Panel (always visible)
  ####
  
  fluidPage(
    h1(HTML("<strong>Covid-19, Neighborhood, and Race</strong>"),
       style = "text-align:center;"),
    h3(HTML("<em>Mapping the Impact of COVID-19 on Marginalized 
            Communities</em>"),
       style = "text-align:center;"),
    tabsetPanel(
      
      ####
      ## Home Page
      ####
      
      tabPanel("General Information",
        sidebarLayout(
          sidebarPanel(
            p("We've created an interactive data visualization that allows for 
              a comparison of COVID-19 impact across different zip codes and 
              ethnicities. Our hope is that this is a useful tool for educators, 
              public health workers, and decision-makers looking for insights 
              into how some communities were disproportionately impacted by 
              COVID-19."),
            br(),
            p(HTML("Our was collected and prepared by <strong>Public Health - 
                   Seattle & King County</strong>, and covers all COVID 19 
                   cases up through March 1, 2023. Their data continues to be 
                   updated, and can be found on their website:")),
            tags$a(href="https://kingcounty.gov/depts/health/covid-19/data/download.aspx",
                   HTML("<strong>Public Health - Seattle & King County</strong>")),
            br(),
            br(),
            p("The ethnicities in this data set are defined as follows:"),
            p("AIAN: American Indian or Alaska Native: individuals who 
            identify as being descended from the original peoples of North 
            America who maintain cultural identification through tribal 
            affiliation or community recognition."),
            p("ASIAN: individuals who identify as being of Asian 
              descent."),
            p("BLACK: individuals who identify as being of African 
            descent, including those who are African American, Afro-Caribbean, 
            or from other parts of the African diaspora."),
            p("HISPANIC: individuals who identify as being of Hispanic, 
              Latino, or Spanish origin, regardless of race."),
            p("Multi/Other: who identify as being of more than one 
            race or ethnicity, or who do not identify with any of the other 
            categories listed."),
            p("NHPI: individuals who identify as being descended from 
              the original peoples of Hawaii, Guam, Samoa, or other Pacific 
              Islands."),
            p("Unknown: This category includes individuals who did not 
              report their race or ethnicity."),
            p("White: This category includes individuals who identify 
              as being of European, Middle Eastern.")
          ),
          mainPanel(
            img(alt = "King County Zip Code Boundaries",
                src = "king_county_zip_code_map.jpg",
                width = "100%",
                height = "100%")
            )
          )
        ),
      
      ####
      ## Page 2 - Zip Code Stats
      ####
      
      tabPanel("Zip Code Stats",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            actionButton("zip_eth_select_all", "Select All"),
            checkboxGroupInput("zip_eth_select", "Ethnicities",
                               choices = unique(covid$race_eth), 
                               selected = unique(covid$race_eth)),
            radioButtons("zip_code_var_select", "Stat to Compare", 
                         choices = c("Population", 
                                     "Case Count", 
                                     "Hospitalized Count", 
                                     "Death Count", 
                                     "PCR Test Count", 
                                     "Positive PCR Test Count"),
                         selected = "Population"),
            verbatimTextOutput("zip_code_text"),
            tableOutput("zip_code_table")),
          mainPanel(
            width = 9, plotOutput("zip_code_plot", height = "1200px")
            )
          )
        ),
      
      ####
      ## Page 3 - Aggregate Racial Impacts
      ####
      
      tabPanel("Aggregate Racial Impacts",
        fluidPage(
          fluidRow(
            column(
              width = 2,
              actionButton("race_eth_select_all", "Select All"),
              checkboxGroupInput("race_eth_select", "Ethnicities",
                                 choices = unique(covid$race_eth), 
                                 selected = unique(covid$race_eth)),
              radioButtons("race_var_select", "Variable to Compare", 
                           choices = c("Case Count", 
                                       "Hospitalized Count", 
                                       "Death Count", 
                                       "PCR Test Count", 
                                       "Positive PCR Test Count"),
                           selected = "Case Count")),
            column(
              width = 10,
              fluidRow(
                tableOutput("race_table")
              ),
              fluidRow(
                column(
                  width = 5,
                  plotOutput("race_plot_1")
                  ),
                column(
                  width = 5,
                  plotOutput("race_plot_2")
                  )
                )
              )
            )
          )
        ),
      
      ####
      ## Page 4 - table showing disproportionate impact in each category
      ####
      
      tabPanel("Demographic Breakdown",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            fluidPage(
              fluidRow(
                p(HTML("<strong>Table will show Variable X per Variable Y:</strong>"))
              ),
              fluidRow(
                column(
                  width = 6,
                  radioButtons("demo_var_select_1", "X", 
                               choices = c("Population",
                                           "Case Count", 
                                           "Hospitalized Count", 
                                           "Death Count"),
                               selected = "Death Count")),
                column(
                  width = 6,
                  radioButtons("demo_var_select_2", "Y",
                               choices = c("Population",
                                           "Case Count", 
                                           "Hospitalized Count", 
                                           "Death Count"),
                               selected = "Case Count"))))),
          mainPanel(tableOutput("demo_table")))),
      
      ####
      ## Page 5 - Conclusion
      ####
    
      tabPanel("Our Observations",
        sidebarLayout(
          sidebarPanel(
            h3("Patterns:"),
            p("Our analysis (look at the example bar chart on the right for 
              reference), showed that certain zip codes and racial groups were 
              disproportionately impacted by the pandemic. In particular, areas 
              with a high concentration of minority communities, were more 
              severely affected than others."),
            h3("Broader Implications:"),
            p("The findings suggest that social determinants of health, such as 
              race, location, and socioeconomic status, play a crucial role in 
              determining the level of care and resources available to 
              individuals affected by the pandemic."),
            h3("Data Quality:"),
            p("When it comes to data quality, it is important to consider the 
              representativeness of the sample population. In the case of 
              COVID-19 testing data, it is true that the data only reflects the 
              people who have chosen to get tested and have the resources to do 
              so. This means that the data may not be representative of the 
              entire population, particularly those who may have limited access 
              to testing or who choose not to get tested. Our dataset was 
              collected by Public Health – Seattle & King County, and we found 
              it to be of reasonable quality."),
            h3("Future Ideas:"),
            p("In the future we hope show more COVID-19 impact indicators, such 
              as vaccination rates, and scrutinize the correlation between 
              social determinants of health and vaccine allocation. 
              Furthermore, we intend to investigate various geographical 
              locations to assess how COVID-19 impact differs among diverse 
              regions and communities.")),
          mainPanel(plotOutput("conclusion_plot")))))))

server <- function(input, output, session) {
  
  ####
  ## Zip Code Stats - Page 2
  ####
  
  # Select All - if all ethnicities are already selected, deselects all,
  #   otherwise will select all ethnicities.
  observeEvent(input$zip_eth_select_all, {
    if(identical(input$zip_eth_select, unique(covid$race_eth))) {
      updateCheckboxGroupInput(session, "zip_eth_select", 
                               choices = unique(covid$race_eth),
                               selected = character(0)) 
    } else {
      updateCheckboxGroupInput(session, "zip_eth_select", 
                               choices = unique(covid$race_eth), 
                               selected = unique(covid$race_eth))
    }
  }) 
  
  # Zip Code Plot
  output$zip_code_plot <- renderPlot({
    
    # Defines which variable to measure
    zip_code_var = covid_alt_column_titles %>% 
      filter(selected == input$zip_code_var_select)

    # Filters data set for the plot
    zip_code_plot_data <- covid %>%
      filter(race_eth %in% input$zip_eth_select) %>% 
      select(geo_id, race_eth, zip_code_var$actual)

    # Plot
    ggplot(zip_code_plot_data,
           aes(x = geo_id,
               y = .data[[zip_code_var$actual]],
               fill = race_eth)) +
      geom_col() +
      xlab("Zip Code") +
      ylab(zip_code_var$selected) +
      ggtitle(paste(zip_code_var$selected, "by Zip Code")) +
      scale_fill_discrete(name = "Ethnicity") +
      coord_flip()
  })
  
  # Zip Code Reactive Text Output
  output$zip_code_text <- renderText({
    paste("Other/Unknown", input$zip_code_var_select, "by Ethnicity:")
  })
  
  # Zip Code Table Output
  output$zip_code_table <- renderTable({
    
    # Defines which variable to measure
    zip_code_var = covid_alt_column_titles %>% 
      filter(selected == input$zip_code_var_select)
    
    # Filters data set for the table
    zip_table_data <- covid %>%
      filter(race_eth %in% input$zip_eth_select,
             geo_id == "Other/Unknown") %>% 
      select(race_eth, zip_code_var$actual)
    
    zip_table_data
  })
  
  ####
  ## Racial Impacts - Page 3
  ####
  
  # Select All - if all ethnicities are already selected, deselects all,
  #   otherwise will select all ethnicities.
  observeEvent(input$race_eth_select_all, {
    if(identical(input$race_eth_select, unique(covid$race_eth))) {
      updateCheckboxGroupInput(session, "race_eth_select", 
                               choices = unique(covid$race_eth),
                               selected = character(0)) 
    } else {
      updateCheckboxGroupInput(session, "race_eth_select", 
                               choices = unique(covid$race_eth), 
                               selected = unique(covid$race_eth))
    }
  }) 
  
  # Filtered data for use on this page
  race_data <- reactive({
    
    # Defines which variable to measure
    race_var = covid_alt_column_titles %>% 
      filter(selected == input$race_var_select)
    
    # Filters data set for the plot
    race_plot_data_1 <- covid %>%
      filter(race_eth %in% input$race_eth_select) %>% 
      select(race_eth, pop, race_var$actual) %>% 
      group_by(race_eth) %>% 
      summarize(Population = sum(pop),
                Variable = sum(.data[[race_var$actual]]))
  })
  
  # Left hand plot; plotted variable selected by user
  output$race_plot_1 <- renderPlot({

    race_plot_data_1 <- race_data()

    ggplot(race_plot_data_1,
           aes(x = "",
               y = Variable,
               fill = race_eth)) +
      geom_col(color = "white") +
      ggtitle(paste(input$race_var_select, "by Ethnicity")) +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_discrete(name = "Ethnicity")
  })
  
  # Right hand plot; plotted variable is population
  output$race_plot_2 <- renderPlot({
    
    race_plot_data_2 <- race_data()

    ggplot(race_plot_data_2,
           aes(x = "",
               y = Population,
               fill = race_eth)) +
      geom_col(color = "white") +
      ggtitle(paste("Population by Ethnicity")) +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_discrete(name = "Ethnicity")
  })
  
  # Table with the actual numbers for the above charts
  output$race_table <- renderTable({
    
    df <- as.data.frame(t(race_data()))
    
    colnames(df) <- input$race_eth_select
    
    outputDF <- df %>% 
      mutate(Variable = c("delete", "Population", input$race_var_select)) %>% 
      relocate(Variable) %>% 
      filter(Variable != "delete")
  })
  
  ####
  ## Demographic Breakdown - Page 4
  ####
  
  # Table output comparing user-selected variables to one another
  output$demo_table <- renderTable({
    
    demo_var_1 = covid_alt_column_titles %>% 
      filter(selected == input$demo_var_select_1)
    
    demo_var_2 = covid_alt_column_titles %>% 
      filter(selected == input$demo_var_select_2)
    
    demo_data <- covid %>% 
      group_by(race_eth) %>% 
      filter(race_eth != "Unknown") %>% 
      summarize(Population = sum(pop),
                `Case Count` = sum(case_count),
                `Hospitalized Count` = sum(hosp_count),
                `Death Count` = sum(death_count))
    
    demo_table_data <- demo_data %>% 
      select(race_eth, demo_var_1$selected, demo_var_2$selected) %>% 
      mutate(Percentage = demo_data[[demo_var_1$selected]] / demo_data[[demo_var_2$selected]] * 100) %>% 
      rename(Ethnicity = race_eth)
    
  })
  
  ####
  ## Conclusion Page - Page 5
  ####
  
  # Calculations done for conclusion page plot
  conc_data <- covid %>% 
    group_by(race_eth) %>% 
    filter(race_eth != "Unknown") %>% 
    summarize(Population = sum(pop),
              Cases = sum(case_count),
              Case_Percentage = Cases / Population * 100)
  
  # Conclusion page plot
  output$conclusion_plot <- renderPlot({
    ggplot(conc_data,
           aes(x = race_eth,
               y = Case_Percentage,
               fill = race_eth)) + 
      geom_col() +
      ylim(0, 100) +
      labs(title = "Communities Impacted by Covid: Covid cases as a percentage 
           of population, separated by ethnicity",
           x = "Ethnicity",
           y = "Percentage of Population") +
      theme_minimal()  
  })
  
}

shinyApp(ui, server)