library(shiny)
library(tidyverse)

covid <- read_csv("data/covidByZipCode.csv")

ui <- shinyUI(
  
  ## Header Panel (always visible)
  fluidPage(
    h1(HTML("<strong>Covid-19, Neighborhood, and Race</strong>"),
       style = "text-align:center;"),
    h3(HTML("<em>Mapping the Impact of COVID-19 on Marginalized 
            Communities</em>"),
       style = "text-align:center;"),
    tabsetPanel(
      
      ## Home Page
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
                   updated, and can be found on their website.")),
            tags$a(href="https://kingcounty.gov/depts/health/covid-19/data/download.aspx",
                   HTML("<strong>Public Health - Seattle & King County</strong>")),
            br(),
            p("The ethnicities in this data set are as follows:"),
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
                src = "kingCountyZipCodes.jpg",
                width = "100%",
                height = "100%")))),
      
      ## Page 2 - bar chart, each bar is a zip code, but ALL zip codes are included; check boxes filter for ethnicity
      tabPanel("Zip Code Stats",
        sidebarLayout(
          sidebarPanel(
            actionButton("select_all", "Select All"),
            checkboxGroupInput("zip", "Select Zip Code(s):",
                               inline = TRUE,
                               choices = unique(covid$geo_id), 
                               selected = unique(covid$geo_id)[1]),
            checkboxGroupInput("variable", "Select Variable(s) to Compare:", 
                               choices = c("case_count"),
                               selected = c("case_count"))),
          mainPanel(plotOutput("my_plot"), verbatimTextOutput("highest_zip")))),
      
      ## Page 3 - make this a pie chart, maybe ethnicity selectable, and then 
      ## radio buttons for which of the several stats, OR just have multiple pie charts? could be cool
      tabPanel("Racial Impacts",
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("race", "Select race(s):", 
                               choices = unique(covid$race_eth), 
                               selected = unique(covid$race_eth)[1]),
            checkboxGroupInput("variable2", "Select to Compare:", 
                               choices = c("case_count"),
                               selected = c("case_count"))),
          mainPanel(plotOutput("plot2")))),
      
      ## Page 4 - table with breakdown of ethnicity per zip code, probably?
      tabPanel("Demographics",
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("checkbox", "Select columns to display:",
                               choices = c("case_count","pop", "hosp_count", "death_count"),
                               selected = "case_count")),
          mainPanel(tableOutput("data_table")))),
      
      tabPanel("Our Observations",
               sidebarLayout(
                 sidebarPanel(
                   h3("Patterns:"),
                   p("Our analysis (look at the example bar chart on the right for reference), showed that certain zip codes and racial groups were disproportionately impacted by the pandemic. In particular, areas with a high concentration of minority communities, were more severely affected than others."),
                   h3("Broader Implications:"),
                   p("The findings suggest that social determinants of health, such as race, location, and socioeconomic status, play a crucial role in determining the level of care and resources available to individuals affected by the pandemic."),
                   h3("Data Quality:"),
                   p("When it comes to data quality, it is important to consider the representativeness of the sample population. In the case of COVID-19 testing data, it is true that the data only reflects the people who have chosen to get tested and have the resources to do so. This means that the data may not be representative of the entire population, particularly those who may have limited access to testing or who choose not to get tested. Our dataset was collected by Public Health – Seattle & King County, and we found it to be of reasonable quality."),
                   h3("Future Ideas:"),
                   p("In the future we hope show more COVID-19 impact indicators, such as vaccination rates, and scrutinize the correlation between social determinants of health and vaccine allocation. Furthermore, we intend to investigate various geographical locations to assess how COVID-19 impact differs among diverse regions and communities.")
                 ),
                 
                 mainPanel(
                   plotOutput("bar_graph")
                 )
               )
              )
    )
  )
)
  

server <- function(input, output, session) {
  
  ## Zip Code Stats - Page 2
  
  ## Racial Impacts - Page 3
  
  ## Demographics - Page 4
  
  filtered_data <- reactive({
    covid %>%
      select(geo_id, input$checkbox)
  })
  
  output$data_table <- renderTable({
    filtered_data()
  })
  
  output$my_plot <- renderPlot({
    data_filtered <- covid %>%
      filter(geo_id %in% input$zip) %>%
      select(input$variable, geo_id)
    
    output$highest_zip <- renderText({
      data_filtered <- covid %>%
        filter(geo_id %in% input$zip) %>%
        group_by(geo_id) %>%
        summarize(total_cases = sum(case_count)) %>%
        arrange(desc(total_cases))
      
      paste("Zip code with the highest COVID cases: ", data_filtered$geo_id[1])
    })
    
    
    ggplot(data_filtered, aes(x = geo_id, y = !!as.symbol(input$variable),fill = geo_id)) +
      geom_bar(stat = "identity") +
      xlab("Zip Code") +
      ylab(input$variable) +
      ggtitle(paste("Bar chart of", input$variable, "by Zip Code")) +
      scale_fill_discrete(name = "Zip Code")
  })
  
  output$plot2 <- renderPlot({
    data_filtered <- covid %>%
      filter(race_eth %in% input$race) %>%
      select(geo_id, race_eth, case_count, pop)
    
    ggplot(data_filtered, aes(x = case_count/pop, y = race_eth , color = race_eth)) +
      geom_line() +
      xlab("Percentage of race per population") +
      ylab(input$variable) +
      ggtitle("a line plot of COVID cases by race per percentage of population") +
      scale_color_discrete(name = "Race")
  })
  
  covid$percent_pop <-   covid$case_count /   covid$pop * 100
  top_race <- unique(covid[order(-covid$percent_pop), ]$race_eth)[1:3]
  dt_top <-   covid[covid$race_eth %in% top_race, ]
  
  output$bar_graph <- renderPlot({
    ggplot(dt_top, aes(x = race_eth, y = percent_pop, fill = race_eth)) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("green", "blue", "pink")) +
      labs(title = "Top Three Race_eth by Case Count Percentage of Pop",
           x = "Race_eth", y = "Percentage of Pop") +
      theme_minimal()  
  })
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "zip", choices = unique(covid$geo_id), selected = unique(covid$geo_id))
  }) 
}

shinyApp(ui, server)