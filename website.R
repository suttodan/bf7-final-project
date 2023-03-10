covid_dataZipcode <- read.csv("COVID19_ZIP-Code (1).csv")

ui <- shinyUI(
  fluidPage(
    h1("Covid-19: The Zip Code and Race Connection"),
    br(),
    h3("Mapping the Impact: How Race and Location Influence COVID-19 Cases", style = "text-align:center;"),
    p(HTML("<strong>This dataset is collected by Public Health – Seattle & King County on Covid-19 cases</strong> grouped by zip code, Race, Population."), style = "text-align:center;"),
    p(HTML("<em>Navigate through the data by clicking on the various tabs to explore the information. To better comprehend which city each zip code belongs to, click on the 'Zip Code Guide' tab. As you review the data, please keep in mind that certain zip codes may be experiencing a greater impact than others. It is important to consider the demographics of the individuals residing in those areas and how factors such as location and race may influence the level of care and resources they receive.</em>"), style = "text-align:center;"),
    br(),
    tabsetPanel(
      tabPanel("Main Page",
               sidebarLayout(
                 sidebarPanel(
                   h3("ABOUT"),
                   p("We are developing a webpage that facilitates a comparison of COVID-19 impact across different zip codes through the use of data visualization, making it easy to interpret and analyze the data.
The webpage we are creating is intended for educators, public health workers, and decision-makers who are looking to gain insights into how different zip codes have been affected by COVID-19.
The primary goal of the website is to shed light on how different zip codes/race groups have been impacted by COVID-19 and how health disparities can vary based on one's geographic location, particularly in terms of prevalent diseases." )
                 ),
                 mainPanel(
                   h3("Below is sample of the data"),
                   tableOutput("sample_data"),
                   p("Below is map of King County"),
                   img(alt = "IMG of King County zip code",
                       src = "https://kingcounty.gov/~/media/operations/GIS/maps/vmc/images/zipcodes_586.ashx?la=en",
                       width = "100%",
                       height = "100%")
                 )
               )
      ),
      tabPanel("Plot",
               sidebarLayout(
                 sidebarPanel(
                   actionButton("select_all", "Select All"),
                   checkboxGroupInput("zip", "Select Zip Code(s):",
                                      choices = unique(covid_dataZipcode$geo_id), 
                                      selected = unique(covid_dataZipcode$geo_id)[1]),
                   
                   checkboxGroupInput("variable", "Select Variable(s) to Compare:", 
                                      choices = c("case_count"),
                                      selected = c("case_count"))
                 ),
                 mainPanel(
                   plotOutput("my_plot"),
                   verbatimTextOutput("highest_zip")
                   
                 )
               )
      ),
      tabPanel("Plot by Race",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("race", "Select race(s):", 
                                      choices = unique(covid_dataZipcode$race_eth), 
                                      selected = unique(covid_dataZipcode$race_eth)[1]),
                   checkboxGroupInput("variable2", "Select to Compare:", 
                                      choices = c("case_count"),
                                      selected = c("case_count")),
                   br(),
                   h3("Key BOX"),
                   p("AIAN: American Indian or Alaska Native: -------- individuals who identify as being descended from the original peoples of North America who maintain cultural identification through tribal affiliation or community recognition.
ASIAN: -------- individuals who identify as being of Asian descent.

BLACK: ------- individuals who identify as being of African descent, including those who are African American, Afro-Caribbean, or from other parts of the African diaspora.


HISPANIC: ------- individuals who identify as being of Hispanic, Latino, or Spanish origin, regardless of race.

Multi/Other: --------- who identify as being of more than one race or ethnicity, or who do not identify with any of the other categories listed.

NHPI: -------  individuals who identify as being descended from the original peoples of Hawaii, Guam, Samoa, or other Pacific Islands.

Unknown: ------- This category includes individuals who did not report their race or ethnicity.

White: -------- This category includes individuals who identify as being of European, Middle Eastern.")
                 ),
                 mainPanel(
                   plotOutput("plot2")
                 )
               )
      ),
      tabPanel("Table",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput(
                     "checkbox",
                     "Select columns to display:",
                     choices = c("case_count","pop", "hosp_count", "death_count"),
                     selected = "case_count"
                   )
                 ),
                 mainPanel(
                   tableOutput("data_table")
                 )
               )
      ),
      tabPanel("Zip Code Guide",
               h3("Zip Code Guide"),
               p("This tab provides a guide to the zip codes included in the dataset."),
               br(),
               p("98001	Algona ----,
98001	Auburn ----,

98001	Federal Way	----,

98002	Auburn	----,

98003	Auburn	----,

98003	Federal Way	----,

98004	Beaux Arts Village	----,

98004	Bellevue	----,

98004	Clyde Hill	----,

98004	Hunts Point	----,

98004	Yarrow Point	----,

98005	Bellevue	----,

98006	Bellevue	----,

98007	Bellevue	----,

98008	Bellevue	----,

98009	Bellevue	----,

98010	Black Diamond	----,

98011	Bothell	----,

98013	Burton	----,

98013	Vashon	----,

98014	Carnation	----,

98015	Bellevue	----,

98019	Duvall	----,

98022	Enumclaw	----,

98023	Auburn	----,

98023	Federal Way	----,

98024	Fall City	----,

98025	Hobart	----,

98027	Issaquah	----,

98028	Kenmore	----,

98028	Bothell	----,

98029	Issaquah	----,

98030	Kent	----,

98031	Kent	----,

98032	Kent ----,

98033	Kirkland	----,

98034	Kirkland	----,

98035	Kent	----,

98038	Maple Valley	----,

98039	Medina	----,

98040	Mercer Island	----,

98041	Bothell	----,

98042	Covington	----,

98042	Kent	----,

98045	North Bend	----,

98047	Auburn	----,

98047	Pacific	----,

98050	Preston	----,

98051	Ravensdale	----,

98052	Redmond	----,

98053	Redmond	----,

98054	Redondo	----,

98055	Renton	----,

98056	Newcastle	----,

98056	Renton	----,

98057	Renton	----,

98058	Renton	----,

98059	Newcastle	----,

98059	Renton	----,

98062	Seahurst	----,

98063	Auburn	----,

98063	Federal Way	----,

98064	Kent	----,

98065	Snoqualmie	----,

98068	Snoqualmie Pass	----,

98068	Snoqualmie	----,

98070	Vashon	----,

98071	Auburn	----,

98072	Woodinville	----,

98073	Redmond	----,

98074	Sammamish	----,

98074	Redmond	----,

98075	Sammamish	----,

98075	Issaquah	----,

98083	Kirkland	----,

98092	Auburn	----,

98093	Auburn	----,

98093	Federal Way	----,

98101	Seattle	----,

98102	Seattle	----,

98103	Seattle	----,

98104	Seattle	----,

98105	Seattle	----,

98106	Seattle	----,

98107	Seattle	----,

98108	Seattle	----,

98108	Tukwila	----,

98109	Seattle	----,

98111	Seattle	----,

98112	Seattle ----,

98114	Seattle	----,

98115	Seattle	----,

98116	Seattle	----,

98117	Seattle	----,

98118	Seattle	----,

98119	Seattle	----,

98121	Seattle	----,

98122	Seattle	----,

98124	Seattle	----,

98125	Seattle	----,

98126	Seattle	----,

98131	Seattle	----,

98132	Seattle	----,

98133	Seattle	----,

98133	Shoreline	----,

98134	Seattle	----,

98136	Seattle	----,

98138	Seattle	----,

98138	Tukwila	----,

98144	Seattle	----,

98145	Seattle	----,

98146	Burien	----,

98146	Seattle	----,

98148	Burien	----,

98148	Des Moines	----,

98148	Normandy Park	----,

98148	Seatac	----,

98148	Seattle	----,

98154	Seattle	----,

98155	Lk Forest Park	----,

98155	Lk Forest Pk	----,

98155	Lake Forest Park ----,

98155	Seattle	----,

98155	Shoreline	----,

98158	Seatac	----,

98158	Seattle	----,

98160	Seattle	----,

98161	Seattle	----,

98164	Seattle	----,

98166	Burien	----,

98166	Normandy Park	----,

98166	Seattle	----,

98168	Burien	----,

98168	Seatac	----,

98168	Seattle	----,

98168	Tukwila	----,

98171	Seattle	----,

98174	Seattle	----,

98177	Seattle	----,

98177	Shoreline	----,

98178	Seattle	----,

98178	Tukwila	----,

98188	Seatac	----,

98188	Seattle	----,

98188	Tukwila	----,

98198	Des Moines	----,

98198	Normandy Park	----,

98198	Seatac	----,

98198	Seattle	----,

98199	Seattle	----,


98224	Baring	----,

98288	Skykomish."),
               br()
               
      )
    ) 
  )
)

server <- function(input, output, session) {
  
  output$sample_data <- renderTable({
    head(covid_dataZipcode)
  })
  
  filtered_data <- reactive({
    covid_dataZipcode %>%
      select(geo_id, input$checkbox)
  })
  
  output$data_table <- renderTable({
    filtered_data()
  })
  
  output$my_plot <- renderPlot({
    data_filtered <- covid_dataZipcode %>%
      filter(geo_id %in% input$zip) %>%
      select(input$variable, geo_id)
    
    output$highest_zip <- renderText({
      data_filtered <- covid_dataZipcode %>%
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
    data_filtered <- covid_dataZipcode %>%
      filter(race_eth %in% input$race) %>%
      select(geo_id, race_eth, case_count, pop)
    
    ggplot(data_filtered, aes(x = case_count/pop, y = race_eth , color = race_eth)) +
      geom_line() +
      xlab("Percentage of race per population") +
      ylab(input$variable) +
      ggtitle("a line plot of COVID cases by race per percentage of population") +
      scale_color_discrete(name = "Race")
    
  })
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "zip", choices = unique(covid_dataZipcode$geo_id), selected = unique(covid_dataZipcode$geo_id))
  }) 
}

shinyApp(ui, server)