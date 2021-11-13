
# !!!!!!!!!1
#Install dashboard theme by below codes, if you dont have library devtools
# please install the devtools first
#library(devtools)
#install_github("nik01010/dashboardthemes")

# package
library(DT)
library(tidyverse)
library(shinydashboard)
library(highcharter) 
library(dashboardthemes)
library(broom)
library(shiny)
library(shinycustomloader)

# Read and process data
death_num <- read.csv("./data/child-deaths-igme-data.csv")
death_rate <- read.csv("./data/child-mortality-igme.csv")
cause <- read.csv("./data/causes-of-death-in-children.csv")
poverty<- read.csv("./data/share-of-population-in-extreme-poverty.csv")
vaccines<- read.csv("./data/global-vaccination-coverage.csv")
Mosquito_nets<-read.csv("./data/children-sleeping-under-treated-bednet.csv")
data(worldgeojson, package = "highcharter")
death_num1<- death_num%>%filter(!Entity == "World")%>%filter(!Code=="")%>%filter(Year >1999)%>%
    rename(death_number = Number.of.under.five.deaths)
death_rate1<-death_rate%>%filter(!Entity == "World")%>%filter(!Code=="")%>%filter(Year >1999)%>%
    rename(mortality = Mortality.rate..under.5..per.1.000.live.births.)
death_all <- merge(death_num1,death_rate1,by = c("Entity","Code","Year"))

cause <-  cause %>%
    rename("Malaria" = "Deaths...Malaria...Sex..Both...Age..Under.5..Number.",
                 "HIV.AIDS" ="Deaths...HIV.AIDS...Sex..Both...Age..Under.5..Number.",
                 "preterm birth complications" = "Deaths...Neonatal.preterm.birth.complications...Sex..Both...Age..Under.5..Number.",
                 "Whooping cough" = "Deaths...Whooping.cough...Sex..Both...Age..Under.5..Number.",
                 "Lower respiratory infections" = "Deaths...Lower.respiratory.infections...Sex..Both...Age..Under.5..Number.",
                 "Congenital birth defects" ="Deaths...Congenital.birth.defects...Sex..Both...Age..Under.5..Number.",
                 "Measles" = "Deaths...Measles...Sex..Both...Age..Under.5..Number.",
                 "neonatal infections" ="Deaths...Neonatal.sepsis.and.other.neonatal.infections...Sex..Both...Age..Under.5..Number.",
                 "Neonatal encephalopathy"="Deaths...Neonatal.encephalopathy.due.to.birth.asphyxia.and.trauma...Sex..Both...Age..Under.5..Number.",
                 "Drowning" = "Deaths...Drowning...Sex..Both...Age..Under.5..Number.",
                 "Tuberculosis" ="Deaths...Tuberculosis...Sex..Both...Age..Under.5..Number.",
                 "Diarrheal diseases" = "Deaths...Diarrheal.diseases...Sex..Both...Age..Under.5..Number.",
                 "Neoplasms" = "Deaths...Neoplasms...Sex..Both...Age..Under.5..Number.",
                 "Meningitis" = "Deaths...Meningitis...Sex..Both...Age..Under.5..Number.",
                 "Nutritional deficiencies" = "Deaths...Nutritional.deficiencies...Sex..Both...Age..Under.5..Number.",
                 "Other neonatal disorders" = "Deaths...Other.neonatal.disorders...Sex..Both...Age..Under.5..Number.")
vaccines <- vaccines %>%
    rename("BCG" = "BCG....of.one.year.olds.immunized.",
           "HepB3" ="HepB3....of.one.year.olds.immunized.",
           "Hib3" = "Hib3....of.one.year.olds.immunized.",
           "IPV1" = "IPV1....of.one.year.olds.immunized.",
           "MCV1" = "MCV1....of.one.year.olds.immunized.",
           "PCV3" ="PCV3....of.one.year.olds.immunized.",
           "Pol3" = "Pol3....of.one.year.olds.immunized.",
           "RCV1" ="RCV1....of.one.year.olds.immunized.",
           "RotaC"="RotaC....of.one.year.olds.immunized.",
           "YFV" = "YFV....of.one.year.olds.immunized.",
           "DTP3" ="DTP3....of.one.year.olds.immunized.",
           "MCV2" = "MCV2....of.children.immunized.")
vaccines1<-vaccines %>%pivot_longer(cols = c(`BCG`:`MCV2`),names_to = "vaccines", values_to = "vaccines_number")

cause1 <-cause %>%
    pivot_longer(cols = c(`Malaria`:`Other neonatal disorders`),names_to = "cause", values_to = "death_number")
cause2 <- cause1%>%group_by(Year,cause)%>%summarise(sum(death_number))%>%rename(sum = `sum(death_number)`)%>%ungroup()%>%
    pivot_wider(names_from = cause,values_from = sum)


##### app.R ##


ui <- dashboardPage(
    dashboardHeader(title = "Child Deaths Analysis"),
    dashboardSidebar(
      ###Overall catalogue
        sidebarMenu(
        menuItem("Global Analysis",
                 menuSubItem("Child Mortality", tabName = "Global"),
                 menuSubItem("Child Death Number", tabName = "Global2"),
                 menuSubItem("Data", tabName = "table1")),
        menuItem("Influencing Factors",
                 menuSubItem("Factors", tabName = "Factor"),
                 menuSubItem("Data", tabName = "table2")),
        menuItem("Measures", 
                 menuSubItem("Measures", tabName = "Measure"),
                 menuSubItem("Data", tabName = "table3")),
        menuItem("About",tabName = "about")
    )),
    ####Contents of each page#######
    dashboardBody(
        tabItems(
          ####first dashboard page###
            tabItem(tabName = "Global",
                    #####dashboard theme######
                    shinyDashboardThemes(theme = "blue_gradient"),
                    ####Title####
                    h2("Child Mortality: Global Analysis ", 
                       style = 	"color:#4682B4"), 
                    fluidRow(
                        column(
                            10,
                            offset = 1,
                            #######country input box#####
                            selectizeInput("countries1", "Select Countries",
                                           unique(death_all$Entity),
                                           multiple = TRUE,
                                           width = "100%"))
                    ),
                 fluidRow( 
                     column(
                         10,
                         hr(),
                         ######help text####
                    helpText("Please Select Countries For Right Plot"))),
                    
                    fluidRow(
                        column(
                            6,
                            #####map output with loader###
                   withLoader(highchartOutput("map",width = "100%"),type = "html", loader = "loader1")),
                   column(
                     2,
                     #####text box####
                     tags$div(
                       tags$h4("Findings"), 
                       "We can see that the highest child mortality rate in the world is in Africa, far higher than in other regions, but that child mortality is declining all over the world.")),
                   column(
                       4,
                       #####line chart output with loader###
                  withLoader(highchartOutput("plot1",width = "100%"),type = "html", loader = "loader1"))),
                   ###year box##
                    sliderInput("Year",
                                "Year",
                                min = 2000,
                                max = 2019,
                                value = 2019,
                                sep = "",
                                width = "100%",
                                animate =
                                    animationOptions(interval = 500, loop = F)
                    ),
                   ),
            tabItem(tabName = "Global2",
                    shinyDashboardThemes(theme = "blue_gradient"),
                    h2("Child Death Number: Global Analysis ", 
                       style = 	"color:#4682B4"), 
                    
                    fluidRow(
                        column(
                            10,
                            offset = 1,
                            selectizeInput("countries2", "Select Countries",
                                           unique(death_all$Entity),
                                           multiple = TRUE,
                                           width = "100%"))
                    ),
                    fluidRow( 
                        column(
                            10,
                            hr(),
                            helpText("Please Select Countries For Right Plot"))),
                    
                    fluidRow(
                        column(
                            6,
                        withLoader(highchartOutput("map2",width = "100%"),type = "html", loader = "loader1")),
                        column(
                          2,
                          #####text box####
                          tags$div(
                            tags$h4("Findings"), 
                            "The country with the highest number of child deaths in the world is Nigeria, followed by India and Pakistan, most likely due to the demographic impact received.")),
                        column(
                            4,
                        withLoader(highchartOutput("plot2",width = "100%"),type = "html", loader = "loader1"))),
                    sliderInput("Year2",
                                "Year",
                                min = 2000,
                                max = 2019,
                                value = 2019,
                                sep = "",
                                width = "100%",
                                animate =
                                    animationOptions(interval = 500, loop = F)
                    ) ),
            
            tabItem(tabName = "table1",
                    shinyDashboardThemes(theme = "blue_gradient"),
                    h2("Data: Global Analysis  ", 
                       style = 	"color:#4682B4"), 
                    
                    fluidRow(dataTableOutput(outputId = "Table1", width = "100%"))
            ),
            
            tabItem(tabName = "Factor",
                    shinyDashboardThemes(theme = "blue_gradient"),
                    h2("Child Death: Factor Analysis ", 
                       style = 	"color:#4682B4"), 
                    fluidRow(
                        column(
                            4,
                            withLoader(highchartOutput("plot3"),type = "html", loader = "loader1")),
                        column(
                          4,
                          #####text box####
                          tags$div(
                            tags$h4("Findings"), 
                            "There is a clear positive correlation between poverty and child mortality, with the higher the proportion of people living in poverty the higher the child mortality rate.The biggest killers of child deaths are pneumonia, complications of prematurity and diarrhoeal diseases, all of which are causing a year-on-year decline in child deaths.")),
                        column(
                            4,
                            withLoader(highchartOutput("plot4"),type = "html", loader = "loader1"))),
                    
                    fluidRow(
                        column(
                            4,
                            withLoader(highchartOutput("plot5"),type = "html", loader = "loader1")),
                        column(
                            3,
                            offset = 1,
                            ###just select one country###
                            selectInput("countries4",
                                        "Select a Country For Left Plot",
                                        unique(cause1$Entity)),
                            ####select many countries###
                            selectizeInput("countries3", "Select Countries For Right Plot",
                                           unique(cause1$Entity),
                                           multiple = TRUE),
                            selectInput("disease", "Select A Disease For Right Plot",
                                       unique(cause1$cause)),
                            ####two side year box###
                      sliderInput("Year3",
                                "Year",
                                min = 2000,
                                max = 2017,
                                value = c(2000,2017),
                                sep = "",
                                width = "70%")),
                      column(
                          4,
                         withLoader(highchartOutput("plot6",width = "100%"),type = "html", loader = "loader1"))
                          
                    )
            ),
        
        
            tabItem(tabName = "table2",
                    shinyDashboardThemes(theme = "blue_gradient"),
                    h2("Data: Factor Analysis  ", 
                       style = 	"color:#4682B4"), 
                    
                    fluidRow(
                            dataTableOutput(outputId = "Table2", width = "100%"),
                       
                        dataTableOutput(outputId = "Table3", width = "100%"))
            ),
            
            tabItem(tabName = "Measure",
                    shinyDashboardThemes(theme = "blue_gradient"),
                    h2("Child Death: Measure Analysis ", 
                       style = 	"color:#4682B4"), 
                    fluidRow(
                        column(
                            4,
                            withLoader(highchartOutput("plot7"),type = "html", loader = "loader1")),
                        column(
                            4,
                            tags$div(
                              tags$h4("Findings"), 
                              "Countries are now actively developing their economies and the proportion of people living in extreme poverty is decreasing every year.In countries where malaria is rampant, countries are actively promoting the use of mosquito nets, and despite the lack of data, we can see that the prevalence of nets is increasing.",
                              tags$br(),tags$br(),
                              "DTP3 refers to the triple vaccine for diphtheria, tetanus and pertussis and is a key indicator of vaccine coverage; RotaC (rotavirus vaccine) is a commonly used vaccine for diarrhoeal diseases, and PVC3 and Hib3 are common vaccines for lower respiratory diseases (Johnson, 2021). These vaccines are available in countries around the world and world vaccination rates are increasing year on year. This increase in vaccination rates has been accompanied by a decrease in the number of child deaths from the corresponding diseases each year.")),
                        column(
                            4,
                            withLoader(highchartOutput("plot8"),type = "html", loader = "loader1"))
                        
                    ),
                    
                    fluidRow(
                        column(
                            4,
                            withLoader(highchartOutput("plot9"),type = "html", loader = "loader1")),
                        column(
                            3,
                            offset = 1,
                            selectInput("countries7",
                                        "Select a Country For Left Plot",
                                        unique(vaccines1$Entity)),
                            selectizeInput("countries8", "Select Countries For Right Plot",
                                           unique(vaccines1$Entity),
                                           multiple = TRUE),
                            selectInput("vaccines", "Select A Disease For Right Plot",
                                        unique(vaccines1$vaccines)),
                            sliderInput("Year5",
                                        "Year",
                                        min = 2000,
                                        max = 2019,
                                        value = c(2000,2019),
                                        sep = "",
                                        width = "70%")),
                        column(
                            4,
                           withLoader(highchartOutput("plot10",width = "100%"),type = "html", loader = "loader1"))
                        
                    )
            ),
            
            tabItem(tabName = "table3",
                    shinyDashboardThemes(theme = "blue_gradient"),
                    h2("Data: Measures  ", 
                       style = 	"color:#4682B4"), 
                    
                    fluidRow(
                        dataTableOutput(outputId = "Table4", width = "100%"),
                        
                        dataTableOutput(outputId = "Table5", width = "100%"))
            ),
            ####about page####
            tabItem(tabName = "about",
                    shinyDashboardThemes(theme = "blue_gradient"),
                    h2("About", 
                       style = 	"color:#4682B4"),
                    tags$div(
                      tags$h4("Background"), 
                      "The death of a child is a tragedy for families and many families are broken as a result,
                      but the fact that we rarely see data on child (under 5) deaths in the news makes one wonder what is really happening with child deaths in the world. 
                      High quality data on child deaths in the world is freely available to our students,but as the latest data is only updated to 2019, I have only analysed the data here up to 2019. ",
                      tags$br(),tags$br(),
                      tags$h4("Questions"), 
                      "In this shiny app, I will explore the following questions." ,
                      tags$br(),
                      "1. What is the situation of child mortality in the world today?", 
                      tags$br(),
                      "2. What is the factors that influence child mortality?" ,
                      tags$br(),
                      "3. Has the country taken any measures?",
                      tags$br(),tags$br(),
                      tags$h4("Data sources"), 
                      "(1)",tags$a(href="https://ourworldindata.org/grapher/child-mortality-igme","child-deaths-igme-data.csv"),
                      tags$br(),
                      "(2)",tags$a(href=" https://ourworldindata.org/grapher/child-mortality-igme?tab=table"," child-mortality-igme-data.csv"),
                      tags$br(),
                      "(3)",tags$a(href=" https://ourworldindata.org/grapher/causes-of-death-in-children?country=~OWID_WRL"," causes-of-death-in-children"),
                      tags$br(),
                      "(4)",tags$a(href="https://ourworldindata.org/grapher/global-vaccination-coverage?country=~OWID_WRL","share-of-population-in-extreme-poverty"),
                      tags$br(),
                      "(5)",tags$a(href="https://ourworldindata.org/grapher/children-sleeping-under-treated-bednet"," children-sleeping-under-treated-bednet"),
                      tags$br(),
                      "(6)",tags$a(href=" https://ourworldindata.org/grapher/share-of-population-in-extreme-poverty?tab=chart&country=BGD%7EBOL%7EM DG%7EIND%7ECHN%7EETH%7ECOD"," share-of-population-in-extreme-poverty"),
                      tags$br(),tags$br(),tags$h4("Code"),
                      "The highchart map references the code from ",tags$a(href="https://www.datanovia.com/en/lessons/highchart-interactive-world-map-in-r/"," here"), " (2018) from datanovia",
                      tags$br(),tags$br(),tags$h4("References"),
                      "Hua, J., & Shaw, R. (2020). Corona Virus (COVID-19). Infodemic and Emerging Issues through a Data Lens: The Case of China. International Journal Of Environmental Research And Public Health, 17(7), 2309. " ,tags$a(href="doi: 10.3390/ijerph17072309", "doi: 10.3390/ijerph17072309."),
                      tags$br(),tags$br(),
                      "Ritchie, H., Mathieu, E., Rodés-Guirao, L., Appel, C., Giattino, C., & Ortiz-Ospina, E. et al. (2021). Coronavirus Pandemic (COVID-19). Retrieved 1 September 2021, from " ,tags$a(href="https://ourworldindata.org/coronavirus", "https://ourworldindata.org/coronavirus."),
                      tags$br(),tags$br(),
                      "Shiny - COVID-19 tracker. (2021). Retrieved 20 October 2021, from ",tags$a(href="https://shiny.rstudio.com/gallery/covid19-tracker.html", "https://shiny.rstudio.com/gallery/covid19-tracker.html."),
                      tags$br(),tags$br(),
                      "Shiny - Drag-and-drop U.S. mapmaker. (2021). Retrieved 23 October 2021, from ",tags$a(href="https://shiny.rstudio.com/gallery/drag-drop-map-maker.html", "https://shiny.rstudio.com/gallery/drag-drop-map-maker.html."),
                      
                    ))
            
            
        ))
)

server <- function(input, output, session) {
    ####Acceptance of controlled data#####
    yearData <- reactive({
        death_all1 <- death_all%>%
            filter(Year == input$Year)%>%
          select(Entity,mortality)%>%
          rename(country = "Entity")%>%
          rename(value = "mortality")
       
    })
    
    yearData2 <- reactive({
        death_all2 <- death_all%>%
            filter(Year == input$Year2)%>%
          select(Entity,death_number)%>%
          rename(country = "Entity")%>%
          rename(value = "death_number")
    })
    
    ####highchart map####
    output$map <- renderHighchart({
      map <- highchart() %>%
        hc_add_series_map(worldgeojson, 
                          yearData(), 
                          value = "value", 
                          joinBy = c('name','country'),
                          name = "child mortality")  %>% 
        hc_colorAxis(stops = color_stops()) %>% 
        hc_mapNavigation(enabled = TRUE)%>%
       hc_title(text = "World Map") %>% 
        hc_subtitle(text = "World child mortality rate in years")
      map
     
    })
    
    plot1 <- reactive({
        death_mortality2 <- death_all %>% 
            filter(Year <= input$Year,
                   Entity %in% input$countries1) 
        death_mortality2
    })
   
    output$plot1 <- renderHighchart({
        #####Text reminding you to select a country will appear for images that cannot be displayed until a country is selected
        validate(
            need(input$countries1, "please select a country")
        )
    ####highchart line graph
      highchart() %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_add_series(plot1(), hcaes(x = Year, y = mortality, group = Entity),type = "line") %>%
        hc_tooltip(crosshairs = TRUE) %>%
        hc_title(text = "Child mortality by country") %>% 
        hc_legend(enabled = FALSE)
        
    })
    
    output$map2 <- renderHighchart({
        
      map2 <- highchart() %>%
        hc_add_series_map(worldgeojson, 
                          yearData2(), 
                          value = "value", 
                          joinBy = c('name','country'),
                          name = "child death number")  %>% 
        hc_colorAxis(stops = color_stops()) %>% 
        hc_mapNavigation(enabled = TRUE)%>%
        hc_title(text = "World Map") %>% 
        hc_subtitle(text = "World child death number in years")
      map2
        
    })
    
    plot2 <- reactive({
        death_number2 <- death_all %>% 
            filter(Year <= input$Year2,
                   Entity %in% input$countries2) 
        death_number2
    })
    
    output$plot2 <- renderHighchart({
        
        validate(
            need(input$countries2, "please select a country")
        )
        
      highchart() %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_add_series(plot2(), hcaes(x = Year, y =death_number, group = Entity),type = "line") %>%
        hc_tooltip(crosshairs = TRUE) %>%
        hc_title(text = "Child deaths number by country") %>% 
        hc_legend(enabled = FALSE)
        
    })
    ####datatable output
    output$Table1 <- renderDataTable({ 
        
        datatable(death_all,
                  rownames = FALSE,
                  filter = 'top',
                  caption = "Table 1: World data on child mortality and death rates",
                  extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy','csv','excel','pdf')))
        
        })
    
    ####highchart liner model
    output$plot3 <- renderHighchart({
        compare <- left_join(poverty,death_rate,by = c("Entity","Year","Code"))%>%
            rename("extreme poverty rate" = "X.1.90.per.day...share.of.population.below.poverty.line","Mortality rate"= "Mortality.rate..under.5..per.1.000.live.births.")%>%
            filter(Year > 1999)
        
        model <- lm(`Mortality rate` ~ `extreme poverty rate`, data = compare)
        fit <- augment(model) %>% 
          arrange(`extreme poverty rate`)
        
        compare %>% 
            hchart('scatter', 
                   hcaes(x =`extreme poverty rate`, y = `Mortality rate`)) %>%
            hc_add_series(fit, 
                          type = "line", 
                          hcaes(x = `extreme poverty rate` , y = .fitted),
                          name = "Fit", id = "fit")  %>%
            hc_tooltip(crosshairs = TRUE) %>%
            hc_title(text = "The relationship between poverty and child mortality")
        
    })
    
    output$plot4 <- renderHighchart({
  
        highchart() %>% 
            hc_xAxis(categories = cause2$Year)%>%
            hc_add_series(name = "Malaria",data = cause2$Malaria)%>%
            hc_add_series(name = "Congenital birth defects",data = cause2$`Congenital birth defects`)%>%
            hc_add_series(name = "Diarrheal diseases",data = cause2$`Diarrheal diseases`)%>%
            hc_add_series(name = "Drowning",data = cause2$Drowning)%>%
            hc_add_series(name = "HIV.AIDS",data = cause2$HIV.AIDS)%>%
            hc_add_series(name = "Lower respiratory infections",data = cause2$`Lower respiratory infections`)%>%
            hc_add_series(name = "Measles",data = cause2$Measles)%>%
            hc_add_series(name = "Meningitis",data = cause2$Meningitis)%>%
            hc_add_series(name = "Neonatal encephalopathy",data = cause2$`Neonatal encephalopathy`)%>%
            hc_add_series(name = "neonatal infections",data = cause2$`neonatal infections`)%>%
            hc_add_series(name = "Neoplasms",data = cause2$Neoplasms)%>%
            hc_add_series(name = "Nutritional deficiencies",data = cause2$`Nutritional deficiencies`)%>%
            hc_add_series(name = "Other neonatal disorders",data = cause2$`Other neonatal disorders`)%>%
            hc_add_series(name = "preterm birth complications",data = cause2$`preterm birth complications`)%>%
            hc_add_series(name = "Tuberculosis",data = cause2$Tuberculosis)%>%
            hc_add_series(name = "Whooping cough",data = cause2$`Whooping cough`)%>%
            hc_title(text = "Child deaths caused by diseases worldwide") %>% 
            hc_subtitle(text = "1990-2017") %>%
            hc_legend(enabled = FALSE)%>%
            hc_tooltip(crosshairs = TRUE)
        
    })
    
    plot5_data <-reactive({
        
        plot5data1 <- cause1%>%
            filter(Year >= input$Year3[1] & Year <= input$Year3[2])%>%
            filter(Entity == input$countries4)
    })
    
    output$plot5 <- renderHighchart({
        
        validate(
            need(input$countries4, "please select a country")
        )
        
        highchart() %>% 
            hc_xAxis(title = list(text = "Year")) %>%
            hc_add_series(plot5_data(), hcaes(x = Year, y =death_number, group = cause),type = "line") %>%
            hc_tooltip(crosshairs = TRUE) %>%
            hc_title(text = "Child deaths due to various diseases in the selected countries") %>% 
            hc_legend(enabled = FALSE)
            
    })
    
    plot5_data2 <-reactive({
        
        validate(
            need(input$countries3, "please select a country")
        )
        
        validate(
            need(input$disease, "please select a disease")
        )
        
        plot5data2 <- cause1%>%
            filter(Year >= input$Year3[1] & Year <= input$Year3[2])%>%
            filter(Entity %in% input$countries3)%>%
            filter(cause == input$disease)
    })
    
    output$plot6 <- renderHighchart({
        
      highchart() %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_add_series(plot5_data2(), hcaes(x = Year, y = death_number, group = Entity),type = "line") %>%
        hc_tooltip(crosshairs = TRUE) %>%
        hc_title(text = "Deaths of children due to a disease") %>% 
        hc_legend(enabled = FALSE)

        
    })
    
    
    output$Table2 <- renderDataTable({ 
        
        datatable(cause,
                  rownames = FALSE,
                  filter = 'top',
                  caption = "Table 2: Disease factors",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(dom = 'Bfrtip',
                                 scrollY = 650,
                                 scrollX = 500,
                                 fixedColumns = TRUE,
                                 buttons = c('copy','csv','excel','pdf')))
        
    })
    
    output$Table3 <- renderDataTable({ 
        
        datatable(poverty,
                  rownames = FALSE,
                  filter = 'top',
                  caption = "Table 3: Economic factors",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(dom = 'Bfrtip',
                                 scrollY = 650,
                                 scrollX = 500,
                                 fixedColumns = TRUE,
                                 buttons = c('copy','csv','excel','pdf')))
        
    })
    
    output$Table4 <- renderDataTable({ 
        
        datatable(vaccines,
                  rownames = FALSE,
                  filter = 'top',
                  caption = "Table 4: Vaccines",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(dom = 'Bfrtip',
                                 scrollY = 650,
                                 scrollX = 500,
                                 fixedColumns = TRUE,
                                 buttons = c('copy','csv','excel','pdf')))
        
    })
    
    output$plot7 <- renderHighchart({
        
        plot7_data <-reactive({
            
            plot7data1 <- poverty%>%
                rename("extreme poverty rate" = "X.1.90.per.day...share.of.population.below.poverty.line")%>%
                filter(Year >= input$Year5[1] & Year <= input$Year5[2])%>%
                filter(Entity %in% input$countries8)
        })
        
        validate(
            need(input$countries8, "please select a country")
        )
        
        highchart() %>% 
            hc_xAxis(title = list(text = "Year")) %>%
            hc_add_series(plot7_data(), hcaes(x = Year, y =`extreme poverty rate`, group = Entity),type = "line") %>%
            hc_tooltip(crosshairs = TRUE) %>%
            hc_title(text = "Change in the proportion of people living in absolute poverty in selected countries") %>% 
            hc_legend(enabled = FALSE)
        
    })
    
    output$plot8 <- renderHighchart({
        
        plot8_data <-reactive({
            
            plot8data1 <- Mosquito_nets %>%
                rename(bed_nets_rate = "Use.of.insecticide.treated.bed.nets....of.under.5.population.")%>%
                filter(Year >= input$Year5[1] & Year <= input$Year5[2])%>%
                filter(Entity %in% input$countries8)
        })
        
        validate(
            need(input$countries8, "please select a country")
        )
        
        highchart() %>% 
            hc_xAxis(title = list(text = "Year")) %>%
            hc_add_series(plot8_data(), hcaes(x = Year, y =bed_nets_rate, group = Entity),type = "line") %>%
            hc_tooltip(crosshairs = TRUE) %>%
            hc_title(text = "Changes in the popularity of mosquito nets in selected countries") %>% 
            hc_legend(enabled = FALSE)
        
    })
    
    plot9_data <-reactive({
        
        plot9data1 <- vaccines1%>%
            filter(Year >= input$Year5[1] & Year <= input$Year5[2])%>%
            filter(Entity == input$countries7)
    })
    
    output$plot9 <- renderHighchart({
        
        validate(
            need(input$countries7, "please select a country")
        )
        
        highchart() %>% 
            hc_xAxis(title = list(text = "Year")) %>%
            hc_add_series(plot9_data(), hcaes(x = Year, y =vaccines_number, group = vaccines),type = "line") %>%
            hc_tooltip(crosshairs = TRUE) %>%
            hc_title(text = "Prevalence of various vaccines") %>% 
            hc_legend(enabled = FALSE)
        
    })
    
    plot10_data <-reactive({
        
        validate(
            need(input$countries8, "please select a country")
        )
        
        validate(
            need(input$vaccines, "please select a vaccines")
        )
        
        plot10data <- vaccines1%>%
            filter(Year >= input$Year5[1] & Year <= input$Year5[2])%>%
            filter(Entity %in% input$countries8)%>%
            filter(vaccines == input$vaccines)
    })
    
    output$plot10 <- renderHighchart({
        
      highchart() %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_add_series(plot10_data(), hcaes(x = Year, y = vaccines_number, group = Entity),type = "line") %>%
        hc_tooltip(crosshairs = TRUE) %>%
        hc_title(text = "Vaccination status of certain vaccines") %>% 
        hc_legend(enabled = FALSE)
      
        
    })
    
    output$Table5 <- renderDataTable({ 
        
        datatable(vaccines,
                  rownames = FALSE,
                  filter = 'top',
                  caption = "Table 5: Prevalence of mosquito nets",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(dom = 'Bfrtip',
                                 scrollY = 650,
                                 scrollX = 500,
                                 fixedColumns = TRUE,
                                 buttons = c('copy','csv','excel','pdf')))
        
    })
}

shinyApp(ui, server)