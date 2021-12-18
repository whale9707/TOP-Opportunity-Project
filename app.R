library(shiny)
library(urbnmapr)
library(leaflet)
library(RColorBrewer)
library (ggplot2)
library(tidyverse)
library(readxl)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(maps)
library(viridis)
library(DT)
library(dplyr)

# preprocess data in overview
overview <- read_excel("overview.xlsx")

## preprocess data in residents tab

residents <- read_excel("residents.xlsx")
colnames(residents)[which(names(residents) == "State of Residence")] <- "state_name" #needs to match the column name in the urbnmapr datasets
colnames(residents)[which(names(residents) == "Major Class of Admission")] <- "addmission_class"
residents<-residents %>%  mutate(across(where(is.character), ~na_if(., "D"))) #change the D values to NA
residents <- tibble::rowid_to_column(residents, "ID")
#Convert data from wide to long format
residents_long <-gather(residents, year, newresidents, '2019_Admissions':'2010_Admission', factor_key=TRUE)
residents_long$year<- recode(residents_long$year, '2019_Admissions' = 2019, '2018_Admission' = 2018, '2017_Admission'=2017,
                             '2016_Admission'=2016, '2015_Admission'=2015, '2014_Admission'=2014, '2013_Admission'=2013,
                             '2012_Admission'=2012, '2011_Admission'=2011, '2010_Admission'=2010)
residents_long=as.data.frame(residents_long)
residents_long$year_new <- substr(residents_long$year, 1, 4)
residents_long$ID <- as.character(residents_long$ID)
residents_long$newresidents <- as.numeric(residents_long$newresidents)
residents_long$admission_class <- residents$addmission_class
residents_long<-na.omit(residents_long)
residents_new <- aggregate(residents_long$newresidents, 
                           by=list(state_name=residents_long$state_name, 
                                   year = residents_long$year_new,
                                   origin_country = residents_long$`Country of Birth`,
                                   admission_class = residents_long$admission_class), 
                           FUN=sum)
residents_new <- setNames(residents_new, replace(names(residents_new), names(residents_new) == 'x', 'residents_sum'))
residents_long <- residents_new

paletteBins <- c(0,  50, 100, 500, 1000, 2000, 3000, 5000, 7500, 10000)
colorPalette <- colorBin(palette = "Blues", domain = residents_long$residents_sum, na.color = "transparent", bins = paletteBins)

# preprocess data in decennial data

df <- read_excel("decennial_df_new.xlsx")

# data in refugee tab
# data in refugee tab

refugee_country <- read_excel("REFUGEE ARRIVALS BY COUNTRY OF NATIONALITY FISCAL YEARS 2010 TO 2019.xlsx",skip = 2)

refugee_country[refugee_country == "X"] <- "0"

colnames(refugee_country)[1] <- "Country of Nationality"
colnames(refugee_country)[2] <- "Number of Refugee 2010"
colnames(refugee_country)[3] <- "Number of Refugee 2011"
colnames(refugee_country)[4] <- "Number of Refugee 2012"
colnames(refugee_country)[5] <- "Number of Refugee 2013"
colnames(refugee_country)[6] <- "Number of Refugee 2014"
colnames(refugee_country)[7] <- "Number of Refugee 2015"
colnames(refugee_country)[8] <- "Number of Refugee 2016"
colnames(refugee_country)[9] <- "Number of Refugee 2017"
colnames(refugee_country)[10] <- "Number of Refugee 2018"
colnames(refugee_country)[11] <- "Number of Refugee 2019"

refugee_country$refugee_2010 <- as.numeric(refugee_country$"Number of Refugee 2010")
refugee_country$refugee_2011 <- as.numeric(refugee_country$"Number of Refugee 2011")
refugee_country$refugee_2012 <- as.numeric(refugee_country$"Number of Refugee 2012")
refugee_country$refugee_2013 <- as.numeric(refugee_country$"Number of Refugee 2013")
refugee_country$refugee_2014 <- as.numeric(refugee_country$"Number of Refugee 2014")
refugee_country$refugee_2015 <- as.numeric(refugee_country$"Number of Refugee 2015")
refugee_country$refugee_2016 <- as.numeric(refugee_country$"Number of Refugee 2016")
refugee_country$refugee_2017 <- as.numeric(refugee_country$"Number of Refugee 2017")
refugee_country$refugee_2018 <- as.numeric(refugee_country$"Number of Refugee 2018")
refugee_country$refugee_2019 <- as.numeric(refugee_country$"Number of Refugee 2019")

refugee_country$compared_10_11 <- refugee_country$refugee_2011 - refugee_country$refugee_2010
refugee_country$compared_11_12 <- refugee_country$refugee_2012 - refugee_country$refugee_2011
refugee_country$compared_12_13 <- refugee_country$refugee_2013 - refugee_country$refugee_2012
refugee_country$compared_13_14 <- refugee_country$refugee_2014 - refugee_country$refugee_2013
refugee_country$compared_14_15 <- refugee_country$refugee_2015 - refugee_country$refugee_2014
refugee_country$compared_15_16 <- refugee_country$refugee_2016 - refugee_country$refugee_2015
refugee_country$compared_16_17 <- refugee_country$refugee_2017 - refugee_country$refugee_2016
refugee_country$compared_17_18 <- refugee_country$refugee_2018 - refugee_country$refugee_2017
refugee_country$compared_18_19 <- refugee_country$refugee_2019  - refugee_country$refugee_2018

world <- map_data("world")

refugee_country$region <- refugee_country$"Country of Nationality"
refugee_vis <- left_join(world, refugee_country)

re2010 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2010), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(fill = "refugee_num") +
  theme_update(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  ggtitle("Refugee Arrivals By Country of Nationality 2010")


re2011 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2011), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(fill = "refugee_num") +
  theme_update(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  ggtitle("Refugee Arrivals By Country of Nationality 2011")

re2012 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2012), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(fill = "refugee_num") +
  theme_update(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  ggtitle("Refugee Arrivals By Country of Nationality 2012")

re2013 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2013), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(fill = "refugee_num") +
  theme_update(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  ggtitle("Refugee Arrivals By Country of Nationality 2013")

re2014 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2014), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(fill = "refugee_num") +
  theme_update(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  ggtitle("Refugee Arrivals By Country of Nationality 2014")

re2015 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2015), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2015", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2016 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2016), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(fill = "refugee_num") +
  theme_update(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  ggtitle("Refugee Arrivals By Country of Nationality 2016")

re2017 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2017), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(fill = "refugee_num") +
  theme_update(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  ggtitle("Refugee Arrivals By Country of Nationality 2017")

re2018 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2018), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(fill = "refugee_num") +
  theme_update(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  ggtitle("Refugee Arrivals By Country of Nationality 2018")

re2019 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2019), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(fill = "refugee_num") +
  theme_update(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  ggtitle("Refugee Arrivals By Country of Nationality 2019")

yeartry <- tibble::tribble(
  ~year,
  "2019", 
  "2018", 
  "2017",
  "2016",
  "2015",
  "2014",
  "2013",
  "2012",
  "2011",
  "2010"
)

## design the ui
ui <- dashboardPage(
  dashboardHeader(title = "TrendR", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("About Us", tabName = "introduction", icon = icon("address-book")),
      menuItem("How to use this Dashboard", tabName = "summary", icon = icon("book-open")),
      menuItem("Overview", tabName = "overview", icon = icon("list")),
      menuItem("Proportion of State Population", tabName = "decennial", icon = icon("chart-bar")),
      menuItem("African Immigrants in the US", tabName = "residents", icon = icon("dashboard")),
      menuItem("African Refugees in the US", tabName = "refugees", icon = icon("th")),
      
      menuItem("Appendix", tabName = "appendix", icon = icon("ellipsis-h"))
    )
  ),
  dashboardBody(
    tabItems(
              #introduction
              tabItem(tabName = "introduction",
                      htmlOutput("intro"),
                      uiOutput("tab1"),
                      uiOutput("tab2"),
                      uiOutput("tab3"),
                      uiOutput("tab4"),
                      uiOutput("tab5"),
                      uiOutput("tab6")
        
      ),
      tabItem(tabName = "summary",
              htmlOutput("summary")
      ),
      tabItem(tabName = "appendix",
              htmlOutput("appendix")
      ),
      
    
  
      #first tab content
      tabItem(tabName = "overview",
              #h1("Overview for African American by Year"),
              #box(column(width = 6, plotOutput("age")), column(width = 6, plotOutput("birthplace")), width = 18),
              #box(column(width = 6, plotOutput("education")), column(width = 6, plotOutput("employment")), width = 18),
              #box(column(width = 6, plotOutput("income")), column(width = 6, plotOutput("school")), width = 18)
              tabBox(
                title = "Overview for African Americans by Year",
                width = 18,
                tabPanel("Age, Employment & Income", 
                         plotOutput("age"),
                         plotOutput("employment"),
                         plotOutput("income")),
                tabPanel("Education & School Enrollment",
                         plotOutput("education"),
                         plotOutput("school"))
              )
              
      )
      
      
      ,
      # third tab content
      tabItem(tabName = "residents", height = 100, 
              h1("African Immigrants admitted to the US by year"),
              box(sliderInput("yeard", "Year (2010-2019):",
                                   min = 2010, max = 2019,
                                   value = 2010, step = 1, sep = "",
                                   animate = animationOptions(interval = 300, loop = TRUE)),height = 100),
              box(pickerInput(inputId = "origin_country",
                                   label = "Country of Birth",
                                   choices= unique(residents_long$origin_country),
                                   options = list(`actions-box` = TRUE),
                                   multiple = T), height = 100),
              box(leafletOutput("map"), width = 12),
            #  valueBoxOutput("locationBox", width=4), 
              valueBoxOutput("progressBox",width= 8), 
              tags$head(tags$style(HTML(".small-box {height: 130px}"))),
              valueBoxOutput(outputId = "statemaxBox"),
              box(plotOutput("admission"), width = 12)
            ),
      
      # Second tab content
    tabItem(tabName = "decennial",
            h1("African American Population (2010-2020), by state"),
            box(plotOutput("decennial"), width = 18),
            br(),
            box(DT::dataTableOutput("decennial_table"), width = 18)
    ),

# Third tab content: refugee
  tabItem(tabName = "refugees",
          titlePanel("Refugee Arrivals By Country of Nationality"),
          sidebarLayout(
            sidebarPanel(
              selectInput(inputId = "year",
                          label = "Choose a year:",
                          choices = (yeartry$year)),
              tableOutput("view")),
            mainPanel(
              imageOutput("refugeemap"),
              plotOutput("refugeebar", height = "550px")
            )
          )
        )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "residents" = residents,
           "pressure" = pressure,
           "cars" = cars)
  })
  sliderValues <- reactive({
    data.frame(
      Name = c("Decennial Data Years"),
      Value = as.character(c(input$yeard)),
      stringsAsFactors = FALSE)
  })
  sliderValues <- reactive({
    data.frame(
      Name = c("Choose a country of birth:"),
      Value = as.character(c(input$origin_country)),
      stringsAsFactors = FALSE)
  })
  

  output$age <- renderPlot({
    ggplot(overview,aes(x=age_year,y=age_group_count,colour=age_group,group=age_group,fill=age_group)) +
      geom_bar(position="stack", stat="identity", color = NA)+
      scale_y_continuous(name="population", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'African American Population by Age')+
      scale_fill_brewer() +
      theme(legend.position="top", 
            legend.text=element_text(size=10), 
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
            panel.background = element_rect(fill = "transparent", color = 'black', linetype = 'dashed'), 
            plot.background = element_rect(fill = "transparent", color = 'black'), 
            panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "black"), 
            panel.grid.minor = element_blank(), 
            legend.background = element_rect(fill = "transparent")
            )
  })
  output$education <- renderPlot({
    ggplot(overview,aes(x=education_year,y=education_count,colour=education,group=education,fill=education)) +
      geom_bar(position="stack", stat="identity", color = NA)+
      scale_y_continuous(name="population", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'African American Population by Education Background')+
      scale_fill_brewer() +
      theme(legend.position="top", 
            legend.text=element_text(size=10), 
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
            panel.background = element_rect(fill = "transparent", color = 'black', linetype = 'dashed'), 
            plot.background = element_rect(fill = "transparent", color = 'black'), 
            panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "black"), 
            panel.grid.minor = element_blank(), 
            legend.background = element_rect(fill = "transparent")
      )
  })
  output$employment <- renderPlot({
    ggplot(overview,aes(x=employment_year,y=employment_count,colour=employment,group=employment,fill=employment)) +
      geom_bar(position="stack", stat="identity", color = NA)+
      scale_y_continuous(name="population", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'African American Population by Employment Status')+
      scale_fill_brewer() +
      theme(legend.position="top", 
            legend.text=element_text(size=10), 
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
            panel.background = element_rect(fill = "transparent", color = 'black', linetype = 'dashed'), 
            plot.background = element_rect(fill = "transparent", color = 'black'), 
            panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "black"), 
            panel.grid.minor = element_blank(), 
            legend.background = element_rect(fill = "transparent")
      )
  })
  output$income <- renderPlot({
    ggplot(overview,aes(x=income_year,y=income_count,colour=income,group=income,fill=income)) +
      geom_line(size = 1.5)+
      geom_point()+
      scale_y_continuous(name="Income", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'Income for African American in the Past 12 Months', subtitle = 'In 2010 inflation-adjusted dollars')+
      scale_color_manual(values=c("#000033", "#0000CC", "#CC99FF", "#66FFFF", "#660099", "#6600FF")) + 
      theme(legend.position="top", 
            legend.text=element_text(size=10), 
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
            panel.background = element_rect(fill = "transparent", color = 'black', linetype = 'dashed'), 
            plot.background = element_rect(fill = "transparent", color = 'black'), 
            panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "black"), 
            panel.grid.minor = element_blank(), 
            legend.background = element_rect(fill = "transparent"))
  })
  
  output$school <- renderPlot({
    ggplot(overview,aes(x=school_year,y=school_count,colour=school_enrollment,group=school_enrollment,fill=school_enrollment)) +
      geom_bar(position="stack", stat="identity", color = NA)+
      scale_y_continuous(name="population", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'African American Population by School Enrollment')+
      scale_fill_brewer() +
      theme(legend.position="top", 
            legend.text=element_text(size=10), 
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
            panel.background = element_rect(fill = "transparent", color = 'black', linetype = 'dashed'), 
            plot.background = element_rect(fill = "transparent", color = 'black'), 
            panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "black"), 
            panel.grid.minor = element_blank(), 
            legend.background = element_rect(fill = "transparent")
      )
  })
  output$map <- renderLeaflet({
    selectedData_1<-residents_long %>% filter(
      residents_long$year==input$yeard, 
      residents_long$origin_country %in% c(input$origin_country)
    )
    validate(
      need(input$origin_country != "", "Please select at least one country of birth")
    )
    selectedData <- aggregate(selectedData_1$residents_sum, 
                                       by=list(state_name=selectedData_1$state_name, 
                                               year = selectedData_1$year),
                                       FUN=sum)
    
    selectedData_2 <- setNames(selectedData, replace(names(selectedData), names(selectedData) == 'x', 'residents_sum'))
    selectedData <- selectedData_2
    
    
    #Fun part: Match our residents data onto geolocations, by matching residents and urbmmapr datasets
    statesdata<-as.data.frame(get_urbn_labels()) #I'll be using this as a dataset from the urbnmapr package, but it has others too (e.g. 'counties')
    statesdata$Residents <-selectedData$residents_sum[match(statesdata$state_name, selectedData$state_name)]
    leaflet(statesdata) %>%
      addTiles()  %>%
      setView(lat = 40, lng = -97, zoom=3.5) %>%
      addCircleMarkers(lng = ~long,
                       lat = ~lat,
                       radius = ~log(Residents) * 2.5,
                       weight = 1,
                       opacity = 1,
                       color = ~ifelse(Residents > 0, "black", "transparent"),
                       fillColor = ~ifelse(Residents > 0, colorPalette(Residents), "transparent"),
                       fillOpacity = 0.8)%>%
      addLegend(pal = colorPalette, values = selectedData$residents_sum, opacity=0.9, title = "Population", position = "bottomleft")
  })
  
  #######################
  output$admission <-renderPlot({
    validate(need(input$origin_country != "", "Please select at least one country of birth"))
    
    selectedData_1<-residents_long %>% filter( residents_long$year==input$yeard, residents_long$origin_country %in% c(input$origin_country))
    
    selectedData <- aggregate(selectedData_1$residents_sum, 
                              by=list(admission_class = selectedData_1$admission_class, state_name=selectedData_1$state_name, 
                                      year = selectedData_1$year), FUN=sum)
    selectedData$admission_class <- recode(selectedData$admission_class,
                                           'Diversity Program'= "Diversity Program",
                                           'Employment Preference 1st' = "Employment Preference", 
                                           'Employment Preference 2nd'= "Employment Preference",
                                           'Employment Preference 3rd Skilled'= "Employment Preference",
                                           'Employment Preference 3rd Unskilled'= "Employment Preference",
                                           'Employment Preference 4th'= "Employment Preference",
                                           'Employment Preference 4th'= "Employment Preference",
                                           'Employment Preference 5th'= "Employment Preference",
                                           'Family Preference 1st'= "Family Preference",
                                           'Family Preference 2nd'= "Family Preference",
                                           'Family Preference 3rd'= "Family Preference",
                                           'Family Preference 4th'= "Family Preference",
                                           'Immediate Relatives -  Parents'= "Parents",
                                           'Immediate Relatives - Children'= "Children",
                                           'Immediate Relatives - Spouses'= "Spouses",
                                           'Refugee and Asylee Adjustments'= "Refugees & Asylum Seekers",
                                           'Other' = "Other")
    
    
    selectedData_2 <- setNames(selectedData, replace(names(selectedData), names(selectedData) == 'x', 'residents_sum'))
    selectedData <- selectedData_2
    
    
    ggplot(selectedData, aes(x = reorder(admission_class, -residents_sum), y = residents_sum, fill = admission_class )) + geom_bar(stat="identity") + 
      ggtitle("Class of Admission")   + scale_fill_brewer(palette="Blues") + theme(legend.position = "none")  +  
      labs(x = "Admission Class", y = "Population") 
  })
  ###########################
  
  output$decennial <- renderPlot({
    ggplot(data = df, mapping = aes(x = State_Code, y = African_American_Proportion_by_State, fill = factor(Year))) + geom_bar(stat='identity', position="dodge")+ ylab('Proportion of state population that is African American') +  scale_fill_manual(values=c("#99CCFF", "#003399"))
  })
  
  #output$decennial <- renderPlot({
 #   ggplot(data = df, mapping = aes(x = State_Code, y = Proportion, fill = factor(Year))) + geom_bar(stat='identity', position="dodge")
 # })
  colnames(df)[colnames(df) == "Proportion"] <- "African_American_Proportion_by_State"
  
  
  output$decennial_table<-DT::renderDataTable({
    DT::datatable(df, options = list(
      pageLength = 20)
    )
    
  })
  
  #output$locationBox <- renderValueBox({  #### I ADDED THIS ##############
  #  valueBox(
  #    paste0(list(input$origin_country)), "", icon = icon("globe-africa"),
   #   color = "blue"
  #  )
 # })
  output$progressBox <- renderValueBox({ #### I ADDED THIS ##############
    selectedData_progress<-residents_long %>%  filter( residents_long$year==input$yeard, residents_long$origin_country %in% c(input$origin_country)
    )
    selectedData_progress<- sum(selectedData_progress[, 'residents_sum'], na.rm = TRUE)
    valueBox(
      paste0(selectedData_progress , ' persons'), "admitted to US in the selected year", icon = icon("flag-usa"),
      color = "light-blue"
    )
  })
  output$statemaxBox <- renderValueBox({ 
    selectedData_statemax<-residents_long %>% filter(
      residents_long$origin_country %in% c(input$origin_country)
    )
    validate(
      need(input$origin_country != "", "Please select at least one country of birth"))
    selectedData_statemax <- aggregate(selectedData_statemax['residents_sum'], by=selectedData_statemax['state_name'], sum)
    maxval<-selectedData_statemax$state_name[which.max(selectedData_statemax$residents_sum)]
    valueBox(
      paste0(maxval, ''), "the state that admitted the largest total number of immigrants between 2010-2019", icon = icon("door-open"),
      color = "blue"
    )
  })
  
  url1 <- a("Bo Crauwels", href = "https://www.linkedin.com/in/bo-crauwels-1381381ba/")
  url2 <- a("Hanzhang Hu", href = "https://www.linkedin.com/in/hanzhang-hu-9a36701a0")
  url3 <- a("Shreyans Kothari", href = "https://www.linkedin.com/in/shreyans-kothari/")
  url4 <- a("Dan Li", href = "https://www.linkedin.com/in/jessica-li-43484095")
  url5 <- a("Pengyun Li", href = "https://www.linkedin.com/in/pengyun-li/")
  url6 <- a("Rui Lu", href = "https://www.linkedin.com/in/rui-lu-98765b19a/")
  
  output$tab1 <- renderUI({
    tagList("Contact", url1)
  })
  output$tab2 <- renderUI({
    tagList("Contact", url2)
  })
  output$tab3 <- renderUI({
    tagList("Contact", url3)
  })
  output$tab4 <- renderUI({
    tagList("Contact", url4)
  })
  output$tab5 <- renderUI({
    tagList("Contact", url5)
  })
  output$tab6 <- renderUI({
    tagList("Contact", url6)
  })
  
  output$intro <- renderUI({
    HTML(paste("<h1>About Us</h1>",
    "<h4> We are a team of Master's students at Columbia University in the City of New York in the Quantitative Methods in Social Sciences (QMSS) Program. Our team members include Bo Crauwels, Hanzhang Hu, Shreyans Kothari, Dan Li, Pengyun Li, and Rui Lu. </h4>",
    "<h1>Main Objective</h1>",
      "<h4>Our goal is to enable community organizations with varying degrees of statistical and technical expertise to utilize publicly available Census data, and make well-informed and data-driven decisions. </h4>",
    "<h4>This dashboard serves as a minimum viable product that allows the user to draw actionable insights and easily navigate the data. For the Census Bureau Data Sprint Challenge, our team decided to narrow down our scope to community organizations that work with immigrants. 
    For the sake of this data product, we further narrowed down our scope to community organizations that work with African immigrants in the US. After several rounds of iterations, user research interviews,and feedback loops, our team created this multi-functional dashboard that depicts various trends over time. 
    The visualizations cover a wide variety of topics from basic demographic data to high-level information on African refugees. In the future, we hope to incorporate additional groups to expand the scope of this project.</h4>",
    "<h4>Click the tabs on the left side to explore some data !</h4>",
    "<h1>Contact Us</h1>",
sep ="<br/>"))
  })
  
  output$summary <- renderUI({
    HTML(paste("<h1>How to use this Dashboard</h1>","<h4>Each tab provides different insights about our variable of interest. Here, we briefly describe each tab.</h4>", 
               "<h2>Tab 1: Overview</h2>",
               "<h4>This tab provides a high-level overview about the African-American population in the US. The various visualizations show changes in the age group composition, employment status, and education attainment for this racial group, in the past decade.</h4>",

               "<h4>As seen from the stacked plots, the overall population for African Americans has increased over the years, however the ‘below 18’ age group has remained virtually unchanged. This unusual pattern is also reflected in the school enrollment graph where the total population enrolled in any education institution has gone down over the years.</h4>",
               "<h2>Tab 2: Proportion of State Population</h2>",
               "<h4>This tab shows the proportion of the total state population that is African American, for the past decade. Between 2010 and 2020, most of the states (except for AK, DC, NY, and SC) show an increase in this proportion of the total state population.</h4>",
               "<h4>In the District of Columbia, the proportion of the state population that is African American declined the most (in the US) by 13.6%, between 2010 (52.211% ) and 2020 (45.118%).</h4>",
               "<h2>Tab 3: African Immigrants in the US</h2>",
               "<h4>This tab displays the origin country for African Immigrants to the US, by year and state, between 2010 and 2019. The dial (for the year) and the drop down menu (for the origin country) at the top of this page allow the user to interact with the visualizations and create more nuanced graphs.</h4>",
               "<h4>The US map shows where the African immigrants reside, in any given year. Selecting (one or more) Origin Country from the drop down menu allows the user to create interesting maps about African immigrants in the US.</h4>",
               "<h4>The barplot at the bottom shows the class of admission for the African immigrants, by origin country and year.</h4>",
                "<h2>Tab 4: African Refugees in the US</h2>",
               "<h4>The African Refugees in the US tab displays data on African refugees. Here, we want to highlight that this data is specifically for refugees and not all immigrants. Refugees are a subcategory of immigrants. This tab displays data on refugees that came to the US over the past decade.</h4>",
               "<h4>The heatmap depicts the country of origin for the refugees. The graph under it showcases the change (from previous year) in the number of refugees entering the US. </h4>",
               sep ="<br/>"))
  })
  
  output$appendix <- renderUI({HTML(paste("<h1>Data Sources</h1>","[1]Decennial Census P.L. 94-171 Redistricting Data","[2]Census Data - American Community Survey (ACS)",sep ="<br/>"))
  })

  # refugee map 
  output$refugeemap <- renderPlot({
    switch(input$year,
           "2019" = re2019,
           "2018" = re2018,
           "2017" = re2017,
           "2016" = re2016,
           "2015" = re2015,
           "2014" = re2014,
           "2013" = re2013,
           "2012" = re2012,
           "2011" = re2011,
           "2010" = re2010)
  })
  
  #refugee bar plot
  output$refugeebar <- renderPlot({
    par(mar = c(14, 5, 5, 5))
    switch(input$year,
           "2019" = barplot(refugee_country$compared_18_19, 
                            col=ifelse(refugee_country$compared_18_19 > 0, "#3449b8", "#df072a"), 
                            names.arg = refugee_country$region, las =2,
                            main="Refugee Population Changes - 2018 & 2019",
                            border="#dddddd"),
           "2018" = barplot(refugee_country$compared_17_18, 
                            col=ifelse(refugee_country$compared_17_18 > 0, "#3449b8", "#df072a"), 
                            names.arg = refugee_country$region, las =2,
                            main="Refugee Population Changes - 2017 & 2018",
                            border="#dddddd"),
           "2017" = barplot(refugee_country$compared_16_17, 
                            col=ifelse(refugee_country$compared_16_17 > 0, "#3449b8", "#df072a"), 
                            names.arg = refugee_country$region, las =2,
                            main="Refugee Population Changes - 2016 & 2017",
                            border="#dddddd"),
           "2016" = barplot(refugee_country$compared_15_16, 
                            col=ifelse(refugee_country$compared_15_16 > 0, "#3449b8", "#df072a"), 
                            names.arg = refugee_country$region, las =2,
                            main="Refugee Population Changes - 2015 & 2016",
                            border="#dddddd"),
           "2015" = barplot(refugee_country$compared_14_15, 
                            col=ifelse(refugee_country$compared_14_15 > 0, "#3449b8", "#df072a"), 
                            names.arg = refugee_country$region, las =2,
                            main="Refugee Population Changes - 2014 & 2015",
                            border="#dddddd"),
           "2014" = barplot(refugee_country$compared_13_14, 
                            col=ifelse(refugee_country$compared_13_14 > 0, "#3449b8", "#df072a"), 
                            names.arg = refugee_country$region, las =2,
                            main="Refugee Population Changes - 2013 & 2014",
                            border="#dddddd"),
           "2013" = barplot(refugee_country$compared_12_13, 
                            col=ifelse(refugee_country$compared_12_13 > 0, "#3449b8", "#df072a"), 
                            names.arg = refugee_country$region, las =2,
                            main="Refugee Population Changes - 2012 & 2013",
                            border="#dddddd"),
           "2012" = barplot(refugee_country$compared_11_12, 
                            col=ifelse(refugee_country$compared_11_12 > 0, "#3449b8", "#df072a"), 
                            names.arg = refugee_country$region, las =2,
                            main="Refugee Population Changes - 2011 & 2012",
                            border="#dddddd"),
           "2011" = barplot(refugee_country$compared_10_11, 
                            col=ifelse(refugee_country$compared_10_11 > 0, "#3449b8", "#df072a"), 
                            names.arg = refugee_country$region, las =2,
                            main="Refugee Population Changes - 2010 & 2011",
                            border="#dddddd")
    )
  })
  
  # refugee data table
  datasetInput3 <- reactive({
    switch(input$year,
           "2019" = refugee_country[, c(1, 11)],
           "2018" = refugee_country[, c(1, 10)],
           "2017" = refugee_country[, c(1, 9)],
           "2016" = refugee_country[, c(1, 8)],
           "2015" = refugee_country[, c(1, 7)],
           "2014" = refugee_country[, c(1, 6)],
           "2013" = refugee_country[, c(1, 5)],
           "2012" = refugee_country[, c(1, 4)],
           "2011" = refugee_country[, c(1, 3)],
           "2010" = refugee_country[, c(1, 2)])
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    datasetInput3()
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)