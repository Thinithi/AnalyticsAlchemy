
#references
#https://github.com/aagarw30/R-Shinyapp-Tutorial/blob/master/shiny_with_shinydashboard_package/10_shinydashboard_header_skins/dashboard_skin.R
#https://community.rstudio.com/t/menuitem-in-sidebar/16726/2
#https://www.rdocumentation.org/packages/shinydashboard/versions/0.7.2/topics/box
#https://shiny.posit.co/r/gallery/widgets/widget-gallery/
#https://dplyr.tidyverse.org/reference/summarise.html 
#https://www.littlemissdata.com/blog/prettytables 
#https://stackoverflow.com/questions/38593153/plotly-regression-line-r
#https://stackoverflow.com/questions/49673942/heat-map-when-the-color-is-scaled-rowwise 
#https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
#https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html 

#####################################################################################################################
#Notes: 
#1)change the logo path to the working directory (excluding /logo/Tourismlogo.png) prior running to include logo
#2) the working directory picks up the current working director (Place folder here)
#####################################################################################################################


#check directory
getwd()
setwd(getwd())

#import required libraries
library(png)
library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(formattable)
library(lubridate)
library(readxl)
library(plotly)
library(data.table)
library(ozmaps)
library(sf)
library(leaflet)
library(RColorBrewer)
library(base)

#### Global data preparation ############################################################################################################################
#import data sets
# For growth Analysis 
data <- read.csv("austrlia_visitor_time.csv")
# For Behavior analysis and composition Analysis 
data02 <- read_excel("Visitors_State.xlsx", sheet = "Visitors_by_state")
# For tourism hotspot identification
data03 <- read_excel("Australia_Tourist_Data_Warehouse.xlsx")

#data preprocessing
#seperating date by day,month and year
data$day <- format(as.Date(data$date, format="%m/%d/%Y"),"%d")
data$month <- format(as.Date(data$date, format="%m/%d/%Y"),"%m")
data$year <- format(as.Date(data$date, format = "%m/%d/%Y"), "%Y")
# excluding total records for world regions
data<-data%>% filter(data$variable_type != "Total")

#importing Australia states boundary lines (Excluding other territories & descending order)
sf_oz <- ozmap("states") 
sf_oz <- sf_oz %>% filter(sf_oz$NAME != "Other Territories") 
sf_oz <- sf_oz %>% arrange(desc(sf_oz$NAME))

# Join the boundary information to the data set using the state name
data02 <- data02 %>%
  left_join(sf_oz, by = c("State" = "NAME"))

# Define the user interface fo the Rshiny Dashboard (Yellow)
title <- tags$a(href='https://www.google.com',
                icon("plane"),
                'Insights Portal', target="_blank", style = "color: #8B4513;")

# change path to working directory
logo <- "C:/Users/Lenovo/Desktop/Thinithi_27523306_Code/logo/Tourismlogo.png"

##################################################################################################################################





################################################################################################################################
#                                                  UI SECTION
#################################################################################################################################

# create dashboard page
ui <- dashboardPage(
  # add header
  dashboardHeader(title = title, titleWidth = 230),
  #dashboard skin colour yellow
  skin="yellow",
  #Add side bar
  dashboardSidebar(
    sidebarMenu(
      #Add tourism Australia logo
      imageOutput("logopic", height = 100),
      #Add tabs to structure the vizualiations
      menuItem("Inbound Tourism Road Map", tabName = "page", icon = icon("home")),
      menuItem("Tourism Growth", tabName = "panel1", icon = icon("th")),
      menuItem("Tourism Behavior", tabName = "panel2", icon = icon("th")),
      menuItem("Tourism Composition", tabName = "panel3", icon = icon("th")),
      menuItem("Tourism Hotspots", tabName = "subpanels", icon = icon("list"),
               menuSubItem("Tourism Hotspots", tabName = "panel4a")
      )
    )
  ),
  #Add dashboard body
  dashboardBody(
    #Edit Tabs
    tabItems(
      
      #home page road map view
      tabItem(tabName = "page",
              #center the title and make it bold
              tags$div(style = "margin-left: 90px;",h2("Australian Inbound Tourism")),
              tags$div(style = "margin-left: 90px;",p("Insights Portal Roadmap")),
              style = "text-align: center;",
              
              # infoBox for Growth
              fluidRow(
                infoBox(
                  HTML("<b>Growth</b>"), uiOutput("progress"), icon = icon("signal"), color = "maroon",
                  column(width = 4,align="center"),
                  subtitle = HTML("<li>Identify growth patterns</li>
                                   <li>Forecast future trends</li>"),
                ),
                
                # infoBox for behavior
                infoBox(
                  HTML("<b>Behavior</b>"), uiOutput("progress2"), icon = icon("code-compare"), color = "orange",
                  column(width = 4,align="center"),
                  subtitle = HTML("<li>Expenditure Habits</li>
                                   <li>Seasonal Behavior</li>"),
                ),
                
                # infoBox for composition
                infoBox(
                  HTML("<b>Composition</b>"), uiOutput("progress3"), icon = icon("earth-americas"), color = "aqua",
                  column(width = 4,align="center"),
                  subtitle = HTML("<li>By State</li>
                                   <li>By Nationality</li>"),
                ),
                
                # infoBox for hotspots
                infoBox(
                  HTML("<b>Hotspots</b>"), uiOutput("progress4"), icon = icon("camera-retro"), color = "light-blue",
                  column(width = 4,align="center"),
                  subtitle = HTML("<li>By tourist content type</li>
                                   <li>By state</li>"),
                )
              ),
              # Add paddings for space
              p(" "),
              #title for data sources
              tags$div(style = "margin-left: 90px;",p(tags$b("Data Sources"))),
              p(" "),
              fluidRow(
                # infoBoxes with links to click on to access data sources
                infoBox("Australian Bureau of Statistics",
                        tags$a(href = "https://www.abs.gov.au/statistics/industry/tourism-and-transport/overseas-arrivals-and-departures-australia/dec-2022/340105.xlsx ",
                               "Short-term visitors Arrivals by country of residence 1991-2022", style = "font-size: 10px;"),
                        tags$a(href = "https://www.abs.gov.au/statistics/industry/tourism-and-transport/overseas-arrivals-and-departures-australia/dec-2022/3401011.xlsx ",
                               "Short-term visitor arrivals by state 1991-2022", style = "font-size: 10px;"),
                        icon = icon("database"), fill=TRUE,color = "black",
                        width = 4
                        ),
                infoBox("Tourism Research Australia",
                        tags$a(href = "https://www.tra.gov.au/ArticleDocuments/185/InternationalVisitorSurvey(IVS)EarlyReleaseSummaryJune2020.xlsx.aspx ",
                               "International visitors across Australian States and Territories", style = "font-size: 10px;"),
                        tags$a(href = "https://www.tra.gov.au/ArticleDocuments/185/IVS_TOURISM_RESULTS_YE_MAR_2020.xlsx.aspx",
                               "International visitors average nights spent and expenditure", style = "font-size: 10px;"),
                        icon = icon("database"), fill=TRUE,color = "black",
                        width = 4
                ),
                infoBox("Web Scraped Data",
                        tags$a(href = "https://atdw.com.au/our-listings/?pge=1",
                               "Australian Tourist Data Warehouse", style = "font-size: 10px;"),
                        tags$a(href = "https://www.state.gov/countries-and-areas-list/",
                               "Countries and world regions", style = "font-size: 10px;"),
                        icon = icon("database"), fill=TRUE,color = "black",
                        width = 4
                )
              )
      ),
      #customise growth tab
      tabItem(tabName = "panel1",
              h2("Australian Inbound Tourism Growth"),
              p("Analyze changes in Tourist arrivals"),
              style = "text-align: center;",
              fluidRow(
                #Add TopN selector
                column(width = 3,
                       sliderInput("topselector", h3("Top Countries (By Avg Visits Per Year)", style = "font-size: 12px;"),
                                   min = 1, max = 100, value = c(1,100), width = "200px")
                ),
                #Add data selection
                column(width = 3,
                       dateRangeInput("dates", label = h3("Base Date | Reference Date", style = "font-size: 12px;"),
                                      format = "yyyy",
                                      startview = "year",
                                      start = "1994-01-01",
                                      end = "2022-01-01")
                ),
                #Add country of residence selection
                column(width = 2,
                       tags$style(".selectize-dropdown-content .choices { font-size: 12px; }"),
                       selectInput("residence", label = h3("Country Of Residence", style = "font-size: 12px;"),
                                   choices =  c("Select All", unique(data$countries)), selected = "Select All")
                ),
                #Add chart type selection
                column(width = 2,
                       tags$style(".selectize-dropdown-content .choices { font-size: 12px; }"),
                       selectInput("charttype", label = h3("Graph Type", style = "font-size: 12px;"),
                                   choices = c("By Country", "By World Region","Overall"), selected = "By Country")
                ),
                #Add trend colour selection
                column(width = 2,
                       tags$style(".selectize-dropdown-content .choices { font-size: 12px; }"),
                       selectInput("trendcolour", label = h3("Trend Colour", style = "font-size: 12px;"),
                                   choices = c("Increasing", "Decreasing","All"), selected = "All")
                ),
                # Add narrative box
                infoBox(
                        HTML(" "),
                        icon = icon("circle-info"), color = "yellow",fill=FALSE,
                        width = 12,
                        subtitle = HTML("Prior Covid impact, nearly 3Mn of the yearly visitors were from New Zealand,UK, Japan, United states, China and Singapore (Refer the highlight table). The Chinese tourists, on the other hand, seem<br>
                                        to portray the highest growth across the years.When looking at the trend of tourist arrivals prior to the lock down, all nationalities seem to have experienced growth from 1994 to 2018 except<br>
                                        for Japan which had experienced a drop of 42%. When looking at the past 10 years, inflow of tourists from all countries have increased except for Japan, Ireland, South Africa, Israel and Netherlands.<br>
                                        When considering world regions,Asian tourist visits seemed to have increased drastically from 2011 onwards. In terms of covid recovery, China, India, Vietnam, Philippines and Sri Lanka are few of the
                                        countries which had recovered to 2008 rates."),
                ),
                #include the line graph and highlight table output
                column(width = 12,
                       splitLayout(cellWidths = c("40%", "60%"),
                                   formattableOutput("highlight_table"),
                                   style = "overflow-y: scroll ; max-height: 380px",
                                   plotlyOutput("trendPlot", height = "380px"))
                
              )
              )
              #
      ),
      #customise Behavior tab
      tabItem(tabName = "panel2",
              h2("Australian Inbound Tourist Behavior"),
              p("Analyze Tourist behavior by nationality"),
              style = "text-align: center;",
              fluidRow(
                #Add country of residence selection
                column(width = 2,
                       tags$style(".selectize-dropdown-content .choices { font-size: 12px; }"),
                       selectInput("residence2", label = h3("Country Of Residence", style = "font-size: 12px;"),
                                   choices =  c("Select All", unique(data02$residence)), selected = "Select All")
                )
              ),
              fluidRow(
              #Add narrative
              infoBox(
                HTML(" "),
                icon = icon("circle-info"), color = "yellow",fill=FALSE,
                width = 12,
                subtitle = HTML("The behavior of tourists from different nationalities are highlighlted below. Some tend to stay for a shorter period and may prefer to spend a lot more during their stay while others may prefer<br>
                                 to stay a longer period and spend much less per night. The scatterplot shows the strong correlation between the two variables, since the p-value is significantly less than 0.05 it can be concluded<br>
                                 that the correlation is statistically significant and that the average nights stayed is inversely proportional to the average night expenditure"),
              )),
              fluidRow(
              #Add scatter plot output
              plotlyOutput("scatter_Plot",height = "400px")),
              p(" "),
              p(" "),
              fluidRow(
                #Add Narrative
                infoBox(
                  HTML(" "),
                  icon = icon("circle-info"), color = "yellow",fill=FALSE,
                  width = 12,
                  subtitle = HTML("At A glance we can see that the average number of tourists visits from all nationalities peak in the month of December. In addition, January and February visits seems to be higher in comparison<br>
                                  to the other months. Most importantly, the heatmap below highlights the peak months of visitors by their country of residence along with an average estimate of the number of visitors."),
                )),
              tags$div(style = "margin-left: 90px; font-size: 16px;",p(" ")),
              fluidRow(
              #Add heatmap output
              plotlyOutput("heatmap_Plot",height = "700px"))
      ),
      #customize composition tab
      tabItem(tabName = "panel3",
              h2("Australian Inbound Tourist Composition"),
              p("Analyze Tourist composition by State"),
              style = "text-align: center;",
              fluidRow(
                #Add country of residence selection
                column(width = 2,
                       tags$style(".selectize-dropdown-content .choices { font-size: 12px; }"),
                       selectInput("residence3", label = h3("Country Of Residence", style = "font-size: 12px;"),
                               choices =  c("Select All", unique(data02$residence)), selected = "Select All")
               ),
               #Add state selection
               column(width = 2,
                      tags$style(".selectize-dropdown-content .choices { font-size: 12px; }"),
                      selectInput("state", label = h3("State", style = "font-size: 12px;"),
                      choices =  c("Select All", unique(data02$State)), selected = "Select All")
              ),
              #Add narrative
                infoBox(
                  HTML(" "),
                  icon = icon("circle-info"), color = "yellow",fill=FALSE,
                  width = 12,
                  subtitle = HTML("Few observations were made when analyzing the tourist visits to each state during the year 2019. New south Wales had significant tourist visits with the main contribution is from tourists<br>
                                  from China, US, New Zealand and UK. Victoria seems to have a significantly high Chinese tourist visits while the second highest is from New Zealand tourists. In addition, Victoria seems to<br>
                                  be having the highest Asian tourists (178,000). Northern Australia, on the other hand, has mostly tourists from Japan (40,000 visits) followed by US tourists (39,000 visits). Western Australia<br>
                                  have more Singaporeans (104,000 visits) and Malaysians (99,000 visits)."
                  )
              )
              ),
              #Add two outputs horizontally
              column(width = 12,
                     p(tags$b("Tourist visits 2019")),
                     tags$div(style = "text-align: left;color: brown",p(tags$b("Colour Scale:Higher Number of visits ~ Darker the colour"))),
                     splitLayout(cellWidths = c("70%", "30%"),
                                 leafletOutput("map1",height = "400px"),
                                 plotlyOutput("bar_Plot",height = "400px"))
                     
              )
      ),
      #Customize Hotspots tab
      tabItem(tabName = "panel4a",
              h2("Australian Tourist Content Hotspots"),
              p("Identify Opportunity Areas"),
              style = "text-align: center;",
              fluidRow(
                #Add tourism content type selector
                column(width = 2,
                       tags$style(".selectize-dropdown-content .choices { font-size: 12px; }"),
                       selectInput("tourismcontent", label = h3("Tourism Content Category", style = "font-size: 12px;"),
                                   choices =  c("Select All","Accommodation","Food and Drink"
                                                ,"Attraction","Event"), selected = "Select All")
                ),
                #Add suburb selector
                column(width = 2,
                       tags$style(".selectize-dropdown-content .choices { font-size: 12px; }"),
                       selectInput("tourismcontent2", label = h3("Select Suburb", style = "font-size: 12px;"),
                                   choices =  c("Select All",data03$Suburb), selected = "Select All")
                ),
                
                #Add state selector
                column(width = 2,
                       tags$style(".selectize-dropdown-content .choices { font-size: 12px; }"),
                       selectInput("state2", label = h3("Select State", style = "font-size: 12px;"),
                                   choices =  c("Select All",data03$State), selected = "Select All")
                ),
                #Add quantity filter
                column(width = 3,
                       sliderInput("topselector2", h3("Tourist Content Quantity)", style = "font-size: 12px;"),
                                   min = 1, max = 230, value = c(1,230), width = "200px")
                ),
                #Add a narrative
                infoBox(
                  HTML(" "),
                  icon = icon("circle-info"), color = "yellow",fill=FALSE,
                  width = 12,
                  subtitle = HTML("Australian data warehouse is a digital content data base thats objective is to create a platform for businesses in the tourism industries to raise awareness. It has over 11 categories of<br>
                                  listings but initially we took listings from 4 main categories which are food & drink, accommodation, attractions, and events. Over 21,000 tourism content listings have been obtained and<br>
                                  visualized to identify hotspots which can be useful for both tourists and investors in the tourism industry. "
                  )
                )
              ),
              #Add a split screen layout
              fluidRow(
              p(tags$b("Tourism Content Lisitngs Australia")),
              column(width = 12,
                     splitLayout(cellWidths = c("80%", "20%"),
                                 #print tourism content map
                                 leafletOutput("map2",height = "400px"),
                                 #print listing summary
                                 formattableOutput("highlight_table2"),height = "400px")
                     
              )
              ),
              p(" "),
              #display bar plot
              fluidRow(
              plotlyOutput("bar_Plot2",height = "250px")
              ),
              p(" "),
              p(tags$b("Listing Details")),
              #display listing detail table
              fluidRow(
                formattableOutput("highlight_table3",height = "500px")
              )
              
      )
    )
  
  )
)


##################################################################################################################################
#                                                            SERVER AREA
###################################################################################################################################
server <- function(input, output, session) {

  
  # Render the image ###############################################################################################################
  output$logopic <- renderImage({
    
    list(src = logo,
         contentType = "image/png",
         width = "100%",
         height = 100)
  }, deleteFile = FALSE)
  
  #add line graph #################################################################################################################
  output$trendPlot <- renderPlotly({
    
    #seperating date day,month and year
data$day <- format(as.Date(data$date, format="%m/%d/%Y"),"%d")
data$month <- format(as.Date(data$date, format="%m/%d/%Y"),"%m")
data$year <- format(as.Date(data$date, format = "%m/%d/%Y"), "%Y")

#filteration
data<-data%>% filter(data$variable_type != "Total")

    # filter countries 
    filtered_countries <- if (input$residence == "Select All") {
      data$countries
    } else {
      input$residence
    }
    
    #filter graph type
    filtered_chartype <- if (input$charttype == "By Country") {
      'countries'
    } else if (input$charttype == "By World Region") {
      'Region'
    } else { 
      'variable_type'
    }
    
    #Filter data set based on date range 
    plot1 <- data %>%
      filter(countries %in% filtered_countries & year >= format(as.Date(input$dates[1], format = "%Y-%m-%d"), "%Y") &
               year <= format(as.Date(input$dates[2], format = "%Y-%m-%d"), "%Y"))
    
    #summarize data
    summary_data0 <- plot1 %>%
      group_by(countries, year) %>%
      summarise(total_records = sum(records), n = n()) %>%
      group_by(countries) %>%
      summarise(average_records = round(mean(total_records)))
    
    #rank the trend
    filtered_trend0 <- summary_data0 %>%
      mutate(rank_trend = rank(desc(average_records), na.last = "keep"))
    
    #filter rank based on selector
    filtered_summary0 <- filtered_trend0 %>% filter(rank_trend >= input$topselector[1] & rank_trend <= input$topselector[2])
    
    #filter relevant countries
    plot2 <- plot1 %>% filter( countries %in% filtered_summary0$countries)
  
    #plot with ggplot and save
    p <- ggplot(plot2) +
      stat_summary(aes(x = year, y = records, group = .data[[filtered_chartype]], color = .data[[filtered_chartype]]),
                   fun = sum, geom = "line", linewidth = 0.9) +
      labs(title = "Short-Term Visitor Arrival Across Time 1992-2022",
           x = "Year of Arrival",
           y = "Number of Visitors Arrived (Thousands)") +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(labels = function(x) format(x / 1000, scientific = FALSE))
    
    #convert ggplot to plotly
    ggplotly(p) %>% layout(showlegend = FALSE)
     
  })
  
  #add highlight table ############################################################################################################
  output$highlight_table <- renderFormattable({
    
    # filter countries
    filtered_countries <- if (input$residence == "Select All") {
      data$countries
    } else {
      input$residence
    }
    
    #date filteration
    #set start date
    start_date = as.Date(input$dates[1])
    #set end date
    end_date = as.Date(input$dates[2])
    #include 3 year prior start date
    start_date0 = start_date %m-% years(3)  
    #include 3 year prior end date
    end_date0 = end_date %m-% years(3)
    
    # Filter data based on selections
    plot1 <- data %>%
      filter(countries %in% filtered_countries & year >= format(as.Date(input$dates[1], format = "%Y-%m-%d"), "%Y") &
               year <= format(as.Date(input$dates[2], format = "%Y-%m-%d"), "%Y"))
    
    #filter data for selected start period
    filtered_data <- plot1 %>%
      filter(year >= format(as.Date(start_date0, format = "%Y-%m-%d"), "%Y") &
               year <= format(as.Date(start_date, format = "%Y-%m-%d"), "%Y")) 
    #filter data for selected end period
    filtered_data0 <- plot1 %>%
      filter(year >= format(as.Date(end_date0, format = "%Y-%m-%d"), "%Y") &
               year <= format(as.Date(end_date, format = "%Y-%m-%d"), "%Y")) 
    
    #Data pre-processing
    summary_data <- plot1 %>%
      group_by(countries, year) %>%
      summarise(total_records = sum(records), n = n()) %>%
      group_by(countries) %>%
      summarise(average_records = round(mean(total_records)))
    
    summary_data0 <- filtered_data %>%
      group_by(countries, year) %>%
      summarise(total_records = sum(records), n = n()) %>%
      group_by(countries) %>%
      summarise(average_records_start = round(mean(total_records)), n = n())
    
    summary_data1 <- filtered_data0 %>%
      group_by(countries, year) %>%
      summarise(total_records = sum(records), n = n()) %>%
      group_by(countries) %>%
      summarise(average_records_end = round(mean(total_records)), n = n())
    
    #combine the processed data
    summary_df <- cbind(summary_data,summary_data0$average_records_start,summary_data1$average_records_end,
                        percent(((summary_data1$average_records_end-summary_data0$average_records_start)/summary_data0$average_records_start)),
                        round(percent(((summary_data1$average_records_end-summary_data0$average_records_start)/summary_data0$average_records_start)),3))
    #order in descending order
    summary_df <- summary_df %>% arrange(desc(average_records))
    colnames(summary_df)<-c("countries","average_records","avg_start","avg_end","Trend","Trend_num")
    
    # Format all numeric columns as integers with commas
    summary_df <- summary_df %>%
      mutate_all(~ format(.x, big.mark = ",", scientific = FALSE))
    
    # filter by trend colour selection
    filtered_trend <- if (input$trendcolour == "Increasing") {
      summary_df_final <- summary_df %>% filter(summary_df$Trend > 0)
    } else if (input$trendcolour == "Decreasing") {
      summary_df_final <- summary_df %>% filter(summary_df$Trend < 0)
    } else {
      summary_df_final <- summary_df
    }
    
    #ranking the trend
    filtered_trend <- filtered_trend %>%
      mutate(rank_trend = rank(desc(Trend_num), na.last = "keep"))

    #filter the data base topn selection and rank
    filtered_summary <- filtered_trend %>% filter(rank_trend >= input$topselector[1] & rank_trend <= input$topselector[2])
    colnames(filtered_summary)<-c("countries","average_visits_per_year","avg_start","avg_end","Trend","Trend_num","rank_trend")
    
    #Set colours for KPIs
    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    customRed = "#ff7f7f"
    
    #format colours
    improvement_formatter <- formatter("span", 
                                       style = x ~ formattable::style(font.weight = "bold", 
                                                                      color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                                       x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
    )
    
    #Create highlight table
    formattable(filtered_summary[, c(1, 2, 5)],
                align =c("l","c","c","c","c", "r"), 
                list(
                  `countries` = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")),
                  `Trend` = improvement_formatter ))
  })
  
  #add scatterplot ##################################################################################################################
  output$scatter_Plot <- renderPlotly({
    
    # filteration
    higlight_country <- if (input$residence2 != 'Select All') {
      input$residence2
    } else {
      'none'
    }
    
    #summarize expenditure
    summary_exp <- data02 %>%
      group_by(residence) %>%
      summarise(average_nights_stayed = max(Est_Avg_Nights),
                average_expense_per_night = max(Exp_Avg_Night_Exp))
    
    #Create a scatter plot using plotly
    scatter_plot <- plot_ly(data = summary_exp, x = summary_exp$average_expense_per_night ,
                            y = summary_exp$average_nights_stayed, type = "scatter",mode = "markers",name="Nationalities",
                            marker = list(symbol = ~ifelse(residence == higlight_country ,"square" , "cross"), size = ~ifelse(residence == higlight_country , 16, 10),
                                          color = ~ifelse(residence == higlight_country , "orange", "blue")),
                            text = ~paste("Country:", residence,"<br>",
                                          "Avg Night Exp:", average_expense_per_night, "<br>",
                                          "Avg Nights Stayed:", average_nights_stayed, "<br>")
    ) %>%
      layout(title = "Average Expense per Night vs. Average Nights Stayed",
             xaxis = list(title = "Average Expense per Night"),
             yaxis = list(title = "Average Nights Stayed") ) 
    
    
    #Line of Best fit
    fit <- lm(average_nights_stayed ~ average_expense_per_night, data = summary_exp)
    
    # Pvalue function obtained from a reference
    p_value_function <- function (modelobject) {
      if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
      f <- summary(modelobject)$fstatistic
      p <- pf(f[1],f[2],f[3],lower.tail=F)
      attributes(p) <- NULL
      return(p)
    }
    
    #derive pvalue
    pvalue<- p_value_function(fit)
    
    #add scatter plot and best fit line 
    scatter_plot %>% add_lines(x = ~average_expense_per_night, y = fitted(fit),mode = 'lines', name = 'Line of Best Fit', line = list(color = 'grey'), 
                               marker = list(color = 'transparent', size = 0),line = list(color = 'transparent')) %>% 
      add_annotations(x = max(summary_exp$average_expense_per_night), y = max(fitted(fit)),
                      text = paste("p-value:", format(pvalue, digits = 4)),
                      showarrow = FALSE, font = list(size = 15))
    
  })
  
  
  #add heatmap #######################################################################################################################
  output$heatmap_Plot <- renderPlotly({
    
    #summarize data
    summary_hm <- data %>%
      group_by(countries, month) %>%
      summarise(average_records = round(mean(records)))
    
    # Normalize records within each country
    summary_hm$normalized_records <- ave(summary_hm$average_records, summary_hm$countries, FUN = function(x) (x - min(x)) / (max(x) - min(x)))
    
    #create a list of month names
    month_names <- c("January", "February", "March", "April", "May", "June",
                     "July", "August", "September", "October", "November", "December")
    
    #plot a heatmap using month names and visit normalized data
    plot_ly(data = summary_hm,
            x = ~month_names[as.integer(summary_hm$month)], y = ~summary_hm$countries,
            z = ~normalized_records, 
            type = "heatmap", 
            colorscale = "Heat",
            text = ~paste("Month: ", month_names[as.integer(summary_hm$month)], "<br>",
                          "Country: ", summary_hm$countries, "<br>",
                          "Normalized visit count: ", round(summary_hm$normalized_records,2), "<br>",
                          "Avg Visits: ", format(summary_hm$average_records, big.mark = ",", scientific = FALSE)),
            hoverinfo = "text",
            showscale = TRUE) %>% 
      layout(xaxis = list(categoryorder = "array", categoryarray = month_names[as.integer(summary_hm$month)]))%>%
      layout(xaxis = list(title = "Month of visit"),
             yaxis = list(title = "Country of Residence"),
             title = "Average Monthly Visits By Country Of Residence")
  })
  
  
  
  #add barplot ######################################################################################################################
  output$bar_Plot <- renderPlotly({
    
    #filter country
    higlight_country_bar <- if (input$residence3 != 'Select All') {
      input$residence3
    } else {
      data02$residence
    }
    
    #filter state 
    higlight_state_bar <- if (input$state != 'Select All') {
      input$state
    } else {
      data02$State
    }
    
    #data preprocessing
    summary_visitors0 <- data02 %>%
      group_by(State,residence) %>%
      summarise(sum_visitors_2019 = sum(Visitors_2019),
                average_nights_stayed = max(Est_Avg_Nights),
                average_expense_per_night = max(Exp_Avg_Night_Exp))%>% 
    filter( residence %in% higlight_country_bar & State %in% higlight_state_bar)
    
    summary_visitors1 <- summary_visitors0  %>%
      group_by(residence) %>%
      summarise(visitors_country_2019 = sum(sum_visitors_2019))
    
    summary_visitors1 <- summary_visitors1 %>% 
      arrange(desc(visitors_country_2019))
    
    #filter data based on selections
    summary_visitors1$residence_factor <- factor(summary_visitors1$residence, levels = unique(summary_visitors1$residence))
    
    #create a country of residence column and convert into a factor to order the graph
    summary_visitors1$residence_factor <- factor(summary_visitors1$residence_factor, levels = rev(levels(summary_visitors1$residence_factor)))
    
    #plot bar graph of 2019 visits
    plot_ly(x = summary_visitors1$visitors_country_2019, y = summary_visitors1$residence_factor, marker = list(pattern = list(shape = "x", color = "darkblue", size=3),text = summary_visitors1$visitors_country_2019),
            type = "bar", orientation = "h" )  %>%
      layout(xaxis = list(title = "Visitors 2019"),
             yaxis = list(title = "Country of Residence"),
             title = "Visitor Statistics",
             showlegend = FALSE) %>% 
      add_annotations(x = summary_visitors1$visitors_country_2019, y = summary_visitors1$residence_factor,
                      text = format(summary_visitors1$visitors_country_2019, big.mark = ",", scientific = FALSE),
                      showarrow = FALSE, font = list(size = 8), xshift = 20) 
    })
  
  #composition map ##############################################################################################################
  output$map1<-renderLeaflet({
    
    #filter based on country
    higlight_country3 <- if (input$residence3 != 'Select All') {
      input$residence3
    } else {
      data02$residence
    }
    
    #filter state 
    higlight_state1 <- if (input$state != 'Select All') {
      input$state
    } else {
      data02$State
    }
    
    ##data preprocessing
    summary_map <- data02 %>%
      filter(data02$residence %in% higlight_country3)%>%
      group_by(State,Longitude,Latitude,geometry) %>%
      summarise(sum_visitors_2019 = sum(Visitors_2019)) 
    summary_map <- summary_map %>% arrange(desc(summary_map$State))
    
    # Create a color palette for the map
    mypalette <- colorQuantile(palette = "YlOrBr", domain = summary_map$sum_visitors_2019, n = 4)
    
    # Tooltip text and include commas for large numbers
    mytext <- paste(
      "<strong>State:</strong> ", summary_map$State, "<br/>",
      "<strong>Visitors 2019:</strong> ", format(summary_map$sum_visitors_2019, big.mark = ",", scientific = FALSE), sep="") %>%
      lapply(htmltools::HTML)
    
    #create a chloropleth map
    leaflet(summary_map) %>%
      addTiles() %>%
      setView(lng = 133, lat = -25, zoom = 4) %>%
      addPolygons(
        data = sf_oz, fillColor = ~mypalette(summary_map$sum_visitors_2019), color = "black",weight = 1,opacity = 1,
        fillOpacity = 0.4,smoothFactor = 0.5,label = mytext,labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"))
    })
  
  
  #proportional symbol map ###########################################################################################################
  output$map2<-renderLeaflet({
    
    # Calculate the count of categories at each location
    count_data0 <- data03 %>%
      filter(!is.na(category)) %>%
      group_by(State,Suburb,category,Lon, Lat) %>%
      summarize(count_id = n())
    
    #highlight_content
    higlight_tourismcontent <- if (input$tourismcontent != 'Select All') {
      input$tourismcontent
    } else {
      count_data0$category
    }
    
    higlight_tourismcontent2 <- if (input$tourismcontent2 != 'Select All') {
      input$tourismcontent2
    } else {
      count_data0$Suburb
    }
    
    
    #filter data
    count_data <- count_data0 %>% filter(category %in% higlight_tourismcontent & count_id >= input$topselector2[1] & count_id <= input$topselector2[2]
                                         & Suburb %in% higlight_tourismcontent2 )
    
    #sort dataset
    count_data <- count_data %>% arrange(desc(count_id))
    
    #feed category the colour palatte
    palt<-colorFactor(palette ="Dark2",
                      levels = unique(count_data$category))
    
    #scales the size of markers based on the largest mark
    scaled<- count_data$count_id / max(count_data$count_id) * 10
    
    #create a map with content listings
    leaflet(count_data) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 133, lat = -25, zoom = 4) %>%
      addCircleMarkers(~ count_data$Lon, ~count_data$Lat, popup = ~as.character(paste(count_data$category, ": ", count_data$count_id, "<br>","Suburb: ", count_data$Suburb, "<br>",
                                "State: ", count_data$State)),radius=scaled,color=~palt(count_data$category),
                                  fillColor= ~palt(count_data$category),fillOpacity =1)%>% addLegend(pal = palt,values=count_data$category,title="Tourist Content",position="topright")
    
  })
    
  
  #tourist content by suburb ########################################################################################################
  output$bar_Plot2 <- renderPlotly({
    
    #filter category
    higlight_tourismcontent3 <- if (input$tourismcontent != 'Select All') {
      input$tourismcontent
    } else {
      data03$category
    }
    
    #filter suburb
    higlight_tourismcontent4 <- if (input$tourismcontent2 != 'Select All') {
      input$tourismcontent2
    } else {
      data03$Suburb
    }
    
    #filter state
    higlight_tourismcontent10 <- if (input$state2 != 'Select All') {
      input$state2
    } else {
      data03$State
    }
  
  #data summarizing
  count_data01_raw <- data03 %>% filter( category %in% higlight_tourismcontent3 & Suburb %in% higlight_tourismcontent4
                                         & State %in% higlight_tourismcontent10)%>%
    group_by(Suburb) %>%
    summarize(count_id = n())
  
  #ranking the trend
  count_data01_raw  <- count_data01_raw %>%
    mutate(rank_trend = rank(desc(count_id), na.last = "keep"))
  
  #filter by rank
  count_data01 <- count_data01_raw %>% arrange(desc(count_id)) %>% filter(rank_trend <= 15 )
  
  #convert suburb to a factor to order the output
  count_data01$suburb_factor <- factor(count_data01$Suburb, levels = unique(count_data01$Suburb))
  
  #create column chart
  plot_ly(data = count_data01,x = count_data01$suburb_factor, y = count_data01$count_id, marker = list(pattern = list(shape = "x", color = "darkblue", size=3),text = count_data01$count_id),
          type = "bar" )  %>%
    layout(xaxis = list(title = "Suburb"),
           yaxis = list(title = "Number of Tourist Content"),
           title = "Visitor Statistics",
           showlegend = FALSE) %>% 
    add_annotations(x = count_data01$suburb_factor, y = count_data01$count_id,
                    text = count_data01$count_id,
                    showarrow = FALSE, font = list(size = 12), xshift = 0, yshift=10)

  })
  
  #add highlight table ###############################################################################################################
  output$highlight_table2 <- renderFormattable({
    
    #category
    higlight_tourismcontent5 <- if (input$tourismcontent != 'Select All') {
      input$tourismcontent
    } else {
      data03$category
    }
    
    #filter suburb
    higlight_tourismcontent6 <- if (input$tourismcontent2 != 'Select All') {
      input$tourismcontent2
    } else {
      data03$Suburb
    }
    
    #filter state
    higlight_tourismcontent11 <- if (input$state2 != 'Select All') {
      input$state2
    } else {
      data03$State
    }
    
    #data preprocessing
    information <- data03 %>%
      filter( category %in% higlight_tourismcontent5 & Suburb %in% higlight_tourismcontent6 & State %in% higlight_tourismcontent11)%>%
      group_by(category) %>%
      summarize(count = format(n(), big.mark = ",", scientific = FALSE)) %>%
      filter(!is.na(category)) %>%
      mutate(records = category)
    information <- information %>% arrange(desc(count))
    
    # Set colours
    color0 <- "purple"
    color1 <- "#E7298A"
    color2 <- "#1B9E77"
    color3 <- "#D95F02"
    
    # Format colors and icons
    improvement_formatter1 <- formatter("span", 
                                       style = x ~ formattable::style(font.weight = "bold", 
                                                                      color = ifelse(x == 'Accommodation', color0,
                                                                                     ifelse(x == 'Food and Drink', color3,
                                                                                            ifelse(x == 'Attraction', color1, color2)))),
                                       x ~ icontext(ifelse(x == 'Accommodation', "bed",
                                                           ifelse(x == 'Food and Drink', "glass",
                                                                  ifelse(x == 'Attraction', "camera", "calendar"))), x)
    )
    
    # Apply formatting to the table
    formattable(information[,1:2],
                align = c("l", "c", "c", "c", "c", "c", "r"),
                list(
                  `countries` = formatter("span", style = ~ formattable::style(color = "grey", font.weight = "bold")),
                  `category` = improvement_formatter1
                )
    )
    
  })
  
  #highlight table 3
  output$highlight_table3 <- renderFormattable({
    
    #filter category
    higlight_tourismcontent7 <- if (input$tourismcontent != 'Select All') {
      input$tourismcontent
    } else {
      data03$category
    }
    
    #filter suburb
    higlight_tourismcontent8 <- if (input$tourismcontent2 != 'Select All') {
      input$tourismcontent2
    } else {
      data03$Suburb
    }
    
    #filter state
    higlight_tourismcontent12 <- if (input$state2 != 'Select All') {
      input$state2
    } else {
      data03$State
    }
    
    #data preprocessing
    information <- data03 %>%
      filter( category %in% higlight_tourismcontent7 & Suburb %in% higlight_tourismcontent8
              & State %in% higlight_tourismcontent12)%>%
      select(title, category, Suburb, Email_text, address, contact_no) %>%
      mutate(records = '.',
             row_number = row_number()) %>%
      filter(row_number <= 20)
    
    # Set colours
    color0 <- "purple"
    color1 <- "#E7298A"
    color2 <- "#1B9E77"
    color3 <- "#D95F02"
    
    # Format colors and icons
    improvement_formatter1 <- formatter("span", 
                                       style = x ~ formattable::style(font.weight = "bold", 
                                                                      color = ifelse(x == 'Accommodation', color0,
                                                                                     ifelse(x == 'Food and Drink', color3,
                                                                                            ifelse(x == 'Attraction', color1, color2)))),
                                       x ~ icontext(ifelse(x == 'Accommodation', "bed",
                                                           ifelse(x == 'Food and Drink', "glass",
                                                                  ifelse(x == 'Attraction', "camera", "calendar"))), x)
    )
    
    # Apply formatting to the table
    formattable(information[,1:6],
                align = c("l", "c", "c", "c", "c", "c", "r"),
                list(
                  `countries` = formatter("span", style = ~ formattable::style(color = "grey", font.weight = "bold")),
                  `category` = improvement_formatter1
                )
    )
    
  })
    
    
}

# Run the Shiny app
shinyApp(ui, server)
