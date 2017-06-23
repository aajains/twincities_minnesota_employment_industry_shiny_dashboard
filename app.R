rm(list = ls())
library(rgdal)
library(leaflet)
library(ggplot2)
library(dplyr)
library(stringi)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(extrafont)
library(showtext)
library(rsconnect)
library(rgeos)
#frame_files <- lapply(sys.frames(), function(x) x$ofile)
#frame_files <- Filter(Negate(is.null), frame_files)
#dirName1 <- dirname(frame_files[[length(frame_files)]])
dirName1 <- "/Users/pgolecha/Desktop/Datathon/"
#setwd(dirName1)
##############################################################################
# Loaading data and other R sources
##############################################################################
#CIVIC DATA
df_ind <- read.csv(paste0("INDUSTRY_DATA/", "INDUSTRY_DATA.csv"))
#MAP DATA (from Metropolitan Council)
Mapdir <- "map_data/"
counties1 <- readOGR(dsn=path.expand(Mapdir), layer="Counties")
counties <- spTransform(counties1, CRS("+proj=longlat +datum=WGS84"))

#MAP DATA (from MNGIS)
Mapdir1 <- "map_data/mngis/"
cities1 <- readOGR(dsn=path.expand(Mapdir1), layer="city_township_unorg")
cities <- spTransform(cities1, CRS("+proj=longlat +datum=WGS84"))
# Filtering only Metropolitan counties
cities <- cities[as.integer(as.vector(cities@data$County_num))
                 %in% c(3,37,53,123,139,163,19),]

County_names <- c(`003`="Anoka", `037`="Dakota", `053`="Hennepin",
                  `123`="Ramsey", `139`="Scott", `163`="Washington",
                  `019`="Carver")

County_list1 <- unlist(lapply(1:nrow(cities@data), function(x)
  County_names[as.character(cities@data$County_num[x])]))
cities@data <- data.frame(cities@data, CountyNames = County_list1)

County_list2 <- unlist(lapply(1:nrow(df_ind), function(x)
  County_names[as.character(stri_pad_left(df_ind$CO_CODE[x],3,pad="0"))]))
df_ind <- data.frame(df_ind, CountyNames = County_list2)
indCivic <- df_ind
cityShape <- cities
countyShape <- counties
citybounds<-bbox(cityShape)
countybounds<-bbox(countyShape)
# cityShape1 <- gSimplify(cities, tol = 0.000001, topologyPreserve = TRUE)
# cityShape <- SpatialPolygonsDataFrame(cityShape1, data=cities@data)
# countyShape1 <- gSimplify(counties, tol = 0.000001, topologyPreserve = TRUE)
# countyShape <- SpatialPolygonsDataFrame(countyShape1, data=counties@data)
#####################################################################
fsize <- 12
mycol <- "orange"
myfont <- "Palatino"
myTheme = theme(axis.title = element_text(family = myfont, face="plain", size=fsize, colour = mycol),
                text = element_text(size=fsize, face="plain", family = myfont, color = mycol),
                plot.title = element_text(family = myfont, face="plain", size=14, hjust=0.5, colour = mycol),
                #axis.title = element_text(family = myfont, face="plain", size=fsize, colour = mycol),
                axis.text.x = element_text(colour = mycol, size = fsize),
                axis.text.y = element_text(colour = mycol, size = fsize),
                axis.ticks = element_line(color = "gray8"),
                axis.ticks.length = unit(0.1,"cm"),
                legend.key = element_blank(),
                panel.background=element_rect(fill = "black", color = "gray30", size = 1),
                plot.background = element_blank(),
                panel.border = element_blank(),
                legend.background = element_rect(fill = "black", color = "black", size = 1), 
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                aspect.ratio=0.2)
# save.image(paste0(dirName1,"/PreApp.RData"))
# load(paste0(dirName1,"/PreApp.RData"))
####################################################################
##############################################################################
# Creating Shiny app
##############################################################################
header <- dashboardHeader(title = "Employment in Twin Cities Metropolitan Area",
                          titleWidth = 450)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("info")),
    menuItem("Jobs", tabName = "jobs", icon = icon("briefcase")),
    menuItem("Workers", tabName = "workers", icon = icon("group"))
  ),
  htmlOutput("ind_selector"),
  htmlOutput("year_selector"),
  htmlOutput("area_text"),
  htmlOutput("county_selector"),
  htmlOutput("city_selector")
)

body <- dashboardBody(tabItems(
  tabItem(tabName = "about",
          fluidRow(
            box(
              title = "About the Application", solidHeader = TRUE,
              status = "warning", width=12, collapsible = TRUE,
              column(12,
                     tags$div(
                       "This dashboard has visualizations of two sets of employment
                       data (jobs and workers) during 2009-2014
                       in 19 industrial sectors including:",
                       tags$span(      # Creates an HTML span.
                         tags$li("Accommodation and Food Services"),
                         tags$li("Agriculture, Forestry, Fishing & Hunting"),
                         tags$li("Educational Services"),
                         tags$li("Finance and Insurance"),
                         tags$li("Health Care and Social Assistance"),
                         tags$li("Real Estate and Rental and Leasing"),
                         tags$li("Transportation and Warehousing"),
                         tags$br()
                       ),
                       "The data is for all cities in the seven counties in
                       the metropolitan region:",
                       tags$span(      # Creates an HTML span.
                         tags$li("Anoka"),
                         tags$li("Carver"),
                         tags$li("Dakota"),
                         tags$li("Hennepin"),
                         tags$li("Ramsay"),
                         tags$li("Scott"),
                         tags$li("Washington")
                       )
                     ),
                     br()
          )
          )
          ),
          fluidRow(
            box(
              title = "About The Data Set", solidHeader = TRUE,
              status = "warning", width=12, collapsible = TRUE, collapsed = TRUE,
              column(12,
                     tags$div(
                       "This data set is from the ",
                       tags$strong("Metropoltian Council, Minnesota, United States"), ".",
                       "The source of this data set is",tags$a(href="http://stats.metc.state.mn.us/data_download/DD_start.aspx",
                                                               "http://stats.metc.state.mn.us/data_download/DD_start.aspx",target="_blank"),
                       ". The two sets of employment data, i.e., Jobs and
                       Workers, are:",
                       tags$span(      # Creates an HTML span.
                         tags$li("Jobs: This data set displays the number of jobs for selected
                                 city, year and industry type. This can also be thought
                                 of as the number of people working in that given area. These
                                 people may or may not be the residents of the same city."),
                         tags$li("Workers: This data set informs about the number of people employed in
                                 given city, year and industry type. This number can also be
                                 considered as the number of employed residents of selected area."),
                         tags$br()
                         )
                         )
                     )
              )
            ),
          fluidRow(
            box(
              title = "Navigating the Dashboard", solidHeader = TRUE,
              status = "warning", width=12, collapsible = TRUE, collapsed = TRUE,
              column(12,
                     tags$div(
                       "There are two main tabs for two data sets, i.e, jobs and workers.
                       For a selected tab, user can choose the industry type and the year to
                       see the heatmap of the number of jobs or workers for all the cities.
                       User can click on any location on the map to show the county and
                       city names along with the relevant numbers. If interested in knowing
                       the distribution of the numbers in various industries, user can select
                       the county and city names and visualize the histogram for the selected year."
                     )
                     )
                     )
                     ),
          fluidRow(
            
            box(
              title = "About Me", solidHeader = TRUE,
              status = "warning", width=12,collapsible = TRUE, collapsed = TRUE,
              column(12,
                     #h3(""),
                     tags$div(
                       "I am ",
                       tags$span(      # Creates an HTML span.
                         tags$strong("Aashish Jain"),
                         ", PhD in Chemical Engineering with computational physics background, with
                         experience in many academic projects focused on data science. Here is the link
                         to my LinkedIn page:", tags$a(href="https://www.linkedin.com/in/aashishjain",
                                                       "https://www.linkedin.com/in/aashishjain"),
                         ". If you have any suggestions,
                         questions, or reviews for this dashboard, please email to",
                         tags$a(href="mailto:aajains@gmail.com", "aajains@gmail.com"), "."
                     )
                     )
                       ),
              br()
              )
            )
          ),
  tabItem(tabName = "jobs",
          leafletOutput('myMap1'),
          plotOutput('myPlot1'),
          HTML('<style>.rChart {width: 200%; height: 500px}</style>'),
          tags$head(tags$style(HTML('
                                    /* main sidebar */
                                    .skin-yellow .main-sidebar {
                                    background-color: black;
                                    }
                                    .box.box-solid.box-primary{
                                    border-right-color:orange;
                                    }
                                    
                                    /* active selected tab in the sidebarmenu */
                                    .skin-yellow .main-sidebar .sidebar .sidebar-menu .active a{
                                    background-color: black;
                                    }
                                    
                                    /* other links in the sidebarmenu */
                                    .skin-yellow .main-sidebar .sidebar .sidebar-menu a{
                                    background-color: black;
                                    }
                                    
                                    /* other links in the sidebarmenu when hovered */
                                    .skin-yellow .main-sidebar .sidebar .sidebar-menu a:hover{
                                    background-color: orange;
                                    }
                                    /* body color */
                                    .content-wrapper,
                                    .right-side {
                                    background-color: black;
                                    }
                                    .js-irs-0 .irs-single,
                                    .js-irs-0 .irs-bar-edge,
                                    .js-irs-0 .irs-bar {
                                    background: orange}
                                    '))),
          tags$style(type="text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }")
          ),
  tabItem(tabName = "workers",
          leafletOutput('myMap2'),
          plotOutput('myPlot2'),
          HTML('<style>.rChart {width: 200%; height: 500px}</style>'),
          tags$head(tags$style(HTML('
                                    /* main sidebar */
                                    .skin-yellow .main-sidebar {
                                    background-color: black;
                                    }
                                    .box.box-solid.box-primary{
                                    border-right-color:orange;
                                    }
                                    
                                    /* active selected tab in the sidebarmenu */
                                    .skin-yellow .main-sidebar .sidebar .sidebar-menu .active a{
                                    background-color: black;
                                    }
                                    
                                    /* other links in the sidebarmenu */
                                    .skin-yellow .main-sidebar .sidebar .sidebar-menu a{
                                    background-color: black;
                                    }
                                    
                                    /* other links in the sidebarmenu when hovered */
                                    .skin-yellow .main-sidebar .sidebar .sidebar-menu a:hover{
                                    background-color: orange;
                                    }
                                    /* body color */
                                    .content-wrapper,
                                    .right-side {
                                    background-color: black;
                                    }
                                    .js-irs-0 .irs-single,
                                    .js-irs-0 .irs-bar-edge,
                                    .js-irs-0 .irs-bar {
                                    background: orange}
                                    '))),
          tags$style(type="text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }")
          )
          )
  )
ind_choices <- append("Select All", sort(unique(as.character(df_ind$INDUSTRY_TYPE))))
########################################
## SERVER
########################################
server <- function(input, output) {
  
  output$county_selector <- renderUI({
    selectInput("county", label = h5("County"),
                choices = sort(unique(as.character(indCivic$CountyNames))),
                multiple = FALSE)
  })
  
  output$area_text <- renderUI({
    tags$h5(tags$br(), tags$br(), paste(
      "Select area to see industrial distribution chart below"
    ))
  })
  
  output$city_selector <- renderUI({
    available <- as.character(indCivic[indCivic$CountyNames == input$county, "CTU_NAME"])
    selectInput("city", label = h5("City"),
                choices = sort(unique(available)),
                multiple = FALSE)
  })
  
  output$ind_selector <- renderUI({
    
    selectInput("industry", label = h5("Industrial Sector"),
                choices = ind_choices, selected = "Select All",
                multiple = FALSE)
    
  })
  
  output$year_selector <- renderUI({
    
    sliderInput("year",label=h5("Year"),min=2009,max=2014,value=2009, step=1,
                round=FALSE, sep="")
    
  })
  
  J_list <- reactive({
    if (input$industry == "Select All") {
      tmp_list <- as.numeric(unlist(lapply(1:nrow(cityShape@data), function(x)
        sum(indCivic$COUNT[indCivic$CTU_ID==as.integer(as.vector(cityShape@data$GNIS_ID))[x] &
                             indCivic$YEAR==input$year &
                             indCivic$JW_INDICATOR=="J" &
                             indCivic$CO_CODE==as.integer(as.vector(cityShape@data$County_num))[x]]))))
      return(tmp_list)
    } else {
      tmp_list <- as.numeric(unlist(lapply(1:nrow(cityShape@data), function(x)
        sum(indCivic$COUNT[indCivic$CTU_ID==as.integer(as.vector(cityShape@data$GNIS_ID))[x] &
                             indCivic$YEAR==input$year &
                             indCivic$JW_INDICATOR=="J" &
                             indCivic$CO_CODE==as.integer(as.vector(cityShape@data$County_num))[x] &
                             indCivic$INDUSTRY_TYPE==input$industry]))))
      return(tmp_list)
    }
  })
  
  W_list <- reactive({
    if (input$industry == "Select All") {
      tmp_list1 <- as.numeric(unlist(lapply(1:nrow(cityShape@data), function(x)
        sum(indCivic$COUNT[indCivic$CTU_ID==as.integer(as.vector(cityShape@data$GNIS_ID))[x] &
                             indCivic$YEAR==input$year &
                             indCivic$JW_INDICATOR=="W" &
                             indCivic$CO_CODE==as.integer(as.vector(cityShape@data$County_num))[x]]))))
      return(tmp_list1)
    } else {
      tmp_list1 <- as.numeric(unlist(lapply(1:nrow(cityShape@data), function(x)
        sum(indCivic$COUNT[indCivic$CTU_ID==as.integer(as.vector(cityShape@data$GNIS_ID))[x] &
                             indCivic$YEAR==input$year &
                             indCivic$JW_INDICATOR=="W" &
                             indCivic$CO_CODE==as.integer(as.vector(cityShape@data$County_num))[x] &
                             indCivic$INDUSTRY_TYPE==input$industry]))))
      return(tmp_list1)
    }
  })
  
  #Initialize the J map
  output$myMap1 <- renderLeaflet({
    tmp_list_initJ <- as.numeric(unlist(lapply(1:nrow(cityShape@data), function(x)
      sum(indCivic$COUNT[indCivic$CTU_ID==as.integer(as.vector(cityShape@data$GNIS_ID))[x] &
                           indCivic$YEAR==2009 &
                           indCivic$JW_INDICATOR=="J" &
                           indCivic$CO_CODE==as.integer(as.vector(cityShape@data$County_num))[x]]))))
    cityShape@data <- data.frame(cityShape@data, Jcount = tmp_list_initJ)
    theData_initJ <- cityShape
    pal1_initJ <- colorNumeric(palette = "Purples", domain = theData_initJ$Jcount)
    city_popup_initJ <- paste("<strong>County: </strong>",
                              theData_initJ$CountyNames, "<br>",
                              "<strong>City: </strong>",
                              theData_initJ$Name, "<br>",
                              "<strong>Jobs: </strong>",
                              theData_initJ$Jcount)
    leaflet() %>% addTiles() %>%
      setView(mean(countybounds[1,]),
              mean(countybounds[2,]),
              zoom=8 
      ) %>%
      addProviderTiles("Stamen.Terrain",
                       options = providerTileOptions(minzoom=8, maxzoom=10)) %>%
      addPolygons(data=countyShape,weight=1,fillColor = "black", fillOpacity = 0, color="blue") %>%
      addPolygons(data=cityShape,weight=0.5,fillColor = "black", fillOpacity = 0, color="black") %>%
      addPolygons(data=theData_initJ,stroke=FALSE, weight=1,smoothFactor=0.1,
                  fillOpacity = 0.5,
                  fillColor = ~pal1_initJ(theData_initJ$Jcount),
                  popup = city_popup_initJ) %>%
      addLegend(position = 'bottomright',
                pal = pal1_initJ, values = theData_initJ$Jcount,
                opacity = 1, title = paste('Jobs in', 2009))
  })
  # Change map when user clicks on any event
  observeEvent(c(input$industry, input$year), {
    cityShape@data <- data.frame(cityShape@data, Jcount = J_list())
    theData1 <- cityShape
    pal1 <- colorNumeric(palette = "Purples", domain = theData1$Jcount)
    city_popup <- paste("<strong>County: </strong>",
                        theData1$CountyNames, "<br>",
                        "<strong>City: </strong>",
                        theData1$Name, "<br>",
                        "<strong>Jobs: </strong>",
                        theData1$Jcount)
    # If the data changes, the polygons are cleared and redrawn, however, 
    # the map (above) is not redrawn
    leafletProxy("myMap1", data = theData1) %>%
      clearShapes() %>%  clearControls() %>% 
      addPolygons(data=countyShape,weight=1,fillColor = "black", fillOpacity = 0, color="blue") %>%
      addPolygons(data=cityShape,weight=0.5,fillColor = "black", fillOpacity = 0, color="black") %>%
      addPolygons(data=theData1,stroke=FALSE, weight=1,smoothFactor=0.1,
                  fillOpacity = 0.5,
                  fillColor = ~pal1(theData1$Jcount),
                  popup = city_popup) %>%
      addLegend(position = 'bottomright',
                pal = pal1, values = theData1$Jcount,
                opacity = 1, title = paste('Jobs in', input$year))
  })
  
  #Initialize the W map
  output$myMap2 <- renderLeaflet({
    tmp_list_initW <- as.numeric(unlist(lapply(1:nrow(cityShape@data), function(x)
      sum(indCivic$COUNT[indCivic$CTU_ID==as.integer(as.vector(cityShape@data$GNIS_ID))[x] &
                           indCivic$YEAR==2009 &
                           indCivic$JW_INDICATOR=="W" &
                           indCivic$CO_CODE==as.integer(as.vector(cityShape@data$County_num))[x]]))))
    cityShape@data <- data.frame(cityShape@data, Wcount = tmp_list_initW)
    theData_initW <- cityShape
    pal1_initW <- colorNumeric(palette = "Reds", domain = theData_initW$Wcount)
    city_popup_initW <- paste("<strong>County: </strong>",
                              theData_initW$CountyNames, "<br>",
                              "<strong>City: </strong>",
                              theData_initW$Name, "<br>",
                              "<strong>Jobs: </strong>",
                              theData_initW$Wcount)
    leaflet() %>% addTiles() %>%
      setView(mean(countybounds[1,]),
              mean(countybounds[2,]),
              zoom=8 
      ) %>%
      addProviderTiles("Stamen.Terrain",
                       options = providerTileOptions(minzoom=8, maxzoom=10)) %>%
      addPolygons(data=countyShape,weight=1,fillColor = "black", fillOpacity = 0, color="blue") %>%
      addPolygons(data=cityShape,weight=0.5,fillColor = "black", fillOpacity = 0, color="black") %>%
      addPolygons(data=theData_initW,stroke=FALSE, weight=1,smoothFactor=0.1,
                  fillOpacity = 0.5,
                  fillColor = ~pal1_initW(theData_initW$Wcount),
                  popup = city_popup_initW) %>%
      addLegend(position = 'bottomright',
                pal = pal1_initW, values = theData_initW$Wcount,
                opacity = 1, title = paste('Workers in', 2009))
  })
  # Change map when user clicks on any event
  observeEvent(c(input$industry, input$year), {
    cityShape@data <- data.frame(cityShape@data, Wcount = W_list())
    theData2 <- cityShape
    pal2 <- colorNumeric(palette = "Reds", domain = theData2$Wcount)
    city_popup2 <- paste("<strong>County: </strong>",
                         theData2$CountyNames, "<br>",
                         "<strong>City: </strong>",
                         theData2$Name, "<br>",
                         "<strong>Jobs: </strong>",
                         theData2$Wcount)
    # If the data changes, the polygons are cleared and redrawn, however, 
    # the map (above) is not redrawn
    leafletProxy("myMap2", data = theData2) %>%
      clearShapes() %>%  clearControls() %>% 
      addPolygons(data=countyShape,weight=1,fillColor = "black", fillOpacity = 0, color="blue") %>%
      addPolygons(data=cityShape,weight=0.5,fillColor = "black", fillOpacity = 0, color="black") %>%
      addPolygons(data=theData2,stroke=FALSE, weight=1,smoothFactor=0.1,
                  fillOpacity = 0.5,
                  fillColor = ~pal2(theData2$Wcount),
                  popup = city_popup2) %>%
      addLegend(position = 'bottomright',
                pal = pal2, values = theData2$Wcount,
                opacity = 1, title = paste('Jobs in', input$year))
  })
  
  
  df1 <- reactive({
    dat1 <- indCivic[indCivic$CTU_NAME==input$city &
                       indCivic$CountyNames==input$county &
                       #indCivic$YEAR==input$year &
                       indCivic$YEAR>2008 &
                       indCivic$JW_INDICATOR=="J",]
  })
  
  df2 <- reactive({
    dat2 <- indCivic[indCivic$CTU_NAME==input$city &
                       indCivic$CountyNames==input$county &
                       #indCivic$YEAR==input$year &
                       indCivic$YEAR>2008 &
                       indCivic$JW_INDICATOR=="W",]
  })
  
  output$myPlot1 <- renderPlot({
    dataset1 <- df1()
    p1 <- ggplot(dataset1,aes(x=INDUSTRY_TYPE, y=COUNT, group=as.factor(YEAR)))  +
      geom_bar(stat="Identity", width=0.7, position = "dodge", aes(fill=as.factor(YEAR))) +
      myTheme + xlab("") + ylab("")  +
      ylim(0, max(dataset1$COUNT, na.rm = TRUE)) +
      theme(axis.text.x=element_text(angle=70, hjust=1)) +
      scale_fill_brewer(name="Year", palette = "Set1") + 
      ggtitle(paste0("Number of jobs in ", input$city, ", ", input$county))
    print(p1)
  }, bg="black")
  
  
  output$myPlot2 <- renderPlot({
    dataset2 <- df2()
    p2 <- ggplot(dataset2,aes(x=INDUSTRY_TYPE, y=COUNT, group=as.factor(YEAR)))  +
      geom_bar(stat="Identity", width=0.7, position = "dodge", aes(fill=as.factor(YEAR))) +
      myTheme + xlab("") + ylab("")  +
      ylim(0, max(dataset2$COUNT, na.rm = TRUE)) +
      theme(axis.text.x=element_text(angle=70, hjust=1)) +
      scale_fill_brewer(name="Year", palette = "Set1") + 
      ggtitle(paste0("Number of workers in ", input$city, ", ", input$county))
    print(p2)
  }, bg="black")
}

myapp <- shinyApp(
  ui = dashboardPage(header, sidebar, body, skin = "yellow"),
  server = server
)
############################################################################