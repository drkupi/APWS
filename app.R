library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(maps)
library(geoshaper)
library(sp)
library(RCurl)
library(stringr)
library(lubridate)
library(XML)
library(dplyr)
library(plyr)
library(rgdal)
library(plotly)

# Declare some global variables
counter <<- 0
countryLayer <<- NULL
alldata <<- NULL
sdata <<- NULL
list_counter <<- 0
cname <<- NULL

# Load background Data
get_climate_data <- function(){
  withProgress(message = 'Loading Data', value=0,{
    data <- read.csv(file="Final_WGN.csv", header=TRUE, sep=",")
    data$secondLocationID <- paste(as.character(data$OBJECTID), "_selectedLayer", sep="")
    cdata <- SpatialPointsDataFrame(data[,c('WLONGITUDE', 'WLATITUDE')] , data)
  })
}


# Define UI ----
ui <- dashboardPage(skin="red",
  dashboardHeader(
    title = "APWS Data Portal"
    ),
  dashboardSidebar(
    HTML('
    
    <p style="text-align:left; margin: 0.7em; font-size: 23px; font-weight: bold; color: orange"> Navigation Help </p>
    
		<p style="text-align:justify; margin: 1em"> First choose a data selection mechanism from "SELECTION PANEL".
		The three available selection mechanisms are <I>Custom Selection</I>, <I>Shape File</I> and <I>Country</I>. </p>
		
		<p style="text-align:justify; margin: 1em">If preferred selection mechanism is <I>Custom Selection</I> then:
		i) choose the shape type within the map (from top-left buttons in map: rectangle or polygon).
		, ii) draw the shape and wait until the "loading data" sign is on (bottom right),
		 iii) use the enabled download button to download selected data (as csv file).</p>
		
		<p style="text-align:justify; margin: 1em"> If preferred selection mechanism is <I>Shape File</I> then
		 i) an input panel will pop-up from which the user can upload their zipped shape folder
		 (make sure thatall files associated with the shape input (e.g., *.shp, *.dbf etc) are
		 saved as as a zipped folder), ii) after shape file is uploaded successfully, 
		 select a shape attribute as the unique identifier, iii) the polygons of the shape file will appear
		 in the map, iv) click on a polygon to select the data within that polygon (only single selection supported), 
		 v) wait until the "loading data" sign is on (bottom right), vi) use the enabled download button to download
		 the selected data (as csv file).  </p>
		 
		 <p style="text-align:justify; margin: 1em">If preferred selection mechanism is <I>Country</I> then:
		i) wait until the "loading data" sign disappears (bottom right of page) and country polygons appear on the map,
		ii) select a country and wait for data loading to finish,
		iii) use the enabled download button to download the selected data (as a csv file).</p>
		 
		 
		 <p style="text-align:justify; margin: 1em">The user may zoom into the selected area and click on any
		 selected data point (marked in blue) to visualize the climate data.</p>
		 
		 <p style="text-align:left; margin: 0.7em; font-size: 16px; color: orange"> App Authors:
		 Uttam Ghimire, Taimoor Akhtar, Narayan Shrestha and Prasad Daggupati, UNIVERSITY OF GUELPH, 
		 Contact Us at: pdaggupa@uoguelph.ca </p>
         
  '),
    width=300
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-size: 22px;
      }
    '))),
    tags$head(tags$style(HTML('
      .box-header h3.box-title{
        font-size: 20px;
      }
    '))),
    shinyjs::useShinyjs(),
    fluidRow(
      column(width = 8,
        box(
          title = "SELECT AREA OF INTEREST", status = "warning",
          solidHeader = TRUE, collapsible = FALSE, width = 12,
          tags$style(type = "text/css", "#map {height: calc(100vh - 150px) !important;}"),
          leafletOutput("map")
        )
      ),
      column(width=4,
        fluidRow(
          box(
            title = "Selection Panel", status = "warning",
            solidHeader = TRUE, collapsible = FALSE, width = 12,
            radioButtons("radio", label = h4("Choose Data Selection Method"),
                         choices = c("Custom Selection" = 1, "Shape File" = 2, "Country" = 3), 
                         inline=TRUE, selected = 1),
            conditionalPanel(
              condition = "input.radio == 2",
              # Input: Select a file ----
              fileInput("shpFile", "Upload Zipped Shape Files", accept = ".zip",
                        multiple = FALSE),
              selectInput("shpId", "Unique Shape Identifier:", choices = NULL, selectize = FALSE)
              
            )
          )
        ),
        fluidRow(
          box(
            title = "Download Selected Data", status = "primary",
            solidHeader = TRUE, collapsible = FALSE, width = 12,
            textOutput("selected_var"),
            downloadButton("downloadData", "Download"),
            helpText("Download will be available once the processing is completed.")
          )
        ),
        fluidRow(
          box(
            title = "Visualize Point Climate Data", status = "primary",
            solidHeader = TRUE, collapsible = FALSE, width = 12,
            helpText("Will be available upon selection of an area of interest and then a data marker."),
            selectInput("dataType", "Climate Variable:", choices = c("Temperature", "Precipitation"), selectize = FALSE),
            plotlyOutput("climePlot")
          )
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  error_id <<- NULL
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      setView(lng=105, lat=20, zoom=3) %>%
      setMaxBounds(lng1 = 60.1
                    , lat1 = -14.8
                    , lng2 = 149.9
                    , lat2 = 54.9 ) %>%
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                          ,color = 'red'
                                                                          ,weight = 3)),
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'red'
                                                                              ,weight = 3)),
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        singleFeature = TRUE,
        editOptions = editToolbarOptions(edit = FALSE, remove = TRUE, selectedPathOptions = selectedPathOptions()))
  })
  
  observeEvent(input$radio,{
    choice <- input$radio
    if (choice == 1){
      proxy <- leafletProxy("map")
      proxy %>% clearShapes()
      proxy %>% clearMarkers()
    }
    
    else if (choice == 2){
      proxy <- leafletProxy("map")
      proxy %>% clearShapes()
      proxy %>% clearMarkers()
    }
    
    else {
      withProgress(message = 'Loading Shapes - Please Wait', value=0,{
        countryLayer <<- readOGR(dsn='./Countries', layer='TM_WORLD_BORDERS_SIMPL-0.3')
        proxy <- leafletProxy("map", data=countryLayer)
        proxy %>% clearShapes()
        proxy %>% clearMarkers()
        proxy %>% addPolygons(fillColor = "green", weight = 1.0,
                              highlight = highlightOptions(weight = 2,
                                                           color="red",
                                                           fillOpacity = 0.3,
                                                           bringToFront = TRUE),
                              label = ~NAME, layerId = ~ISO3)
      })
      
    }
  })
  
  observeEvent(input$map_draw_new_feature,{
    # Step 1 - Load data if this is the first time selection is invoked
    if (counter == 0){
      alldata <<- get_climate_data()
      counter<<-counter+1
    }
   #viewCenter = unlist(input$map_draw_new_feature$geometry$coordinates[[1]][[1]])
    # Step 2 - Locate data in selected region
    
    found_in_bounds <- findLocations(shape = input$map_draw_new_feature
                                     , location_coordinates = alldata
                                     , location_id_colname = "OBJECTID")
    selected <- subset(alldata, OBJECTID %in% found_in_bounds)
    
    # Step 3 - Print Data Summary of Selected Data and Paste their Locations    
    output$selected_var <- renderText({ 
      paste("Number of Observations Found: ",length(selected))
    })
    proxy <- leafletProxy("map")
    proxy %>% clearShapes()
    proxy %>% clearMarkers()
    #proxy %>% setView(lng=viewCenter[1], lat = viewCenter[2], input$Map_zoom)
    proxy %>% addCircleMarkers(data = selected,
                         radius = 3,
                         lat = selected$WLATITUDE,
                         lng = selected$WLONGITUDE,
                         fillOpacity = 0.5,
                         color = "blue",
                         stroke = FALSE,
                         layerId = ~OBJECTID
                         )
    # Step 4 - Save Data as csv and Enable Download
    shinyjs::enable("downloadData")
    sdata <<-as.data.frame(selected)
  })
  
   # If shape file is being selected for rendering data
  observeEvent(input$map_shape_click,{
    x <- input$map_shape_click
    y <- x$id
    if (input$radio == 3){
      myPolygon <- countryLayer[countryLayer@data$ISO3==y,]
    }
    else {
      myPolygon <- countryLayer[countryLayer@data[[cname]]==y,] 
    }
    
    if (counter == 0){
      alldata <<- get_climate_data()
      counter<<-counter+1
    }
    newP = myPolygon[1]@polygons
    selected_locs <- sp::over(alldata, sp::SpatialPolygons(newP))
    x = (alldata[which(!is.na(selected_locs)), "OBJECTID"])
    selected_loc_id = as.character(x[["OBJECTID"]])
    selected <- subset(alldata, OBJECTID %in% selected_loc_id)
    
    output$selected_var <- renderText({ 
      paste("Number of Observations Found: ",length(selected))
    })
    
    proxy <- leafletProxy("map")
    proxy %>% clearShapes()
    proxy %>% addCircleMarkers(data = selected,
                               radius = 3,
                               lat = selected$WLATITUDE,
                               lng = selected$WLONGITUDE,
                               fillOpacity = 0.5,
                               color = "blue",
                               stroke = FALSE,
                               layerId = ~OBJECTID
    )
    # Step 4 - Save Data as csv and Enable Download
    shinyjs::enable("downloadData")
    sdata <<-as.data.frame(selected)
    
  })
  
  # The following code handles the case where shape file is provided by user
  observeEvent(input$shpFile, {
    
    if (!is.null(error_id))
      removeNotification(error_id)
    error_id <<- NULL
    
    inFile <- input$shpFile
    if (is.null(inFile))
      return(NULL)
    
    list_counter <<- 0
    
    # Once the filepath is known, copy file to server folder
    withProgress(message = "Uploading and unzipping shape file - Please Wait", value=0,{
      #file.copy(inFile$datapath, file.path(getwd(), inFile$name))
      unlink("./Scratch", recursive = TRUE)
      dir.create("Scratch")
      unzip(inFile$datapath, exdir = "./Scratch")
    })
    
    # Check shape file format and load shape file  
    dbfFiles <- list.files(paste("./Scratch",tools::file_path_sans_ext(inFile$name),sep = "/"),pattern = "\\.dbf$")
    if (length(dbfFiles) == 0){
      error_id <<- showNotification("No Shape File Found !!!!", type = "error")
    }
    else if (length(dbfFiles) > 1){
      error_id <<- showNotification("Multiple Shape Files found. Choosing first file.", type = "warning")
      withProgress(message = 'Loading Shapes - Please Wait', value=0,{
        countryLayer <<- readOGR(dsn=paste("./Scratch",tools::file_path_sans_ext(inFile$name),sep = "/"), layer=tools::file_path_sans_ext(dbfFiles[1]))
        updateSelectInput(session, "shpId",
                          choices = colnames(countryLayer@data))

      })
    }
    else {
      withProgress(message = 'Loading Shapes - Please Wait', value=0,{
        countryLayer <<- readOGR(dsn=paste("./Scratch",tools::file_path_sans_ext(inFile$name),sep = "/"), layer=tools::file_path_sans_ext(dbfFiles[1]))
        updateSelectInput(session, "shpId",
                          choices = colnames(countryLayer@data))
      })
    }
  })
  
  # When custom shape file is selected, user also has to select the unique layer identifier
  observeEvent(input$shpId,{
    if (list_counter > 0){
      cname <<- input$shpId
      withProgress(message = 'Plotting - Please Wait', value=0,{
        proxy <- leafletProxy("map", data=countryLayer)
        proxy %>% clearShapes()
        proxy %>% clearMarkers()
        proxy %>% addPolygons(fillColor = "red", weight = 1.0,
                              highlight = highlightOptions(weight = 2,
                                                           color="red",
                                                           fillOpacity = 0.8,
                                                           bringToFront = TRUE),
                              label = countryLayer@data[[cname]], layerId = countryLayer@data[[cname]])
      })
    }
    shinyjs::enable("shpId")
    list_counter <<- list_counter + 1  
  },
  ignoreInit = TRUE)
  
  
  observeEvent(input$map_marker_click,{
    x <- input$map_marker_click
    y <- x$id
    month <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
    maxT <- as.vector(as.numeric(sdata[sdata$OBJECTID==y,7:18]))
    minT <- as.vector(as.numeric(sdata[sdata$OBJECTID==y,19:30]))
    meanP <- as.vector(as.numeric(sdata[sdata$OBJECTID==y,55:66]))
    stdP <- as.vector(as.numeric(sdata[sdata$OBJECTID==y,67:78]))
    
    plotData <<- data.frame(month, maxT, minT, meanP, stdP)
    plotData$month <<- factor(plotData$month, levels = plotData[["month"]])
    
    proxy <- leafletProxy("map")
    proxy  %>% addCircleMarkers(x$lng, x$lat, radius=6, color="black", fillColor="orange",
                              fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected")
    if (input$dataType == "Temperature"){
      output$climePlot <- renderPlotly({
        p <- plot_ly(plotData, x = ~month, y = ~maxT, name = 'Max Temperature', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
          add_trace(y = ~minT, name = 'Min Temperature', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
          layout(title = "Average High and Low Temperatures", xaxis = list(title = 'MONTHS'), yaxis = list(title = 'Temperature (C)'))
      })
    }
    else{
      output$climePlot <- renderPlotly({
        p <- plot_ly(plotData, x = ~month, y = ~meanP, name = 'Mean Rainfall', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
          layout(title = "Mean Rainfall", xaxis = list(title = 'MONTHS'), yaxis = list(title = 'Rainfall (mm)'))
      })
    }
    shinyjs::enable("dataType")
  })
  
  
  observeEvent(input$dataType,{
    if (input$dataType == "Temperature"){
      output$climePlot <- renderPlotly({
        p <- plot_ly(plotData, x = ~month, y = ~maxT, name = 'Max Temperature', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
          add_trace(y = ~minT, name = 'Min Temperature', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
          layout(title = "Average High and Low Temperatures", xaxis = list(title = 'MONTHS'), yaxis = list(title = 'Temperature (C)'))
      })
    }
    else{
      output$climePlot <- renderPlotly({
        p <- plot_ly(plotData, x = ~month, y = ~meanP, name = 'Mean Rainfall', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
          layout(title = "Mean Rainfall", xaxis = list(title = 'MONTHS'), yaxis = list(title = 'Rainfall (mm)'))
      })
    }
  },
  ignoreInit = TRUE)
  
  
  # Downloadable csv of selected data ----
  output$downloadData <- downloadHandler(
    filename = "selectedData.csv",
    content = function(file) {
      write.csv(sdata, file, row.names = FALSE)
    }
  )
  
  # disable the downdload button on page load
  shinyjs::disable("downloadData")
  shinyjs::disable("shpId")
  shinyjs::disable("dataType")
}

# Run the app ----
shinyApp(ui = ui, server = server)