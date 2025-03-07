# Install and load necessary libraries

library(shinylive)
library(httpuv)


if (!require(shiny)) install.packages("shiny")
if (!require(leaflet)) install.packages("leaflet")
if (!require(dplyr)) install.packages("dplyr")
if (!require(rsconnect)) install.packages("rsconnect")
if (!require(plotly)) install.packages("plotly")
if (!require(leaflet.extras)) install.packages("leaflet.extras")
if (!require(rsconnect)) install.packages("rsconnect")
if (!require(sf)) install.packages("sf")

# load raster package
if (!require(raster)) install.packages("raster")
library(raster)


# for the analysis part, KDA and nn
if (!require(spatstat)) install.packages("spatstat")
library(spatstat)


library(leaflet.extras)

# needed for shinyapps # redundant ,since we use shinylive
library(rsconnect)

library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(rsconnect)
library(plotly)

# Define the UI
ui <- fluidPage(
  titlePanel("Accident Atlas Visualization"),
  
  fluidRow(
    column(3,
           wellPanel(
             # Geographic filters
             selectInput("uland", "Federal State (ULAND)",
                         choices = NULL, selected = NULL, multiple = TRUE),
             selectInput("uregbez", "Administrative District (UREGBEZ)",
                         choices = NULL, selected = NULL, multiple = TRUE),
             selectInput("ukreis", "County (UKREIS)",
                         choices = NULL, selected = NULL, multiple = TRUE),
             selectInput("ugemeinde", "Municipality (UGEMEINDE)",
                         choices = NULL, selected = NULL, multiple = TRUE),
             
             # Other filters (unchanged)
             selectInput("ukategorie", "Accident Category",
                         choices = c("With Fatalities" = 1,
                                     "With Severe Injuries" = 2,
                                     "With Minor Injuries" = 3),
                         selected = NULL, multiple = TRUE),
             selectInput("umonat", "Accident Month",
                         choices = NULL, selected = NULL, multiple = TRUE),
             selectInput("uwochentag", "Weekday",
                         choices = c("Sunday" = 1,
                                     "Monday" = 2,
                                     "Tuesday" = 3,
                                     "Wednesday" = 4,
                                     "Thursday" = 5,
                                     "Friday" = 6,
                                     "Saturday" = 7),
                         selected = NULL, multiple = TRUE),
             selectInput("ustunde", "Accident Hour",
                         choices = NULL, selected = NULL, multiple = TRUE),
             selectInput("ulichtverh", "Light Conditions",
                         choices = c("Daylight" = 0,
                                     "Twilight" = 1,
                                     "Darkness" = 2),
                         selected = NULL, multiple = TRUE),
             
             checkboxGroupInput("istvehicle", "Vehicle Involvement",
                                choices = c("Bicycle" = "IstRad",
                                            "Car" = "IstPKW",
                                            "Pedestrian" = "IstFuss",
                                            "Motorcycle" = "IstKrad",
                                            "Commercial Vehicle" = "IstGkfz",
                                            "Others" = "IstSonstige")),
             
             # Add toggle for clustering
             checkboxInput("useClustering", "Group Points", value = TRUE),
             
             checkboxInput("useHeatmap", "Accident Hotspot Analysis", value = FALSE),
             
             checkboxInput("useKDE", "Kernel Density Estimation", value = FALSE),
             
             checkboxInput("useNearestNeighbor", "Nearest Neighbor Analysis", value = FALSE),
             
             actionButton("update", "Update Map", class = "btn-primary"),
             
             actionButton("showInfo", "Information", icon = icon("info-circle"), class = "btn-info")
             
             
           )
    ),
    
    column(9,
           # Map
           leafletOutput("map", height = "600px"),
           
           # Statistics
           verbatimTextOutput("stats"),
           
           # Plotly plot for accident count over time
           plotlyOutput("timePlot"),
           
           # Plotly plot for hourly accident distribution
           plotlyOutput("hourPlot"),
           
           plotlyOutput("lightPlot"),
           
           plotlyOutput("weekdayPlot")
           
           
           
           
    )
  )
)
# Define the server
server <- function(input, output, session) {
  
  # Reactive values to store the data
  data_raw <- reactiveVal(NULL)
  data_filtered <- reactiveVal(NULL)
  
  # Load the data when the app starts
  observe({
    withProgress(message = 'Loading data...', {
      df <- read.csv("Unfallorte2023_LinRef_1.csv", sep = ";", dec = ",")
      #df <- read.csv("C:\\Users\\e.dogan\\Downloads\\Unfallorte2023_EPSG25832_CSV\\csv\\Unfallorte2023_LinRef_1.csv",
      #              sep = ";",
      #             dec = ",")
    })
    
    # Clean the data
    df_clean <- df[!is.na(df$XGCSWGS84) & !is.na(df$YGCSWGS84) &
                     is.finite(df$XGCSWGS84) & is.finite(df$YGCSWGS84) &
                     df$XGCSWGS84 >= -180 & df$XGCSWGS84 <= 180 &
                     df$YGCSWGS84 >= -90 & df$YGCSWGS84 <= 90, ]
    
    data_raw(df_clean)
    data_filtered(df_clean)
    
    # Update all geographic dropdown menus
    updateSelectInput(session, "uland",
                      choices = sort(unique(as.character(df_clean$ULAND))))
    updateSelectInput(session, "uregbez",
                      choices = sort(unique(as.character(df_clean$UREGBEZ))))
    updateSelectInput(session, "ukreis",
                      choices = sort(unique(as.character(df_clean$UKREIS))))
    updateSelectInput(session, "ugemeinde",
                      choices = sort(unique(as.character(df_clean$UGEMEINDE))))
    
    # Update other dropdown menus
    updateSelectInput(session, "umonat",
                      choices = sort(unique(as.character(df_clean$UMONAT))))
    updateSelectInput(session, "ustunde",
                      choices = sort(unique(as.character(df_clean$USTUNDE))))
  })
  
  # Filter the data based on the selected parameters when the update button is clicked
  observeEvent(input$update, {
    req(data_raw())
    
    withProgress(message = 'Filtering data...', {
      df <- data_raw()
      
      # Apply geographic filters
      if (!is.null(input$uland) && length(input$uland) > 0) {
        df <- df[df$ULAND %in% input$uland, ]
      }
      
      if (!is.null(input$uregbez) && length(input$uregbez) > 0) {
        df <- df[df$UREGBEZ %in% input$uregbez, ]
      }
      
      if (!is.null(input$ukreis) && length(input$ukreis) > 0) {
        df <- df[df$UKREIS %in% input$ukreis, ]
      }
      
      if (!is.null(input$ugemeinde) && length(input$ugemeinde) > 0) {
        df <- df[df$UGEMEINDE %in% input$ugemeinde, ]
      }
      
      # Apply other filters
      if (!is.null(input$ukategorie) && length(input$ukategorie) > 0) {
        df <- df[df$UKATEGORIE %in% as.integer(input$ukategorie), ]
      }
      
      if (!is.null(input$umonat) && length(input$umonat) > 0) {
        df <- df[df$UMONAT %in% as.integer(input$umonat), ]
      }
      
      if (!is.null(input$uwochentag) && length(input$uwochentag) > 0) {
        df <- df[df$UWOCHENTAG %in% as.integer(input$uwochentag), ]
      }
      
      if (!is.null(input$ustunde) && length(input$ustunde) > 0) {
        df <- df[df$USTUNDE %in% as.integer(input$ustunde), ]
      }
      
      if (!is.null(input$ulichtverh) && length(input$ulichtverh) > 0) {
        df <- df[df$ULICHTVERH %in% as.integer(input$ulichtverh), ]
      }
      
      # Vehicle type filters
      if (!is.null(input$istvehicle) && length(input$istvehicle) > 0) {
        for (vehicle in input$istvehicle) {
          df <- df[df[[vehicle]] == 1, ]
        }
      }
      
      data_filtered(df)
    })
  })
  
  # Add cascading filter functionality
  observeEvent(input$uland, {
    req(data_raw())
    df <- data_raw()
    
    # Filter only by selected Bundesland
    df <- df[df$ULAND %in% input$uland, ]
    
    # Check if the Bundesland has Regierungsbezirk data
    if (any(!is.na(df$UREGBEZ))) {
      updateSelectInput(session, "uregbez",
                        choices = sort(unique(as.character(df$UREGBEZ))),
                        selected = NULL)
    } else {
      updateSelectInput(session, "uregbez", choices = NULL, selected = NULL)
    }
    
    # Update Kreise based on Bundesland
    updateSelectInput(session, "ukreis",
                      choices = sort(unique(as.character(df$UKREIS))),
                      selected = NULL)
  })
  
  
  observeEvent(input$uregbez, {
    req(data_raw())
    df <- data_raw()
    
    # Always filter by selected Bundesland first
    df <- df[df$ULAND %in% input$uland, ]
    
    # If Regierungsbezirk is selected, filter further
    if (!is.null(input$uregbez) && length(input$uregbez) > 0) {
      df <- df[df$UREGBEZ %in% input$uregbez, ]
    }
    
    updateSelectInput(session, "ukreis",
                      choices = sort(unique(as.character(df$UKREIS))),
                      selected = NULL)
  })
  
  
  observeEvent(input$ukreis, {
    req(data_raw())
    df <- data_raw()
    
    # Always filter by selected Bundesland first
    df <- df[df$ULAND %in% input$uland, ]
    
    # If Regierungsbezirk is selected, filter further
    if (!is.null(input$uregbez) && length(input$uregbez) > 0) {
      df <- df[df$UREGBEZ %in% input$uregbez, ]
    }
    
    # Filter by selected Kreis
    if (!is.null(input$ukreis) && length(input$ukreis) > 0) {
      df <- df[df$UKREIS %in% input$ukreis, ]
    }
    
    updateSelectInput(session, "ugemeinde",
                      choices = sort(unique(as.character(df$UGEMEINDE))),
                      selected = NULL)
  })
  
  
  output$map <- renderLeaflet({
    df <- data_filtered()
    
    if (is.null(df) || nrow(df) == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 10.4515, lat = 51.1657, zoom = 6)
    } else {
      pal <- colorFactor(
        palette = c("darkred", "red", "orange"),
        domain = c(1, 2, 3)
      )
      
      # Create base map
      map <- leaflet(df) %>%
        addTiles() %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = c(1, 2, 3),
          title = "Unfallkategorie",
          labels = c("With Fatalities", "With Serious Injuries", "With Minor Injuries"),
          opacity = 0.7
        )
      
      # Check if heatmap is enabled
      if (input$useHeatmap) {
        map <- map %>%
          addHeatmap(
            lng = ~XGCSWGS84,
            lat = ~YGCSWGS84,
            intensity = ~UKATEGORIE, # Weight by category
            blur = 20,
            max = 1,
            radius = 15
          )
      } else {
        # Add markers with or without clustering
        if (input$useClustering) {
          map <- map %>%
            addCircleMarkers(
              lng = ~XGCSWGS84,
              lat = ~YGCSWGS84,
              radius = 5,
              color = ~pal(UKATEGORIE),
              stroke = FALSE,
              fillOpacity = 0.7,
              popup = ~paste(
                "<b>Date:</b>", UJAHR, "/", UMONAT, "<br>",
                "<b>Hour:</b>", USTUNDE, "<br>",
                "<b>Category:</b>", UKATEGORIE, "<br>",
                "<b>Accident Type:</b>", UART
              ),
              clusterOptions = markerClusterOptions()
            )
        } else {
          map <- map %>%
            addCircleMarkers(
              lng = ~XGCSWGS84,
              lat = ~YGCSWGS84,
              radius = 5,
              color = ~pal(UKATEGORIE),
              stroke = FALSE,
              fillOpacity = 0.7,
              popup = ~paste(
                "<b>Date:</b>", UJAHR, "/", UMONAT, "<br>",
                "<b>Hour:</b>", USTUNDE, "<br>",
                "<b>Category:</b>", UKATEGORIE, "<br>",
                "<b>Accident Type:</b>", UART
              )
            )
        }
      }
      
      # Auto-adjust view
      if (nrow(df) > 0) {
        map <- map %>%
          fitBounds(
            lng1 = min(df$XGCSWGS84),
            lat1 = min(df$YGCSWGS84),
            lng2 = max(df$XGCSWGS84),
            lat2 = max(df$YGCSWGS84)
          )
      } else {
        map <- map %>%
          setView(lng = 10.4515, lat = 51.1657, zoom = 6)
      }
      
      # Kernel Density Estimation
      if (input$useKDE) {
        # Convert data to spatial points
        points <- st_as_sf(df, coords = c("XGCSWGS84", "YGCSWGS84"), crs = 4326)
        
        # Define the extent of the study area
        bbox <- st_bbox(points)
        window <- owin(xrange = c(bbox$xmin, bbox$xmax), yrange = c(bbox$ymin, bbox$ymax))
        
        # Convert to ppp object
        ppp_data <- ppp(st_coordinates(points)[, 1], st_coordinates(points)[, 2], window = window)
        
        # Perform Kernel Density Estimation
        dens <- density(ppp_data, sigma = bw.diggle(ppp_data))
        
        # Convert to raster for leaflet
        raster_dens <- raster(dens)
        crs(raster_dens) <- "+proj=longlat +datum=WGS84"
        
        # Add KDE layer to map
        map <- map %>%
          addRasterImage(raster_dens, colors = colorNumeric("RdYlGn", values(raster_dens), na.color = "transparent"), opacity = 0.8) %>%
          addLegend(pal = colorNumeric("RdYlGn", values(raster_dens), na.color = "transparent"), 
                    values = values(raster_dens), 
                    title = "Density")
      }
      
      # Nearest Neighbor Analysis
      if (input$useNearestNeighbor) {
        # Convert data to spatial points if not already done
        if (!exists("points")) {
          points <- st_as_sf(df, coords = c("XGCSWGS84", "YGCSWGS84"), crs = 4326)
        }
        
        # Calculate nearest neighbor distances
        nn_dist <- nndist(ppp_data)
        
        # Basic descriptive statistics
        mean_nn <- mean(nn_dist)
        sd_nn <- sd(nn_dist)
        
        # Display statistics on the map (as a popup or in a separate panel)
        # Here, we'll display it as a popup on the map
        map <- map %>%
          addPopups(lng = mean(df$XGCSWGS84), lat = mean(df$YGCSWGS84), 
                    popup = paste("Mean Nearest Neighbor Distance:", round(mean_nn, 4), "<br>",
                                  "SD Nearest Neighbor Distance:", round(sd_nn, 4)))
      }
      
      map
    }
  })
  
  
  # Create a reactive expression to check if multiple geographic selections exist
  hasMultipleGeoSelections <- reactive({
    # Check if multiple selections exist in any geographic filter
    (length(input$uland) > 1) || 
      (length(input$uregbez) > 1) || 
      (length(input$ukreis) > 1) || 
      (length(input$ugemeinde) > 1)
  })
  
  # Create a reactive expression to determine which geographic filter to use for separation
  getSeparationField <- reactive({
    # Return the filter with multiple selections, prioritizing the most specific level
    if (length(input$ugemeinde) > 1) {
      return("UGEMEINDE")
    } else if (length(input$ukreis) > 1) {
      return("UKREIS")
    } else if (length(input$uregbez) > 1) {
      return("UREGBEZ") 
    } else if (length(input$uland) > 1) {
      return("ULAND")
    } else {
      return(NULL) # No multiple selections
    }
  })
  
  
  
  
  
  output$stats <- renderText({
    df <- data_filtered()
    
    if (is.null(df)) {
      return("Loading data...")
    }
    
    # Base stats
    stats_text <- paste(
      "Number of displayed accidents:", nrow(df), "\n",
      "Accidents by category:\n",
      "  With Fatalities:", sum(df$UKATEGORIE == 1), "\n",
      "  With Serious Injuries:", sum(df$UKATEGORIE == 2), "\n",
      "  With Minor Injuries:", sum(df$UKATEGORIE == 3)
    )
    
    # Get separation field if multiple selections exist
    sep_field <- getSeparationField()
    show_separated <- !is.null(sep_field)
    
    if (show_separated) {
      # Add stats by the separation field
      stats_text <- paste0(stats_text, "\n\nBreakdown by ", sep_field, ":\n")
      
      # Get unique values of the separation field
      unique_values <- unique(df[[sep_field]])
      
      for (value in unique_values) {
        # Filter data for this specific value
        subset_df <- df[df[[sep_field]] == value, ]
        stats_text <- paste0(stats_text, 
                             "  ", sep_field, " ", value, ":\n",
                             "    Number of Accidents: ", nrow(subset_df), "\n",
                             "    With Fatalities: ", sum(subset_df$UKATEGORIE == 1), "\n",
                             "    With Serious Injuries: ", sum(subset_df$UKATEGORIE == 2), "\n",
                             "    With Minor Injuries: ", sum(subset_df$UKATEGORIE == 3), "\n"
        )
      }
    }
    
    stats_text
  })
  
  observeEvent(input$showInfo, {
    showModal(modalDialog(
      title = "About Accident Atlas Visualization",
      HTML("
      <div style='font-size: 14px;'>
        <p><strong>This application visualizes traffic accident data.</strong></p>
        
        <h4>Features:</h4>
        <ul>
          <li>Filter accidents by location (e.g. 5,5,15 for Münster), time, and type</li>
          <li>View accidents on an interactive map</li>
          <li>Analyze accident patterns with various statistical methods</li>
          <li>Visualize data through multiple charts</li>
        </ul>
        
        <h4>Instructions:</h4>
        <ol>
          <li>Select filters on the left panel</li>
          <li>Click 'Update Map' to apply filters</li>
          <li>Toggle between different visualization options</li>
          <li>View statistics below the map</li>
        </ol>
        
        <p>Data source: German Accident Atlas (Unfallatlas)</p>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  # Generate the Plotly plot showing accident counts over time
  output$timePlot <- renderPlotly({
    df <- data_filtered()
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    # Get separation field if multiple selections exist
    sep_field <- getSeparationField()
    show_separated <- !is.null(sep_field)
    
    if (show_separated) {
      # Use string interpolation to create a dynamic group_by statement
      df_grouped <- df %>%
        group_by_at(c("UJAHR", "UMONAT", sep_field)) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(date = as.Date(paste(UJAHR, UMONAT, "01", sep = "-"))) %>%
        arrange(date)
      
      # Convert grouping variable to factor
      df_grouped[[sep_field]] <- as.factor(df_grouped[[sep_field]])
      
      # Create the line chart with colors by the separation field
      plot_ly(df_grouped, x = ~date, y = ~count, color = as.formula(paste0("~", sep_field)), 
              type = 'scatter', mode = 'lines+markers',
              colors = "Set1", marker = list(size = 8)) %>%
        layout(
          title = paste0("Number of Accidents Over Time by ", sep_field),
          xaxis = list(title = "Date", tickformat = "%b %Y"),
          yaxis = list(title = "Number of Accidents"),
          hovermode = "closest",
          legend = list(title = list(text = sep_field))
        )
    } else {
      # Default behavior: group only by year and month
      df_grouped <- df %>%
        group_by(UJAHR, UMONAT) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(date = as.Date(paste(UJAHR, UMONAT, "01", sep = "-"))) %>%
        arrange(date)
      
      # Create the line chart without separation
      plot_ly(df_grouped, x = ~date, y = ~count, type = 'scatter', mode = 'lines+markers',
              line = list(color = 'rgb(0, 123, 255)', width = 2),
              marker = list(color = 'rgb(0, 123, 255)', size = 8)) %>%
        layout(
          title = "Number of Accidents Over Time",
          xaxis = list(title = "Date", tickformat = "%b %Y"),
          yaxis = list(title = "Number of Accidents"),
          hovermode = "closest"
        )
    }
  })
  
  output$hourPlot <- renderPlotly({
    df <- data_filtered()
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    # Get separation field if multiple selections exist
    sep_field <- getSeparationField()
    show_separated <- !is.null(sep_field)
    
    if (show_separated) {
      # Group by hour AND separation field
      df_grouped <- df %>%
        group_by_at(c("USTUNDE", sep_field)) %>%
        summarise(count = n(), .groups = 'drop')
      
      # Convert grouping variable to factor
      df_grouped[[sep_field]] <- as.factor(df_grouped[[sep_field]])
      
      # Create the line chart with color by separation field
      plot_ly(df_grouped, x = ~USTUNDE, y = ~count, color = as.formula(paste0("~", sep_field)), 
              type = 'scatter', mode = 'lines+markers',
              colors = "Set1", marker = list(size = 8)) %>%
        layout(
          title = paste0("Number of Accidents by Hour and ", sep_field),
          xaxis = list(title = "Hour", tickvals = 0:23, tickmode = "array"),
          yaxis = list(title = "Number of Accidents"),
          hovermode = "closest",
          legend = list(title = list(text = sep_field))
        )
    } else {
      # Default behavior: group only by hour
      df_grouped <- df %>%
        group_by(USTUNDE) %>%
        summarise(count = n(), .groups = 'drop')
      
      # Create the line chart without separation
      plot_ly(df_grouped, x = ~USTUNDE, y = ~count, type = 'scatter', mode = 'lines+markers',
              line = list(color = 'rgb(255, 99, 71)', width = 2),
              marker = list(color = 'rgb(255, 99, 71)', size = 8)) %>%
        layout(
          title = "Number of Accidents by Hour",
          xaxis = list(title = "Hour", tickvals = 0:23, tickmode = "array"),
          yaxis = list(title = "Number of Accidents"),
          hovermode = "closest"
        )
    }
  })
  
  output$lightPlot <- renderPlotly({
    df <- data_filtered()
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    # Get separation field if multiple selections exist
    sep_field <- getSeparationField()
    show_separated <- !is.null(sep_field)
    
    # Map values to labels
    light_labels <- c("Daylight", "Twilight", "Darkness")
    
    if (show_separated) {
      # Group by light condition AND separation field
      df_grouped <- df %>%
        group_by_at(c("ULICHTVERH", sep_field)) %>%
        summarise(count = n(), .groups = 'drop')
      
      df_grouped$ULICHTVERH <- factor(df_grouped$ULICHTVERH, levels = c(0, 1, 2), labels = light_labels)
      df_grouped[[sep_field]] <- as.factor(df_grouped[[sep_field]])
      
      # Create the line chart with color by separation field
      plot_ly(df_grouped, x = ~ULICHTVERH, y = ~count, color = as.formula(paste0("~", sep_field)), 
              type = 'scatter', mode = 'lines+markers',
              colors = "Set1", marker = list(size = 10), line = list(shape = "spline")) %>%
        layout(
          title = paste0("Accidents by Lighting Conditions and ", sep_field),
          xaxis = list(title = "Lighting Conditions", categoryorder = "array", categoryarray = light_labels),
          yaxis = list(title = "Number of Accidents"),
          hovermode = "closest",
          legend = list(title = list(text = sep_field))
        )
    } else {
      # Default behavior: group only by light condition
      df_grouped <- df %>%
        group_by(ULICHTVERH) %>%
        summarise(count = n(), .groups = 'drop')
      
      df_grouped$ULICHTVERH <- factor(df_grouped$ULICHTVERH, levels = c(0, 1, 2), labels = light_labels)
      
      # Create the line chart without separation
      plot_ly(df_grouped, x = ~ULICHTVERH, y = ~count, type = 'scatter', mode = 'lines+markers',
              line = list(color = 'rgb(54, 162, 235)', width = 2, shape = "spline"),
              marker = list(color = 'rgb(54, 162, 235)', size = 10)) %>%
        layout(
          title = "Accidents by Lighting Conditions",
          xaxis = list(title = "Lighting Conditions", categoryorder = "array", categoryarray = light_labels),
          yaxis = list(title = "Number of Accidents"),
          hovermode = "closest"
        )
    }
  })
  
  
  
  output$weekdayPlot <- renderPlotly({
    df <- data_filtered()
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    
    
    # Get separation field if multiple selections exist
    sep_field <- getSeparationField()
    show_separated <- !is.null(sep_field)
    
    # Map numeric weekday values to their respective names
    weekday_labels <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    
    if (show_separated) {
      # Group by weekday AND separation field
      df_grouped <- df %>%
        group_by_at(c("UWOCHENTAG", sep_field)) %>%
        summarise(count = n(), .groups = 'drop')
      
      df_grouped$UWOCHENTAG <- factor(df_grouped$UWOCHENTAG, levels = 1:7, labels = weekday_labels)
      df_grouped[[sep_field]] <- as.factor(df_grouped[[sep_field]])
      
      # Create the line chart with color by separation field
      plot_ly(df_grouped, x = ~UWOCHENTAG, y = ~count, color = as.formula(paste0("~", sep_field)), 
              type = 'scatter', mode = 'lines+markers',
              colors = "Set1", marker = list(size = 10), line = list(shape = "spline")) %>%
        layout(
          title = paste0("Number of Accidents by Weekday and ", sep_field),
          xaxis = list(
            title = "Weekday", 
            categoryorder = "array", 
            categoryarray = weekday_labels
          ),
          yaxis = list(title = "Number of Accidents"),
          hovermode = "closest",
          legend = list(title = list(text = sep_field))
        )
    } else {
      # Default behavior: group only by weekday
      df_grouped <- df %>%
        group_by(UWOCHENTAG) %>%
        summarise(count = n(), .groups = 'drop')
      
      df_grouped$UWOCHENTAG <- factor(df_grouped$UWOCHENTAG, levels = 1:7, labels = weekday_labels)
      
      # Create the line chart without separation
      plot_ly(df_grouped, x = ~UWOCHENTAG, y = ~count, type = 'scatter', mode = 'lines+markers',
              line = list(color = 'rgb(75, 192, 192)', width = 2, shape = "spline"),
              marker = list(color = 'rgb(75, 192, 192)', size = 10)) %>%
        layout(
          title = "Number of Accidents by Weekday",
          xaxis = list(
            title = "Weekday", 
            categoryorder = "array", 
            categoryarray = weekday_labels
          ),
          yaxis = list(title = "Number of Accidents"),
          hovermode = "closest"
        )
    }
  })
  

  
}


# Run the application
shinyApp(ui = ui, server = server)