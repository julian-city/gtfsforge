#gtfsforge prototype 2

#version : 2.01

#DATE : 2025 July 27

#Improvements since version 2.0 : City selection and more dynamic agency details
#Improved colours

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)     
library(purrr)     
library(stringr)
library(shinyjs)
library(tibble)  
library(lubridate)
library(jsonlite)
library(data.table)
library(gtfstools)
library(DT)
library(osrm)

#UI-----------------------------

# UI Definition
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # CSS and JavaScript in the head
  tags$head(
    tags$style(
    HTML(
      "
      /* Light mode (default) */
      :root {
        --bg-color: #ffffff;
        --text-color: #333333;
        --panel-bg: #f5f5f5;
        --input-bg: #ffffff;
        --border-color: #dddddd;
        --hover-color: #e9ecef;
        --btn-default-bg: #e9ecef;
        --btn-default-color: #333333;
      }

      /* Dark mode */
      :root[data-bs-theme='dark'] {
        --bg-color: #1a1a1a;
        --text-color: #ffffff;
        --panel-bg: #2d2d2d;
        --input-bg: #3d3d3d;
        --border-color: #404040;
        --hover-color: #404040;
        --btn-default-bg: #404040;
        --btn-default-color: #ffffff;
      }

      /* Apply variables */
      body {
        background-color: var(--bg-color);
        color: var(--text-color);
      }

      .well, .panel {
        background-color: var(--panel-bg);
        border-color: var(--border-color);
      }

      .form-control {
        background-color: var(--input-bg);
        color: var(--text-color);
        border-color: var(--border-color);
      }

      .form-control:focus {
        background-color: var(--input-bg);
        color: var(--text-color);
      }

      .btn-default {
        background-color: var(--btn-default-bg);
        color: var(--btn-default-color);
        border-color: var(--border-color);
      }

      .dataTables_wrapper {
        color: var(--text-color);
      }

      .dataTable {
        color: var(--text-color);
        background-color: var(--panel-bg);
      }

      .dataTable tbody tr {
        background-color: var(--panel-bg) !important;
        color: var(--text-color) !important;
      }

      .dataTable tbody tr:hover {
        background-color: var(--hover-color) !important;
      }

      .navbar {
        background-color: var(--panel-bg);
        border-color: var(--border-color);
      }

      .navbar-default .navbar-nav > li > a {
        color: var(--text-color);
      }

      .navbar-default .navbar-nav > .active > a {
        background-color: var(--hover-color);
        color: var(--text-color);
      }

      /* Loading indicator styles */
    #loading-content {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      display: none;
      background-color: rgba(0, 0, 0, 0.5);
      z-index: 10000;
      display: flex;
      justify-content: center;
      align-items: center;
    }

    .loading-spinner {
      width: 50px;
      height: 50px;
      border: 5px solid #f3f3f3;
      border-top: 5px solid #3498db;
      border-radius: 50%;
      animation: spin 1s linear infinite;
    }

    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
    "
    )
  ),tags$script(HTML("
    function toggleTheme() {
      const root = document.documentElement;
      const currentTheme = root.getAttribute('data-bs-theme');
      const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
      root.setAttribute('data-bs-theme', newTheme);
      
      // Store the preference
      localStorage.setItem('theme', newTheme);
      
      // Update button text
      const btn = document.getElementById('theme-toggle');
      btn.innerHTML = newTheme === 'dark' ? '☀️ Light Mode' : '🌙 Dark Mode';
    }

    // Set initial theme from stored preference
    document.addEventListener('DOMContentLoaded', function() {
      const storedTheme = localStorage.getItem('theme') || 'light';
      document.documentElement.setAttribute('data-bs-theme', storedTheme);
      const btn = document.getElementById('theme-toggle');
      btn.innerHTML = storedTheme === 'dark' ? '☀️ Light Mode' : '🌙 Dark Mode';
      
      // Add right-click handling for the routes map (add this part)
      $(document).on('contextmenu', '#routes_map', function(e) {
        e.preventDefault();
        return false;
      });
    });

    // Loading indicator JavaScript with delay
    var loadingTimeout;
    
    $(document).on('shiny:busy', function() {
      // Only show the loading indicator if the app stays busy for more than 1 second
      loadingTimeout = setTimeout(function() {
        $('#loading-content').show();
      }, 1000); // 1000ms = 1 second delay
    });
    
    $(document).on('shiny:idle', function() {
      // Clear the timeout if the app becomes idle before the delay expires
      clearTimeout(loadingTimeout);
      // Hide the loading indicator
      $('#loading-content').hide();
    });
    
  // City selection functions
  function selectCity(cityName) {
    Shiny.setInputValue('selected_city_name', cityName);
  }
  
  // Custom message handlers for suggestions
  Shiny.addCustomMessageHandler('showSuggestions', function(html) {
    $('#city_suggestions').html(html).show();
  });
  
  Shiny.addCustomMessageHandler('hideSuggestions', function(message) {
    $('#city_suggestions').hide();
  });
  
  // Hide suggestions when clicking outside
  $(document).on('click', function(e) {
    if (!$(e.target).closest('#city_suggestions, #city_search').length) {
      $('#city_suggestions').hide();
    }
  });
  "))
  ),
  
  #loading indicator div
  div(id = "loading-content", div(class = "loading-spinner")),
  
  #Module architecture
  navbarPage(
    title = "gtfsforge",
    tags$script(
      "
    $(document).on('keydown', function(e) {
      if (e.key === 'Backspace') {
        Shiny.setInputValue('backspace_pressed', Math.random());
      }
    });
  "
    ),
    # div for the theme toggle
    header = div(
      style = "position: absolute; right: 10px; top: 10px; z-index: 1000;",
      tags$button(
        id = "theme-toggle",
        onclick = "toggleTheme()",
        class = "btn btn-default btn-sm",
        "🌙 Dark Mode"
      )
    ),
    
    #home tab
    tabPanel(
      tags$span(HTML("&#127968;")),
      #unicode house emoji
      fluidPage(
        titlePanel("Create your own gtfs"),
        
        # File upload section
        wellPanel(
          h3("Load your gtfsforge project"),
          p(
            "To continue working on a previous gtfsforge project, upload your .rds file:"
          ),
          fileInput(
            "load_ssfs",
            "",
            multiple = FALSE,
            accept = ".rds",
            placeholder = "Drag and drop or click to select file"
          ),
          tags$small(
            "Upload a transit model .rds file previously created with gtfsforge"
          )
        ),
        
        # Upload sample transit systems
        wellPanel(
          h3("Load a sample transit network"),
          p(
            "To explore this tool, you can get started by loading a sample based on Montreal's transit network. The Ligne Jaune model is the simplest and will help you familiarize yourself with how gtfsforge works."
          ),
          actionButton("load_yellowline_ssfs", "STM Ligne Jaune", class = "btn-success"),
          actionButton("load_metro_ssfs", "STM Metro", class = "btn-success"),
          actionButton("load_mileend_ssfs", "STM Mile-End bus network", class =
                         "btn-success"),
          actionButton("load_shbest_ssfs","STM and exo (Repentigny) stops", class = "btn-success")
        ),
        
        # Instructions
        wellPanel(
          h3("Create your own gtfs"),
          p("Build your transit system model by following these steps:"),
          
          h4("1. Create the stops of your transit system"),
          p(
            "In the 'stops' module:",
            tags$ul(
              tags$li("Click on the map to add stops"),
              tags$li("Provide unique stop IDs and stop names for each stop"),
              tags$li("Edit the location and details for existing stops"),
              tags$li(
                "Limitation : for now, it is not possible to delete stops once they have been created"
              )
            )
          ),
          
          h4("2. Create your routes"),
          p(
            "In the 'routes' module:",
            tags$ul(
              tags$li(
                "Create and edit route variants. A route variant corresponds to a unique stop pattern for trips in your transit network (e.g. a simple transit line will have a route variant for one direction and another for the opposite). Each route variant is associated with one shape."
              ),
              tags$li(
                "Create and edit route geometries by selecting stops in the desired order and by creating waypoints by clicking on the map. You may delete waypoints or remove stops from a route variant by right-clicking."
              ),
              tags$li(
                "Toggle between network and simple drawing modes. Network drawing mode calculates the path along the Open Street Maps road network between stops and waypoints."
              )
            )
          ),
        
          h4("3. Define service patterns"),
          p("In the 'calendar' module:", tags$ul(
            tags$li(
              "Specify which days of the week each service operates, as well as the date ranges for each service patterns."
            ),
            tags$li(
              "The table in this module is identical to the calendar table in gtfs and is passed on directly."
            )
          )),
          
          h4("4. Configure service spans"),
          p("In 'spans' module:", tags$ul(
            tags$li("Define operating hours for each route / service combination.")
          )),
          
          h4("5. Specify headways and speeds by hour in the headways module."),
          p("In 'headways' module:", tags$ul(
            tags$li(
              "After configuring service spans, initialize the headways and speeds by hour table in this module, then edit details by route variant, service pattern and hour."
            ),
            tags$li(
              "To create service gaps during the service period (for example, for a route that only operates at peak hours), you can simply delete the rows from this table for the hours during which the route should not run."
            )
          )),
          
          h4("6. When finished, return Home to create and export your gtfs below !")
        ),
        
        #Select city
        wellPanel(
          h3("Select your city"),
          p("Choose a city to automatically set the map center and agency timezone:"),
          div(
            style = "position: relative;",
            textInput("city_search", "Search for a city", placeholder = "Type city name..."),
            div(id = "city_suggestions", 
                style = "position: absolute; z-index: 1000; background: white; border: 1px solid #ccc; 
                 max-height: 200px; overflow-y: auto; width: 100%; display: none;")
          ),
          actionButton("select_city", "Select City", class = "btn-info"),
          tags$br(),
          tags$small("This will update map centers and agency information")
        ),
        
        #agency details form
        wellPanel(
        h4("Agency details"),
        p("Edit before exporting your project to GTFS"),
        textInput("agency_id", "Agency ID", "STM"),
        textInput("agency_name", "Agency name", "Société de transport de Montréal"),
        textInput("agency_url","Agency URL","http://www.stm.info"),
        textInput("agency_timezone", "Agency timezone", "America/Montreal")
        ),
        
        # Export gtfs
        wellPanel(
          h3("export gtfs"),
          textInput("exportgtfs_filename", "Filename:", value = "gtfs.zip"),
          downloadButton("download_gtfs", "Download GTFS", class = "btn-primary")
        ),
        
        # Export raw ssfs
        wellPanel(
          h3("Export raw project file (ssfs)"),
          p(
            "If you want to export your project and continue working later, export .rds file:"
          ),
          textInput("exportssfs_filename", "Filename:", value = "transit_system.rds"),
          downloadButton("download_ssfs", "Download Transit System", class = "btn-primary"),
          tags$br(),
          tags$br(),
          tags$small(
            "Your transit system will be saved as an .rds file that you can reload later"
          )
        ),
        
        #Info
        wellPanel(
          h3("About"),
          p(HTML("gtfsforge is an app prototype for creating and editing GTFS transit data for rapid scenario modelling & general purposes. It was developed by <a href='https://julian.city' target='_blank'>Julian Villafuerte Diaz</a>.")),
          p("This is the second (2.0) version of this app prototype, deployed in July 2025. It is in active development. Please get in touch with your feedback and ideas for improvement !"),
          p(HTML("<a href='https://julian.city' target='_blank'>Get in touch</a>"))
        )
        
      )
    ),
    
    # Stops Tab
    tabPanel("stops", fluidPage(
      titlePanel("stops"),
      sidebarLayout(
        sidebarPanel(
          #selectInput("stops_sf_objects", "Load stops from environment", #useful in development
          #             choices = NULL),
          #actionButton("load_stops_sf", "Load selected stops"),
          helpText("Click on the map to add a new stop or click an existing stop to edit it."),
          uiOutput("editPanel")
          #hr(),
          #actionButton("commit_stops", "Save to Transit System"),
          #downloadButton("download_geojson", "Download Stops GeoJSON")
        ),
        mainPanel(leafletOutput("stops_map", height = "600px"))
      )
    )),
    
    # Routes Tab
    tabPanel("routes", fluidPage(
      titlePanel("routes"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          helpText("First select or create a route variant, then click on stops to build your route sequence and start drawing your route. Click elsewhere on the map or along the existing route path to create waypoints. Click on an existing waypoint to select it for movement, and click on the desired location to move it."),
          
          # Route variant selection/creation
          selectInput("existing_rvar", "Edit existing route variant", choices = NULL),
          actionButton("load_rvar", "Load selected variant", class = "btn-info"),
          hr(),
          
          # Route details form
          h4("Route details"),
          textInput("rvar_id", "Route variant ID", ""),
          textInput("route_id", "Route ID", ""),
          textInput("route_long_name", "Route name", ""),
          selectInput(
            "direction_id",
            "Direction ID",
            choices = c("0" = 0, "1" = 1),
            selected = "0"
          ),
          textInput("trip_headsign", "Trip headsign", ""),
          selectInput(
            "route_type",
            "Route type",
            choices = c(
              "Bus" = 3,
              "Tram" = 0,
              "Metro" = 1,
              "Rail" = 2,
              "Ferry" = 4,
              "Cable tram" = 5,
              "Gondola" = 6,
              "Funicular" = 7,
              "Trolleybus" = 11,
              "Monorail" = 12
            ),
            selected = "Bus"
          ),
          hr(),
          
          # Drawing mode toggle
          h4("Drawing Mode"),
          radioButtons("drawing_mode", "Select drawing mode:", 
                       choices = c("Road Network" = "network", "Free Drawing" = "free"),
                       selected = "network"),
          
          helpText("Road Network mode will generate paths along streets. Free Drawing mode allows manual drawing."),
          hr(),
          
          # Action buttons
          actionButton("save_route", "Save route", class = "btn-success"),
          actionButton("clear_all", "Clear all", class = "btn-warning"),
          actionButton("remove_last_point", "Remove last point", class = "btn-default")
        ),
        mainPanel(
          # Map and stop sequence table
          fluidRow(
            column(8, 
                   leafletOutput("routes_map", height = "600px"),
                   tags$div(id = "map_instructions", class = "alert alert-info", style = "display: none;", 
                            "Please enter a Route Variant ID first before clicking on the map.")
            ),
            column(4,
                   h4("Stop Sequence"),
                   DTOutput("selected_stops_table"),
                   tags$div(id = "edit_point_instructions", style = "margin-top: 10px;", 
                            tags$ul(
                              tags$li("Click on a stop to add it to your sequence"),
                              tags$li("Right-click on a stop to remove it")
                            ))
            )
          )
        )
      )
    )),
    
    #calendar tab
    tabPanel("calendar", fluidPage(
      titlePanel("calendar"),
      sidebarLayout(
        sidebarPanel(
          textInput("service_id", "Service ID", placeholder = "Enter service ID"),
          
          h4("Days of Operation"),
          fluidRow(
            column(
              6,
              selectInput(
                "monday",
                "Monday",
                choices = c("No" = 0, "Yes" = 1),
                selected = 0
              ),
              selectInput(
                "tuesday",
                "Tuesday",
                choices = c("No" = 0, "Yes" = 1),
                selected = 0
              ),
              selectInput(
                "wednesday",
                "Wednesday",
                choices = c("No" = 0, "Yes" = 1),
                selected = 0
              ),
              selectInput(
                "thursday",
                "Thursday",
                choices = c("No" = 0, "Yes" = 1),
                selected = 0
              )
            ),
            column(
              6,
              selectInput(
                "friday",
                "Friday",
                choices = c("No" = 0, "Yes" = 1),
                selected = 0
              ),
              selectInput(
                "saturday",
                "Saturday",
                choices = c("No" = 0, "Yes" = 1),
                selected = 0
              ),
              selectInput(
                "sunday",
                "Sunday",
                choices = c("No" = 0, "Yes" = 1),
                selected = 0
              )
            )
          ),
          
          h4("Service Period"),
          dateInput(
            "start_date",
            "Start date",
            value = "2000-01-01",
            min = "1970-01-01",
            max = "2099-12-31",
            format = "yyyy-mm-dd"
          ),
          dateInput(
            "end_date",
            "End Date",
            value = "2099-12-31",
            min = "1970-01-01",
            max = "2099-12-31",
            format = "yyyy-mm-dd"
          ),
          
          actionButton("add_service", "Add service", class = "btn-success"),
          actionButton("clear_service", "Clear form", class = "btn-warning")
        ),
        mainPanel(
          DTOutput("calendar_table"),
          actionButton("delete_selected_service", "Delete selected service", class = "btn-danger")
        )
      )
    )),
    
    #spans tab
    tabPanel("spans", fluidPage(
      titlePanel("spans"),
      sidebarLayout(
        sidebarPanel(
          selectInput("span_rvar_id", "Route variant", choices = NULL),
          selectInput("span_service_id", "Service ID", choices = NULL),
          
          h4("Service Times"),
          textInput(
            "first_dep",
            "First departure",
            value = "05:00:00",
            placeholder = "HH:MM:SS"
          ),
          textInput(
            "last_dep",
            "Last departure",
            value = "23:00:00",
            placeholder = "HH:MM:SS"
          ),
          
          actionButton("add_span", "Add service span", class = "btn-success"),
          actionButton("clear_span", "Clear form", class = "btn-warning"),
          hr(),
          actionButton("delete_selected_span", "Delete selected row", class = "btn-danger")
        ),
        mainPanel(DTOutput("spans_table"))
      )
    )),
    
    #headways hsh tab
    tabPanel("headways", fluidPage(
      titlePanel("headways & speeds by hour"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          numericInput(
            "default_headway",
            "Default headway (minutes)",
            value = 12,
            min = 1,
            max = 60
          ),
          actionButton("initialize_hsh", "Initialize / reset table", class = "btn-danger"),
          helpText(
            "After spans have been defined, initialize your headways table. WARNING : if there is data in the headways table, clicking this button will overwrite it. Use the drop downs below to pull up the headway tables already in your model."
          ),
          hr(),
          selectInput("hsh_rvar_id", "Route variant", choices = NULL),
          selectInput("hsh_service_id", "Service ID", choices = NULL),
          hr(),
          # Form for adding/editing rows
          conditionalPanel(
            condition = "output.editing_hsh == true",
            h4("Edit Row"),
            textInput("edit_hour_dep", "Hour (HH:00:00)", ""),
            numericInput(
              "edit_headway",
              "Headway (minutes)",
              value = 12,
              min = 1,
              max = 60
            ),
            numericInput(
              "edit_speed",
              "Speed (km/h)",
              value = 20,
              min = 5,
              max = 431
            ),
            actionButton("save_hsh_edit", "Save changes", class = "btn-success"),
            actionButton("cancel_hsh_edit", "Cancel", class = "btn-warning")
          ),
          conditionalPanel(
            condition = "output.editing_hsh == false",
            actionButton("edit_hsh_row", "Edit selected row", class = "btn-info"),
            actionButton("add_hsh_row", "Add new row", class = "btn-success"),
            actionButton("delete_selected_hsh", "Delete selected", class = "btn-danger")
          )
        ),
        mainPanel(DTOutput("hsh_table"))
      )
    ))
  )
)

#SERVER-------------------------

server <- function(input, output, session) {
  
  #   #   #
  #
  #   SHARED FUNCTIONS AND REACTIVE VALUES
  #
  #   #   #
  
  ssfs_to_gtfs <- function(ssfs,
                           agency_id = "STM",
                           agency_name = "Société de transport de Montréal",
                           agency_url = "http://www.stm.info",
                           agency_timezone = "America/Montreal") {
    #ssfs input must be of class "list" and include the following :
    
    #ssfs$stops : a points sf with vectors stop_id(chr),stop_name(chr),and geometry(POINT)
    
    #ssfs$shapes : a lines sf with vectors shape_id(chr), and geometry(LINESTRING)
    
    #ssfs$rvar : a tibble with vectors rvar_id(chr),route_id(chr),direction_id(int),
    #trip_headsign (chr), shape_id(chr), agency_id(chr), route_short_name(chr), and
    #route_long_name(chr)
    
    #ssfs$stop_seq : a tibble with vectors rvar_id(chr), stop_id(chr), stop_sequence(int),
    #and speed_factor(dbl)
    
    #ssfs$calendar : a tibble with vectors service_id(chr),monday(dbl),tuesday(dbl),
    #wednesday(dbl),thursday(dbl),friday(dbl),saturday(dbl),start_date(date),end_date(date)
    
    #ssfs$span : a tibble with vectors rvar_id(chr), service_id(chr), first_dep(chr),
    #last_dep(chr)
    
    #ssfs$hsh : a tibble with vectors rvar_id (chr), service_id (chr), hour_dep (chr),
    #headway (dbl), speed(dbl)
    
    agency <- tibble(
      agency_id = agency_id,
      agency_name = agency_name,
      agency_url = agency_url,
      agency_timezone = agency_timezone
    )
    
    #ROUTES-----------
    
    routes <-
      ssfs$rvar %>%
      select(route_id,
             agency_id,
             route_short_name,
             route_long_name,
             route_type) %>%
      distinct()
    
    #TRIPS and trip start times-----------
    
    #initialize trip ids
    
    trips <-
      tibble(
        rvar_id = as.character(),
        trip_id = as.character(),
        route_id = as.character(),
        service_id = as.character(),
        trip_headsign = as.character(),
        direction_id = as.integer(),
        shape_id = as.character(),
        trip_dep = as.character()
      )
    
    for (i in 1:nrow(ssfs$span)) {
      rvar_id_i <- ssfs$span[i, ]$rvar_id
      
      service_id_i <- ssfs$span[i, ]$service_id
      
      route_id_i <-
        ssfs$rvar %>%
        filter(rvar_id == rvar_id_i) %>%
        pull(route_id)
      
      cat("\rCalculating trips for route", route_id_i)
      
      trip_headsign_i <-
        ssfs$rvar %>%
        filter(rvar_id == rvar_id_i) %>%
        pull(trip_headsign)
      
      direction_id_i <-
        ssfs$rvar %>%
        filter(rvar_id == rvar_id_i) %>%
        pull(direction_id)
      
      shape_id_i <-
        ssfs$rvar %>%
        filter(rvar_id == rvar_id_i) %>%
        pull(shape_id)
      
      first_dep <- ssfs$span[i, ]$first_dep
      
      last_dep <- ssfs$span[i, ]$last_dep
      
      headways <-
        ssfs$hsh %>%
        filter(rvar_id == rvar_id_i, service_id == service_id_i) %>%
        select(hour_dep, headway)
      
      #initialize the while loop to build out list of trips (departure times)
      trip_dep <- first_dep
      next_dep_duration <- as.duration(minutes(0)) #this refreshes the condition on the below loop
      
      while (next_dep_duration < as.duration(hms(last_dep))) {
        #print(trip_dep[length(trip_dep)]) #useful for debugging
        #takes the last / latest departure in the vector of departures trip_dep
        prev_dep <- as.duration(hms(trip_dep[length(trip_dep)]))
        #identify the hour of departure of this trip
        hour_prev_dep <- sprintf("%02d:00:00", as.numeric(floor(as.numeric(prev_dep) / 3600)))
        #identify based on the ssfs what the headway is at this hour
        headway <- headways %>% filter(hour_dep == hour_prev_dep) %>% pull(headway)
        
        #IF there is no headway value associated with the hour of the previous departure
        #AND there is no hour specified in the headways table beyond the hour of the previous departure
        #THEN end the loop
        #ELSE IF no headway value associated with the hour of the previous departure
        #AND there is an hour that is specified in the headways table beyond the hour of the previous departure
        #THEN set the next_dep_duration to that hour
        #ELSE calculate the next departure based on the headway and the previous hour
        
        if (is.na(headway) &
            all(as.duration(hms(hour_prev_dep)) >= as.duration(hms(headways$hour_dep)))) {
          break
        } else if (is.na(headway)) {
          length_hours_prior <- #index of the TRUE value furthest along the result of this logical statement
            max(which(as.duration(hms(
              hour_prev_dep
            )) >= as.duration(hms(
              headways$hour_dep
            ))))
          next_dep_duration <- as.duration(hms(headways$hour_dep[length_hours_prior +
                                                                   1]))
        } else{
          #determine the time of the next departure, encoded as duration
          next_dep_duration <- prev_dep + as.duration(seconds(headway * 60))
          #the duration coding enables us to write departure times beyond 24:00:00 and to
          #set the condition that ends this while loop
          #identify what the hour of the subsequent departure would be
          hour_next_dep <- sprintf("%02d:00:00", as.numeric(floor(
            as.numeric(next_dep_duration) / 3600
          )))
          
          #If that hour is NOT within the list of hours specified in the headways table
          #AND there is no hour beyond the that one listed
          #THEN break the loop
          #ELSE IF that hour is NOT within the list of hours specified in the headways table
          #AND there is a subsequent hour listed in the headways table
          #THEN overwrite next_dep_duration to that hour
          
          if (!hour_next_dep %in% headways$hour_dep &
              all(as.duration(hms(hour_next_dep)) > as.duration(hms(headways$hour_dep)))) {
            break
          } else if (!hour_next_dep %in% headways$hour_dep) {
            length_hours_prior <- #index of the TRUE value furthest along the result of this logical statement
              max(which(as.duration(hms(
                hour_next_dep
              )) >= as.duration(hms(
                headways$hour_dep
              ))))
            next_dep_duration <- as.duration(hms(headways$hour_dep[length_hours_prior +
                                                                     1]))
          }
        }
        
        #hours minutes days calculated separately to encode times up to 32:00:00
        next_dep_h <- round(as.numeric(floor(
          as.numeric(next_dep_duration) / 3600
        )), 0) #REMOVED the %% that was here previously
        next_dep_m <- round(as.numeric(floor(as.numeric(
          next_dep_duration
        ) / 60)) %% 60, 0)
        next_dep_s <- round(as.numeric(next_dep_duration) %% 60, 0)
        
        next_dep <- sprintf("%02d:%02d:%02d", next_dep_h, next_dep_m, next_dep_s)
        
        trip_dep <- c(trip_dep, next_dep)
      }
      
      #build out trip ids
      
      trips_i <-
        tibble(
          rvar_id = rvar_id_i,
          #will need to remove this later, am leaving it in for development
          route_id = route_id_i,
          service_id = service_id_i,
          trip_headsign = trip_headsign_i,
          direction_id = direction_id_i,
          shape_id = shape_id_i,
          trip_dep = trip_dep
        )
      
      trips_i <-
        trips_i %>%
        mutate(trip_id = row_number(), .before = route_id) %>%
        mutate(trip_id = sprintf("%04d", trip_id)) %>% #to have all trip ids the same length... might be pertinent?
        mutate(trip_id = str_c(rvar_id, "_", service_id, "_", trip_id))
      
      trips <- bind_rows(trips, trips_i)
      
    }
    
    #STOP TIMES-----------------------
    
    #calculate interstop distances
    
    stop_seq <-
      ssfs$stop_seq %>%
      left_join(ssfs$rvar %>% select(rvar_id, shape_id), by = "rvar_id")
    
    shapes_points <-
      ssfs$shapes %>%
      st_cast("POINT")
    
    #initialize
    
    stop_seq$interstop_dist <- NA
    
    for (i in 1:(nrow(stop_seq) - 1)) {
      cat("\rCalculating interstop distance",
          i,
          "of",
          nrow(stop_seq) - 1)
      #CONDITIONS
      #next stop needs to be part of the same sequence AND
      #part of the same rvar_id (just another way of verifying the same stop sequence)
      #ELSE the NA assignment remains
      
      if ((stop_seq$stop_sequence[i] + 1 == stop_seq$stop_sequence[i + 1]) &
          (stop_seq$rvar_id[i] == stop_seq$rvar_id[i + 1])) {
        rvar_id_i <- stop_seq$rvar_id[i]
        
        shape_id_i <- stop_seq$shape_id[i]
        
        #shapes points for only the shape_id associated with the rvar_id associated with stop i
        shapes_points_i <-
          shapes_points %>%
          filter(shape_id == shape_id_i)
        
        current_stop_id <- stop_seq$stop_id[i]
        next_stop_id <- stop_seq$stop_id[i + 1]
        
        current_stop <-
          ssfs$stops %>%
          filter(stop_id == current_stop_id)
        
        next_stop <-
          ssfs$stops %>%
          filter(stop_id == next_stop_id)
        
        #nearest points along shapes_points to current and next stops
        
        interstop_segment_points <-
          shapes_points_i[st_nearest_feature(current_stop, shapes_points_i):st_nearest_feature(next_stop, shapes_points_i), ]
        
        interstop_dist_i <-
          as.numeric(
            interstop_segment_points %>%
              summarise(do_union = FALSE) %>% #do_union retains the order of the points
              st_cast("LINESTRING") %>%
              st_length()
          )
        
        stop_seq$interstop_dist[i] <- interstop_dist_i
      } else{
        stop_seq$interstop_dist[i] <- NA
      }
    }
    
    #write stop times
    
    #initialize stop times
    
    stop_times <-
      tibble(
        stop_id = as.character(),
        departure_time = as.character(),
        trip_id = as.character(),
        stop_sequence = as.integer()
      )
    
    #use $span for the loop as each row represents a unique rvar_id * service id combo
    
    for (i in 1:length(ssfs$span$rvar_id)) {
      rvar_id_i <- ssfs$span[i, ]$rvar_id
      
      service_id_i <- ssfs$span[i, ]$service_id
      
      cat(
        "\rCalculating stop times for rvar_id",
        rvar_id_i,
        "and service_id",
        service_id_i,
        "(",
        i,
        "of",
        length(ssfs$span$rvar_id),
        ")"
      )
      
      #hsh for rvar_id and service_id combo
      
      hsh_i <-
        ssfs$hsh %>%
        filter(rvar_id == rvar_id_i, service_id == service_id_i) %>%
        select(hour_dep, headway, speed)
      
      #identify the trips
      
      trips_i <-
        trips %>%
        filter(rvar_id == rvar_id_i, service_id == service_id_i) %>%
        select(trip_id, trip_dep)
      
      #establish template for stop_times based on stop_seq
      
      stop_times_template <-
        stop_seq %>%
        filter(rvar_id == rvar_id_i) %>%
        select(stop_id, stop_sequence, speed_factor, interstop_dist)
      
      for (i in 1:nrow(trips_i)) {
        trip_id_i <- trips_i[i, ]$trip_id
        
        trip_dep_i <- trips_i[i, ]$trip_dep
        
        trip_dep_dur <- as.duration(hms(trip_dep_i))
        
        stop_times_i <- stop_times_template
        
        stop_times_i$departure_time <- NA
        
        stop_times_i$trip_id <- trip_id_i
        
        #set speed for the trip based on initial departure time
        hour_dep_i <- sprintf("%02d:00:00", as.numeric(floor(as.numeric(trip_dep_dur) / 3600)))
        
        #determine what the commercial speed is for that hour, based on the hsh table
        speed_i <- hsh_i %>% filter(hour_dep == hour_dep_i) %>% pull(speed)
        
        stop_times_i$departure_time[1] <- trip_dep_i
        #NB IN BRACKETS IS ALWAYS 1 NEVER i because it's for initializing
        
        #print(trip_id_i) #useful for debugging
        
        for (i in 2:nrow(stop_times_i)) {
          # Convert previous departure time to POSIXct
          prev_dep <- as.duration(hms(stop_times_i$departure_time[i - 1]))
          #and the speed factor associated with the previous stop (within the template)
          speed_factor <- stop_times_i$speed_factor[i - 1]
          #adjust the speed based on the speed factor
          speed <- speed_i * speed_factor
          #speed in meters per second
          speed_ms <- speed * (1000 / 3600)
          
          dist_to_next_stop <- stop_times_i$interstop_dist[i - 1]
          
          current_dep_dur <- prev_dep + as.duration(seconds(dist_to_next_stop /
                                                              speed_ms))
          
          current_dep_h <- as.numeric(floor(as.numeric(current_dep_dur) / 3600)) #REMOVED the %% that was here previously
          current_dep_m <- as.numeric(floor(as.numeric(current_dep_dur) / 60)) %% 60
          current_dep_s <- round(as.numeric(floor(
            as.numeric(current_dep_dur) %% 60
          )), 0) #necessary to add rounding to have sprintf work
          
          # Convert current departure time to "hh:mm:ss" format
          stop_times_i$departure_time[i] <- sprintf("%02d:%02d:%02d",
                                                    current_dep_h,
                                                    current_dep_m,
                                                    current_dep_s)
        }
        
        stop_times_i <-
          stop_times_i %>%
          select(trip_id, departure_time, stop_id, stop_sequence)
        
        stop_times <-
          bind_rows(stop_times, stop_times_i)
        
      }
    }
    
    #modifications to gtfs_to_ssfs:
    
    stop_times <-
      stop_times %>%
      mutate(arrival_time = departure_time) %>%
      select(trip_id,
             arrival_time,
             departure_time,
             stop_id,
             stop_sequence)
    
    #SHAPES---------
    
    shapes <-
      ssfs$shapes %>%
      st_cast("POINT") %>%
      mutate(
        coords = st_coordinates(geometry),
        shape_pt_lat = coords[, "Y"],
        shape_pt_lon = coords[, "X"]
      ) %>%
      as_tibble() %>%
      select(shape_id, shape_pt_lat, shape_pt_lon) %>%
      group_by(shape_id) %>%
      mutate(shape_pt_sequence = row_number(), .before = shape_pt_lat) %>%
      ungroup()
    
    #STOPS----------
    
    stops <-
      ssfs$stops %>%
      mutate(
        coords = st_coordinates(geometry),
        stop_lat = coords[, "Y"],
        stop_lon = coords[, "X"]
      ) %>%
      as_tibble() %>%
      select(stop_id, stop_name, stop_lat, stop_lon)
    
    #compile gtfs---------
    
    #modify trips to drop the rvar_id and trip_dep columns
    
    trips <-
      trips %>%
      select(-c(rvar_id, trip_dep))
    
    #write the gtfs with data tables
    
    gtfs <- list(
      agency = as.data.table(agency),
      calendar = as.data.table(ssfs$calendar),
      routes = as.data.table(routes),
      shapes = as.data.table(shapes),
      stop_times = as.data.table(stop_times),
      stops = as.data.table(stops),
      trips = as.data.table(trips)
    )
    
    class(gtfs) <- c("gtfs", "dt_gtfs", class(gtfs))
    
    return(gtfs)
  }
  
  # Initialize ssfs : data structure for the whole app
  ssfs <- reactiveVal(
    list(
      stops = st_sf(
        stop_id = character(),
        stop_name = character(),
        geometry = st_sfc(crs = 4326),
        stringsAsFactors = FALSE
      ),
      shapes = st_sf(
        rvar_id = character(),
        geometry = st_sfc(crs = 4326),
        stringsAsFactors = FALSE
      ),
      rvar = data.frame(
        rvar_id = character(),
        route_id = character(),
        direction_id = integer(),
        trip_headsign = character(),
        route_long_name=character(),
        route_type=integer(),
        stringsAsFactors = FALSE
      ),
      stop_seq = data.frame(
        rvar_id = character(),
        stop_id = character(),
        stop_sequence = integer(),
        stop_name = character(),
        stringsAsFactors = FALSE
      ),
      calendar = data.frame(
        service_id = character(),
        monday = integer(),
        tuesday = integer(),
        wednesday = integer(),
        thursday = integer(),
        friday = integer(),
        saturday = integer(),
        sunday = integer(),
        start_date = character(),
        end_date = character(),
        stringsAsFactors = FALSE
      ),
      span = data.frame(
        rvar_id = character(),
        service_id = character(),
        first_dep = character(),
        last_dep = character(),
        stringsAsFactors = FALSE
      ),
      hsh = data.frame(
        rvar_id = character(),
        service_id = character(),
        hour_dep = character(),
        headway = double(),
        speed = double(),
        stringsAsFactors = FALSE
      )
    )
  )
  
  #reactive values for cities db and agency info on home page / in gtfs
  
  # Load cities data from GitHub
  cities_data <- reactiveVal(NULL)
  
  # Reactive values for map center and agency info
  map_center <- reactiveVal(list(lng = -73.567, lat = 45.5017)) # Montreal default
  agency_info <- reactiveVal(list(
    agency_id = "STM",
    agency_name = "Société de transport de Montréal", 
    agency_url = "http://www.stm.info",
    agency_timezone = "America/Montreal"
  ))
  
  # Filtered cities for autocomplete
  filtered_cities <- reactiveVal(data.frame())
  
  # Load cities data from GitHub on app startup
  observe({
    tryCatch({
      url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/cities_db.rds"
      
      # Create a temporary file
      temp_file <- tempfile(fileext = ".rds")
      
      # Download the file
      download.file(url, temp_file, mode = "wb")
      
      # Load the file
      cities_df <- readRDS(temp_file)
      
      # Clean up
      unlink(temp_file)
      
      cities_data(cities_df)
      showNotification("Cities database loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading cities database:", e$message), type = "warning")
      # Create empty fallback
      cities_data(data.frame(name = character(), lat = numeric(), long = numeric(), tz = character()))
    })
  })
  
  # Helper function for marker size calculation for stops on maps
  calculateMarkerSize <- function(zoom) {
    base_size <- 2
    adjusted_size <- base_size * (1.2 ^ (zoom - 10))
    return(min(max(adjusted_size, 4), 15))
  }
  
  #current zoom reactive value
  current_zoom <- reactiveVal(10)
  
  #function for adding base maps
  addBaseMaps <- function(map) {
    map %>%
      addProviderTiles("CartoDB.Positron", group = "Positron") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("OpenStreetMap.HOT", group = "OSM") %>%
      addLayersControl(
        baseGroups = c("Positron", "Satellite", "OSM"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # Function to update any map with current ssfs data
  updateMapWithSsfsData <- function(map_id, current_data, highlight_ids = NULL, show_stops = TRUE, show_shapes = TRUE) {
    proxy <- leafletProxy(map_id)
    
    # Clear all existing content
    proxy %>% 
      clearGroup("shapes") %>%
      clearGroup("stops") %>%
      clearMarkers()  # For backward compatibility
    
    # Add shapes first (as bottom layer)
    if(show_shapes && !is.null(current_data$shapes) && nrow(current_data$shapes) > 0) {
      for(i in 1:nrow(current_data$shapes)) {
        line_coords <- st_coordinates(current_data$shapes$geometry[i])
        proxy <- proxy %>%
          addPolylines(
            lng = line_coords[, 1],
            lat = line_coords[, 2],
            group = "shapes",
            color = "#05AEEF",
            weight = 2,
            opacity = 0.6
          )
      }
    }
    
    # Add stops (on top of shapes)
    if(show_stops && !is.null(current_data$stops) && nrow(current_data$stops) > 0) {
      # Calculate marker size based on current zoom
      marker_size <- calculateMarkerSize(current_zoom())
      
      # Determine colors based on highlight IDs if provided
      fill_colors <- if(!is.null(highlight_ids)) {
        ifelse(current_data$stops$stop_id %in% highlight_ids, "#B2182B", "#7f7f7f")
      } else {
        "#7f7f7f"
      }
      
      proxy <- proxy %>%
        addCircleMarkers(
          data = current_data$stops,
          radius = marker_size,
          color = "white",
          weight=1,
          stroke = TRUE,
          fillColor=fill_colors,
          fillOpacity = 0.7,
          layerId = ~stop_id,
          popup = ~paste("ID:", stop_id, "<br>Name:", stop_name),
          group = "stops"
        )
    }
    
    return(proxy)
  }
  
  #   #   #
  #
  ##   HOME MODULE-------
  #
  #   #   #
  
  # Handle file upload
  observeEvent(input$load_ssfs, {
    req(input$load_ssfs)
    tryCatch({
      loaded_ssfs <- readRDS(input$load_ssfs$datapath)
      ssfs(loaded_ssfs)
      showNotification("Transit system loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  #handle load_ligne_jaune_ssfs
  observeEvent(input$load_yellowline_ssfs, {
    tryCatch({
      # URL to your raw GitHub file
      url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/sample_networks/ligne_jaune.rds"
      
      # Create a temporary file
      temp_file <- tempfile(fileext = ".rds")
      
      # Download the file
      download.file(url, temp_file, mode = "wb")
      
      # Load the file
      ljaune_ssfs <- readRDS(temp_file)
      
      # Clean up
      unlink(temp_file)
      
      #adjust ssfs to new data structure (no shape_id)
      
      shape_to_rvar_id <- 
        ljaune_ssfs$rvar %>% 
        select(shape_id,rvar_id)
      
      ljaune_ssfs$rvar <- 
        ljaune_ssfs$rvar %>% 
        select(-shape_id)
      
      ljaune_ssfs$shapes <- 
        ljaune_ssfs$shapes %>% 
        left_join(shape_to_rvar_id,
                  by="shape_id") %>% 
        select(rvar_id,geometry)
      
      ssfs(ljaune_ssfs)
      
      showNotification("STM Ligne Jaune loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading STM Ligne Jaune:", e$message),
                       type = "error")
    })
  })
  
  #handle load_metro_ssfs
  observeEvent(input$load_metro_ssfs, {
    tryCatch({
      url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/sample_networks/metro.rds"
      
      # Create a temporary file
      temp_file <- tempfile(fileext = ".rds")
      
      # Download the file
      download.file(url, temp_file, mode = "wb")
      
      # Load the file
      mtlmetro_ssfs <- readRDS(temp_file)
      
      # Clean up
      unlink(temp_file)
      
      #adjust ssfs to new data structure (no shape_id)
      
      shape_to_rvar_id <- 
        mtlmetro_ssfs$rvar %>% 
        select(shape_id,rvar_id)
      
      mtlmetro_ssfs$rvar <- 
        mtlmetro_ssfs$rvar %>% 
        select(-shape_id)
      
      mtlmetro_ssfs$shapes <- 
        mtlmetro_ssfs$shapes %>% 
        left_join(shape_to_rvar_id,
                  by="shape_id") %>% 
        select(rvar_id,geometry)
      
      ssfs(mtlmetro_ssfs)
      
      showNotification("STM metro network loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading STM metro network:", e$message),
                       type = "error")
    })
  })
  
  #handle load_mileend_ssfs
  observeEvent(input$load_mileend_ssfs, {
    tryCatch({
      url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/sample_networks/mileend.rds"
      
      # Create a temporary file
      temp_file <- tempfile(fileext = ".rds")
      
      # Download the file
      download.file(url, temp_file, mode = "wb")
      
      # Load the file
      mtlmileend_ssfs <- readRDS(temp_file)
      
      # Clean up
      unlink(temp_file)
      
      #adjust ssfs to new data structure (no shape_id)
      
      shape_to_rvar_id <- 
        mtlmileend_ssfs$rvar %>% 
        select(shape_id,rvar_id)
      
      mtlmileend_ssfs$rvar <- 
        mtlmileend_ssfs$rvar %>% 
        select(-shape_id)
      
      mtlmileend_ssfs$shapes <- 
        mtlmileend_ssfs$shapes %>% 
        left_join(shape_to_rvar_id,
                  by="shape_id") %>% 
        select(rvar_id,geometry)
      
      ssfs(mtlmileend_ssfs)
      
      showNotification("STM Mile-End bus network loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading STM Mile-End bus network:", e$message),
                       type = "error")
    })
  })
  
  #handle load_shbest
  observeEvent(input$load_shbest_ssfs, {
    tryCatch({
      # URL to your raw GitHub file
      url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/sample_networks/shbest.rds"
      
      # Create a temporary file
      temp_file <- tempfile(fileext = ".rds")
      
      # Download the file
      download.file(url, temp_file, mode = "wb")
      
      # Load the file
      shbest_ssfs <- readRDS(temp_file)
      
      # Clean up
      unlink(temp_file)
      
      #update ssfs reactive value with shbest
      ssfs(shbest_ssfs)
      
      showNotification("STM and EXO (Repentigny) stops loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading STM and EXO stops", e$message),
                       type = "error")
    })
  })
  
  # City search autocomplete
  observeEvent(input$city_search, {
    if (!is.null(cities_data()) && nrow(cities_data()) > 0) {
      search_term <- input$city_search
      
      if (nchar(search_term) >= 2) {
        # Filter cities that match the search term (case insensitive)
        #matches <- cities_data()[grepl(search_term, cities_data()$name, ignore.case = TRUE), ]
        
        matches <- cities_data() %>%
          filter(str_detect(tolower(name),tolower(str_escape(search_term))))
        
        if (nrow(matches) > 0 && nrow(matches) <= 10) {
          # Show suggestions if we have 1-10 matches
          filtered_cities(matches)
          
          # Create suggestion HTML
          suggestions_html <- paste0(
            "<div style='padding: 5px; cursor: pointer; border-bottom: 1px solid #eee;' ",
            "onclick='selectCity(\"", matches$name, "\")'>",
            matches$name, "</div>",
            collapse = ""
          )
          
          # Show suggestions dropdown
          session$sendCustomMessage("showSuggestions", suggestions_html)
        } else if (nrow(matches) == 1) {
          # Exactly one match - hide suggestions
          session$sendCustomMessage("hideSuggestions", "")
          filtered_cities(matches)
        } else {
          # No matches or too many matches
          session$sendCustomMessage("hideSuggestions", "")
          filtered_cities(data.frame())
        }
      } else {
        # Search term too short
        session$sendCustomMessage("hideSuggestions", "")
        filtered_cities(data.frame())
      }
    }
  })
  
  # Handle city selection from dropdown
  observeEvent(input$selected_city_name, {
    updateTextInput(session, "city_search", value = input$selected_city_name)
    session$sendCustomMessage("hideSuggestions", "")
    #to force hiding suggestions
    filtered_cities(data.frame())
    
  })
  
  # Handle select city button
  observeEvent(input$select_city, {
    search_term <- input$city_search
    
    if (is.null(search_term) || search_term == "") {
      showNotification("Please enter a city name", type = "warning")
      return()
    }
    
    if (is.null(cities_data()) || nrow(cities_data()) == 0) {
      showNotification("Cities database not loaded", type = "error")
      return()
    }
    
    # Find exact matches (case insensitive)
    exact_matches <- cities_data()[tolower(cities_data()$name) == tolower(search_term), ]
    
    if (nrow(exact_matches) == 0) {
      showNotification("City not found. Please select from the suggestions.", type = "warning")
      return()
    } else if (nrow(exact_matches) > 1) {
      showNotification("Multiple cities found with that name. Please be more specific.", type = "warning")
      return()
    } else {
      # Exactly one match - update all values
      selected_city <- exact_matches[1, ]
      
      # Update map center
      map_center(list(lng = selected_city$long, lat = selected_city$lat))
      
      # Update agency info
      agency_info(list(
        agency_id = "LI",
        agency_name = "Lorem Ipsum",
        agency_url = "https://julian.city",
        agency_timezone = selected_city$tz
      ))
      
      # Update agency form inputs
      updateTextInput(session, "agency_id", value = "LI")
      updateTextInput(session, "agency_name", value = "Lorem Ipsum")
      updateTextInput(session, "agency_url", value = "https://julian.city")
      updateTextInput(session, "agency_timezone", value = selected_city$tz)
      
      # Hide suggestions
      session$sendCustomMessage("hideSuggestions", "")
      
      showNotification(paste("City set to:", selected_city$name), type = "message")
    }
  })
  
  # Handle ssfs download
  output$download_ssfs <- downloadHandler(
    filename = function() {
      if (!grepl("\\.rds$", input$exportssfs_filename)) {
        paste0(input$exportssfs_filename, ".rds")
      } else {
        input$exportssfs_filename
      }
    },
    content = function(file) {
      current_ssfs <- ssfs()
      saveRDS(current_ssfs, file)
    }
  )
  
  # Handle gtfs download
  output$download_gtfs <- downloadHandler(
    filename = function() {
      if (!grepl("\\.zip$", input$exportgtfs_filename)) {
        paste0(input$exportgtfs_filename, ".zip")
      } else {
        input$exportgtfs_filename
      }
    },
    content = function(file) {
      current_ssfs <- ssfs()
      agency <- agency_info()  # Use reactive agency info
      
      current_ssfs$rvar <-
        current_ssfs$rvar %>% as_tibble() %>%
        mutate(agency_id = agency$agency_id, .before = route_long_name) %>%
        mutate(route_short_name = route_id, .before = route_long_name) %>%
        mutate(shape_id = rvar_id) %>% 
        mutate(direction_id = as.integer(direction_id),
               route_type = as.integer(route_type))
      
      current_ssfs$shapes <- 
        current_ssfs$shapes %>% 
        rename(shape_id=rvar_id)
      
      current_ssfs$stop_seq <-
        current_ssfs$stop_seq %>% as_tibble() %>%
        select(-stop_name) %>%
        mutate(speed_factor = 1)
      
      unique_stop_ids <-
        current_ssfs$stop_seq$stop_id %>% unique()
      
      current_ssfs$stops <-
        current_ssfs$stops %>%
        filter(stop_id %in% unique_stop_ids)
      
      current_ssfs$calendar <-
        current_ssfs$calendar %>% as_tibble() %>%
        mutate(start_date = as.Date(start_date),
               end_date = as.Date(end_date))
      
      current_ssfs$span <-
        current_ssfs$span %>% as_tibble()
      
      current_ssfs$hsh <-
        current_ssfs$hsh %>% as_tibble()
      
      current_gtfs <- ssfs_to_gtfs(current_ssfs,
                                   agency_id = agency$agency_id,
                                   agency_name = agency$agency_name,
                                   agency_url = agency$agency_url,
                                   agency_timezone = agency$agency_timezone)
      
      write_gtfs(current_gtfs, file)
    }
  )
  
  #   #   #
  #
  ##   STOPS MODULE---------
  #
  #   #   #
  
  #reactive value for stops should be handled by the ssfs but just in case
  # Reactive values to store stops data and editing state
  #stops <- reactiveVal(
  #  st_sf(
  #    stop_id = character(),
  #    stop_name = character(),
  #    geometry = st_sfc(crs = 4326),
  #    stringsAsFactors = FALSE
  #  )
  #)
  
  stops_temp_point <- reactiveVal(NULL)
  stops_editing_id <- reactiveVal(NULL)
  
  # Dynamic UI for stops edit panel
  output$editPanel <- renderUI({
    if (!is.null(stops_temp_point())) {
      div(
        h4("Stop Details"),
        textInput("edit_stop_id", "Stop ID", value = if (!is.null(stops_editing_id())) {
          current_data <- ssfs()
          current_data$stops$stop_id[current_data$stops$stop_id == stops_editing_id()]
        } else
          ""),
        textInput("edit_stop_name", "Stop Name", value = if (!is.null(stops_editing_id())) {
          current_data <- ssfs()
          current_data$stops$stop_name[current_data$stops$stop_id == stops_editing_id()]
        } else
          ""),
        actionButton("save_stop", "Save Stop", class = "btn-primary"),
        actionButton("cancel_edit", "Cancel")
      )
    }
  })
  
  # Initialize stops map
  output$stops_map <- renderLeaflet({
    center <- map_center()
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addBaseMaps() %>%
      setView(lng = center$lng, lat = center$lat, zoom = 12) %>%
      htmlwidgets::onRender(
        "
      function(el, x) {
        this.on('zoomend', function(e) {
          Shiny.setInputValue('stops_map_zoom', this.getZoom());
        });
      }
    "
      )
  })
  
  #observer for level of zoom on stops map
  observeEvent(input$stops_map_zoom, {
    current_zoom(input$stops_map_zoom)
  })
  
  #FOR UPLOAD STOPS FUNCTIONALITY
  # Observer to update available SF objects in environment
  #observe({
  # Get all objects in global environment
  #  all_objects <- ls(envir = .GlobalEnv)
  #  # Filter for SF objects with shape_id column
  #  stops_sf_objects <- sapply(all_objects, function(x) {
  #    obj <- get(x, envir = .GlobalEnv)
  #    return(inherits(obj, "sf") && "stop_id" %in% names(obj))
  #  })
  #  stops_sf_names <- names(stops_sf_objects)[stops_sf_objects]
  #  
  #  updateSelectInput(session, "stops_sf_objects", choices = c("", stops_sf_names))
  #})
  
  # Update stops map content
  observe({
    current_data <- ssfs()
    temp <- stops_temp_point()
    
    # Update map with all data 
    proxy <- updateMapWithSsfsData(
      "stops_map", 
      current_data, 
      show_shapes = TRUE  # Show shapes behind stops
    )
    
    # Add temporary point if in editing mode
    if (!is.null(temp)) {
      proxy %>%
        addCircleMarkers(
          lng = temp[1],
          lat = temp[2],
          layerId = "temp",
          color = "#B2182B",
          stroke = FALSE,
          fillOpacity = 0.7,
          radius = calculateMarkerSize(current_zoom())
        )
    }
  })
  
  # Handle stops map clicks
  observeEvent(input$stops_map_click, {
    click <- input$stops_map_click
    
    if (is.null(stops_editing_id())) {
      #start editing new stop
      stops_temp_point(c(click$lng, click$lat))
      #clear input fields for new stop
      updateTextInput(session, "edit_stop_id", value = "")
      updateTextInput(session, "edit_stop_name", value = "")
    } else {
      #update point location while editing existing stop
      stops_temp_point(c(click$lng, click$lat))
    }
  })
  
  # Handle existing stop clicks
  observeEvent(input$stops_map_marker_click, {
    click <- input$stops_map_marker_click
    
    if (click$id != "temp") {
      #start editing existing stop
      current_data <- ssfs()
      selected_data <- current_data$stops[current_data$stops$stop_id == click$id, ]
      point_coords <- st_coordinates(selected_data$geometry)[1, ]
      
      stops_editing_id(click$id)
      stops_temp_point(point_coords)
      
      #update text inputs with existing stop details
      updateTextInput(session, "edit_stop_id", value = selected_data$stop_id)
      updateTextInput(session, "edit_stop_name", value = selected_data$stop_name)
    }
  })
  
  # Save stop (new or edited)
  observeEvent(input$save_stop, {
    if (!is.null(stops_temp_point()) &&
        !is.null(input$edit_stop_id) &&
        !is.null(input$edit_stop_name) &&
        input$edit_stop_id != "" &&
        input$edit_stop_name != "") {
      temp <- stops_temp_point()
      current_data <- ssfs()
      new_stop <- st_sf(
        stop_id = input$edit_stop_id,
        stop_name = input$edit_stop_name,
        geometry = st_sfc(st_point(c(temp[1], temp[2])), crs = 4326),
        stringsAsFactors = FALSE
      )
      
      if (is.null(stops_editing_id())) {
        # Add new stop
        current_data$stops <- rbind(current_data$stops, new_stop)
      } else {
        # Update existing stop
        current_data$stops <- current_data$stops[current_data$stops$stop_id != stops_editing_id(), ]
        current_data$stops <- rbind(current_data$stops, new_stop)
      }
      
      #update ssfs with new stop
      ssfs(current_data)
      
      #reset editing state
      stops_editing_id(NULL)
      stops_temp_point(NULL)
    }
  })
  
  # Cancel stops editing
  observeEvent(input$cancel_edit, {
    stops_editing_id(NULL)
    stops_temp_point(NULL)
  })
  
  # Observer for loading selected SF object
  #observeEvent(input$load_stops_sf, {
  #  req(input$stops_sf_objects)
  #  
  #  tryCatch({
  #    selected_sf <- get(input$stops_sf_objects, envir = .GlobalEnv)
  #    
  #    # Transform to CRS 4326 if needed
  #    if (st_crs(selected_sf) != 4326) {
  #      selected_sf <- st_transform(selected_sf, 4326)
  #CONSIDER adding additional verifications and transformations to ensure conformity
  #    }
  
  # Update the stops reactive value with the loaded data
  #    current_ssfs <- ssfs()
  
  #    current_ssfs$stops <- selected_sf
  
  #    ssfs(current_ssfs)
  
  #    showNotification(paste("Loaded stops from", input$stops_sf_objects),
  #                     type = "message")
  
  # }, error = function(e) {
  #    showNotification(paste("Error loading stops:", e$message), type = "error")
  #  })
  #})
  
  #Integrate download geojson functionality ? Requires collection name input in UI
  
  #output$download_geojson <- downloadHandler(
  #  filename = function() {
  #    paste0(input$collection_name, ".geojson")
  #  },
  #  content = function(file) {
  #IS ssfs$stops() THE RIGHT WAY TO BRING IN THE VALUE ?
  #    st_write(ssfs$stops(), file, driver = "GeoJSON")
  #    showNotification("GeoJSON file downloaded.", type = "message")
  #  }
  #)
  
  #   #   #
  #
  ##   ROUTES MODULE-----------
  #
  #   #   #
  
  # Function to calculate threshold distance from existing points (unchanged)
  calculateThreshold <- function(zoom) {
    # Base threshold at zoom level 10 is 0.02
    base_threshold <- 0.02
    # Adjust threshold exponentially based on zoom difference from base level
    # Smaller number when zoomed in, larger when zoomed out
    adjusted_threshold <- base_threshold * (2 ^ (10 - zoom))
    # Clamp the threshold to reasonable limits
    return(min(max(adjusted_threshold, 0.0001), 0.1))
  }
  
  # Modified function to generate partial route segments between nodes
  generateRouteSegment <- function(from_point, to_point, drawing_mode = "network") {
    
    #Initialize result_points
    result_points <- data.frame(lng = numeric(), lat = numeric())
    
    if (drawing_mode == "network") {
      # Try OSRM routing
      tryCatch({
        from_sf <- st_sf(geometry = st_sfc(st_point(from_point), crs = 4326))
        to_sf <- st_sf(geometry = st_sfc(st_point(to_point), crs = 4326))
        
        route <- osrmRoute(src = from_sf, dst = to_sf, overview = "full")
        route_coords <- st_coordinates(route$geometry)
        
        # Add all points from route
        for (j in 1:nrow(route_coords)) {
          result_points <- rbind(result_points, data.frame(
            lng = route_coords[j, 1],
            lat = route_coords[j, 2]
          ))
        }
      }, error = function(e) {
        # If OSRM fails, just add direct line (start and end points)
        result_points <- rbind(
          data.frame(lng = from_point[1], lat = from_point[2]),
          data.frame(lng = to_point[1], lat = to_point[2])
        )
      })
    } else {
      # Free drawing mode - just connect with straight line
      result_points <- rbind(
        data.frame(lng = from_point[1], lat = from_point[2]),
        data.frame(lng = to_point[1], lat = to_point[2])
      )
    }
    
    return(result_points)
  }
  
  #in v250505.R, buildFullRouteFromNodes() and updateRouteSegment() used to be here. Removed.

  # Function to convert nodes to stop sequence
  generateStopSequenceFromNodes <- function(nodes = NULL, rvar_id = NULL) {
    
    if (is.null(nodes)) nodes <- route_nodes()
    if (is.null(rvar_id)) rvar_id <- active_rvar_id()
    
    # Extract nodes that are stops
    stop_nodes <- nodes[nodes$is_stop, ]
    
    if (nrow(stop_nodes) == 0) return(data.frame())
    
    # Create stop sequence
    stop_seq <- data.frame(
      rvar_id = rep(rvar_id, nrow(stop_nodes)),
      stop_id = stop_nodes$stop_id,
      stop_sequence = 1:nrow(stop_nodes),
      stop_name = stop_nodes$stop_name,
      stringsAsFactors = FALSE
    )
    
    return(stop_seq)
  }
  
  # Reactive values for the combined routes/shapes functionality
  current_sequence <- reactiveVal(data.frame(
    rvar_id = character(),
    stop_id = character(),
    stop_sequence = integer(),
    stop_name = character(),
    stringsAsFactors = FALSE
  ))
  
  route_nodes <- reactiveVal(data.frame(
    node_id = integer(),      # Sequential ID for the node
    lng = numeric(),          # Longitude
    lat = numeric(),          # Latitude
    is_stop = logical(),      # Is this node a stop?
    stop_id = character(),    # If it's a stop, what's its ID (empty otherwise)
    stop_name = character(),  # If it's a stop, what's its name (empty otherwise)
    index = integer(), # Bridge field with route_points
    stringsAsFactors = FALSE
  ))
  
  route_points <- reactiveVal(data.frame(
    index = numeric(),
    lng = numeric(),
    lat = numeric()
  ))
  
  route_editing_mode <- reactiveVal(FALSE)
  selected_point_index <- reactiveVal(NULL)
  active_rvar_id <- reactiveVal(NULL)
  
  # Add a reactive value to track when a marker was last clicked
  last_marker_click_time <- reactiveVal(0)
  
  # Initialize routes map
  output$routes_map <- renderLeaflet({
    center <- map_center()
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addBaseMaps() %>%
      setView(lng = center$lng, lat = center$lat, zoom = 12) %>%
      addLayersControl(
        overlayGroups = c("stops", "routes", "current_route"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      showGroup("stops") %>% 
      showGroup("routes") %>%
      showGroup("current_route") %>%
      htmlwidgets::onRender("
      function(el, x) {
        this.on('zoomend', function(e) {
          Shiny.setInputValue('routes_map_zoom', this.getZoom());
        });
        
        // Capture right-click events
        this.on('contextmenu', function(e) {
          Shiny.setInputValue('routes_map_right_click', {
            lat: e.latlng.lat,
            lng: e.latlng.lng
          }, {priority: 'event'});
        });
      }
    ")
  })
  
  # Update zoom level when map is zoomed
  observeEvent(input$routes_map_zoom, {
    current_zoom(input$routes_map_zoom)
  })
  
  # Show/hide map instructions based on whether rvar_id is set
  observe({
    if (is.null(active_rvar_id()) || active_rvar_id() == "") {
      shinyjs::show("map_instructions")
    } else {
      shinyjs::hide("map_instructions")
    }
  })
  
  # observe block that updates map
  observe({
    curr_nodes <- route_nodes()
    curr_points <- route_points()
    current_data <- ssfs()
    
    proxy <- leafletProxy("routes_map") %>%
      clearGroup("stops") %>%
      clearGroup("routes") %>%
      clearGroup("current_route") %>%
      clearGroup("route_nodes")
    
    # Add existing routes (except current one being edited)
    if (!is.null(current_data$shapes) && nrow(current_data$shapes) > 0) {
      for (i in 1:nrow(current_data$shapes)) {
        # Skip the current route being edited
        if (!is.null(active_rvar_id()) && current_data$shapes$rvar_id[i] == active_rvar_id()) {
          next
        }
        
        line_coords <- st_coordinates(current_data$shapes$geometry[i])
        proxy <- proxy %>%
          addPolylines(
            lng = line_coords[, 1],
            lat = line_coords[, 2],
            group = "routes",
            color = "#05AEEF",
            weight = 2,
            opacity = 0.6
          )
      }
    }
    
    # Add all stops
    if (!is.null(current_data$stops) && nrow(current_data$stops) > 0) {
      marker_size <- calculateMarkerSize(current_zoom())
      
      # Get IDs of stops that are in current nodes
      stop_ids_in_nodes <- curr_nodes$stop_id[curr_nodes$is_stop]
      
      # Determine colors based on whether stop is in sequence
      fill_colors <- ifelse(current_data$stops$stop_id %in% stop_ids_in_nodes, "#B2182B", "#7f7f7f")
      
      proxy <- proxy %>%
        addCircleMarkers(
          data = current_data$stops,
          radius = marker_size,
          color = "white",
          weight=1,
          stroke = TRUE,
          fillColor=fill_colors,
          fillOpacity = 0.7,
          layerId = ~stop_id,
          popup = ~paste("ID:", stop_id, "<br>Name:", stop_name),
          group = "stops"
        )
    }
    
    # Add current route being edited
    if (nrow(curr_points) > 1) {
      
      proxy <- proxy %>%
        addPolylines(
          lng = curr_points$lng,
          lat = curr_points$lat,
          group = "current_route",
          color = "#B2182B",
          weight = 4,
          opacity = 0.8
        )
    }
    
    # Add node markers (with different styles for stops vs. waypoints)
    if (nrow(curr_nodes) > 0) {
      # Add stop nodes
      stop_nodes <- curr_nodes[curr_nodes$is_stop, ]
      if (nrow(stop_nodes) > 0) {
        
        proxy <- proxy %>%
          addCircleMarkers(
            lng = stop_nodes$lng,
            lat = stop_nodes$lat,
            group = "route_nodes",
            radius = 8,
            color = "#B2182B",
            fillColor = "#B2182B",
            fillOpacity = 0.9,
            stroke = TRUE,
            weight = 2,
            layerId = paste0("stop_", stop_nodes$node_id),
            label = paste0("Stop: ", stop_nodes$stop_name)
          )
      }
      
      # Add waypoint nodes
      waypoint_nodes <- curr_nodes[!curr_nodes$is_stop, ]
      if (nrow(waypoint_nodes) > 0) {
        proxy <- proxy %>%
          addCircleMarkers(
            lng = waypoint_nodes$lng,
            lat = waypoint_nodes$lat,
            group = "route_nodes",
            radius = 6,
            color = "orange",
            fillColor = "orange",
            fillOpacity = 0.9,
            stroke = TRUE,
            weight = 2,
            layerId = paste0("waypoint_", waypoint_nodes$node_id),
            label = "Waypoint"
          )
      }
      
      # Highlight selected node
      if (!is.null(selected_point_index())) {
        selected_node <- curr_nodes[curr_nodes$node_id == selected_point_index(), ]
        if (nrow(selected_node) > 0) {
          proxy <- proxy %>%
            addCircleMarkers(
              lng = selected_node$lng,
              lat = selected_node$lat,
              group = "route_nodes",
              radius = 8,
              color = "#FFE999",
              fillColor = "#FFE999",
              fillOpacity = 0.9,
              stroke = TRUE,
              weight = 3,
              layerId = "selected_node"
            )
        }
      }
    }
    
    return(proxy)
  })
  
  # Load existing route
  observeEvent(input$load_rvar, {
    req(input$existing_rvar)
    
    current_ssfs <- ssfs()
    selected_rvar <- current_ssfs$rvar[current_ssfs$rvar$rvar_id == input$existing_rvar, ]
    
    # Update inputs
    updateTextInput(session, "rvar_id", value = selected_rvar$rvar_id)
    updateTextInput(session, "route_id", value = selected_rvar$route_id)
    updateTextInput(session, "route_long_name", value = selected_rvar$route_long_name)
    updateSelectInput(session, "direction_id", selected = selected_rvar$direction_id)
    updateTextInput(session, "trip_headsign", value = selected_rvar$trip_headsign)
    updateSelectInput(session, "route_type", selected = selected_rvar$route_type)
    
    # Set active route variant
    active_rvar_id(input$existing_rvar)
    
    # Load stop sequence
    stop_seq <- current_ssfs$stop_seq[current_ssfs$stop_seq$rvar_id == input$existing_rvar, ]
    current_sequence(stop_seq)
    
    # Load shape and extract nodes
    shape_data <- current_ssfs$shapes[current_ssfs$shapes$rvar_id == input$existing_rvar, ]
    
    if (nrow(shape_data) > 0) {
      # Get all points from the shape
      coords <- st_coordinates(shape_data$geometry)
      full_points <- data.frame(
        index = 1:nrow(coords),
        lng = coords[, 1],
        lat = coords[, 2]
      )
      route_points(full_points)
      
      # Create nodes from stops
      nodes_df <- data.frame(
        node_id = integer(),
        lng = numeric(),
        lat = numeric(),
        is_stop = logical(),
        stop_id = character(),
        stop_name = character(),
        index = integer(),  # referring to index in route_points (full points)
        stringsAsFactors = FALSE
      )
      
      # First add nodes for each stop in the sequence
      if (nrow(stop_seq) > 0) {
        for (i in 1:nrow(stop_seq)) {
          stop_id <- stop_seq$stop_id[i]
          stop_data <- current_ssfs$stops[current_ssfs$stops$stop_id == stop_id, ]
          #caution : could stop_data be empty? important to write a warning message here just in case?
            stop_coords <- st_coordinates(stop_data$geometry)
            
            # Find closest point on route to this stop
            distances <- sqrt((full_points$lng - stop_coords[1, 1])^2 + 
                                (full_points$lat - stop_coords[1, 2])^2)
            closest_idx <- which.min(distances)
            
            # Add as a node
            nodes_df <- rbind(nodes_df, data.frame(
              node_id = i,
              lng = stop_coords[1, 1],
              lat = stop_coords[1, 2],
              is_stop = TRUE,
              stop_id = stop_id,
              stop_name = stop_seq$stop_name[i],
              index = closest_idx,  #immediately applied based on closest point on route to this stop
              stringsAsFactors = FALSE
            ))
        }
        
        #just in case, renaming rows of nodes_df to remove the X that otherwise appears
        row.names(nodes_df) <- 1:nrow(nodes_df)
        
      }
      
      #update route nodes
      route_nodes(nodes_df)
      }
  })
  
  # Set active route variant when rvar_id is changed
  observeEvent(input$rvar_id, {
    if (!is.null(input$rvar_id) && input$rvar_id != "") {
      active_rvar_id(input$rvar_id)
    }
  })
  
  # Stop click handler - adds stop nodes
  observeEvent(input$routes_map_marker_click, {
    req(active_rvar_id())
    click <- input$routes_map_marker_click
  
    # Set the timestamp of this marker click
    last_marker_click_time(as.numeric(Sys.time()))
    
    # Check if clicked on a waypoint
    if (!is.null(click) && grepl("^waypoint_", click$id)) {
      # Clicked on a node - select it for potential movement
      node_id <- as.numeric(gsub("waypoint_", "", click$id))
        
      selected_point_index(node_id)
      showNotification("Waypoint selected. Click on map to move it.", type = "message")
      
      #check if the node clicked is the selected waypoint
      } else if (!is.null(click) && click$id=="selected_node"){
          
        #deselect the point for movement and notify the user.
          selected_point_index(NULL)
          showNotification("Waypoint deselected. Movement cancelled.", type = "message")
    
      #check if clicked on a stop among current nodes
      } else if (!is.null(click) && grepl("^stop_",click$id)){
        
        #nothing happens - notify user that stop is already part of the route
        showNotification("Stop already in route stop sequence. Cannot add stop again.", type = "warning")
        
      #otherwise, it is a click on a stop elsewhere that can be added to the route
      } else if (!is.null(click)) {
        
        current_data <- ssfs()
        clicked_stop <- current_data$stops[current_data$stops$stop_id == click$id, ]
        curr_nodes <- route_nodes()
        curr_points <- route_points()
        
        #if a waypoint is already selected for movement, move the waypoint to the clicked stop
        #give it the attributes of the clicked stop
        if(!is.null(selected_point_index())){
          
          #CURR NODES NO, SP NOT NULL 
          #(waypoint location moved & given stop attributes, stop added to route, stop sequence recalculated)
          
          before_idx <- selected_point_index() - 1
          after_idx <- selected_point_index() + 1
          
          #divide the nodes and route points into segments a, b and c
          #segments b and c (before and after the selected point that is moved to a new point)
          #will be calculated later
          #index values in segment d need to be adjusted based on the difference of
          #segment bc before and after
          
          #segment A : 
          nodes_a <- curr_nodes[1:before_idx,]
          nodes_a_idx_max <- max(nodes_a$index)
          
          points_a <- curr_points[1:nodes_a_idx_max,]
          
          #segment D : 
          nodes_d <- curr_nodes[after_idx:nrow(curr_nodes),]
          nodes_d_idx_min <- min(nodes_d$index)
          
          points_d <- curr_points[nodes_d_idx_min:nrow(curr_points),]
          
          #number of points in BC before : 
          nb_points_bc_before <- 
            min(points_d$index)-max(points_a$index)-1
          #specifically the points BETWEEN segments a and d, 
          #excluding the last point of a and the first point of d
          
          #generate segment B
          
          #from the end of segment a to the new stop
          from_point <- c(curr_nodes[before_idx,]$lng,curr_nodes[before_idx,]$lat)
          to_point <- c(st_coordinates(clicked_stop)[1],st_coordinates(clicked_stop)[2])
          
          segment_b <- generateRouteSegment(from_point,to_point,drawing_mode = input$drawing_mode)
          
          #EXCLUDING the first point which is included in points_a
          points_b <- 
            segment_b[2:nrow(segment_b),] %>% 
            mutate(index=row_number()+nodes_a_idx_max,
                   .before="lng")
          
          #point b index max will be the new index value for the moved node AND added stop
          #and will serve to adjust the index values of points_d and nodes_d
          
          points_b_idx_max <- 
            max(points_b$index)
          
          #generate segment C
          
          #from new stop to beginning of segment d
          
          from_point <-  c(st_coordinates(clicked_stop)[1],st_coordinates(clicked_stop)[2])
          to_point <- c(curr_nodes[after_idx,]$lng,curr_nodes[after_idx,]$lat)
          
          segment_c <- generateRouteSegment(from_point,to_point,drawing_mode = input$drawing_mode)
          
          points_c <- 
            segment_c[2:(nrow(segment_c)-1),] %>% #EXCLUDING the first and the last point,
            #which are included in the other segments already
            mutate(index=row_number()+points_b_idx_max,
                   .before="lng")
          
          points_bc <- rbind(points_b,points_c)
          
          #adjust index values for points and nodes d : 
          
          nb_points_bc_after <- nrow(points_bc)
          
          adj_index_d <- nb_points_bc_after-nb_points_bc_before
          #this will help add or subtract from the index values, depending on if the new segment bc
          #has more or fewer points than the old segment bc
          
          points_d <- 
            points_d %>% 
            mutate(index=index+adj_index_d)
          
          nodes_d <- 
            nodes_d %>% 
            mutate(index=index+adj_index_d)
          #called nodes_d but it's really just the third set of nodes
          #nodes_a, new node, and nodes_d
          
          #create new stop node based on clicked stop to replace waypoint
          
          node_bc <- 
            data.frame(
              node_id=selected_point_index(),
              lng=st_coordinates(clicked_stop)[1],
              lat=st_coordinates(clicked_stop)[2],
              is_stop=TRUE,
              stop_id=clicked_stop$stop_id,
              stop_name=clicked_stop$stop_name,
              index=points_b_idx_max
            )
          
          #reconstitute curr_points and curr_nodes
          
          curr_points <- 
            rbind(points_a,points_b,points_c,points_d)
          
          curr_nodes <- 
            rbind(nodes_a,node_bc,nodes_d)
          
          #rename rows for good form
          row.names(curr_points) <- 1:nrow(curr_points)
          row.names(curr_nodes) <- 1:nrow(curr_nodes)
          
          #update the reactive values
          route_points(curr_points)
          route_nodes(curr_nodes)
          selected_point_index(NULL)
          
          showNotification("Waypoint moved to stop & adopted stop properties.", type = "message")
          
        } else { #add stop to the end of the route, OR add first node
          
          # Get stop coordinates
          stop_coords <- st_coordinates(clicked_stop$geometry)
          
          if (nrow(curr_nodes) >= 1) {
            #if there's 1 node or more, add the new node to the existing ones
            
            #get the existing max index from the nodes
            nodes_a_idx_max <- max(curr_nodes$index)
            
            #Route a new segment from the last existing node to the new node
            #using the last existing node coords as from_point and
            #stop_coords from clicked stop as to_point
            from_lng <- curr_nodes[nrow(curr_nodes),]$lng
            from_lat <- curr_nodes[nrow(curr_nodes),]$lat
            
            from_point <- c(from_lng,from_lat)
            to_point <- c(stop_coords[1],stop_coords[2])
            
            new_segment <- generateRouteSegment(from_point,to_point,drawing_mode = input$drawing_mode)
            
            new_points <- 
              new_segment[2:nrow(new_segment),] %>% #EXCLUDING the first point
              #which is included in the previous segment already
              mutate(index=row_number()+nodes_a_idx_max,
                     .before="lng")
            
            curr_points <- rbind(curr_points,new_points)
            
            #just in case, rename curr_point row names
            row.names(curr_points) <- 1:nrow(curr_points)
            
            #max index of new curr_points becomes the index of new node
            #(associated with stop_coords)
            
            new_node_index <- max(curr_points$index)
            
            # Create new node
            new_node <- data.frame(
              node_id = max(curr_nodes$node_id) + 1,
              lng = stop_coords[1],
              lat = stop_coords[2],
              is_stop = TRUE,
              stop_id = clicked_stop$stop_id,
              stop_name = clicked_stop$stop_name,
              index = new_node_index,
              stringsAsFactors = FALSE
            )
            
            # Add to nodes
            curr_nodes <- rbind(curr_nodes, new_node)
            
            #curr_nodes %>% st_as_sf(coords=c("lng","lat"),crs=4326) %>% mapview()
            
            route_points(curr_points)
            route_nodes(curr_nodes)
          } else {
            #it's the first node !
            route_nodes(data.frame(
              node_id = 1,
              lng = stop_coords[1],
              lat = stop_coords[2],
              is_stop = TRUE,
              stop_id = clicked_stop$stop_id,
              stop_name = clicked_stop$stop_name,
              index = 1,
              stringsAsFactors = FALSE
            ))
            route_points(data.frame(
              index = 1,
              lng = stop_coords[1],
              lat = stop_coords[2]
            ))
          }
          
        }
        
        # Update current sequence for display
        current_sequence(generateStopSequenceFromNodes())
      }
    })

  # Map click handler
  observeEvent(input$routes_map_click, {
    req(active_rvar_id())
    
    # Check if this map click is too close in time to a marker click
    current_time <- as.numeric(Sys.time())
    time_since_marker_click <- current_time - last_marker_click_time()
    
    # If a marker was clicked within the last 100ms, ignore this map click
    if (time_since_marker_click < 0.1) {
      return()
    }
    
    click <- input$routes_map_click
    curr_nodes <- route_nodes()
    curr_points <- route_points()
    
    #a waypoint is selected for movement : move it to the clicked location
    #and update route points and route nodes
    #based on the number of current nodes
    if (!is.null(selected_point_index())) {
      # Move the selected node to the new location
      idx <- which(curr_nodes$node_id == selected_point_index())
      
      #only one node and it is moved, replace its geo coords
      if(nrow(curr_nodes)==1){
        
        curr_nodes$lat <- click$lat
        curr_nodes$lng <- click$lng
        
        curr_points$lat <- click$lat
        curr_points$lng <- click$lng
        
      #More than one node but the first node is selected for movement
      }else if(idx==1){
        
        #remove and replace the first segment of points
        
        #how many points in the first segment before the point move?
        nb_points_before <- curr_nodes[2,]$index-1
        
        #recreate new first segment based on new position of first point
        from_point <- c(click$lng,click$lat)
        to_point <- c(curr_nodes[2,]$lng,curr_nodes[2,]$lat)
        
        new_segment <- generateRouteSegment(from_point,to_point,drawing_mode = input$drawing_mode)
        
        #new points to be used to replace previous points
        new_points <- 
          new_segment[1:(nrow(new_segment)-1),] %>% #EXCLUDING the last point,
          #which is included in the retained segment
          mutate(index=row_number(),
                 .before="lng")
        
        #nrow(new_points) is used for the new number of points
        
        adj_index <- nrow(new_points)-nb_points_before
        
        #define retained points
        curr_points <- 
          rbind(
            new_points,
            curr_points[(nb_points_before+1):nrow(curr_points),] %>% 
              mutate(index=index+adj_index))
        
        #rename rows..
        row.names(curr_points) <- 1:nrow(curr_points)
        
        #this part will need to be reworked to handle various cases related to stop status...
        curr_nodes[1,]$lng <- click$lng
        curr_nodes[1,]$lat <- click$lat
        
        curr_nodes <- 
          rbind(
            curr_nodes[1,],
            curr_nodes[2:nrow(curr_nodes),] %>% 
              mutate(index=index+adj_index))
        
        #the selected node is the last node
      }else if(idx==nrow(curr_nodes)){
        
        #recreate new last segment based on new position of last point
        from_point <- c(curr_nodes[idx-1,]$lng,curr_nodes[idx-1,]$lat)
        to_point <- c(click$lng,click$lat)
        
        new_segment <- generateRouteSegment(from_point,to_point,drawing_mode=input$drawing_mode)
        
        #points in retained segment
        
        nb_points_retained <- 
          curr_nodes[idx-1,]$index
        
        #new points to be used to replace previous points
        new_points <- 
          new_segment[2:(nrow(new_segment)),] %>% #EXCLUDING the first point,
          #which is included in the retained segment
          mutate(index=row_number()+nb_points_retained,
                 .before="lng")
        
        #define new curr points
        curr_points <- 
          rbind(
            curr_points[1:nb_points_retained,],
            new_points)
        
        #rename rows..
        row.names(curr_points) <- 1:nrow(curr_points)
        
        #this part will need to be reworked to handle various cases related to stop status...
        curr_nodes[idx,]$lng <- click$lng
        curr_nodes[idx,]$lat <- click$lat
        curr_nodes[idx,]$index <- max(curr_points$index)
        
        #move node selected for movement mid-route
      }else{
        
        before_idx <- idx - 1
        after_idx <- idx + 1
        
        #divide the nodes and route points into segments a, b and c
        #segments b and c (before and after the selected point that is moved to a new point)
        #will be calculated later
        #index values in segment d need to be adjusted based on the difference of
        #segment bc before and after
        
        #segment A : 
        nodes_a <- curr_nodes[1:before_idx,]
        nodes_a_idx_max <- max(nodes_a$index)
        
        points_a <- curr_points[1:nodes_a_idx_max,]
        
        #segment D : 
        nodes_d <- curr_nodes[after_idx:nrow(curr_nodes),]
        nodes_d_idx_min <- min(nodes_d$index)
        
        points_d <- curr_points[nodes_d_idx_min:nrow(curr_points),]
        
        #number of points in BC before : 
        nb_points_bc_before <- 
          min(points_d$index)-max(points_a$index)-1
        #specifically the points BETWEEN segments a and d, 
        #excluding the last point of a and the first point of d
        
        #generate segment B
        
        #from the end of segment a to the new stop
        from_point <- c(curr_nodes[before_idx,]$lng,curr_nodes[before_idx,]$lat)
        to_point <- c(click$lng,click$lat)
        
        segment_b <- generateRouteSegment(from_point,to_point,drawing_mode = input$drawing_mode)
        
        #EXCLUDING the first point which is included in points_a
        points_b <- 
          segment_b[2:nrow(segment_b),] %>% 
          mutate(index=row_number()+nodes_a_idx_max,
                 .before="lng")
        
        #point b index max will be the new index value for the moved node AND added stop
        #and will serve to adjust the index values of points_d and nodes_d
        
        points_b_idx_max <- 
          max(points_b$index)
        
        #generate segment C
        
        #from new stop to beginning of segment d
        
        from_point <-  c(click$lng,click$lat)
        to_point <- c(curr_nodes[after_idx,]$lng,curr_nodes[after_idx,]$lat)
        
        segment_c <- generateRouteSegment(from_point,to_point,drawing_mode = input$drawing_mode)
        
        points_c <- 
          segment_c[2:(nrow(segment_c)-1),] %>% #EXCLUDING the first and the last point,
          #which are included in the other segments already
          mutate(index=row_number()+points_b_idx_max,
                 .before="lng")
        
        points_bc <- rbind(points_b,points_c)
        
        #adjust index values for points and nodes d : 
        
        nb_points_bc_after <- nrow(points_bc)
        
        adj_index_d <- nb_points_bc_after-nb_points_bc_before
        #this will help add or subtract from the index values, depending on if the new segment bc
        #has more or fewer points than the old segment bc
        
        points_d <- 
          points_d %>% 
          mutate(index=index+adj_index_d)
        
        nodes_d <- 
          nodes_d %>% 
          mutate(index=index+adj_index_d)
        #called nodes_d but it's really just the third set of nodes
        #nodes_a, new node, and nodes_d
        
        node_bc <- 
          data.frame(
            node_id=idx,
            lng=click$lng,
            lat=click$lat,
            is_stop=FALSE, #this is a waypoint if it's selected and being moved
            #not possible to select a stop node for movement
            stop_id="",
            stop_name="",
            index=points_b_idx_max
          )
        
        #reconstitute curr_points and curr_nodes
        
        curr_points <- 
          rbind(points_a,points_b,points_c,points_d)
        
        curr_nodes <- 
          rbind(nodes_a,node_bc,nodes_d)
        
        #rename rows for good form
        row.names(curr_points) <- 1:nrow(curr_points)
        row.names(curr_nodes) <- 1:nrow(curr_nodes)
        
      }
      
      #reactive value update, notification, reset selected point index to null
      route_points(curr_points)
      route_nodes(curr_nodes)
      selected_point_index(NULL)  # Reset selection
      showNotification("Waypoint moved", type = "message")
      
    } else if (nrow(curr_nodes) >= 1) {
      
      if(nrow(curr_nodes)>=2){
      #check if click is near a segment, and if so, add point along segment
        
      point_added <- FALSE
      
      for (i in 1:(nrow(curr_points) - 1)) {
        
        # Get segment endpoints
        p1 <- curr_points[i, ]
        p2 <- curr_points[i + 1, ]
        
        # Calculate distance from click to line segment
        d <- abs((p2$lat - p1$lat) * click$lng -
                   (p2$lng - p1$lng) * click$lat +
                   p2$lng * p1$lat - p2$lat * p1$lng
        ) /
          sqrt((p2$lat - p1$lat) ^ 2 + (p2$lng - p1$lng) ^ 2)
        
        # Also check if click is within the bounding box of the segment
        within_bounds <- (
          min(p1$lng, p2$lng) <= click$lng &&
            click$lng <= max(p1$lng, p2$lng) &&
            min(p1$lat, p2$lat) <= click$lat &&
            click$lat <= max(p1$lat, p2$lat)
        )
        
        # If click is close to segment and within bounds
        if (d < calculateThreshold(current_zoom()) &&
            within_bounds) {
          
          new_pt_idx <- p1$index + 1
          
          # Create new point
          new_point <- data.frame(
            index = new_pt_idx, #index of point before, plus 1
            lng = click$lng,
            lat = click$lat
          )
          
          # Insert new point between segment endpoints
          new_points <- rbind(curr_points[1:i, ], new_point, 
                              curr_points[(i + 1):nrow(curr_points), ])
          
          # Reindex all points to maintain sequence
          new_points$index <- 1:nrow(new_points)
          
          curr_points <- new_points
          
          #create waypoint node for this point
          
          #first, identify the nodes before this new point
          
          nodes_a <- 
            curr_nodes %>% 
            filter(index<=i)
          
          nodes_b <- 
            curr_nodes %>% 
            filter(index>i) %>% 
            mutate(node_id=node_id+1, #add one more node
                   index=index+1) #add one more point
          
          new_node <- 
            data.frame(
              node_id=max(nodes_a$node_id)+1,
              lng=click$lng,
              lat=click$lat,
              is_stop=FALSE,
              stop_id="",
              stop_name="",
              index=new_pt_idx
            )
          
          curr_nodes <- rbind(nodes_a,new_node,nodes_b)
          
          #rename rows for good form
          row.names(curr_points) <- 1:nrow(curr_points)
          row.names(curr_nodes) <- 1:nrow(curr_nodes)
          
          point_added <- TRUE
          
          #update the reactive values
          route_points(curr_points)
          route_nodes(curr_nodes)
          
          showNotification("Waypoint added along route", type = "message")
          
          break
        }
      }
      
        if(!point_added){ #SUPER REDUNDANT ! repeats because I don't know how to better organise the conditions
          #add second waypoint
          
          #get the existing max index from the nodes
          nodes_a_idx_max <- max(curr_nodes$index)
          
          #Route a new segment from the last existing node to the new node
          #using the last existing node coords as from_point and
          #stop_coords from clicked stop as to_point
          from_lng <- curr_nodes[nrow(curr_nodes),]$lng
          from_lat <- curr_nodes[nrow(curr_nodes),]$lat
          
          from_point <- c(from_lng,from_lat)
          to_point <- c(click$lng,click$lat)
          
          new_segment <- generateRouteSegment(from_point,to_point,drawing_mode=input$drawing_mode)
          
          new_points <- 
            new_segment[2:nrow(new_segment),] %>% #EXCLUDING the first point
            #which is included in the previous segment already
            mutate(index=row_number()+nodes_a_idx_max,
                   .before="lng")
          
          curr_points <- rbind(curr_points,new_points)
          
          #just in case, rename curr_point row names
          row.names(curr_points) <- 1:nrow(curr_points)
          
          #max index of new curr_points becomes the index of new node
          #(associated with stop_coords)
          
          new_node_index <- max(curr_points$index)
          
          # Create new node
          new_node <- data.frame(
            node_id = max(curr_nodes$node_id) + 1,
            lng = click$lng,
            lat = click$lat,
            is_stop = FALSE,
            stop_id = "",
            stop_name = "",
            index = new_node_index,
            stringsAsFactors = FALSE
          )
          
          # Add to nodes
          curr_nodes <- rbind(curr_nodes, new_node)
          
          #curr_nodes %>% st_as_sf(coords=c("lng","lat"),crs=4326) %>% mapview()
          
          route_points(curr_points)
          route_nodes(curr_nodes)
        }
        
      } else if (nrow(curr_nodes)==1){
        
        #add second waypoint
        
        #get the existing max index from the nodes
        nodes_a_idx_max <- max(curr_nodes$index)
        
        #Route a new segment from the last existing node to the new node
        #using the last existing node coords as from_point and
        #stop_coords from clicked stop as to_point
        from_lng <- curr_nodes[nrow(curr_nodes),]$lng
        from_lat <- curr_nodes[nrow(curr_nodes),]$lat
        
        from_point <- c(from_lng,from_lat)
        to_point <- c(click$lng,click$lat)
        
        new_segment <- generateRouteSegment(from_point,to_point,drawing_mode = input$drawing_mode)
        
        new_points <- 
          new_segment[2:nrow(new_segment),] %>% #EXCLUDING the first point
          #which is included in the previous segment already
          mutate(index=row_number()+nodes_a_idx_max,
                 .before="lng")
        
        curr_points <- rbind(curr_points,new_points)
        
        #just in case, rename curr_point row names
        row.names(curr_points) <- 1:nrow(curr_points)
        
        #max index of new curr_points becomes the index of new node
        #(associated with stop_coords)
        
        new_node_index <- max(curr_points$index)
        
        # Create new node
        new_node <- data.frame(
          node_id = max(curr_nodes$node_id) + 1,
          lng = click$lng,
          lat = click$lat,
          is_stop = FALSE,
          stop_id = "",
          stop_name = "",
          index = new_node_index,
          stringsAsFactors = FALSE
        )
        
        # Add to nodes
        curr_nodes <- rbind(curr_nodes, new_node)
        
        #curr_nodes %>% st_as_sf(coords=c("lng","lat"),crs=4326) %>% mapview()
        
        route_points(curr_points)
        route_nodes(curr_nodes)
        
      } else {
      #there are no nodes or waypoints yet. Do nothing other than prompt the user
      #to start the route with a stop (routes should start at stops)
      
      showNotification("Click on a stop to start your route", type = "warning")
      
      }
    }
    })

  # Right-click handler to remove nodes
  observeEvent(input$routes_map_right_click, {
    req(active_rvar_id())
    
    click <- input$routes_map_right_click
    curr_nodes <- route_nodes()
    curr_points <- route_points()
    
    if (nrow(curr_nodes) == 0) return()
    
    # Find closest node
    distances <- sqrt((curr_nodes$lng - click$lng)^2 + (curr_nodes$lat - click$lat)^2)
    closest_idx <- which.min(distances)
    
    # If click is close enough to a node, remove it
    if (distances[closest_idx] < calculateThreshold(current_zoom())) {
      # Check node position for specialized handling
      
      # REMOVING FIRST NODE OR ONLY NODE
      if (closest_idx == 1) {
        
        #REMOVING FIRST NODE
        if (nrow(curr_nodes) > 1) {
          
          # Remove the node
          curr_nodes <- curr_nodes[-closest_idx, ]
          
          # Re-number remaining nodes
          curr_nodes$node_id <- 1:nrow(curr_nodes)
          
          #determine adjustment of index by number of points
          #based on the current index value for the new first node
          #in order to bring this value to 1 and have the others within nodes and routes_points adjust accordingly
          
          index_adj <- curr_nodes[1,]$index-1
          
          curr_nodes <- curr_nodes %>% mutate(index=index-index_adj)
          
          curr_points <- 
            curr_points[(index_adj+1):nrow(curr_points),] %>% 
            mutate(index=row_number())
          
          #resetting row names, just in case.
          
          row.names(curr_nodes) <- 1:nrow(curr_nodes)
          row.names(curr_points) <- 1:nrow(curr_points)
          
          #update reactive values
          route_nodes(curr_nodes)
          route_points(curr_points)
        } else {
          # Removing only node, clear everything
          route_nodes(data.frame(
            node_id = integer(),
            lng = numeric(),
            lat = numeric(),
            is_stop = logical(),
            stop_id = character(),
            stop_name = character(),
            index= integer(),
            stringsAsFactors = FALSE
          ))
          route_points(data.frame(index = integer(), lng = numeric(), lat = numeric()))
        }
        
        #REMOVING LAST NODE
      } else if (closest_idx == nrow(curr_nodes)) {
        
        # Remove the node
        curr_nodes <- curr_nodes[-closest_idx, ]
        
        #(below is recycled from backspace handler)
        #(in principle, the previous condition should handle cases where input is 1 node and output is 0)
        #just in case, rename remaining rows
        row.names(curr_nodes) <- 1:nrow(curr_nodes)
        
        if(nrow(curr_nodes)>0){
          #if at least 1 node is left, then
          #identify new max index on the line and reduce route points to this
          max_index <- 
            max(curr_nodes$index)
          
          curr_points <- 
            curr_points %>%
            filter(index<=max_index)
          
          #just in case, rename remaining rows
          row.names(curr_points) <- 1:nrow(curr_points)
          
        }else{
          #if no nodes left, then clear points
          curr_points <- data.frame(index = integer(), lng = numeric(), lat = numeric())
        }
        
        route_nodes(curr_nodes)
        route_points(curr_points)
        
        #REMOVING NODE IN MIDDLE
      } else if (nrow(curr_nodes) > 2) { 
        
        # Identify the nodes before and after this one
        before_idx <- closest_idx - 1
        after_idx <- closest_idx + 1
        
        #divide the nodes and route points into segments a, b and c
        #segment b is the one that will be calculated after
        #index values in segment c need to be adjusted based on the difference 
        #in the number of points in segment b before and after
        
        #segment A : 
        nodes_a <- curr_nodes[1:before_idx,]
        nodes_a_idx_max <- max(nodes_a$index)
        
        points_a <- curr_points[1:nodes_a_idx_max,]
        
        #segment C : 
        nodes_c <- curr_nodes[after_idx:nrow(curr_nodes),]
        nodes_c_idx_min <- min(nodes_c$index)
        
        points_c <- curr_points[nodes_c_idx_min:nrow(curr_points),]
        
        #number of points in B before : 
        nb_points_b_before <- 
          min(points_c$index)-max(points_a$index)-1
        #specifically the points BETWEEN segments a and c, 
        #excluding the last point of a and the first point of c
        
        #generate segment B : 
        
        #from the node before to the node after
        from_point <- c(curr_nodes[before_idx,]$lng,curr_nodes[before_idx,]$lat)
        to_point <- c(curr_nodes[after_idx,]$lng,curr_nodes[after_idx,]$lat)
        
        segment_b <- generateRouteSegment(from_point,to_point,drawing_mode=input$drawing_mode)
        
        points_b <- 
          segment_b[2:(nrow(segment_b)-1),] %>% #EXCLUDING the first and the last point,
          #which are included in the other segments already
          mutate(index=row_number()+nodes_a_idx_max,
                 .before="lng")
        
        #adjust index for points and nodes in c :
        
        nb_points_b_after <- nrow(points_b)
        
        adj_index_c <- nb_points_b_after-nb_points_b_before
        #this will help add or subtract from the index values, depending on if the new segment b
        #has more or fewer points than the old segment b
        
        points_c <- 
          points_c %>% 
          mutate(index=index+adj_index_c)
        
        nodes_c <- 
          nodes_c %>% 
          mutate(index=index+adj_index_c)
        
        #reconstitute curr_points and curr_nodes
        
        curr_points <- 
          rbind(points_a,points_b,points_c)
        
        curr_nodes <- 
          rbind(nodes_a,nodes_c) %>% 
          mutate(node_id=row_number()) #reorder the node ids
        
        #rename rows, just in case
        row.names(curr_points) <- 1:nrow(curr_points)
        row.names(curr_nodes) <- 1:nrow(curr_nodes)
        
        #update the reactive values
        route_points(curr_points)
        route_nodes(curr_nodes)
      }
      
      # Update stop sequence
      current_sequence(generateStopSequenceFromNodes())
      
      showNotification("Node removed", type = "message")
    }
  })
  
  # Backspace key handler - use the same logic as remove_last_point
  observeEvent(input$backspace_pressed, {
    curr_nodes <- route_nodes()
    curr_points <- route_points()
    
    #backspace can only trigger removal of nodes if there is at least one, obviously.
    #could be interesting to develop : 
      #condition that backspace + active node selected means that it removes the active node
      #otherwise, it will remove the last point along the sequence.

    if (nrow(curr_nodes) > 0) {
      curr_nodes <- 
        curr_nodes[-nrow(curr_nodes),]
      
      #just in case, rename remaining rows
      row.names(curr_nodes) <- 1:nrow(curr_nodes)
      
      if(nrow(curr_nodes)>0){
        #if at least 1 node is left, then
        #identify new max index on the line and reduce route points to this
        max_index <- 
          max(curr_nodes$index)
        
        curr_points <- 
          curr_points %>%
          filter(index<=max_index)
      }else{
        #if no nodes left, then clear points
        curr_points <- data.frame(index = integer(), lng = numeric(), lat = numeric())
      }
      
      #update reactive values
      route_nodes(curr_nodes)
      route_points(curr_points)

      # Update stop sequence
      current_sequence(generateStopSequenceFromNodes())
      
      showNotification("Last node removed", type = "message")
    }
  })
  
  # Render stop sequence table
  output$selected_stops_table <- renderDT({
    req(current_sequence())
    
    datatable(
      current_sequence(),
      selection = 'single',
      rownames = FALSE,
      options = list(
        pageLength = -1,
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = 0:1),  # Hide rvar_id and stop_id
          list(visible = TRUE, targets = 2:3)    # Show stop_sequence and stop_name
        )
      )
    )
  })
  
  # Save route handler
  observeEvent(input$save_route, {
    req(
      active_rvar_id(),
      input$route_id,
      input$direction_id,
      input$trip_headsign,
      input$route_long_name,
      input$route_type
    )
    
    curr_points <- route_points()
    
    # Check if there are enough points
    # ADD LATER : check if there are enough stops in the stop sequence (at least 2)
    if (nrow(curr_points) < 2) {
      showNotification("Please add at least 2 points to create a route", type = "warning")
      return()
    }
    
    # Sort route points by index to ensure correct order
    curr_points <- curr_points[order(curr_points$index), ]
    
    # Create route shape
    coords_matrix <- as.matrix(curr_points[, c("lng", "lat")])
    line_feature <- st_linestring(coords_matrix)
    new_shape_sf <- st_sf(
      rvar_id = active_rvar_id(),
      geometry = st_sfc(line_feature, crs = 4326)
    )
    
    # Create new rvar entry
    new_rvar <- data.frame(
      rvar_id = active_rvar_id(),
      route_id = input$route_id,
      direction_id = as.integer(input$direction_id),
      trip_headsign = input$trip_headsign,
      route_long_name = input$route_long_name,
      route_type = as.integer(input$route_type),
      stringsAsFactors = FALSE
    )
    
    # Get stop sequence
    stop_seq <- generateStopSequenceFromNodes()
    
    # Update ssfs
    current_ssfs <- ssfs()
    
    # Remove existing entries if updating
    current_ssfs$rvar <- current_ssfs$rvar[current_ssfs$rvar$rvar_id != active_rvar_id(), ]
    current_ssfs$stop_seq <- current_ssfs$stop_seq[current_ssfs$stop_seq$rvar_id != active_rvar_id(), ]
    current_ssfs$shapes <- current_ssfs$shapes[current_ssfs$shapes$rvar_id != active_rvar_id(), ]
    
    # Add new data
    current_ssfs$rvar <- rbind(current_ssfs$rvar, new_rvar)
    if (nrow(stop_seq) > 0) {
      current_ssfs$stop_seq <- rbind(current_ssfs$stop_seq, stop_seq)
    }
    current_ssfs$shapes <- rbind(current_ssfs$shapes, new_shape_sf)
    
    ssfs(current_ssfs)
    
    # Update route variant selection choices
    updateSelectInput(session, "existing_rvar", choices = c("", current_ssfs$rvar$rvar_id))
    
    # Clear current inputs and sequence
    clearInputs()
    showNotification("Route saved successfully", type = "message")
  })
  
  # Clear all inputs function 
  clearInputs <- function() {
    updateTextInput(session, "rvar_id", value = "")
    updateTextInput(session, "route_id", value = "")
    updateTextInput(session, "route_long_name", value = "")
    updateSelectInput(session, "direction_id", selected = "0")
    updateTextInput(session, "trip_headsign", value = "")
    updateSelectInput(session, "route_type")
    
    current_sequence(data.frame(
      rvar_id = character(),
      stop_id = character(),
      stop_sequence = integer(),
      stop_name = character(),
      stringsAsFactors = FALSE
    ))
    
    route_points(data.frame(
      index = integer(),
      lng = numeric(),
      lat = numeric()
    ))
    
    route_nodes(data.frame(
      node_id = integer(),
      lng = numeric(),
      lat = numeric(),
      is_stop = logical(),
      stop_id = character(),
      stop_name = character(),
      index = integer(),
      stringsAsFactors = FALSE
    ))
    
    active_rvar_id(NULL)
    selected_point_index(NULL)
  }
  
  # Clear all button handler
  observeEvent(input$clear_all, {
    clearInputs()
  })
  
  # Update existing route variants dropdown
  observe({
    updateSelectInput(session, "existing_rvar",
                      choices = c("", ssfs()$rvar$rvar_id))
  })
  
  #   #   #
  #
  ##   CALENDAR MODULE--------
  #
  #   #   #
  
  # Calendar table display
  output$calendar_table <- renderDT({
    current_data <- ssfs()
    datatable(
      current_data$calendar,
      selection = 'single',
      rownames = FALSE,
      options = list(
        pageLength = 10,
        ordering = FALSE,
        dom = 't'
      )
    )
  })
  
  # Generate next default service_id
  get_next_service_id <- function() {
    current_data <- ssfs()
    if (nrow(current_data$calendar) == 0) {
      return("S1")
    }
    existing_ids <- current_data$calendar$service_id
    numeric_part <- as.integer(gsub("S", "", existing_ids))
    sprintf("S%d", max(numeric_part) + 1)
  }
  
  # Clear service form
  observeEvent(input$clear_service, {
    updateTextInput(session, "service_id", value = "")
    updateSelectInput(session, "monday", selected = 0)
    updateSelectInput(session, "tuesday", selected = 0)
    updateSelectInput(session, "wednesday", selected = 0)
    updateSelectInput(session, "thursday", selected = 0)
    updateSelectInput(session, "friday", selected = 0)
    updateSelectInput(session, "saturday", selected = 0)
    updateSelectInput(session, "sunday", selected = 0)
    updateDateInput(session, "start_date", value = "2000-01-01")
    updateDateInput(session, "end_date", value = "2099-12-31")
  })
  
  # Add new service
  observeEvent(input$add_service, {
    current_data <- ssfs()
    
    # Validate service_id
    service_id <- if (input$service_id == "") {
      get_next_service_id()
    } else {
      input$service_id
    }
    
    # Check if service_id already exists
    if (service_id %in% current_data$calendar$service_id) {
      showNotification("Service ID already exists. Please use a different ID.",
                       type = "warning")
      return()
    }
    
    # Validate dates
    start_date <- as.character(input$start_date)
    end_date <- as.character(input$end_date)
    
    if (start_date > end_date) {
      showNotification("Start date must be before or equal to end date", type = "warning")
      return()
    }
    
    # Create new service entry
    new_service <- data.frame(
      service_id = service_id,
      monday = as.integer(input$monday),
      tuesday = as.integer(input$tuesday),
      wednesday = as.integer(input$wednesday),
      thursday = as.integer(input$thursday),
      friday = as.integer(input$friday),
      saturday = as.integer(input$saturday),
      sunday = as.integer(input$sunday),
      start_date = start_date,
      end_date = end_date,
      stringsAsFactors = FALSE
    )
    
    # Add to calendar table
    current_data$calendar <- rbind(current_data$calendar, new_service)
    ssfs(current_data)
    
    # Clear form
    updateTextInput(session, "service_id", value = "")
    showNotification("Service added successfully", type = "message")
  })
  
  # Delete selected service
  observeEvent(input$delete_selected_service, {
    req(input$calendar_table_rows_selected)
    current_data <- ssfs()
    
    if (length(input$calendar_table_rows_selected) > 0) {
      current_data$calendar <- current_data$calendar[-input$calendar_table_rows_selected, ]
      ssfs(current_data)
      showNotification("Service deleted successfully", type = "message")
    }
  })
  
  #   #   #
  #
  ##   SPANS MODULE--------
  #
  #   #   #
  
  # Function to validate and format time string (from spans standalone app)
  format_time <- function(time_str) {
    # Remove any non-digit or non-colon characters
    clean_str <- gsub("[^0-9:]", "", time_str)
    
    # Split into components
    parts <- strsplit(clean_str, ":")[[1]]
    
    if (length(parts) == 1) {
      # Only hours provided
      hours <- as.numeric(parts[1])
      mins <- 0
      secs <- 0
    } else if (length(parts) == 2) {
      # Hours and minutes provided
      hours <- as.numeric(parts[1])
      mins <- as.numeric(parts[2])
      secs <- 0
    } else if (length(parts) == 3) {
      # Hours, minutes, and seconds provided
      hours <- as.numeric(parts[1])
      mins <- as.numeric(parts[2])
      secs <- as.numeric(parts[3])
    } else {
      return(NULL)  # Invalid format
    }
    
    # Validate ranges
    if (hours < 0 || hours > 30 ||
        mins < 0 || mins > 59 ||
        secs < 0 || secs > 59) {
      return(NULL)
    }
    
    # Format as HH:MM:SS
    sprintf("%02d:%02d:%02d", hours, mins, secs)
  }
  
  # Update rvar_id choices based on ssfs$rvar
  observe({
    current_data <- ssfs()
    rvar_choices <- if (nrow(current_data$rvar) > 0) {
      current_data$rvar$rvar_id
    } else {
      character(0)
    }
    updateSelectInput(session, "span_rvar_id", choices = c("", rvar_choices))
  })
  
  # Update service_id choices based on ssfs$calendar
  observe({
    current_data <- ssfs()
    service_choices <- if (nrow(current_data$calendar) > 0) {
      current_data$calendar$service_id
    } else {
      character(0)
    }
    updateSelectInput(session, "span_service_id", choices = c("", service_choices))
  })
  
  # Spans table display
  output$spans_table <- renderDT({
    current_data <- ssfs()
    datatable(
      current_data$span,
      selection = 'single',
      rownames = FALSE,
      options = list(
        pageLength = -1,
        ordering = FALSE,
        dom = 't'
      )
    )
  })
  
  # Clear span form
  observeEvent(input$clear_span, {
    updateSelectInput(session, "span_rvar_id", selected = "")
    updateSelectInput(session, "span_service_id", selected = "")
    updateTextInput(session, "first_dep", value = "05:00:00")
    updateTextInput(session, "last_dep", value = "23:00:00")
  })
  
  # Add new span
  observeEvent(input$add_span, {
    req(input$span_rvar_id, input$span_service_id)
    
    # Validate times
    first_dep <- format_time(input$first_dep)
    last_dep <- format_time(input$last_dep)
    
    if (is.null(first_dep) || is.null(last_dep)) {
      showNotification("Invalid time format. Use HH:MM:SS (00-30:00-59:00-59).",
                       type = "error")
      return()
    }
    
    if (first_dep >= last_dep) {
      showNotification("First departure must be before last departure", type = "warning")
      return()
    }
    
    current_data <- ssfs()
    
    # Check if combination already exists
    existing_span <- current_data$span[current_data$span$rvar_id == input$span_rvar_id &
                                         current_data$span$service_id == input$span_service_id, ]
    
    if (nrow(existing_span) > 0) {
      showNotification("This route variant and service ID combination already exists.",
                       type = "warning")
      return()
    }
    
    # Create new span entry
    new_span <- data.frame(
      rvar_id = input$span_rvar_id,
      service_id = input$span_service_id,
      first_dep = first_dep,
      last_dep = last_dep,
      stringsAsFactors = FALSE
    )
    
    # Add to spans table
    current_data$span <- rbind(current_data$span, new_span)
    ssfs(current_data)
    
    showNotification("Service span added successfully", type = "message")
  })
  
  # Delete selected span
  observeEvent(input$delete_selected_span, {
    req(input$spans_table_rows_selected)
    current_data <- ssfs()
    
    if (length(input$spans_table_rows_selected) > 0) {
      current_data$span <- current_data$span[-input$spans_table_rows_selected, ]
      ssfs(current_data)
      showNotification("Service span deleted successfully", type = "message")
    }
  })
  
  #   #   #
  #
  ##   HEADWAYS AND SPEEDS BY HOUR MODULE----------
  #
  #   #   #
  
  # Headways module server functions
  
  # Generate HSH table from span and rvar data
  generate_hsh_table <- function(span_data, rvar_data, default_headway) {
    if (nrow(span_data) == 0)
      return(data.frame())
    
    result <- data.frame()
    
    for (i in 1:nrow(span_data)) {
      # Extract hours from first_dep and last_dep
      first_dep_hour <- as.numeric(substr(span_data$first_dep[i], 1, 2))
      last_dep_hour <- as.numeric(substr(span_data$last_dep[i], 1, 2))
      
      # Generate sequence of hours
      hours <- first_dep_hour:last_dep_hour
      
      # Format hours as HH:00:00
      formatted_hours <- sapply(hours, function(h) {
        sprintf("%02d:00:00", h)
      })
      
      # Get route type and determine speed
      rvar_id <- span_data$rvar_id[i]
      route_type <- rvar_data$route_type[rvar_data$rvar_id == rvar_id]
      speed <- if (length(route_type) > 0 &&
                   route_type %in% c(1, 2, 12))
        40
      else
        20
      
      # Create data frame for this span
      span_hours <- data.frame(
        rvar_id = span_data$rvar_id[i],
        service_id = span_data$service_id[i],
        hour_dep = formatted_hours,
        headway = default_headway,
        speed = speed,
        stringsAsFactors = FALSE
      )
      
      result <- rbind(result, span_hours)
    }
    
    # Order by rvar_id, service_id, and hour_dep
    if (nrow(result) > 0) {
      result <- result[order(result$rvar_id, result$service_id, result$hour_dep), ]
    }
    
    return(result)
  }
  
  #create reactive value for editing state
  editing_hsh <- reactiveVal(FALSE)
  
  # Add this to make the editing state visible to the UI
  output$editing_hsh <- reactive({
    editing_hsh()
  })
  outputOptions(output, "editing_hsh", suspendWhenHidden = FALSE)
  
  #make the editing state available to the UI
  observe({
    updateQueryString(paste0("?editing_hsh=", editing_hsh()))
  })
  
  # Update rvar_id and service_id choices
  observe({
    current_data <- ssfs()
    
    # Update rvar_id choices
    rvar_choices <- if (nrow(current_data$rvar) > 0) {
      current_data$rvar$rvar_id
    } else {
      character(0)
    }
    updateSelectInput(session, "hsh_rvar_id", choices = c("", rvar_choices))
    
    # Update service_id choices
    service_choices <- if (nrow(current_data$calendar) > 0) {
      current_data$calendar$service_id
    } else {
      character(0)
    }
    updateSelectInput(session, "hsh_service_id", choices = c("", service_choices))
  })
  
  # Initialize HSH table
  observeEvent(input$initialize_hsh, {
    current_data <- ssfs()
    if (nrow(current_data$span) == 0) {
      showNotification("No span data available to initialize table", type = "warning")
      return()
    }
    
    # Validate that all rvar_ids in span exist in rvar table
    invalid_rvars <- setdiff(current_data$span$rvar_id, current_data$rvar$rvar_id)
    if (length(invalid_rvars) > 0) {
      showNotification(sprintf(
        "Some route variants in spans are not defined in routes: %s",
        paste(invalid_rvars, collapse = ", ")
      ),
      type = "error")
      return()
    }
    
    new_hsh <- generate_hsh_table(current_data$span,
                                  current_data$rvar,
                                  input$default_headway)
    
    current_data$hsh <- new_hsh
    ssfs(current_data)
    showNotification("Headways table initialized successfully", type = "message")
  })
  
  # Reactive value for editing state
  editing_hsh <- reactiveVal(FALSE)
  
  # Load and display HSH data
  observeEvent(input$load_hsh, {
    req(input$hsh_rvar_id, input$hsh_service_id)
    editing_hsh(FALSE)
  })
  
  # Render HSH table
  output$hsh_table <- renderDT({
    req(input$hsh_rvar_id, input$hsh_service_id)
    current_data <- ssfs()
    
    filtered_data <- current_data$hsh[current_data$hsh$rvar_id == input$hsh_rvar_id &
                                        current_data$hsh$service_id == input$hsh_service_id, ]
    
    # Create display data frame without rvar_id and service_id
    display_data <- filtered_data[, c("hour_dep", "headway", "speed")]
    
    datatable(
      display_data,
      selection = 'single',
      rownames = FALSE,
      options = list(
        pageLength = 24,
        ordering = TRUE,
        order = list(list(0, 'asc')),
        # Order by hour_dep (now index 0)
        dom = 't'
      ),
      colnames = c("Hour", "Headway (min)", "Speed (km/h)")
    )
  })
  
  # Add new row handler
  observeEvent(input$add_hsh_row, {
    current_data <- ssfs()
    
    # Get span data for validation
    span_data <- current_data$span[current_data$span$rvar_id == input$hsh_rvar_id &
                                     current_data$span$service_id == input$hsh_service_id, ]
    
    if (nrow(span_data) == 0) {
      showNotification("No span data found for this route variant and service", type = "error")
      return()
    }
    
    # Clear form inputs
    updateTextInput(session, "edit_hour_dep", value = "")
    updateNumericInput(session, "edit_headway", value = input$default_headway)
    
    # Get default speed based on route type
    route_type <- current_data$rvar$route_type[current_data$rvar$rvar_id == input$hsh_rvar_id]
    default_speed <- if (length(route_type) > 0 &&
                         route_type %in% c(1, 2, 12))
      40
    else
      20
    updateNumericInput(session, "edit_speed", value = default_speed)
    
    # Show edit form
    editing_hsh(TRUE)
  })
  
  # Save edits handler
  observeEvent(input$save_hsh_edit, {
    current_data <- ssfs()
    
    # Validate hour format is HH:00:00
    if (!grepl("^\\d{2}:00:00$", input$edit_hour_dep)) {
      showNotification("Invalid hour format. Use HH:00:00", type = "error")
      return()
    }
    
    # Get span data for validation
    span_data <- current_data$span[current_data$span$rvar_id == input$hsh_rvar_id &
                                     current_data$span$service_id == input$hsh_service_id, ]
    
    # Extract hours for comparison
    edit_hour <- as.numeric(substr(input$edit_hour_dep, 1, 2))
    first_hour <- as.numeric(substr(span_data$first_dep, 1, 2))
    last_hour <- as.numeric(substr(span_data$last_dep, 1, 2))
    
    # Validate hour is within span range
    if (edit_hour < first_hour || edit_hour > last_hour) {
      showNotification("Hour must be within span range", type = "error")
      return()
    }
    
    # Create new row
    new_row <- data.frame(
      rvar_id = input$hsh_rvar_id,
      service_id = input$hsh_service_id,
      hour_dep = input$edit_hour_dep,
      headway = input$edit_headway,
      speed = input$edit_speed,
      stringsAsFactors = FALSE
    )
    
    # Remove existing row if editing
    if (length(input$hsh_table_rows_selected) > 0) {
      filtered_data <- current_data$hsh[current_data$hsh$rvar_id == input$hsh_rvar_id &
                                          current_data$hsh$service_id == input$hsh_service_id, ]
      row_to_edit <- filtered_data[input$hsh_table_rows_selected, ]
      
      current_data$hsh <- current_data$hsh[!(
        current_data$hsh$rvar_id == input$hsh_rvar_id &
          current_data$hsh$service_id == input$hsh_service_id &
          current_data$hsh$hour_dep == row_to_edit$hour_dep
      ), ]
    }
    
    # Add new/updated row
    current_data$hsh <- rbind(current_data$hsh, new_row)
    current_data$hsh <- current_data$hsh[order(
      current_data$hsh$rvar_id,
      current_data$hsh$service_id,
      current_data$hsh$hour_dep
    ), ]
    
    ssfs(current_data)
    editing_hsh(FALSE)
    showNotification("Changes saved successfully", type = "message")
  })
  
  # Cancel edit handler
  observeEvent(input$cancel_hsh_edit, {
    editing_hsh(FALSE)
  })
  
  # Delete selected handler
  observeEvent(input$delete_selected_hsh, {
    req(input$hsh_table_rows_selected)
    current_data <- ssfs()
    
    filtered_data <- current_data$hsh[current_data$hsh$rvar_id == input$hsh_rvar_id &
                                        current_data$hsh$service_id == input$hsh_service_id, ]
    
    row_to_delete <- filtered_data[input$hsh_table_rows_selected, ]
    
    current_data$hsh <- current_data$hsh[!(
      current_data$hsh$rvar_id == input$hsh_rvar_id &
        current_data$hsh$service_id == input$hsh_service_id &
        current_data$hsh$hour_dep == row_to_delete$hour_dep
    ), ]
    
    ssfs(current_data)
    showNotification("Row deleted successfully", type = "message")
  })
  
  # Edit row handler
  observeEvent(input$edit_hsh_row, {
    req(input$hsh_table_rows_selected)
    current_data <- ssfs()
    filtered_data <- current_data$hsh[current_data$hsh$rvar_id == input$hsh_rvar_id &
                                        current_data$hsh$service_id == input$hsh_service_id, ]
    
    # Get values from selected row
    selected_row <- filtered_data[input$hsh_table_rows_selected, ]
    
    # Update form inputs with current values
    updateTextInput(session, "edit_hour_dep", value = selected_row$hour_dep)
    updateNumericInput(session, "edit_headway", value = selected_row$headway)
    updateNumericInput(session, "edit_speed", value = selected_row$speed)
    
    # Show edit form
    editing_hsh(TRUE)
  })
  
}

#APP----------------------------

#run the app
shinyApp(ui = ui, server = server)
