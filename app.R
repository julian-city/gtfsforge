#gtfsforge prototype

#last updated : 25 February 2025
#author : Julian Villafuerte Diaz
#created with the assistance of Claude, deepseek, and ChatGPT

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)     
library(purrr)     
library(stringr)    
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
  # CSS and JavaScript in the head
  tags$head(tags$style(
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
  ), tags$script(
    HTML(
      "
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
    "
    )
  )),
  
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
        titlePanel("Welcome to gtfsforge"),
        
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
                         "btn-success")
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
          
          h4("2. Draw line geometries"),
          p(
            "In the 'shapes' module:",
            tags$ul(
              tags$li("Create points on the map that define the path of your line"),
              tags$li("Provide a unique shape ID for each line"),
              tags$li(
                "Edit shapes by selecting them in the dropdown. In edit mode, you can add additional nodes between existing nodes of the line."
              ),
              tags$li(
                "NOTE : for bus and other routes that travel on the road network, you can go straight to the Routes module after defining stops and create your lines there. You can go back to the shapes module to edit the shapes for your bus routes after creating them initially in the Routes module."
              ),
              tags$li(
                "Limitation : it is not yet possible to delete nodes from an existing line."
              )
            )
          ),
          
          h4("3. Define the stop sequence and route details of each route variant"),
          p(
            "About the 'routes' module:",
            tags$ul(
              tags$li(
                "A route variant is a unique path or pattern that a transit service follows. For example, a route that has an inbound (e.g. north) and outbound (e.g. south) service consists of two route variants that must be defined in this module."
              ),
              tags$li(
                "Use this module to specify a unique stop sequence and route variant ID for each route variant, along with other information about the route such as the direction and the route name. Each route variant must be associated with a shape (line)."
              ),
              tags$li(
                "If you are creating bus or other routes that travel on the road network, you can create the route variant shape in this module with the 'generate route path on road network' functionality, which calculates the shortest path along the sequence of stops using OSRM."
              )
            )
          ),
          
          h4("4. Define service patterns"),
          p("In the 'calendar' module:", tags$ul(
            tags$li(
              "Specify which days of the week each service operates, as well as the date ranges for each service patterns."
            ),
            tags$li(
              "The table in this module is identical to the calendar table in gtfs and is passed on directly."
            )
          )),
          
          h4("5. Configure service spans"),
          p("In 'spans' module:", tags$ul(
            tags$li("Define operating hours for each route / service combination.")
          )),
          
          h4("6. Specify headways and speeds by hour in the headways module."),
          p("In 'headways' module:", tags$ul(
            tags$li(
              "After configuring service spans, initialize the headways and speeds by hour table in this module, then edit details by route variant, service pattern and hour."
            ),
            tags$li(
              "To create service gaps during the service period (for example, for a route that only operates at peak hours), you can simply delete the rows from this table for the hours during which the route should not run."
            )
          )),
          
          h4("7. When finished, return Home to create and export your gtfs below !")
        ),
        
        # Export gtfs
        wellPanel(
          h3("Export gtfs"),
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
          p(HTML("gtfsforge is an open-source app for creating and editing GTFS transit data developed by <a href='https://julian.city' target='_blank'>Julian Villafuerte Diaz</a>.")),
          p("This prototype was launched in February 2025 and is in active development. Please get in touch with your feedback and ideas for improvement !"),
          p(HTML("<a href='https://github.com/julian-city/gtfsforge' target='_blank'>GitHub</a>")),
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
    
    # Shapes Tab
    tabPanel("shapes", fluidPage(
      titlePanel("shapes"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selectedLine", "Select line to edit:", choices = NULL),
          actionButton("editLine", "Edit selected line"),
          actionButton("duplicateLine", "Duplicate selected line"),
          actionButton("deleteLine", "Delete selected line", class =
                         "btn-danger"),
          hr(),
          textInput("lineId", "Shape ID:", placeholder = "Enter ID for current shape"),
          actionButton("clear", "Clear current line", class = "btn-warning"),
          actionButton("addLine", "Save current line", class = "btn-success"),
          helpText(
            "To get started, enter a unique shape ID, draw your first line consisting of points on the map by clicking along your desired route, hit 'save', and repeat. Edit saved lines with the dropdown above.",
            tags$ul(
              tags$li("Click on the map to add points sequentially to your line."),
              tags$li("Press the Backspace key to remove the last point"),
              tags$li("When editing an existing line :", 
                      tags$ul(
                        tags$li("Click along an existing segment to add a point there."),
                        tags$li("Click directly on a point to select it, then click elsewhere to move it."),
                        tags$li("The end of a line closest to your click will be extended when adding points.")))
            ),
          ),
          hr(),
          helpText(
            "To create a line connecting stops along the road network, create the line in the Routes module. You can return to this module to edit it."
          )
          #actionButton("commit_shapes", "Save to Transit System",
          #              class="btn-primary")
          #downloadButton("download_shapes", "Download Shapes GeoJSON"),
          #hr(),
          #h4("Shapes in system:"),
          #verbatimTextOutput("linesInfo"),
          #h4("Current line coordinates:"),
          #verbatimTextOutput("coordsInfo")
        ),
        mainPanel(leafletOutput("shapes_map", height = "600px"))
      )
    )),
    
    # Routes Tab
    tabPanel("routes", fluidPage(
      titlePanel("routes"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          helpText(
            "Stops must be loaded into the model before creating routes. To create a route variant, provide route details, click on stops sequentially, and click 'Save Current Route'. You can edit saved route variant details and stop sequences with the below dropdown."
          ),
          selectInput("existing_rvar", "Edit existing route variant", choices = NULL),
          actionButton("load_rvar", "Load selected variant", class = "btn-info"),
          hr(),
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
          selectInput("rvar_shape_id", "Shape ID", choices = NULL),
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
          actionButton("save_route", "Save current route", class = "btn-success"),
          actionButton("clear_all", "Clear all", class = "btn-warning")#,
          #hr(),
          #helpText("When you have finished defining the routes of your model, click below."),
          #actionButton("commit_routes", "Save routes to model",
          #              class = "btn-primary")
          #hr(),
          #h4("Saved Route Variants:"),
          #verbatimTextOutput("saved_routes_info")
        ),
        mainPanel(fluidRow(
          column(8, leafletOutput("routes_map", height = "600px")),
          column(
            4,
            h4("Generate route path on road network"),
            textInput("stop_seq_shape_id", "Shape ID", ""),
            actionButton("create_shape", "Create new line", class = "btn-info"),
            hr(),
            h4("Stop Sequence"),
            DTOutput("selected_stops_table"),
            actionButton("delete_selected", "Delete Selected Stop", class = "btn-danger")
            
          )
        ))
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
        shape_id = character(),
        geometry = st_sfc(crs = 4326),
        stringsAsFactors = FALSE
      ),
      rvar = data.frame(
        rvar_id = character(),
        route_id = character(),
        direction_id = integer(),
        trip_headsign = character(),
        shape_id = character(),
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
      colors <- if(!is.null(highlight_ids)) {
        ifelse(current_data$stops$stop_id %in% highlight_ids, "red", "blue")
      } else {
        "blue"
      }
      
      proxy <- proxy %>%
        addCircleMarkers(
          data = current_data$stops,
          radius = marker_size,
          color = colors,
          stroke = FALSE,
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
  #   HOME MODULE
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
      
      ssfs(mtlmileend_ssfs)
      
      showNotification("STM Mile-End bus network loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading STM Mile-End bus network:", e$message),
                       type = "error")
    })
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
      
      current_ssfs$rvar <-
        current_ssfs$rvar %>% as_tibble() %>%
        mutate(agency_id = "STM", .before = route_long_name) %>%
        mutate(route_short_name = route_id, .before = route_long_name) %>%
        mutate(direction_id = as.integer(direction_id),
               route_type = as.integer(route_type))
      
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
               end_date = as.Date(start_date))
      
      current_ssfs$span <-
        current_ssfs$span %>% as_tibble()
      
      current_ssfs$hsh <-
        current_ssfs$hsh %>% as_tibble()
      
      current_gtfs <- ssfs_to_gtfs(current_ssfs)
      
      write_gtfs(current_gtfs, file)
    }
  )
  
  #   #   #
  #
  #   STOPS MODULE
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
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addBaseMaps() %>%
      setView(lng = -73.567,
              lat = 45.5017,
              zoom = 10) %>%
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
          color = "red",
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
  #   SHAPES MODULE
  #
  #   #   #
  
  #helper function for shapes in editing mode
  updateShapesMapWithEditing <- function() {
    current_data <- ssfs()
    current_points <- points()
    
    # Get a leaflet proxy without clearing everything first
    proxy <- leafletProxy("shapes_map")
    
    # Clear specific groups but NOT all markers
    proxy %>% 
      clearGroup("shapes") %>%
      clearGroup("stops") %>%
      clearGroup("current_line")
    
    # Add shapes layer
    if(!is.null(current_data$shapes) && nrow(current_data$shapes) > 0) {
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
    
    # Add stops layer
    if(!is.null(current_data$stops) && nrow(current_data$stops) > 0) {
      marker_size <- calculateMarkerSize(current_zoom())
      proxy <- proxy %>%
        addCircleMarkers(
          data = current_data$stops,
          radius = marker_size,
          color = "blue",
          stroke = FALSE,
          fillOpacity = 0.7,
          layerId = ~stop_id,
          popup = ~paste("ID:", stop_id, "<br>Name:", stop_name),
          group = "stops"
        )
    }
    
    # Add editing points last (on top)
    if (nrow(current_points) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          lng = current_points$lng,
          lat = current_points$lat,
          group = "current_line",
          radius = 5,
          color = ifelse(1:nrow(current_points) == selected_point_index(), 
                         "yellow", "red"),
          layerId = paste0("point_", 1:nrow(current_points)),
          options = if (editing_mode()) 
            markerOptions(draggable = TRUE)
          else 
            markerOptions(draggable = FALSE)
        )
      
      if (nrow(current_points) > 1) {
        proxy <- proxy %>%
          addPolylines(
            lng = current_points$lng,
            lat = current_points$lat,
            group = "current_line",
            color = "red",
            weight = 3
          )
      }
    }
    
    return(proxy)
  }
  
  #reactive value for the points that are being drawn on the map (to make up an eventual line)
  points <- reactiveVal(data.frame(
    index = numeric(),
    lng = numeric(),
    lat = numeric()
  ))
  editing_mode <- reactiveVal(FALSE)
  selected_line_id <- reactiveVal(NULL)
  moving_point_index <- reactiveVal(NULL)
  selected_point_index <- reactiveVal(NULL)
  
  # Initialize shapes map
  output$shapes_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addBaseMaps() %>%
      setView(lng = -73.567,
              lat = 45.5017,
              zoom = 10) %>%
      
      #the below from shapes_shiny_v250202_03.R
      addScaleBar() %>%
      htmlwidgets::onRender(
        "
      function(el, x) {
        this.on('zoomend', function(e) {
          Shiny.setInputValue('shapes_map_zoom', this.getZoom());
        });
      }
    "
      )
  })
  
  #observer to handle input for selected line (CONFIRM WHAT THIS DOES)
  observe({
    current_data <- ssfs()
    if (!is.null(current_data$shapes)) {
      updateSelectInput(session, "selectedLine", choices = current_data$shapes$shape_id)
    } else {
      updateSelectInput(session, "selectedLine", choices = character(0))
    }
  })
  
  #observer for current level of zoom
  observeEvent(input$shapes_map_zoom, {
    current_zoom(input$shapes_map_zoom)
    
    # If in editing mode, redraw the editing points
    current_pts <- points()
    if (nrow(current_pts) > 0 && editing_mode()) {
      # Force immediate redraw
      isolate({
        updateShapesMapWithEditing()
      })
    }
  })
  
  #observer for when backspace to remove the last point in the sequence of current points for the line
  observeEvent(input$backspace_pressed, {
    if (nrow(points()) > 0) {
      current_points <- points()
      points(current_points[-nrow(current_points), ])
      updateShapesMapWithEditing()
    }
  })
  
  #very important observer to handle map clicks based on where they occur in relation to existing points
  observeEvent(input$shapes_map_click, {
    click <- input$shapes_map_click
    current_points <- points()
    
    if (editing_mode()) {
      if (!is.null(selected_point_index())) {
        # Move the selected point to the new location
        current_points[selected_point_index(), c("lng", "lat")] <- c(click$lng, click$lat)
        points(current_points)
        selected_point_index(NULL)  # Reset the selected point index
      } else {
        # Check if the click is on an existing point
        distances_to_points <- sqrt((current_points$lng - click$lng) ^ 2 +
                                      (current_points$lat - click$lat) ^
                                      2)
        closest_point_index <- which.min(distances_to_points)
        
        if (distances_to_points[closest_point_index] < calculateThreshold(current_zoom())) {
          selected_point_index(closest_point_index)  # Select this point for movement
        } else {
          # If we have at least 2 points, check for adding points along line or at ends
          if (nrow(current_points) >= 2) {
            # First check if click is near any line segment
            point_added <- FALSE
            
            for (i in 1:(nrow(current_points) - 1)) {
              # Get segment endpoints
              p1 <- current_points[i, ]
              p2 <- current_points[i + 1, ]
              
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
                # Create new point with intermediate index
                new_point <- data.frame(
                  index = (
                    current_points$index[i] + current_points$index[i + 1]
                  ) / 2,
                  lng = click$lng,
                  lat = click$lat
                )
                
                # Insert new point between segment endpoints
                new_points <- rbind(current_points[1:i, ], new_point, current_points[(i +
                                                                                        1):nrow(current_points), ])
                
                # Reindex all points to maintain sequence
                new_points$index <- 1:nrow(new_points)
                points(new_points)
                point_added <- TRUE
                break
              }
            }
            
            # If point wasn't added along a segment, check for adding at ends
            if (!point_added) {
              # Calculate distances to first and last points
              dist_to_first <- sqrt((current_points$lng[1] - click$lng) ^
                                      2 +
                                      (current_points$lat[1] - click$lat) ^
                                      2
              )
              dist_to_last <- sqrt((current_points$lng[nrow(current_points)] - click$lng) ^
                                     2 +
                                     (current_points$lat[nrow(current_points)] - click$lat) ^
                                     2
              )
              
              if (dist_to_first < dist_to_last) {
                # Add point at beginning and reverse sequence
                new_point <- data.frame(
                  index = 0,
                  lng = click$lng,
                  lat = click$lat
                )
                # Combine points
                new_points <- rbind(new_point, current_points)
                # Reindex all points in reverse order
                new_points$index <- nrow(new_points):1
                # Sort by new index to maintain correct sequence
                new_points <- new_points[order(new_points$index), ]
                points(new_points)
              } else {
                # Add point at end
                new_point <- data.frame(
                  index = nrow(current_points) + 1,
                  lng = click$lng,
                  lat = click$lat
                )
                new_points <- rbind(current_points, new_point)
                points(new_points)
              }
            }
          } else {
            # If less than 2 points, simply add the new point
            new_point <- data.frame(
              index = nrow(current_points) + 1,
              lng = click$lng,
              lat = click$lat
            )
            new_points <- rbind(current_points, new_point)
            points(new_points)
          }
        }
      }
    } else {
      # Not in editing mode - add points sequentially
      new_point <- data.frame(
        index = nrow(current_points) + 1,
        lng = click$lng,
        lat = click$lat
      )
      new_points <- rbind(current_points, new_point)
      points(new_points)
    }
    
    updateShapesMapWithEditing()
  })
  
  #function for calculating threshold distance from an existing point which brings
  #a point into edit mode
  calculateThreshold <- function(zoom) {
    # Base threshold at zoom level 10 is 0.01
    base_threshold <- 0.01
    # Adjust threshold exponentially based on zoom difference from base level
    # Smaller number when zoomed in, larger when zoomed out
    adjusted_threshold <- base_threshold * (2 ^ (10 - zoom))
    # Clamp the threshold to reasonable limits
    return(min(max(adjusted_threshold, 0.0001), 0.1))
  }
  
  # Update shapes map content
  observe({
    current_data <- ssfs()
    
    # Update map with all data
    updateMapWithSsfsData(
      "shapes_map", 
      current_data,
      show_stops = TRUE,  # Show stops on top of shapes
      show_shapes = TRUE
    )
  })
  
  #observer that updates map if shapes are added to ssfs$shapes...
  #(ensure this does not conflict with the way updateMapWithSsfsData works elsewhere in this program)
  observe({
    current_data <- ssfs()
    
    # Only proceed if there are shapes
    if (!is.null(current_data$shapes) &&
        nrow(current_data$shapes) > 0) {
      proxy <- leafletProxy("shapes_map") %>%
        clearGroup("saved_lines")
      
      # Add all shapes from ssfs$shapes
      for (i in 1:nrow(current_data$shapes)) {
        line_coords <- st_coordinates(current_data$shapes$geometry[i])
        proxy <- proxy %>%
          addPolylines(
            lng = line_coords[, 1],
            lat = line_coords[, 2],
            group = "saved_lines",
            color = "blue",
            weight = 2,
            opacity = 0.8
          )
      }
    }
  })
  
  #handler for editing lines
  observeEvent(input$editLine, {
    req(input$selectedLine)
    current_data <- ssfs()
    selected_line <- current_data$shapes[current_data$shapes$shape_id == input$selectedLine, ]
    coords <- st_coordinates(selected_line$geometry)
    # Create data frame with all required columns including index
    points(data.frame(
      index = 1:nrow(coords),
      # Add sequential index
      lng = coords[, 1],
      lat = coords[, 2]
    ))
    updateTextInput(session, "lineId", value = input$selectedLine)
    editing_mode(TRUE)
    selected_line_id(input$selectedLine)
    updateShapesMapWithEditing()
  })
  
  #Observer and map update action for duplicating lines
  observeEvent(input$duplicateLine, {
    req(input$selectedLine)
    current_data <- ssfs()
    selected_line <- current_data$shapes[current_data$shapes$shape_id == input$selectedLine, ]
    new_id <- paste0(input$selectedLine, "_copy")
    new_line <- selected_line
    new_line$shape_id <- new_id
    
    current_data$shapes <- rbind(current_data$shapes, new_line)
    ssfs(current_data)
    updateShapesMapWithEditing()
  })
  
  #Observer handler for deleting lines from shapes
  observeEvent(input$deleteLine, {
    req(input$selectedLine)
    current_data <- ssfs()
    current_data$shapes <- current_data$shapes[current_data$shapes$shape_id != input$selectedLine, ]
    ssfs(current_data)
    
    updateShapesMapWithEditing()
  })
  
  #add line to shapes
  observeEvent(input$addLine, {
    req(input$lineId)
    if (nrow(points()) >= 2) {
      coords_matrix <- as.matrix(points()[, c("lng", "lat")])
      line_feature <- st_linestring(coords_matrix)
      new_line_sf <- st_sf(shape_id = input$lineId,
                           geometry = st_sfc(line_feature, crs = 4326))
      
      current_data <- ssfs()
      if (is.null(current_data$shapes)) {
        #should this be current_data$shapes or ssfs$shapes ?
        current_data$shapes <- new_line_sf
        
      } else if (editing_mode()) {
        #in integrated_transit_app it's current_data$shapes
        current_data$shapes <- rbind(current_data$shapes[current_data$shapes$shape_id != selected_line_id(), ], new_line_sf)
        editing_mode(FALSE)
        selected_line_id(NULL)
      } else {
        if (input$lineId %in% current_data$shapes$shape_id) {
          showNotification("Line ID already exists. Please use a different ID.",
                           type = "warning")
          return()
        }
        current_data$shapes <- rbind(current_data$shapes, new_line_sf)
      }
      
      ssfs(current_data)
      points(data.frame(
        index = numeric(),
        lng = numeric(),
        lat = numeric()
      ))
      updateTextInput(session, "lineId", value = "")
      updateShapesMapWithEditing()
      showNotification(paste("Line", input$lineId, "added to SF object"), type = "message")
    } else {
      showNotification("Please add at least 2 points to create a line", type = "warning")
    }
  })
  
  observeEvent(input$clear, {
    points(data.frame(
      index = numeric(),
      lng = numeric(),
      lat = numeric()
    ))
    editing_mode(FALSE)
    selected_line_id(NULL)
    selected_point_index(NULL)
    updateShapesMapWithEditing()
  })
  
  
  #   #   #
  #
  #   ROUTES MODULE
  #
  #   #   #
  
  #reactive value specific to the routes module
  #rvar_table <- reactiveVal(data.frame(
  #  rvar_id = character(),
  #  route_id = character(),
  #  direction_id = integer(),
  #  trip_headsign = character(),
  #  shape_id = character(),
  #  route_long_name=character(),
  #  route_type=integer(),
  #  stringsAsFactors = FALSE
  #))
  
  #stop_seq_table <- reactiveVal(data.frame(
  #  rvar_id = character(),
  #  stop_id = character(),
  #  stop_sequence = integer(),
  #  stop_name = character(),
  #  stringsAsFactors = FALSE
  #))
  
  current_sequence <- reactiveVal(
    data.frame(
      rvar_id = character(),
      stop_id = character(),
      stop_sequence = integer(),
      stop_name = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Initialize map with consistent style
  output$routes_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addBaseMaps() %>%
      setView(lng = -73.567,
              lat = 45.5017,
              zoom = 10) %>%
      htmlwidgets::onRender(
        "
        function(el, x) {
          this.on('zoomend', function(e) {
            Shiny.setInputValue('routes_map_zoom', this.getZoom());
          });
        }
      "
      )
  })
  
  # Update zoom level when map is zoomed
  observeEvent(input$routes_map_zoom, {
    current_zoom(input$routes_map_zoom)
  })
  
  # Update routes map content
  observe({
    curr_seq <- current_sequence()
    current_data <- ssfs()
    
    # Update map highlighting stops in the current sequence
    updateMapWithSsfsData(
      "routes_map", 
      current_data,
      highlight_ids = curr_seq$stop_id,  # Highlight stops in sequence
      show_stops = TRUE,
      show_shapes = TRUE
    )
  })
  
  # Handle map clicks
  observeEvent(input$routes_map_marker_click, {
    req(input$rvar_id)
    click <- input$routes_map_marker_click
    
    #to inform stops_data (correct to do it this way ?)
    current_data <- ssfs()
    stops_data <- current_data$stops
    
    if (!is.null(click)) {
      clicked_stop <- stops_data[stops_data$stop_id == click$id, ]
      curr_seq <- current_sequence()
      
      # Only add if stop isn't already in sequence
      if (!click$id %in% curr_seq$stop_id) {
        new_stop <- data.frame(
          rvar_id = input$rvar_id,
          stop_id = clicked_stop$stop_id,
          stop_sequence = nrow(curr_seq) + 1,
          stop_name = clicked_stop$stop_name,
          stringsAsFactors = FALSE
        )
        
        current_sequence(rbind(curr_seq, new_stop))
      }
    }
  })
  
  # Render stop sequence table with reordering
  #DRAG AND DROP REORDERING NOT FUNCTIONAL. Otherwise, OK.
  output$selected_stops_table <- renderDT({
    req(current_sequence())
    
    datatable(
      current_sequence(),
      selection = 'single',
      rownames = FALSE,
      extensions = 'RowReorder',
      options = list(
        pageLength = -1,
        rowReorder = list(
          dataSrc = which(names(current_sequence()) == "stop_sequence") - 1,
          selector = 'td:first-child'
        ),
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = 0:1),
          # Hide first two columns (rvar_id and stop_id)
          list(visible = TRUE, targets = 2:3)    # Show last two columns (stop_sequence and stop_name)
        )
      ),
      callback = JS(
        "
      table.on('row-reorder', function(e, diff, edit) {
        Shiny.setInputValue('stops_reorder_info',
          {rows: diff.map(d => ({old: d.oldPosition, new: d.newPosition}))},
          {priority: 'event'}
        );
      });
    "
      )
    )
  })
  
  # Handle table reordering with simplified logic
  #DOES NOT WORK AS INTENDED, revise
  observeEvent(input$stops_reorder_info, {
    info <- input$stops_reorder_info
    curr_seq <- current_sequence()
    
    if (!is.null(info$rows) && length(info$rows) > 0) {
      # Create a vector of the current positions
      positions <- seq_len(nrow(curr_seq))
      
      # Process each move one at a time
      for (move in info$rows) {
        old_pos <- move$old + 1  # Convert from 0-based to 1-based indexing
        new_pos <- move$new + 1
        
        # Create the new ordering
        if (old_pos < new_pos) {
          # Moving down
          positions <- append(positions[-old_pos], positions[old_pos], after = new_pos - 1)
        } else {
          # Moving up
          positions <- append(positions[-old_pos], positions[old_pos], after = new_pos - 1)
        }
      }
      
      # Reorder the data frame using the final positions
      curr_seq <- curr_seq[positions, ]
      
      # Update stop_sequence to match new order
      curr_seq$stop_sequence <- seq_len(nrow(curr_seq))
      
      # Update the reactive value
      current_sequence(curr_seq)
    }
  })
  
  # Delete selected stop
  observeEvent(input$delete_selected, {
    req(input$selected_stops_table_rows_selected)
    curr_seq <- current_sequence()
    
    # Remove selected row
    curr_seq <- curr_seq[-input$selected_stops_table_rows_selected, ]
    
    # Resequence remaining stops
    if (nrow(curr_seq) > 0) {
      curr_seq$stop_sequence <- 1:nrow(curr_seq)
    }
    
    current_sequence(curr_seq)
  })
  
  # Save current route
  observeEvent(input$save_route, {
    req(
      input$rvar_id,
      input$route_id,
      input$direction_id,
      input$trip_headsign,
      input$rvar_shape_id,
      input$route_long_name,
      input$route_type
    )
    
    # Create new rvar entry
    new_rvar <- data.frame(
      rvar_id = input$rvar_id,
      route_id = input$route_id,
      direction_id = input$direction_id,
      trip_headsign = input$trip_headsign,
      shape_id = input$rvar_shape_id,
      route_long_name = input$route_long_name,
      route_type = input$route_type,
      stringsAsFactors = FALSE
    )
    
    # Update tables
    current_ssfs <- ssfs()
    current_rvar <- current_ssfs$rvar
    current_stop_seq <- current_ssfs$stop_seq
    #current_rvar <- rvar_table() #vestigal
    #current_stop_seq <- stop_seq_table() #vestigal
    
    # Remove existing entries if updating
    current_rvar <- current_rvar[current_rvar$rvar_id != input$rvar_id, ]
    current_stop_seq <- current_stop_seq[current_stop_seq$rvar_id != input$rvar_id, ]
    
    # Add new data
    current_ssfs$rvar <- rbind(current_rvar, new_rvar)
    current_ssfs$stop_seq <- rbind(current_stop_seq, current_sequence())
    
    ssfs(current_ssfs)
    
    # Update route variant selection choices
    updateSelectInput(session,
                      "existing_rvar",
                      choices = c("", ssfs()$rvar$rvar_id))
    
    # Clear current inputs and sequence
    clearInputs()
  })
  
  # Create shape from stop sequence
  observeEvent(input$create_shape, {
    req(input$stop_seq_shape_id)
    req(nrow(current_sequence()) >= 2)  # Need at least 2 stops for a shape
    
    tryCatch({
      # Get current stop sequence and create shape
      current_stop_seq_table <- current_sequence()
      current_ssfs <- ssfs()
      current_stops <- current_ssfs$stops
      
      current_stop_seq_shape <-
        osrmRoute(
          loc = current_stop_seq_table %>%
            select(stop_id) %>%
            left_join(current_stops, by = "stop_id") %>%
            st_as_sf(),
          overview = "full"
        )
      
      # Process the shape and add shape_id
      new_shape <- current_stop_seq_shape %>%
        as_tibble() %>%
        select(geometry) %>%
        st_as_sf() %>%
        mutate(shape_id = input$stop_seq_shape_id,
               .before = geometry)
      
      # Add to shapes table
      current_shapes <- current_ssfs$shapes
      
      # Check if shape_id already exists
      if (input$stop_seq_shape_id %in% current_shapes$shape_id) {
        showNotification("Shape ID already exists. Please use a different ID.",
                         type = "warning")
        return()
      }
      
      current_ssfs$shapes <- rbind(current_shapes, new_shape)
      
      ssfs(current_ssfs)
      
      showNotification(sprintf(
        "Shape '%s' created successfully",
        input$stop_seq_shape_id
      ),
      type = "message")
      
      # Clear shape_id input
      updateTextInput(session, "stop_seq_shape_id", value = "")
      
    }, error = function(e) {
      showNotification(sprintf("Error creating shape: %s", e$message), type = "error")
    })
  })
  
  # Clear all inputs and current sequence
  clearInputs <- function() {
    updateTextInput(session, "rvar_id", value = "")
    updateTextInput(session, "route_id", value = "")
    updateTextInput(session, "route_long_name", value = "")
    updateSelectInput(session, "direction_id", selected = "0")
    updateTextInput(session, "trip_headsign", value = "")
    updateSelectInput(session, "rvar_shape_id")
    updateSelectInput(session, "route_type")
    current_sequence(
      data.frame(
        rvar_id = character(),
        stop_id = character(),
        stop_sequence = integer(),
        stop_name = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  observeEvent(input$clear_all, {
    clearInputs()
  })
  
  # Load existing route variant
  observeEvent(input$load_rvar, {
    req(input$existing_rvar)
    
    current_ssfs <- ssfs()
    
    # Get rvar data
    selected_rvar <- current_ssfs$rvar[current_ssfs$rvar$rvar_id == input$existing_rvar, ]
    
    # Update inputs
    updateTextInput(session, "rvar_id", value = selected_rvar$rvar_id)
    updateTextInput(session, "route_id", value = selected_rvar$route_id)
    updateTextInput(session, "route_long_name", value = selected_rvar$route_long_name)
    updateSelectInput(session, "direction_id", selected = selected_rvar$direction_id)
    updateTextInput(session, "trip_headsign", value = selected_rvar$trip_headsign)
    updateSelectInput(session, "rvar_shape_id", selected = selected_rvar$shape_id)
    updateSelectInput(session, "route_type", selected = selected_rvar$route_type)
    
    # Load stop sequence
    current_sequence(current_ssfs$stop_seq[current_ssfs$stop_seq$rvar_id == input$existing_rvar, ])
  })
  
  # Update existing route variants dropdown
  observe({
    updateSelectInput(session,
                      "existing_rvar",
                      choices = c("", ssfs()$rvar$rvar_id)) #DOES DOUBLE INDEX $ $ OF REACTIVE VALUE WORK ?
  })
  
  # Update shape_id dropdown
  observe({
    current_ssfs <- ssfs()
    current_shapes <- current_ssfs$shapes
    
    updateSelectInput(session,
                      "rvar_shape_id",
                      choices = c("", current_shapes$shape_id))
  })
  
  # Display saved routes info
  #output$saved_routes_info <- renderText({
  #  current_rvar <- rvar_table()
  #  if(nrow(current_rvar) > 0) {
  #    paste(apply(current_rvar, 1, function(x) {
  #      sprintf("%s (%s)", x["rvar_id"], x["trip_headsign"])
  #    }), collapse = "\n")
  #  } else {
  #    "No routes saved"
  #  }
  #})
  
  #commit route information to ssfs
  #observeEvent(input$commit_routes,{
  #  if(nrow(ssfs()$rvar)>0 & nrow(ssfs()$stop_seq) > 0){
  #    current_ssfs <- ssfs()
  #    current_rvar <- rvar_table()
  #    current_stop_seq <- stop_seq_table()
  #
  #    current_ssfs$rvar <- current_rvar
  #    current_ssfs$stop_seq <- current_stop_seq
  #
  #    ssfs(current_ssfs)
  #
  #    showNotification(
  #      "Routes successfully updated in transit system model",
  #      type="message"
  #    )
  #  }else{
  #    showNotification("No route information to add to transit model",
  #                     type="warning")
  #  }
  #})
  
  #   #   #
  #
  #   CALENDAR MODULE
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
  #   SPANS MODULE
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
  #   HEADWAYS AND SPEEDS BY HOUR MODULE
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
