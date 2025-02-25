#create sample networks

#last updated : February 25, 2025

library(tidyverse)
library(sf)
library(data.table)
library(gtfstools)

#gtfs to ssfs function--------------------------

#a function that converts gtfs to a simplified speed and frequency specification
#a new data structure that is easy to manipulate and to reconvert into gtfs
#without major sacrifice to representativeness of reality

gtfs_to_ssfs <- function(gtfs,
                         routes = NULL,
                         max_date = NULL) {
  #THREE parameters
  #gtfs must be a gtfs imported by gtfstools : an object with class "dt_gtfs","gtfs","list"
  #it must contain the following tables, generally conforming with the fields that are
  #required or conditionally required in the global standard as per the gtfs.org documentation
  #plus shapes
  #(details here: https://gtfs.org/documentation/schedule/reference/)
  
  #gtfs$agency : agency_id (chr), agency_name (chr), agency_url (chr), agency_timezone (chr)
  #gtfs$calendar : all fields listed and required in documentation linked above
  #gtfs$routes : route_id (chr),agency_id (chr),route_short_name(chr), route_long_name(chr),route_type(int)
  #gtfs$shapes : shape_id (chr), shape_pt_lat (dbl),shape_pt_lon (dbl), shape_pt_sequence(int)
  #gtfs$stops : stop_id (chr), stop_name(chr), stop_lat(dbl),stop_lon(dbl)
  #gtfs$stop_times : trip_id(chr),arrival_time(chr),departure_time(chr),stop_id(chr),stop_sequence(int)
  #gtfs$trips : route_id(chr),service_id(chr),trip_id(chr),shape_id(chr)
  
  #route must be a character string or vector. Can include any number of specified routes.
  #max_date, if specified, will return a ssfs which describes the service in the week
  #PRECEEDING the specified date. Must be date (example : max_date=as.date("2024-10-16"))
  
  if (is.null(routes)) {
    route_service_ids <-
      gtfs$trips %>% as_tibble() %>%
      select(service_id) %>%
      distinct() %>%
      pull(service_id) #this turns this into a vector -- wow!
    
    #routes is frequently used in the script, so assigning this with all the routes
    #is the simplest thing to do
    routes <- gtfs$routes %>% pull(route_id)
  } else{
    route_service_ids <-
      gtfs$trips %>% as_tibble() %>%
      filter(route_id %in% routes) %>%
      select(service_id) %>%
      distinct() %>%
      pull(service_id) #this turns this into a vector -- wow!
  }
  #we only want the trips associated with service ids that are associated with the last states of service
  #for each day of the week
  
  day <- c("monday",
           "tuesday",
           "wednesday",
           "thursday",
           "friday",
           "saturday",
           "sunday")
  
  if (!is.null(max_date)) {
    min_date <- max_date - days(7)
    
    route_calendar <-
      gtfs$calendar %>% as_tibble() %>%
      #only include service ids associated with the route(s) of interest
      filter(service_id %in% route_service_ids) %>%
      filter(start_date < max_date &
               end_date > min_date)
    #these conditions could be questioned and revised,
    #but at first glance they seem solid enough
  } else {
    #if max_date is NULL, then we assign the last day of service described in the gtfs
    #as the max date
    max_date <-
      gtfs$calendar %>% as_tibble() %>%
      summarise(max_date = max(end_date)) %>%
      pull(max_date)
    
    min_date <- max_date - days(7)
    
    route_calendar <-
      gtfs$calendar %>% as_tibble() %>%
      #only include service ids associated with the route(s) of interest
      filter(service_id %in% route_service_ids) %>%
      filter(start_date < max_date &
               end_date > min_date)
  }
  
  #we want to create a table specifying, for each day of the week, what service ids
  #are active on the last day of the service period
  #(OR of the last day of the specified date range if this argument is provided
  #this is something that will need to be added in another instance)
  
  #initialize the table
  
  service_ids_byday <- list()
  
  for (i in day) {
    service_ids_byday[[i]] <-
      route_calendar %>%
      filter(!!sym(i) == 1) %>% # this enables us to index the column / vector based on i (day of week)
      #filter(end_date == max(end_date)) %>% #probably vestigal since even when max_date=NULL
      #the last 7 days are identified above and used to filter service_ids.
      #this generated useless error messages when routes only included weekday routes for example
      select(service_id)
  }
  
  #we can now override route_service_ids with another one based on the above
  #which only includes the latest by day of week
  
  route_service_ids <- service_ids_byday %>%
    purrr::map_df( ~ .x) %>%    # Combine all data frames into one
    distinct(service_id) %>%    # Get distinct service_id
    pull(service_id)            # Convert to a vector
  
  #the trips that interest us
  
  trips <-
    gtfs$trips %>% as_tibble() %>%
    filter(route_id %in% routes, service_id %in% route_service_ids)
  
  #force add direction_id if it is absent from trips
  
  trips_colnames <- trips %>% colnames()
  
  if (!"direction_id" %in% trips_colnames) {
    trips$direction_id <- 0
  }
  
  #define rvar-------------------------------------
  
  #gathering route info : useful for assigning trip_headsign if it's not
  #present in trips (using route_long_name) and then later on for finishing the
  #definition of rvar
  
  route_info <-
    gtfs$routes %>% as_tibble() %>%
    filter(route_id %in% routes) %>%
    select(route_id,
           agency_id,
           route_short_name,
           route_long_name,
           route_type)
  
  #identifying distinct variant types within the trips of interest
  
  rvar_to_stop_seq <-
    gtfs$stop_times %>% as_tibble() %>%
    filter(trip_id %in% trips$trip_id) %>%
    select(trip_id, stop_id, stop_sequence) %>%
    left_join(trips %>%
                select(route_id, trip_id, direction_id), by = "trip_id") %>%
    group_by(route_id, trip_id, direction_id) %>%
    summarise(stop_pattern = list(data.frame(stop_id, stop_sequence))) %>% #stop_id & stop_sequence pattern of each trip
    ungroup() %>%
    group_by(route_id, direction_id, stop_pattern) %>%
    summarise(trip_ids = list(c(trip_id)), #trip ids by unique stop_id & stop_sequence pattern
              count = n()) %>%
    ungroup() %>%
    group_by(route_id, direction_id) %>%
    arrange(-count) %>% #so that the primary rvar_id by direction receives suffix _1
    #row number takes into account position within the group (direction_id) so it is possible to define distinct rvar_id
    mutate(rvar_id = paste0(
      as.character(route_id),
      "_",
      as.character(direction_id),
      "_",
      row_number()
    )) %>%
    ungroup()
  
  trip_id_to_rvar_id <-
    rvar_to_stop_seq %>%
    select(trip_ids, rvar_id) %>%
    unnest(trip_ids) %>% #lengthens the tibble for a 1 to 1 association of trip_id to rvar_id
    rename(trip_id = trip_ids)
  
  #defining calendar : service id consolidation--------
  
  #consolidating services that occur on the same day of the week
  service_combos <-
    service_ids_byday %>%
    map( ~ .x$service_id) %>%    # Extract service_ids for each day
    enframe(name = "day", value = "service_combo") %>%  # Convert to a tibble with day and combination
    group_by(service_combo) %>%
    summarise(days = list(day)) %>% #identify days of week associated with each service combo
    mutate(service_combo_id = paste0("S", row_number())) #defining the service combo id
  
  service_combo_to_service_id <-
    service_combos %>%
    select(service_combo, service_combo_id) %>%
    unnest(service_combo) %>%
    rename(service_id = service_combo)
  #this can be used for rewriting service ids
  
  #service_combo_to_day <-
  #  service_combos %>%
  #  select(service_combo_id,days) %>%
  #  unnest(days) %>%
  #  rename(day=days)
  #this could be used for writing the calendar
  
  calendar <-
    route_calendar %>%
    left_join(service_combo_to_service_id, by = "service_id") %>%
    select(service_combo_id, start_date, end_date) %>%
    group_by(service_combo_id) %>%
    summarise(start_date = min(start_date),
              end_date = max(end_date)) %>%
    left_join(service_combos %>% select(-service_combo)) %>%
    rename(service_id = service_combo_id)
  
  calendar <-
    calendar %>%
    #convert days (list) into a single string for each entry
    mutate(days = sapply(days, function(x)
      paste(x, collapse = ","))) %>%
    mutate(
      monday = if_else(str_detect(days, "monday"), 1, 0),
      tuesday = if_else(str_detect(days, "tuesday"), 1, 0),
      wednesday = if_else(str_detect(days, "wednesday"), 1, 0),
      thursday = if_else(str_detect(days, "thursday"), 1, 0),
      friday = if_else(str_detect(days, "friday"), 1, 0),
      saturday = if_else(str_detect(days, "saturday"), 1, 0),
      sunday = if_else(str_detect(days, "sunday"), 1, 0)
    ) %>%
    select(
      service_id,
      monday,
      tuesday,
      wednesday,
      thursday,
      friday,
      saturday,
      sunday,
      start_date,
      end_date
    )
  
  #refining trip details : based on new service ids------
  
  trips_colnames <- trips %>% colnames()
  
  if ("trip_headsign" %in% trips_colnames) {
    trips <- trips %>%
      left_join(service_combo_to_service_id, by = "service_id") %>%
      select(-service_id) %>%
      rename(service_id = service_combo_id) %>%
      left_join(trip_id_to_rvar_id, by = "trip_id") %>%
      select(trip_id,
             route_id,
             direction_id,
             rvar_id,
             trip_headsign,
             service_id,
             shape_id) %>%
      arrange(service_id, rvar_id)
  } else{
    trips <- trips %>%
      left_join(service_combo_to_service_id, by = "service_id") %>%
      select(-service_id) %>%
      rename(service_id = service_combo_id) %>%
      left_join(trip_id_to_rvar_id, by = "trip_id") %>%
      left_join(
        route_info %>% select(route_id, route_long_name) %>%
          rename(trip_headsign = route_long_name),
        by = "route_id"
      ) %>%
      select(trip_id,
             route_id,
             direction_id,
             rvar_id,
             trip_headsign,
             service_id,
             shape_id) %>%
      arrange(service_id, rvar_id)
  }
  
  trips_speed <-
    get_trip_speed(gtfs = gtfs, trip_id = trips$trip_id) %>%
    as_tibble() %>%
    select(trip_id, speed)
  
  trips <-
    trips %>%
    left_join(trips_speed, by = "trip_id")
  
  #defining shapes------------------------------------------
  
  #defining one shape id per rvar_id
  #in most cases this should be a one-to-one association but this is for good measure
  #the criterion applied is the SHORTEST shape id per rvar_id is retained
  #and applied to all associated rvar_id
  
  unique_shape_id <-
    trips %>%
    pull(shape_id) %>%
    unique()
  
  shapes <-
    gtfs$shapes %>%
    as_tibble() %>%
    filter(shape_id %in% unique_shape_id) %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"),
             crs = 4269) %>%
    arrange(shape_id, shape_pt_sequence) %>%
    group_by(shape_id) %>%
    summarise(do_union = FALSE) %>% #ensures that point geometries are not merged
    st_cast("LINESTRING") %>%
    mutate(length = st_length(geometry)) %>%
    mutate(length = as.numeric(length)) #length in meters
  
  trips_to_shortest_shape_id <-
    trips %>%
    select(rvar_id, shape_id) %>%
    distinct() %>%
    left_join(shapes %>% as_tibble() %>% select(-geometry), by = "shape_id") %>%
    group_by(rvar_id) %>%
    mutate(min_length = min(length)) %>%
    ungroup() %>%
    filter(length == min_length) %>%
    select(rvar_id, shape_id) %>%
    rename(shape_id_shortest = shape_id)
  
  #overwrite shape id with shortest shape id associated with each rvar_id
  trips <-
    trips %>%
    left_join(trips_to_shortest_shape_id, by = "rvar_id") %>%
    select(-shape_id) %>% #remove existing shape_id column
    rename(shape_id = shape_id_shortest)#define the new shape_id column as the current
  
  #unique shape_id may have changed now
  unique_shape_id <-
    trips %>%
    pull(shape_id) %>%
    unique()
  
  #limit the extent of shapes to the ones that we are retaining
  shapes <-
    shapes %>%
    filter(shape_id %in% unique_shape_id) %>%
    select(-length)
  
  #create shape points, which we will need later to calculate interstop distances
  
  shapes_points <-
    gtfs$shapes %>%
    as_tibble() %>%
    filter(shape_id %in% unique_shape_id) %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"),
             crs = 4269) %>%
    arrange(shape_id, shape_pt_sequence)
  
  #define rvar------------------------------
  
  rvar <-
    trips %>%
    select(rvar_id, route_id, direction_id, trip_headsign, shape_id) %>%
    distinct()
  
  #route info definition used to be here, was moved up earlier in the code
  #pour renseigner trip_headsign
  
  rvar <-
    rvar %>%
    left_join(route_info, by = "route_id")
  
  #define stops--------------------------------
  
  stop_ids <-
    gtfs$stop_times %>% as_tibble() %>%
    filter(trip_id %in% trips$trip_id) %>%
    pull(stop_id) %>%
    unique()
  
  stops <-
    gtfs$stops %>% as_tibble() %>%
    filter(stop_id %in% stop_ids) %>%
    select(stop_id, stop_name, stop_lat, stop_lon) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4269)
  
  #define stop_seq by rvar_id-----------------------------
  
  stop_seq_proto <-
    rvar_to_stop_seq %>%
    select(rvar_id, stop_pattern) %>%
    arrange(rvar_id) %>%
    unnest(stop_pattern) %>%
    mutate(
      stop_seq_id = str_c(
        rvar_id,
        "_",
        as.character(stop_id),
        "_",
        as.character(stop_sequence)
      ),
      .before = rvar_id
    )
  
  #THIS MAY BE NECESSARY TO REVISE : is the system that feeds STOP_SEQUENCE
  #consistent with the previous ? to my understanding, yes 12.10.2024 JV
  interstop_times <-
    gtfs$stop_times %>% as_tibble() %>%
    filter(trip_id %in% trips$trip_id) %>%
    left_join(trips %>% select(trip_id, rvar_id)) %>%
    mutate(departure_time = hms(departure_time)) %>%
    arrange(rvar_id, trip_id, stop_sequence) %>%
    mutate(interstop_time = if_else(
      lead(stop_sequence) == stop_sequence + 1,
      as.numeric(as.duration(lead(departure_time) - departure_time)),
      #interstop_time in seconds
      NA_real_ #NA in numeric format
    )) %>%
    select(rvar_id, stop_id, stop_sequence, interstop_time) %>%
    group_by(rvar_id, stop_id, stop_sequence) %>%
    summarise(mean_interstop_time = mean(interstop_time)) %>%
    ungroup() %>%
    arrange(rvar_id, stop_sequence) %>%
    mutate(
      stop_seq_id = str_c(
        rvar_id,
        "_",
        as.character(stop_id),
        "_",
        as.character(stop_sequence)
      ),
      .before = rvar_id
    )
  
  #calculate interstop distances
  
  #initialize
  
  stop_seq_proto$interstop_dist <- NA
  
  for (i in c(1:(length(stop_seq_proto$stop_seq_id) - 1))) {
    cat(
      "\rCalculating interstop distance",
      i,
      "of",
      length(stop_seq_proto$stop_seq_id) - 1
    )
    #CONDITIONS
    #next stop needs to be part of the same sequence AND
    #part of the same rvar_id (just another way of verifying the same stop sequence)
    #ELSE the NA assignment remains
    
    if ((stop_seq_proto$stop_sequence[i] + 1 == stop_seq_proto$stop_sequence[i +
                                                                             1]) &
        (stop_seq_proto$rvar_id[i] == stop_seq_proto$rvar_id[i + 1])) {
      rvar_id_i <- stop_seq_proto$rvar_id[i]
      
      shape_id_i <-
        rvar %>%
        filter(rvar_id == rvar_id_i) %>%
        pull(shape_id)
      
      #shapes points for only the shape_id associated with the rvar_id associated with stop i
      shapes_points_i <-
        shapes_points %>%
        filter(shape_id == shape_id_i)
      
      current_stop_id <- stop_seq_proto$stop_id[i]
      next_stop_id <- stop_seq_proto$stop_id[i + 1]
      
      current_stop <-
        stops %>%
        filter(stop_id == current_stop_id)
      
      next_stop <-
        stops %>%
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
      
      stop_seq_proto$interstop_dist[i] <- interstop_dist_i
    } else{
      stop_seq_proto$interstop_dist[i] <- NA
    }
  }
  
  #calculate interstop factor
  
  #overall average speed by rvar
  
  rvar_speeds_overall <-
    trips %>%
    group_by(rvar_id) %>%
    summarise(rvar_speed = mean(speed))
  
  stop_seq <-
    stop_seq_proto %>%
    left_join(interstop_times %>% select(stop_seq_id, mean_interstop_time),
              by = "stop_seq_id") %>%
    left_join(rvar_speeds_overall, by = "rvar_id") %>%
    mutate(interstop_speed =
             (interstop_dist / mean_interstop_time) * 3.6) %>%
    mutate(speed_factor =
             round((interstop_speed / rvar_speed), 1)) %>%
    select(rvar_id, stop_id, stop_sequence, speed_factor)
  
  #this writes a gtfs where
  #VARIATIONS IN INTERSTOP TIME ARE DISTRIBUTED THE SAME THROUGHOUT THE DAY
  #LES VARIATIONS DES VITESSES INTER-ARRÊT SONT RÉPARTIES DE LA MÊME FAÇON POUR TOUTE LA JOURNÉE
  #important to communicate this.
  
  #define span--------------------------
  
  span <-
    gtfs$stop_times %>% as_tibble() %>%
    filter(trip_id %in% trips$trip_id) %>%
    left_join(trips %>% select(trip_id, rvar_id, service_id)) %>%
    filter(stop_sequence == 1) %>%
    group_by(rvar_id, service_id) %>%
    mutate(departure_time = as.numeric(as.duration(hms(departure_time)))) %>% #conversion to duration in seconds
    summarise(first_dep = min(departure_time),
              last_dep = max(departure_time)) %>%
    mutate(
      #sprintf in order to handle times past 24:00:00
      first_dep = sprintf(
        "%02d:%02d:%02d",
        first_dep %/% 3600,
        (first_dep %% 3600) %/% 60,
        first_dep %% 60
      ),
      last_dep = sprintf(
        "%02d:%02d:%02d",
        last_dep %/% 3600,
        (last_dep %% 3600) %/% 60,
        last_dep %% 60
      )
    ) %>%
    ungroup()
  
  #define hsh--------------------------
  
  #headways and speeds by hour by service and rvar_id
  #we just want the first stop time per trip to define this
  
  hsh <-
    gtfs$stop_times %>% as_tibble() %>%
    filter(trip_id %in% trips$trip_id) %>%
    filter(stop_sequence == 1) %>%
    left_join(trips %>% select(trip_id, rvar_id, service_id, speed)) %>%
    mutate(departure_time_s = as.numeric(as.duration(hms(departure_time)))) %>% #converts departure time to duration in seconds
    mutate(hour_dep = sprintf("%02d:00:00", as.numeric(floor(
      departure_time_s / 3600
    )))) %>%
    select(rvar_id, service_id, hour_dep, departure_time_s, speed) %>%
    arrange(rvar_id, service_id, departure_time_s) %>%
    mutate(interval_to_next = if_else(
      #in minutes
      (service_id == lead(service_id)) & (rvar_id == lead(rvar_id)),
      as.numeric(floor(
        lead(departure_time_s) - departure_time_s
      ) / 60),
      NA_real_
    )) %>% #turn interval_to_next values above 60 to NA
    #this will mean that if there is an hour_dep associated with the
    #rvar_id to service_id combo but interval to next is NA
    #then either only one trip will be created starting at the hour OR the only
    #trip that will be generated will be the one that departs at the time that equals
    #previous departure + the previous hour's interval
    mutate(interval_to_next = if_else(interval_to_next >= 60, NA_real_, interval_to_next)) %>%
    group_by(rvar_id, service_id, hour_dep) %>%
    summarise(headway = round(median(interval_to_next, na.rm = TRUE), 0), speed =
                round(mean(speed), 1)) %>%
    ungroup()
  
  #return ssfs-----------------------
  
  ssfs <- list(
    stops = stops,
    shapes = shapes,
    rvar = rvar,
    stop_seq = stop_seq,
    calendar = calendar,
    span = span,
    hsh = hsh
  )
  
  return(ssfs)
}

#download stm data-----------------------------

gtfs_stm <- read_gtfs("https://www.stm.info/sites/default/files/gtfs/gtfs_stm.zip")

#set up sample networks --------------------------

#gtfsforge actually works with a simplified version of ssfs : call it s3fs ?
#beyond converting the network to ssfs, these models are further simplified to a format that imports into the tool and
#matches the ssfs reactive value data structure

#ligne jaune scenario

ljaune_ssfs <- gtfs_to_ssfs(gtfs_stm, routes = c("4"))

ljaune_ssfs$shapes <-
  ljaune_ssfs$shapes %>%
  st_transform(4326)

ljaune_ssfs$stops <-
  ljaune_ssfs$stops %>%
  st_transform(4326)

ljaune_ssfs$rvar <-
  ljaune_ssfs$rvar %>%
  select(-c(agency_id, route_short_name)) %>%
  mutate(direction_id = as.character(direction_id),
         route_type = as.character(route_type)) %>%
  as.data.frame()

stop_id_to_stopname <-
  ljaune_ssfs$stops %>% as.data.frame() %>%
  select(stop_id, stop_name)

ljaune_ssfs$stop_seq <-
  ljaune_ssfs$stop_seq %>%
  select(-speed_factor) %>%
  left_join(stop_id_to_stopname, by = "stop_id") %>%
  as.data.frame()

ljaune_ssfs$calendar <-
  ljaune_ssfs$calendar %>%
  mutate(across(monday:sunday, as.integer)) %>%
  mutate(start_date = as.character(start_date),
         end_date = as.character(end_date)) %>%
  as.data.frame()

ljaune_ssfs$span <-
  ljaune_ssfs$span %>% as.data.frame()

ljaune_ssfs$hsh <-
  ljaune_ssfs$hsh %>%
  mutate(headway = as.integer(headway)) %>%
  as.data.frame()

#STM METRO scenario
    
    mtlmetro_ssfs <- gtfs_to_ssfs(gtfs_stm, routes = c("1", "2", "4", "5"))
    
    mtlmetro_ssfs$shapes <-
      mtlmetro_ssfs$shapes %>%
      st_transform(4326)
    
    mtlmetro_ssfs$stops <-
      mtlmetro_ssfs$stops %>%
      st_transform(4326)
    
    mtlmetro_ssfs$rvar <-
      mtlmetro_ssfs$rvar %>%
      select(-c(agency_id, route_short_name)) %>%
      mutate(direction_id = as.character(direction_id),
             route_type = as.character(route_type)) %>%
      as.data.frame()
    
    stop_id_to_stopname <-
      mtlmetro_ssfs$stops %>% as.data.frame() %>%
      select(stop_id, stop_name)
    
    mtlmetro_ssfs$stop_seq <-
      mtlmetro_ssfs$stop_seq %>%
      select(-speed_factor) %>%
      left_join(stop_id_to_stopname, by = "stop_id") %>%
      as.data.frame()
    
    mtlmetro_ssfs$calendar <-
      mtlmetro_ssfs$calendar %>%
      mutate(across(monday:sunday, as.integer)) %>%
      mutate(start_date = as.character(start_date),
             end_date = as.character(end_date)) %>%
      as.data.frame()
    
    mtlmetro_ssfs$span <-
      mtlmetro_ssfs$span %>% as.data.frame()
    
    mtlmetro_ssfs$hsh <-
      mtlmetro_ssfs$hsh %>%
      mutate(headway = as.integer(headway)) %>%
      as.data.frame()
    
    #mtl mile end scenario
    
    mtlmileend_ssfs <- gtfs_to_ssfs(gtfs_stm,
                                    routes = c("46", "51", "160", "161", "80", "480", "30", "31"))
    
    mtlmileend_ssfs$shapes <-
      mtlmileend_ssfs$shapes %>%
      st_transform(4326)
    
    mtlmileend_ssfs$stops <-
      mtlmileend_ssfs$stops %>%
      st_transform(4326)
    
    mtlmileend_ssfs$rvar <-
      mtlmileend_ssfs$rvar %>%
      select(-c(agency_id, route_short_name)) %>%
      mutate(direction_id = as.character(direction_id),
             route_type = as.character(route_type)) %>%
      as.data.frame()
    
    stop_id_to_stopname <-
      mtlmileend_ssfs$stops %>% as.data.frame() %>%
      select(stop_id, stop_name)
    
    mtlmileend_ssfs$stop_seq <-
      mtlmileend_ssfs$stop_seq %>%
      select(-speed_factor) %>%
      left_join(stop_id_to_stopname, by = "stop_id") %>%
      as.data.frame()
    
    mtlmileend_ssfs$calendar <-
      mtlmileend_ssfs$calendar %>%
      mutate(across(monday:sunday, as.integer)) %>%
      mutate(start_date = as.character(start_date),
             end_date = as.character(end_date)) %>%
      as.data.frame()
    
    mtlmileend_ssfs$span <-
      mtlmileend_ssfs$span %>% as.data.frame()
    
    mtlmileend_ssfs$hsh <-
      mtlmileend_ssfs$hsh %>%
      mutate(headway = as.integer(headway)) %>%
      as.data.frame()
    
#export gtfsforge compatible scenarios as rds---------------------------------
    
write_rds(ljaune_ssfs,"ligne_jaune.rds")

write_rds(mtlmetro_ssfs,"metro.rds")

write_rds(mtlmileend_ssfs,"mileend.rds")
    