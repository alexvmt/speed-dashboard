library(shiny)
library(move2)
library(sf)
library(dplyr)
library(tidyr)
library(units)

# disable scientific notation
options(scipen = 999)

# set default for speed threshold
default_speed_threshold <- 5

shinyModuleUserInterface <- function(id, label) {
  
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
  
  # create function to insert linebreaks
  linebreaks <- function(n){HTML(strrep(br(), n))}
  
  tagList(
    titlePanel("Speed Dashboard"),
    fluidRow(
      column(2,
             selectInput(ns("speed_units"),
                         "Speed units",
                         choices = c("m/s", "km/h"),
                         selected = c("km/h"))
      ),
      column(2,
             selectInput(ns("distance_units"),
                         "Distance units",
                         choices = c("m", "km"),
                         selected = c("km"))
      ),
      column(2,
             selectInput(ns("time_units"),
                         "Time units",
                         choices = c("s", "min", "h", "d"),
                         selected = c("h"))
      ),
      column(2,
             numericInput(ns("speed_threshold"),
                          "Speed threshold",
                          default_speed_threshold)
      ),
      column(2,
             textInput(ns("threshold_behavior_tag"),
                       "Threshold behavior tag",
                       placeholder = "e. g. flight")
      ),
      column(2,
             linebreaks(1),
             downloadButton(ns("download_table"),
                            "Download table")
      )
    ),
    dataTableOutput(ns("speed_summary"))
  )

}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  
  # all IDs of UI functions need to be wrapped in ns()
  ns <- session$ns
  current <- reactiveVal(data)
  
  # transform data
  rctv_data_transformed <- reactive({
    
    # ensure that data is in epsg 4326
    data_transformed <- st_transform(data, 4326)
    
    # get id column name
    id_column_name <- mt_track_id_column(data_transformed)
    
    # get time column name
    time_column_name <- mt_time_column(data_transformed)
    
    # extract coordinates
    data_transformed$long <- st_coordinates(data_transformed)[, 1]
    data_transformed$lat <- st_coordinates(data_transformed)[, 2]
    
    list(data_transformed = data_transformed,
         id_column_name = id_column_name,
         time_column_name = time_column_name)
    
  })
  
  # create speed summary table
  rctv_speed_summary <- reactive({
    
    # load reactive data
    data_transformed <- rctv_data_transformed()$data_transformed
    id_column_name <- rctv_data_transformed()$id_column_name
    time_column_name <- rctv_data_transformed()$time_column_name
    
    # calculate between locations speed
    data_transformed$speed <- mt_speed(data_transformed, units = input$speed_units)
    
    # calculate between locations distance
    data_transformed$distance <- mt_distance(data_transformed, units = input$distance_units)
    
    # calculate between locations time
    data_transformed$time <- mt_time_lags(data_transformed, units = input$time_units)
    
    # convert spatial into regular dataframe
    data_transformed <- as.data.frame(data_transformed)
    
    # calculate observations, distance and time above speed threshold
    n_distance_and_time_above_speed_threshold <- data_transformed %>% 
      mutate(speed = set_units(speed, NULL),
             distance = set_units(distance, NULL),
             time = set_units(time, NULL)) %>% 
      filter(speed > input$speed_threshold
             & !is.na(speed)
             & !is.na(distance)
             & !is.na(time)) %>% 
      group_by(.data[[id_column_name]]) %>% 
      summarise(n_above_speed_threshold = n(),
                distance_above_speed_threshold = sum(distance),
                time_above_speed_threshold = sum(time))
    
    # calculate max and median speed
    max_and_median_speed <- data_transformed %>% 
      mutate(speed = set_units(speed, NULL)) %>% 
      filter(!is.na(speed)) %>% 
      group_by(.data[[id_column_name]]) %>% 
      summarise(first_time = first(.data[[time_column_name]]),
                first_location = paste0("(", first(long), ", ", first(lat), ")"),
                last_time = last(.data[[time_column_name]]),
                last_location = paste0("(", last(long), ", ", last(lat), ")"),
                max_speed = max(speed),
                median_speed = median(speed))
    
    # create final speed summary table
    speed_summary <- max_and_median_speed %>% 
      left_join(n_distance_and_time_above_speed_threshold, by = id_column_name) %>% 
      mutate(n_above_speed_threshold = replace_na(n_above_speed_threshold, 0),
             distance_above_speed_threshold = replace_na(distance_above_speed_threshold, 0),
             time_above_speed_threshold = replace_na(time_above_speed_threshold, 0)) %>% 
      mutate(max_speed = round(set_units(max_speed, input$speed_units, mode = "standard"), 2),
             median_speed = round(set_units(median_speed, input$speed_units, mode = "standard"), 2),
             distance_above_speed_threshold = round(set_units(distance_above_speed_threshold, input$distance_units, mode = "standard"), 2),
             time_above_speed_threshold = round(set_units(time_above_speed_threshold, input$time_units, mode = "standard"), 2)) %>% 
      mutate(speed_threshold = set_units(input$speed_threshold, input$speed_units, mode = "standard"),
             threshold_behavior_tag = input$threshold_behavior_tag)
    
    speed_summary
    
  })
  
  output$speed_summary <- renderDataTable({ rctv_speed_summary() })
  
  # add units to column names for downloading table
  rctv_speed_summary_download <- reactive({

    speed_summary_download <- rctv_speed_summary() %>% 
      rename(setNames("max_speed", paste0("max_speed", "_", gsub("/", "", input$speed_units)))) %>% 
      rename(setNames("median_speed", paste0("median_speed", "_", gsub("/", "", input$speed_units)))) %>% 
      rename(setNames("distance_above_speed_threshold", paste0("distance_above_speed_threshold", "_", input$distance_units))) %>% 
      rename(setNames("time_above_speed_threshold", paste0("time_above_speed_threshold", "_", input$time_units))) %>% 
      rename(setNames("speed_threshold", paste0("speed_threshold", "_", gsub("/", "", input$speed_units))))

    speed_summary_download

  })
  
  # add reactive value that can be incremented and thus used as trigger when downloading table
  rv <- reactiveValues(download_flag = 0)
  
  # download table via click on download button
  output$download_table <- downloadHandler(

    filename <- function(){"speed_summary.csv"},
    content <- function(fname){
      
      # save speed summary table as csv
      write.csv(rctv_speed_summary_download(), file = fname, row.names = FALSE)
      
      # increment download flag so that download can be registered and info is written to log
      rv$download_flag <- rv$download_flag + 1
      
    }
  
  )
  
  # log info on downloaded table
  observeEvent(rv$download_flag, {
    
    logger.info(paste0("Dowloading speed summary table with following inputs: ",
                       "speed units: ",
                       input$speed_units,
                       "; distance units: ",
                       input$distance_units,
                       "; time units: ",
                       input$time_units,
                       "; speed threshold: ",
                       input$speed_threshold,
                       "; threshold behavior tag: ",
                       input$threshold_behavior_tag))
  
  }, ignoreInit = TRUE)
  
  # data must be returned. Either the unmodified input data, or the modified data by the app
  return(reactive({ current() }))
  
}
