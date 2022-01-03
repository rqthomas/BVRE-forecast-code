library(tidyverse)
library(lubridate)
lake_directory <- here::here()
setwd(lake_directory)
forecast_site <- "bvre"
configure_run_file <- "configure_run.yml"
update_run_config <- TRUE

message("Checking for NOAA forecasts")
noaa_ready <- FLAREr::check_noaa_present(lake_directory,
                                         configure_run_file)

if(!noaa_ready){
  config <- FLAREr::set_configuration(configure_run_file,lake_directory)
  lapsed_time <- as.numeric(as.duration(Sys.time() - lubridate::as_datetime(config$run_config$forecast_start_datetime)))/(60*60)
  if(lapsed_time > 24){
    FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file = NA, new_horizon = 35, day_advance = 1, new_start_datetime = FALSE)
  }
}

if(noaa_ready){
  
  message("Generating targets")
  source(file.path("01_generate_targets.R"))
  
  setwd(lake_directory)
  
  message("Generating inflow forecast")
  #source(file.path("02_run_inflow_forecast.R"))
  
  setwd(lake_directory)
  
  message("Generating forecast")
  source(file.path("03_run_flarer_forecast.R"))
  
  setwd(lake_directory)
  
  message("Generating plots")
  source(file.path("04_visualize.R"))
}
