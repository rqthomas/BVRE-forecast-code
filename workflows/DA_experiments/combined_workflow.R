library(tidyverse)
library(lubridate)
lake_directory <- here::here()
setwd(lake_directory)
forecast_site <- "bvre"
configure_run_file <- "configure_run.yml"
update_run_config <- TRUE
config_set_name <- "DA_experiments"

config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

message("Checking for NOAA forecasts") #Note - this only works if connected to s3 bucket
#noaa_ready <- FLAREr::check_noaa_present(lake_directory,
#                                         configure_run_file,
#                                         config_set_name = config_set_name)

noaa_ready <- TRUE #Noaa forecasts are pulled onto local computer in workflow

if(!noaa_ready){
  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
  lapsed_time <- as.numeric(as.duration(Sys.time() - lubridate::as_datetime(config$run_config$forecast_start_datetime)))/(60*60)
  if(lapsed_time > 24){
    FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file = NA, new_horizon = NA, day_advance = 1, new_start_datetime = TRUE)
  }
}

config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
if(!is.null(config$run_config$forecast_fails)){
  if(config$run_config$forecast_fails > 0){
    config$run_config$forecast_fails <- 0
    FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file = NA, new_horizon = , day_advance = 1, new_start_datetime = TRUE)
    noaa_ready <- FLAREr::check_noaa_present(lake_directory,
                                             configure_run_file,
                                             config_set_name = config_set_name)
  }
}


#DA frequency vectors
daily = seq.Date(as.Date("2020-12-01"), as.Date("2021-12-31"), by = 1)
date_list <- list(day1 = daily,
                  day3 = daily[seq(1, length(daily), 3)]) #add in others




if(noaa_ready){
  
  if(!is.null(config$run_config$forecast_fails)){
    config$run_config$forecast_fails <- config$run_config$forecast_fails + 1
  }else{
    config$run_config$forecast_fails <- 1
  }
  FLAREr::update_run_config(config, lake_directory, configure_run_file, new_start_datetime = TRUE)
  
  message("Generating targets")
  source(file.path("workflows", config_set_name, "01_generate_targets.R"))
  
  setwd(lake_directory)
  
  message("Generating inflow forecast")
  source(file.path("workflows", config_set_name, "02_run_inflow_forecast.R"))
  
  for(da_freq in date_list) {
    for(date in daily) {
      
      if(date == daily[1]) {
        config$run_config$forecast_start_datetime <- format(date, "%Y-%m-%d %H:%M:%S")
        config$run_config$start_datetime <- format((date-30), "%Y-%m-%d %H:%M:%S")

      } else {
        config$run_config <- yaml::read_yaml("restart/bvre/bvre_DA_experiments_test/configure_run.yml")
      }
      
      message("Prep flare forecast")
      source(file.path("workflows", config_set_name, "2.5_prep_flare_forecast.R"))
      
      obs[1, , ]
      
      start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
      if (is.na(config$run_config$forecast_start_datetime)) {
        end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
        forecast_start_datetime <- end_datetime
      } else {
        forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
        end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
      }
      full_time <- seq(start_datetime, end_datetime, by = "1 day")
      full_time <- as.Date(full_time)
      
      idx <- which(!full_time %in% da_freq)
      idx 
      
      obs[1, idx, ] <- NA
      
      setwd(lake_directory)
      
      message("Generating forecast")
      source(file.path("workflows", config_set_name, "03_run_flarer_forecast.R"))
  
      setwd(lake_directory)
      
      message("Generating plots")
      source(file.path("workflows", config_set_name, "04_visualize.R"))
      
      # config <- FLAREr::set_configuration(configure_run_file, lake_directory, config_set_name = config_set_name)
      # config$run_config$forecast_fails <- 0
      FLAREr::update_run_config(config, lake_directory, configure_run_file, new_start_datetime = TRUE, day_advance =1)
      
      
    }
  }
}
  


      

# message("Generating evaluation metrics")
# source(file.path("workflows", config_set_name, "05_assess_forecast.R"))
