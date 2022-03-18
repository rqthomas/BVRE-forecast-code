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

if(!is.null(config$run_config$forecast_fails)){
  if(config$run_config$forecast_fails > 0){
    config$run_config$forecast_fails <- 0
    FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file = NA, new_horizon = NA, day_advance = 1, new_start_datetime = TRUE)
    noaa_ready <- FLAREr::check_noaa_present(lake_directory,
                                             configure_run_file,
                                             config_set_name = config_set_name)
  }
}


#DA frequency vectors
daily = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = 1) 
date_list <- list(daily = daily,
                  daily_2 = daily[seq(1, length(daily), 2)],
                  daily_5 = daily[seq(1, length(daily), 5)],
                  weekly = daily[seq(1, length(daily), 7)],
                  fortnightly = daily[seq(1, length(daily), 14)],
                  monthly = daily[seq(1, length(daily), 30)]) 


if(noaa_ready){
  
  if(!is.null(config$run_config$forecast_fails)){
    config$run_config$forecast_fails <- config$run_config$forecast_fails + 1
  }else{
    config$run_config$forecast_fails <- 1
  }

  setwd(lake_directory)
  
  message("Generating targets")
  source(file.path(lake_directory, "workflows", config_set_name, "01_generate_targets.R"))
  
  setwd(lake_directory)
  
  message("Generating inflow forecast")
  source(file.path(lake_directory, "workflows", config_set_name, "02_run_inflow_forecast.R"))
  
  for(da_freq in 1:length(date_list)) {
    for(date in 1:length(daily[1:4])) {
      
      if(date == 1) {
        config$run_config$forecast_start_datetime <- format(daily[date], "%Y-%m-%d %H:%M:%S")
        config$run_config$start_datetime <- format((daily[date]-30), "%Y-%m-%d %H:%M:%S")
        config$run_config$end_datetime <- format(daily[date]+35, "%Y-%m-%d %H:%M:%S")
      } else {
        config$run_config <- yaml::read_yaml("restart/bvre/bvre_DA_experiments_test/configure_run.yml")
      }
      
      message("Prep flare forecast")
      source(file.path(lake_directory, "workflows", config_set_name, "2.5_prep_flare_forecast.R"))
      
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

      idx <- which(!full_time %in% date_list[[da_freq]])
      obs[1, idx, ] <- NA
      
      setwd(lake_directory)
      
      #set output directory so each frequency/experiment is saved in a separate folder
      config$file_path$forecast_output_directory <- file.path(lake_directory,"forecasts","bvre","DA_experiments",names(date_list)[da_freq])
      
      message("Generating forecast")
      source(file.path(lake_directory, "workflows", config_set_name, "03_run_flarer_forecast.R"))
  
      setwd(lake_directory)
      
      message("Generating plots")
      source(file.path(lake_directory, "workflows", config_set_name, "04_visualize.R"))
      
      message("Generating evaluation metrics")
      source(file.path(lake_directory, "workflows", config_set_name, "05_assess_forecast.R"))
      
      FLAREr::update_run_config(config, lake_directory, configure_run_file, new_start_datetime = TRUE, day_advance =1)
      
    }
  }
}

