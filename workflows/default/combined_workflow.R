library(magrittr)
library(lubridate)
library(readr)
library(hydroGOF)
lake_directory <- here::here()
setwd(lake_directory)
forecast_site <- "bvre"
configure_run_file <- "configure_run.yml"
config_files <- "configure_flare.yml"
update_run_config <- TRUE
config_set_name <- "DA_experiments"
sim_name <- config_set_name
use_archive <- FALSE

if(use_archive){
  use_s3 <- FALSE
}else{
  Sys.setenv('AWS_DEFAULT_REGION' = 's3',
             'AWS_S3_ENDPOINT' = 'flare-forecast.org',
             'USE_HTTPS' = TRUE)
  use_s3 <- FALSE
}

start_from_scratch <- FALSE
time_start_index <- 334

num_forecasts <- 366
days_between_forecasts <- 1
forecast_horizon <- 35 
starting_date <- as_date("2020-11-22") #changed from 11-27
second_date <- starting_date + months(1) + days(10) #changed days to get back to 01-01

start_dates <- rep(NA, num_forecasts)
start_dates[1:2] <- c(starting_date, second_date)
for(i in 3:num_forecasts){
  start_dates[i] <- as_date(start_dates[i-1]) + days(days_between_forecasts)
}

site <- "bvre"

start_dates <- as_date(start_dates)
forecast_start_dates <- start_dates + days(days_between_forecasts)
forecast_start_dates <- as_date(c(NA, forecast_start_dates[-1]))
forecast_start_dates <- forecast_start_dates[1:length(forecast_start_dates)-1]

#DA frequency vectors
daily = seq.Date(as.Date("2020-11-22"), as.Date("2022-02-01"), by = 1) #changed this from 11-27 to see if the day of the week affects forecast skill
date_list <- list(daily = daily,                                      #changing end to 2022-02-01 because need observations to evaluate forecasts
                  daily_2 = daily[seq(1, length(daily), 2)],
                  daily_5 = daily[seq(1, length(daily), 5)],
                  weekly = daily[seq(1, length(daily), 7)],
                  fortnightly = daily[seq(1, length(daily), 14)],
                  monthly = daily[seq(1, length(daily), 30)]) 

if(start_from_scratch){
  if(use_s3){
    FLAREr::delete_restart(site, config_set_name)
  }
  if(file.exists(file.path(lake_directory, "restart", site, "DA_experiments",configure_run_file))){
    unlink(file.path(lake_directory, "restart", site, "DA_experiments", configure_run_file))
  }
  
  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
  config$run_config$start_datetime <- as.character(paste0(start_dates[1], " 00:00:00"))
  config$run_config$forecast_start_datetime <- as.character(paste0(start_dates[2], " 00:00:00"))
  config$run_config$forecast_horizon <- 0
  config$run_config$restart_file <- NA
  run_config <- config$run_config
  yaml::write_yaml(run_config, file = file.path(config$file_path$configuration_directory, configure_run_file))
} else {
  config <- FLAREr::set_configuration(configure_run_file, lake_directory, config_set_name = config_set_name)
  time_start_index <- grep(as.Date(config$run_config$forecast_start_datetime), forecast_start_dates)
}


run_config <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
run_config$configure_flare <- config_files
run_config$use_s3 <- use_s3
yaml::write_yaml(run_config, file = file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  
  message("Generating targets")
  #source(file.path(lake_directory, "workflows", config_set_name, "01_generate_targets.R")) #commenting out for now becasue vahid is working on the git files
  
  setwd(lake_directory)
  
  #message("Generating inflow forecast")
  #source(file.path(lake_directory, "workflows", config_set_name, "02_run_inflow_forecast.R"))
  
  for(da_freq in 3:length(date_list)) { 
    for(date in time_start_index:length(forecast_start_dates)) { #note that 2021-11-24 does NOT want to run - get a GLM error saying "Day 2459559 (2021-12-10) not found" - manually restarting on 2021-11-24
                                                                #wondwering if this error is because there are no 11-25-21 noaa forecasts? trying to start on 11-26 instead
     
      #Download and process observations (already done)
      cycle <- "00"
      
      if(!use_archive){
        FLAREr::get_stacked_noaa(lake_directory, config, averaged = TRUE)
      }
      
      if(!is.na(config$run_config$restart_file)) {
        config$run_config$restart_file <- basename(config$run_config$restart_file)
      }
      # config$run_config$restart_file <- file.path(config$file_path$forecast_output_directory, paste0("bvre-", forecast_start_dates[date-1], "-DA_experiments.nc"))
      # config$run_config$start_datetime <- as.character(paste0(forecast_start_dates[date-1], "00:00:00"))
      # config$run_config$forecast_start_datetime <- as.character(paste0(forecast_start_dates[date+1], " 00:00:00"))
      
      if(config$run_config$forecast_horizon > 0){
        noaa_forecast_path <- FLAREr::get_driver_forecast_path(config,
                                                               forecast_model = config$met$forecast_met_model)
        if(!use_archive){
          FLAREr::get_driver_forecast(lake_directory, forecast_path = noaa_forecast_path)
        }
        forecast_dir <- file.path(config$file_path$noaa_directory, noaa_forecast_path)
      }else{
        forecast_dir <- NULL
      }
      
      dir.create(file.path(lake_directory, "flare_tempdir", config$location$site_id,
                           config_set_name), recursive = TRUE, showWarnings = FALSE)
      
      # Added check for NOAA files - if not present skips and moves forecast start date +1 day - TNM
     if(date > 1){
       file_chk <- list.files(forecast_dir)
     } else{
       file_chk <- 0
     }
      
      if(length(file_chk) == 0) {
        message("No NOAA forecast files for: ", forecast_start_dates[date])
        config$run_config$forecast_start_datetime <- as.character(paste0(forecast_start_dates[date+1], " 00:00:00"))
        next
      }
       
       met_out <- FLAREr::generate_glm_met_files(obs_met_file = file.path(config$file_path$qaqc_data_directory, paste0("observed-met_",config$location$site_id,".nc")),
                                                 out_dir = config$file_path$execute_directory,
                                                 forecast_dir = forecast_dir,
                                                 config = config)
    
      met_out$filenames <- met_out$filenames[!stringr::str_detect(met_out$filenames, "ens00")]
      
      
      message("Prep flare forecast")
      source(file.path(lake_directory, "workflows", config_set_name, "2.5_prep_flare_forecast.R"))
      print(forecast_dir)

      
      #Create observation matrix
      obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
                                       obs_config = obs_config,
                                       config)
      obs[1, , ]
    
      full_time <- seq(lubridate::as_datetime(config$run_config$start_datetime), lubridate::as_datetime(config$run_config$start_datetime) + lubridate::days(35), by = "1 day")
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
      
      if(config$run_config$use_s3){
        success <- aws.s3::put_object(file = pdf_file, object = file.path(config$location$site_id, basename(pdf_file)), bucket = "analysis")
    #    if(success){
    #      unlink(pdf_file)
    #    }
      }
      
      restart_date <- as.character(lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1)) #to save each restart file
      config <- FLAREr::update_run_config(config = config, lake_directory = lake_directory, configure_run_file = configure_run_file,
                                       saved_file = saved_file, 
                                       new_start_datetime = TRUE, day_advance =days_between_forecasts, new_horizon = forecast_horizon)
      file.copy(from = file.path(config$file_path$restart_directory,configure_run_file),
                to = file.path(config$file_path$restart_directory, paste0("configure_run",restart_date,".yml")))
      
      
      #unlink(forecast_dir, recursive = TRUE)
      setwd(lake_directory)
      #unlink(file.path(lake_directory, "flare_tempdir", config$location$site_id, config_set_name), recursive = TRUE)
      if (config$run_config$use_s3) {
        success <- aws.s3::put_object(file = saved_file, object = file.path(config$location$site_id,
                                                                            basename(saved_file)), bucket = "forecasts")
       # if (success) {
       #   unlink(saved_file)
       # }
      }
    }
  }

  

#config$run_config$forecast_start_datetime <- "2021-11-30 00:00:00"
#config$run_config$start_datetime <- "2021-11-29 00:00:00"
#config$run_config$restart_file <- "bvre-2021-11-29-DA_experiments.nc"