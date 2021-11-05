library(tidyverse)
library(lubridate)

#load packages
if (!require("pacman"))install.packages("pacman")
pacman::p_load(httr,EcoHydRology,GSODR,curl,elevatr,raster,soilDB,rgdal,lattice,lubridate)

#files.sources <- list.files(file.path(lake_directory, "R"), full.names = TRUE)
#sapply(files.sources, source)

lake_directory <- here::here()
s3_mode <- TRUE

configuration_file <- "configure_flare.yml"
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
forecast_site <- run_config$forecast_site

if(file.exists("~/.aws")){
  warning(paste("Detected existing AWS credentials file in ~/.aws,",
                "Consider renaming these so that automated upload will work"))
}

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

#Note: lake_directory need to be set prior to running this script
config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",configuration_file))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")

if(s3_mode){
  restart_exists <- aws.s3::object_exists(object = file.path(forecast_site, "configure_run.yml"), bucket = "restart")
  if(restart_exists){
    aws.s3::save_object(object = file.path(forecast_site, "configure_run.yml"), bucket = "restart", file = file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
  }
  config$file_path$noaa_directory <- file.path(lake_directory, "drivers", "noaa")
  config$file_path$inflow_directory <- file.path(lake_directory, "drivers", "inflow")
}else{
  config$file_path$noaa_directory <- file.path(dirname(lake_directory), "drivers", "noaa")
  config$file_path$inflow_directory <- file.path(dirname(lake_directory), "drivers", "inflow")
}
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
config$run_config <- run_config


if(config$run_config$forecast_horizon > 0){
  # Set up timings
  #Weather Drivers
  start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  if(is.na(config$run_config$forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
  }
  forecast_hour <- lubridate::hour(forecast_start_datetime)
  if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
  forecast_path <- file.path(config$file_path$noaa_directory, "NOAAGEFS_1hr",config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
  
  
  #Weather Drivers
  
  noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
  
  if(s3_mode){
    aws.s3::save_object(object = file.path(forecast_site, "fcre-targets-inflow.csv"), bucket = "targets", file = file.path(config$file_path$qaqc_data_directory, "fcre-targets-inflow.csv"))
    aws.s3::save_object(object = file.path(forecast_site, "observed-met_fcre.nc"), bucket = "targets", file = file.path(config$file_path$qaqc_data_directory, "observed-met_fcre.nc"))
    
    if(config$run_config$forecast_horizon > 0){
      noaa_forecast_path <- file.path(lake_directory,"drivers/noaa", config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
      
      download_s3_objects(lake_directory,
                          bucket = "drivers",
                          prefix = file.path("noaa", config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour))
    }
  }else{
    local_noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
    noaa_forecast_path <- file.path(lake_directory, "drivers/noaa", config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
    files <- list.files(noaa_forecast_path, full.names = TRUE)
    for(i in 1:length(files)){
      dir.create(noaa_forecast_path)
      file.copy(from = files[i], to = noaa_forecast_path)
    }
  }
  
  #Data needed for inflow model
  
  #soil data
  url= "https://websoilsurvey.sc.egov.usda.gov/DSD/Download/AOI/wfu1odcjhsdqqd4capo2doux/wss_aoi_2021-03-22_13-16-30.zip"
  download.file(url,
                destfile = file.path(config$file_path$data_directory,"mysoil.zip")) #Note: will probably have to update wss_aoi date if it's been a while - go to wss homepage and click on start wss link on right of page
  unzip("mysoil.zip")            #zoom in to site, use define aoi tool to select desired area, go to download soils data tab, click "create download link", right click and copy link address, paste on line 10
  
  message("Forecasting inflow and outflows")
  # Forecast Inflows
  
  forecast_files <- list.files(noaa_forecast_path, full.names = TRUE)
  if(length(forecast_files) == 0){
    stop(paste0("missing forecast files at: ", noaa_forecast_path))
  }
  temp_flow_forecast <- forecast_inflows_outflows(inflow_obs = file.path(config$file_path$qaqc_data_directory, "fcre-targets-inflow.csv"),
                                                  forecast_files = forecast_files,
                                                  obs_met_file = file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc"),
                                                  output_dir = config$file_path$inflow_directory,
                                                  inflow_model = config$inflow$forecast_inflow_model,
                                                  inflow_process_uncertainty = FALSE,
                                                  forecast_location = config$file_path$forecast_output_directory,
                                                  config = config,
                                                  s3_mode = s3_mode,
                                                  bucket = "drivers")
}