#renv::restore()

#install.packages("gsheet")
library(tidyverse)
library(lubridate)

lake_directory <- here::here()

s3_mode <- TRUE

source(file.path(lake_directory, "R", "met_qaqc.R"))
source(file.path(lake_directory, "R", "in_situ_qaqc.R"))
source(file.path(lake_directory, "R", "temp_oxy_chla_qaqc.R"))
source(file.path(lake_directory, "R", "extract_CTD.R"))
source(file.path(lake_directory, "R", "extract_secchi.R"))

if(file.exists("~/.aws")){
  warning(paste("Detected existing AWS credentials file in ~/.aws,",
                "Consider renaming these so that automated upload will work"))
}

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

realtime_insitu_repo <- "https://github.com/FLARE-forecast/BVRE-data.git"
realtime_met_repo <- "https://github.com/FLARE-forecast/FCRE-data.git"

configuration_file <- "configure_flare.yml"
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
forecast_site <- run_config$forecast_site
sim_name <- run_config$sim_name

config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration","observation_processing","observation_processing.yml"))
#Note: lake_directory need to be set prior to running this script
config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",configuration_file))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config_obs$data_location <- config$file_path$data_directory
config$file_path$noaa_directory <- file.path(dirname(lake_directory), "drivers", "noaa")

setwd(file.path(lake_directory, "data_raw"))
if(!dir.exists(file.path(lake_directory, "data_raw", config_obs$realtime_insitu_location))){
  system(paste("git clone --depth 1 --single-branch --branch",config_obs$realtime_insitu_location, realtime_insitu_repo, config_obs$realtime_insitu_location, sep = " "))
}else{
  setwd(file.path(lake_directory, "data_raw", config_obs$realtime_insitu_location))
  system("git pull")
}

#PULL FROM FCR MET
setwd(file.path(lake_directory, "data_raw"))
if(!dir.exists(file.path(lake_directory, "data_raw", config_obs$realtime_met_station_location))){
  system(paste("git clone --depth 1 --single-branch --branch",config_obs$realtime_met_station_location, realtime_met_repo, config_obs$realtime_met_station_location, sep = " "))
}else{
  setwd(file.path(lake_directory, "data_raw", config_obs$realtime_met_station_location))
  system("git pull")
}


#download secchi up to 2020
if(!file.exists(file.path(config$file_path$data_directory, "Secchi_depth_2013_2020.csv"))){
  data  <-  "https://portal.edirepository.org/nis/dataviewer?packageid=edi.198.8&entityid=336d0a27c4ae396a75f4c07c01652985" # URL from EDI: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.499.2&statisticalFileType=r
download.file(data,
              file.path(lake_directory, "data_raw","Secchi_depth_2013_2020.csv"),method="curl")
}


if(!file.exists(file.path(config$file_path$data_directory, "Met_final_2015_2020.csv"))){
  data  <-  "https://portal.edirepository.org/nis/dataviewer?packageid=edi.389.5&entityid=3d1866fecfb8e17dc902c76436239431" # URL from EDI: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.499.2&statisticalFileType=r
  try(download.file(data, destfile = file.path(config$file_path$data_directory, "Met_final_2015_2020.csv"), 
                    method = "curl"))
}


cleaned_met_file <- file.path(config$file_path$qaqc_data_directory, 
                              paste0("observed-met_",forecast_site,".nc"))
if(is.null(config_obs$met_file)){
  met_qaqc(realtime_file = file.path(config$file_path$data_directory, config_obs$met_raw_obs_fname[1]),
           qaqc_file = file.path(config$file_path$data_directory, config_obs$met_raw_obs_fname[2]),
           cleaned_met_file = cleaned_met_file,
           input_file_tz = "EST",
           nldas = NULL)
}else{
  file.copy(file.path(config$file_path$data_directory,config_obs$met_file), cleaned_met_file, overwrite = TRUE)
}


cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory, 
                                            paste0(forecast_site, "-targets-insitu.csv"))

source(file.path(lake_directory,"R/combine_bvre_insitu.R"))

combine_bvre_insitu(lake_directory, config_obs)

config_obs$data_location <- config$file_path$data_directory
if(is.null(config_obs$combined_obs_file)){
  in_situ_qaqc(insitu_obs_fname = file.path(config_obs$data_location, "bvrewaterquality.csv"),
               data_location = config_obs$data_location,
               maintenance_file = file.path(config_obs$data_location,config_obs$maintenance_file),
               ctd_fname = NA,
               nutrients_fname =  NA,
               secchi_fname = NA,
               cleaned_observations_file_long = cleaned_observations_file_long,
               lake_name_code = config_obs$lake_name_code,
               config_obs = config_obs)
}else{
  file.copy(file.path(config$file_path$data_directory,config_obs$combined_obs_file), cleaned_observations_file_long, overwrite = TRUE)
}

if(s3_mode){
  aws.s3::put_object(file = cleaned_observations_file_long, object = file.path(forecast_site, basename(cleaned_observations_file_long)), bucket = "targets")
  #aws.s3::put_object(file = cleaned_inflow_file, object = file.path(forecast_site, paste0(forecast_site, "-targets-inflow.csv")), bucket = "targets")
  aws.s3::put_object(file = cleaned_met_file , object = file.path(forecast_site, basename(cleaned_met_file)), bucket = "targets")
}
