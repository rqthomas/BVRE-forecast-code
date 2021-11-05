renv::restore()

#install.packages("gsheet")
library(tidyverse)
library(lubridate)

lake_directory <- here::here()

s3_mode <- TRUE

files.sources <- list.files(file.path(lake_directory, "R"), full.names = TRUE)
sapply(files.sources, source)

if(file.exists("~/.aws")){
  warning(paste("Detected existing AWS credentials file in ~/.aws,",
                "Consider renaming these so that automated upload will work"))
}

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast")

configuration_file <- "configure_flare.yml"
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
forecast_site <- run_config$forecast_site

config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration","observation_processing","observation_processing.yml"))
#Note: lake_directory need to be set prior to running this script
config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",configuration_file))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config_obs$data_location <- config$file_path$data_directory
config$file_path$noaa_directory <- file.path(dirname(lake_directory), "drivers", "noaa")

setwd(file.path(lake_directory, "data_raw"))
if(!dir.exists(file.path(lake_directory, "data_raw", config_obs$realtime_insitu_location))){
  system(paste0("git clone --depth 1 --single-branch --branch ",config_obs$realtime_insitu_location, " https://github.com/FLARE-forecast/BVRE-data.git ", config_obs$realtime_insitu_location))
}else{
  setwd(file.path(lake_directory, "data_raw", config_obs$realtime_insitu_location))
  system("git pull")
}

#PULL FROM FCR MET
setwd(file.path(lake_directory, "data_raw"))
if(!dir.exists(file.path(lake_directory, "data_raw", config_obs$realtime_insitu_location))){
  system(paste0("git clone --depth 1 --single-branch --branch ",config_obs$realtime_insitu_location, " https://github.com/FLARE-forecast/BVRE-data.git ", config_obs$realtime_insitu_location))
}else{
  setwd(file.path(lake_directory, "data_raw", config_obs$realtime_insitu_location))
  system("git pull")
}


#download secchi up to 2020
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.198.8&entityid=336d0a27c4ae396a75f4c07c01652985",
              "data_raw/Secchi_depth_2013_2020.csv",method="curl")


if(!file.exists(file.path(config$file_path$data_directory, "Met_final_2015_2020.csv"))){
  data  <-  "https://portal.edirepository.org/nis/dataviewer?packageid=edi.389.5&entityid=3d1866fecfb8e17dc902c76436239431" # URL from EDI: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.499.2&statisticalFileType=r
  try(download.file(data, destfile = file.path(config$file_path$data_directory, "Met_final_2015_2020.csv"), 
                    method = "curl"))
}

if(!file.exists(file.path(config$file_path$data_directory, "Secchi_depth_2013_2020.csv"))){
  data  <-  "https://portal.edirepository.org/nis/dataviewer?packageid=edi.198.8&entityid=336d0a27c4ae396a75f4c07c01652985" # URL from EDI: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.499.2&statisticalFileType=r
  try(download.file(data, destfile = file.path(config$file_path$data_directory, "Secchi_depth_2013_2020.csv"), 
                    method = "curl"))
}

files.sources <- list.files(file.path(lake_directory, "R"), full.names = TRUE)
sapply(files.sources, source)

if(is.null(config_obs$met_file)){
  met_qaqc(realtime_file = file.path(config$file_path$data_directory, config_obs$met_raw_obs_fname[1]),
           qaqc_file = file.path(config$file_path$data_directory, config_obs$met_raw_obs_fname[2]),
           cleaned_met_file_dir = config$file_path$qaqc_data_directory,
           input_file_tz = "EST",
           nldas = file.path(config$file_path$data_directory, config_obs$nldas))
}else{
  file.copy(file.path(config$file_path$data_directory,config_obs$met_file), cleaned_met_file, overwrite = TRUE)
}

cleaned_inflow_file <- paste0(config$file_path$qaqc_data_directory, "/fcre-targets-inflow.csv")

if(is.null(config_obs$inflow1_file)){
  inflow_qaqc(realtime_file = file.path(config_obs$data_location, config_obs$inflow_raw_file1),
              #qaqc_file = file.path(config_obs$data_location, config$inflow_raw_file1[2]),
              nutrients_file = file.path(config_obs$data_location, config_obs$nutrients_fname),
              cleaned_inflow_file ,
              config_obs$local_tzone,
              input_file_tz = 'EST')
}else{
  file.copy(file.path(config$file_path$data_directory,config_obs$inflow1_file), cleaned_inflow_file, overwrite = TRUE)
}


cleaned_observations_file_long <- paste0(config$file_path$qaqc_data_directory,
                                         "/fcre-targets-insitu.csv")

config_obs$data_location <- config$file_path$data_directory
if(is.null(config_obs$combined_obs_file)){
  in_situ_qaqc(insitu_obs_fname = file.path(config_obs$data_location,config_obs$insitu_obs_fname),
               data_location = config_obs$data_location,
               maintenance_file = file.path(config_obs$data_location,config_obs$maintenance_file),
               ctd_fname = file.path(config_obs$data_location,config_obs$ctd_fname),
               nutrients_fname =  file.path(config_obs$data_location, config_obs$nutrients_fname),
               secchi_fname = file.path(config_obs$data_location, config_obs$secchi_fname),
               cleaned_observations_file_long = cleaned_observations_file_long,
               lake_name_code = config_obs$lake_name_code,
               config = config_obs)
}else{
  file.copy(file.path(config$file_path$data_directory,config_obs$combined_obs_file), cleaned_observations_file_long, overwrite = TRUE)
}

if(s3_mode){
  aws.s3::put_object(file = cleaned_observations_file_long, object = file.path(forecast_site, paste0(forecast_site, "-targets-insitu.csv")), bucket = "targets")
  aws.s3::put_object(file = cleaned_inflow_file, object = file.path(forecast_site, paste0(forecast_site, "-targets-inflow.csv")), bucket = "targets")
  aws.s3::put_object(file = file.path(config$file_path$qaqc_data_directory, paste0("observed-met_",forecast_site, ".nc")), object = file.path(forecast_site, paste0("observed-met-noaa_",forecast_site,".nc")), bucket = "targets")
}