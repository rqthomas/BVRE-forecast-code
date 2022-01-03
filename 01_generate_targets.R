library(tidyverse)
library(lubridate)

message("Beginning generate targets")

#' Set the lake directory to the repository directory

lake_directory <- here::here()

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

#' Source the R files in the repository

source(file.path(lake_directory, "R", "met_qaqc.R"))
source(file.path(lake_directory, "R", "in_situ_qaqc.R"))
source(file.path(lake_directory, "R", "temp_oxy_chla_qaqc.R"))
source(file.path(lake_directory, "R", "extract_CTD.R"))
source(file.path(lake_directory, "R", "extract_secchi.R"))
source(file.path(lake_directory, "R", "extract_nutrients.R"))
source(file.path(lake_directory, "R", "extract_ch4.R"))

#' Generate the `config_obs` object and create directories if necessary

config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml")
dir.create(file.path(lake_directory, "targets", config_obs$site_id), showWarnings = FALSE)
use_s3 <- TRUE

#' Clone or pull from data repositories

FLAREr::get_git_repo(lake_directory,
             directory = config_obs$realtime_insitu_location,
             git_repo = "https://github.com/FLARE-forecast/BVRE-data.git")

FLAREr::get_git_repo(lake_directory,
             directory = config_obs$realtime_insitu_location,
             git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")

download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-metstation-data/FCRmet.csv?raw=true",
              "data_raw/fcre-metstation-data/FCRmet.csv")

FLAREr::get_git_repo(lake_directory,
             directory = config_obs$realtime_met_station_location,
             git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")

#' Download various files from the BVR-GLM repo - get these onto the s3 bucket??

download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/field_data/field_gases.csv?raw=true",
              "data_raw/field_gasses.csv") 

#INFLOW - currently using a file on my local bvr_glm repo - could go in s3 bucket as "bvre-targets-inflow.csv" 

#OUTFLOW
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_spillway_outflow_2014_2019_20200917_nldasInflow.csv?raw=true",
              "data_raw/BVR_spillway_outflow_2014_2019_20200917_nldasInflow.csv")


#download NLDAS data (note: will need to grab new one once appended with 2020/2021 data) - only need this for filling in missing met days in bvr inflow file
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_GLM_NLDAS_010113_123119_GMTadjusted.csv?raw=true",
              "data_raw/BVR_GLM_NLDAS_010113_123119_GMTadjusted.csv")


#' Download files from EDI

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/389/5/3d1866fecfb8e17dc902c76436239431",
             file = config_obs$met_raw_obs_fname[2],
             lake_directory)

#get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/271/5/c1b1f16b8e3edbbff15444824b65fe8f",
#             file = config_obs$insitu_obs_fname[2],
#             lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/198/8/336d0a27c4ae396a75f4c07c01652985",
             file = config_obs$secchi_fname,
             lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/200/11/d771f5e9956304424c3bc0a39298a5ce",
             file = config_obs$ctd_fname,
             lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/199/8/da174082a3d924e989d3151924f9ef98",
             file = config_obs$nutrients_fname,
             lake_directory)

#' Clean up observed meteorology
cleaned_met_file <- met_qaqc(realtime_file = file.path(lake_directory, "data_raw", config_obs$met_raw_obs_fname[1]),
                             qaqc_file = file.path(lake_directory, "data_raw", config_obs$met_raw_obs_fname[2]),
                             cleaned_met_file = file.path(lake_directory, "targets", config_obs$site_id, paste0("observed-met_",config_obs$site_id,".nc")),
                             input_file_tz = "EST",
                             nldas = NULL)

#' Clean up observed insitu measurements
cleaned_insitu_file <- in_situ_qaqc(insitu_obs_fname = file.path(lake_directory,"data_raw", config_obs$insitu_obs_fname),
                                    data_location = file.path(lake_directory,"data_raw"),
             maintenance_file = file.path(lake_directory, "data_raw", config_obs$maintenance_file),
             ctd_fname = NA,
             nutrients_fname =  NA,
             secchi_fname = NA,
             cleaned_insitu_file = file.path(lake_directory,"targets", config_obs$site_id, paste0(config_obs$site_id,"-targets-insitu.csv")),
             lake_name_code = config_obs$site_id,
             config_obs = config_obs)

#' Move targets to s3 bucket

message("Successfully generated targets")

FLAREr::put_targets(site_id = config_obs$site_id,
            cleaned_insitu_file,
            cleaned_met_file,
            use_s3 = TRUE)

message("Successfully moved targets to s3 bucket")

