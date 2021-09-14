lake_directory <- getwd()
config_obs <- yaml::read_yaml(file.path(lake_directory, "configuration/observation_processing/observation_processing.yml"))
config <- yaml::read_yaml(file.path(lake_directory, "configuration/FLAREr/configure_flare.yml"))
config$data_location <- file.path(lake_directory,"data_raw")
run_config <- yaml::read_yaml(file.path(lake_directory, "configuration/FLAREr","configure_run.yml"))

#download INFLOW files from BVR GLM repo
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_inflow_2014_2019_20200708_allfractions_2poolsDOC_withch4.csv?raw=true",
              "data_raw/BVR_inflow_2014_2019_20200708_allfractions_2poolsDOC_withch4.csv")
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_inflow_2014_2019_20200917_allfractions_2poolsDOC_withch4_nldasInflow.csv?raw=true",
              "data_raw/BVR_inflow_2014_2019_20200917_allfractions_2poolsDOC_withch4_nldasInflow.csv")

#download OUTFLOW file from BVR GLM repo
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_spillway_outflow_2014_2019_20200917_nldasInflow.csv?raw=true",
              "data_raw/BVR_spillway_outflow_2014_2019_20200917_nldasInflow.csv")

#download chem file from BVR GLM repo
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/chem.csv?raw=true",
              "data_raw/chem.csv")

#download NLDAS data (note: will need to grab new one once appended with 2020/2021 data)
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_GLM_NLDAS_010113_123119_GMTadjusted.csv?raw=true",
              "data_raw/BVR_GLM_NLDAS_010113_123119_GMTadjusted.csv")

#download CTD data from EDI
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.200.11&entityid=d771f5e9956304424c3bc0a39298a5ce",
              "data_raw/CTD_final_2013_2020.csv")

#download various field_data files
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/field_data/CTD_final_2013_2019.csv?raw=true",
              "data_raw/CTD_final_2013_2019.csv")
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/field_data/CleanedObsChla.csv?raw=true",
              "data_raw/CleanedObsChla.csv")
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/field_data/CleanedObsOxy.csv?raw=true",
              "data_raw/CleanedObsOxy.csv")
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/field_data/CleanedObsTemp.csv?raw=true",
              "data_raw/CleanedObsTemp.csv")
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/field_data/Secchi_depth_2013-2019.csv?raw=true",
              "data_raw/Secchi_depth_2013-2019.csv")
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/field_data/field_gases.csv?raw=true",
              "data_raw/field_gases.csv")

#download FCR met data
download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-metstation-data/FCRmet.csv?raw=true",
              "data_raw/FCRmet.csv")

#download EDI fcr met file
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.389.5&entityid=3d1866fecfb8e17dc902c76436239431",
              "data_raw/Met_final_2015_2020.csv")

#download secchi up to 2020
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.198.8&entityid=336d0a27c4ae396a75f4c07c01652985",
              "data_raw/Secchi_depth_2013_2020.csv")

#download BVR sensor files
download.file("https://github.com/FLARE-forecast/BVRE-data/blob/master/bjorn?raw=true",
              "data_raw/bjorn")
download.file("https://github.com/FLARE-forecast/BVRE-data/blob/bvre-platform-data/BVRplatform.csv?raw=true",
              "data_raw/BVRplatform.csv")
download.file("https://github.com/FLARE-forecast/BVRE-data/blob/bvre-platform-data/BVR_maintenance_log.txt?raw=true",
              "data_raw/BVR_maintenance_log.txt")

#download BVR temp data from EDI
inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.725.1&entityid=9f4d77dc90db2d87e4cdec8b7584d504" 
infile1 <- paste0(config$data_location,"/BVR_EDI_2020.csv")
download.file(inUrl1,infile1,method="curl")

inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.725.1&entityid=1ca1080101e38fb2b4bb9455c1faed5b" 
infile1 <- paste0(config$data_location,"/BVR_Maintenance_2020.csv")
download.file(inUrl1,infile1,method="curl")

inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.725.1&entityid=e05f13c7e54a6e9a3b19a03a37a54e3d" 
infile1 <- paste0(config$data_location,"/BVR_Depth_offsets_2020.csv")
download.file(inUrl1,infile1,method="curl") #not sure if I need this, but can get depth of sensors by subtracting offsets from Depth_m_13 in BVR_EDI_2020.csv

# download NOAA data
# source a function here
#NOTE: the S3 bucket isn't working properly because I can't open the 222 byte nc files - don't know why...

source(file.path(lake_directory, "R", "noaa_download_s3.R"))

# set a start and end date for NOAA forecasts and check which days are not available in local NOAA directory
dates <- seq.Date(as.Date('2021-03-01'), as.Date('2021-04-05'), by = 'day') # cycle through historical dates 
noaa_directory <- file.path(lake_directory, "forecasted_drivers")
outfile <- file.path(lake_directory, 'data_processed') # file path where you want the output file to go
model_name = "observed-met-noaa"
config <- config
hist_file <- 'observed-met-noaa_bvre.nc'

download_dates <- c()
for (i in 1:length(dates)) {
  fpath <- file.path(config$file_path$noaa_directory, config$met$forecast_met_model, "bvre", dates[i])
  if(dir.exists(fpath)){
    message(paste0(dates[i], ' already downloaded'))
  }else{
    download_dates <- c(download_dates, dates[i])
  }
}

download_dates <- na.omit(download_dates)
download_dates <- as.Date(download_dates, origin = '1970-01-01')

if(length(download_dates>1)){
  for (i in 1:length(download_dates)) {
    # for(j in 1:length(cycle)){
    noaa_download_s3(siteID = 'bvre',
                     date = download_dates[i],
                     cycle = '00', #cycle[j],
                     noaa_horizon = run_config$forecast_horizon,
                     noaa_directory = config$file_path$noaa_directory,
                     noaa_model = "noaa/NOAAGEFS_1hr",
                     noaa_hour = 1)
  }
  
}

#nc <- ncdf4::nc_open(file.path(config$file_path$noaa_directory,"bvre/2021-03-01/00/NOAAGEFS_1hr_bvre_2021-03-01T00_2021-03-17T00_ens28.nc"))
#nc <- ncdf4::nc_open(file.path(config$file_path$noaa_directory,"bvre/2021-03-01/00/NOAAGEFS_1hr_fcre_2021-03-01T00_2021-03-17T00_ens14.nc"))

#nc <- ncdf4::nc_open(file.path(config$file_path$qaqc_data_directory,"observed-met-noaa_bvre.nc"))

