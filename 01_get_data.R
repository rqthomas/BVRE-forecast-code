lake_directory <- getwd()
config <- yaml::read_yaml(file.path(lake_directory, "configuration/observation_processing/observation_processing.yml"))
config$data_location <- file.path(lake_directory,"data_raw")

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


