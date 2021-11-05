lake_directory <- here::here()
#download FCR met data
download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-metstation-data/FCRmet.csv?raw=true",
              "data_raw/FCRmet.csv",method="curl")

#download EDI fcr met file
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.389.5&entityid=3d1866fecfb8e17dc902c76436239431",
              "data_raw/Met_final_2015_2020.csv",method="curl")

#download secchi up to 2020
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.198.8&entityid=336d0a27c4ae396a75f4c07c01652985",
              "data_raw/Secchi_depth_2013_2020.csv",method="curl")

#download BVR sensor files
download.file("https://github.com/FLARE-forecast/BVRE-data/blob/master/bjorn?raw=true",
              "data_raw/bjorn",method="curl")
download.file("https://github.com/FLARE-forecast/BVRE-data/blob/bvre-platform-data/BVRplatform.csv?raw=true",
              "data_raw/BVRplatform.csv",method="curl")
download.file("https://github.com/FLARE-forecast/BVRE-data/blob/bvre-platform-data/BVR_maintenance_log.txt?raw=true",
              "data_raw/BVR_maintenance_log.txt",method="curl")