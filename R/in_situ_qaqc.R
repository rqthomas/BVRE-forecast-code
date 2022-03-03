in_situ_qaqc <- function(insitu_obs_fname,
                         data_location,
                         maintenance_file,
                         ctd_fname,
                         nutrients_fname,
                         secchi_fname,
                         cleaned_insitu_file,
                         site_id,
                         config_obs){

  print("QAQC BVR sensors")

  d <- temp_oxy_chla_qaqc(realtime_file = insitu_obs_fname[1],
                          qaqc_file = insitu_obs_fname[2],
                          offset_file = insitu_obs_fname[3],
                          maintenance_file = maintenance_file,
                          config_obs = config_obs)

  if(exists("ctd_fname")){
    if(!is.na(ctd_fname)){
      print("QAQC CTD")
      d_ctd <- extract_CTD(fname = file.path(config_obs$file_path$data_directory,config_obs$ctd_fname),
                           input_file_tz = "EST",
                           local_tzone = config_obs$local_tzone,
                           focal_depths = config_obs$focal_depths,
                           config_obs = config_obs)
      d <- rbind(d,d_ctd)
    }
  }


  if(exists("nutrients_fname")){
    if(!is.na(nutrients_fname)){
      print("QAQC Nutrients")
      d_nutrients <- extract_nutrients(fname = file.path(config_obs$file_path$data_directory,config_obs$nutrients_fname),
                                       input_file_tz = "EST",
                                       local_tzone = config_obs$local_tzone,
                                       focal_depths = config_obs$focal_depths)
      d <- rbind(d,d_nutrients)
    }
  }


  if(exists("ch4_fname")){
    if(!is.na(ch4_fname)){
      print("QAQC CH4")
      d_ch4 <- extract_ch4(fname = file.path(config_obs$file_path$data_directory,config_obs$ch4_fname),
                           input_file_tz = "EST",
                           local_tzone  = config_obs$local_tzone,
                           focal_depths = config_obs$focal_depths)
      d <- rbind(d,d_ch4)
    }
  }
  
  #drop NA rows
  d <- d[!is.na(d$value),]
  
  first_day  <- lubridate::as_datetime(paste0(lubridate::as_date(min(d$timestamp)), " ", config_obs$averaging_period_starting_hour))
  first_day <- lubridate::force_tz(first_day, tzone = "UTC")

  last_day <- lubridate::as_datetime(paste0(lubridate::as_date(max(d$timestamp)), " ", config_obs$averaging_period_starting_hour))
  last_day <- lubridate::force_tz(last_day, tzone = "UTC")

  full_time_local <- seq(first_day, last_day, by = "1 day")

  d_clean <- NULL


  for(i in 1:length(config_obs$target_variable)){
    print(paste0("Extracting ",config_obs$target_variable[i]))
    #depth_breaks <- sort(c(bins1, bins2))
    time_breaks <- seq(first_day, last_day, by = config_obs$averaging_period[i])

    d_curr <- d %>%
      dplyr::filter(variable == config_obs$target_variable[i],
                    method %in% config_obs$measurement_methods[[i]]) %>% #NOTE, ADDED CTD AS METHOD SO WE HAVE TEMP AND DO DATA BEFORE 2020
      dplyr::mutate(time_class = cut(timestamp, breaks = time_breaks, labels = FALSE)) %>%
      dplyr::group_by(time_class, depth) %>%
      dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(datetime = time_breaks[time_class]) %>%
      dplyr::mutate(variable = config_obs$target_variable[i]) %>%
      dplyr::select(datetime, depth, variable, value) %>%
      dplyr::mutate(date = lubridate::as_date(datetime))

    if(config_obs$averaging_period[i] == "1 hour"){
      d_curr <- d_curr %>%
      dplyr::mutate(hour = lubridate::hour(datetime)) %>%
      dplyr::filter(hour == lubridate::hour(first_day))  #getting rid of this filter because I need more observations!
    }else{
      d_curr <- d_curr %>%
        dplyr::mutate(hour = NA) %>%
        dplyr::mutate(hour = as.numeric(hour))
    }

    d_curr <- d_curr %>% dplyr::select(-datetime)

    d_clean <- rbind(d_clean,  d_curr) 
  }

  d_clean <- d_clean %>% tidyr::drop_na(value)

  if(!is.na(secchi_fname)){

    d_secchi <- extract_secchi(fname = secchi_fname,
                               input_file_tz = "EST",
                               local_tzone  = config_obs$local_tzone,
                               focal_depths = config_obs$focal_depths)

    d_secchi <- d_secchi %>%
      dplyr::mutate(date = lubridate::as_date(timestamp)) %>%
      dplyr::mutate(hour = 0) %>%
      dplyr::select(-timestamp)

    d_clean <- rbind(d_clean,d_secchi) 
  }

  d_clean <- d_clean %>% dplyr::select(date, hour, depth, value, variable)
  
  d_clean$value <- round(d_clean$value, digits = 4)
  
  #selectively withholding obs to test effects on DA/forecast skill
  dates <- unique(d_clean$date[d_clean$variable=="temperature"])
  every_other_dates <- dates[seq(1, length(dates), 2)]
  every_5_dates <- dates[seq(1, length(dates), 5)]
  weekly_dates <- dates[seq(1, length(dates), 7)]
  fortnightly_dates <- dates[seq(1, length(dates), 14)]
  monthly_dates <- dates[seq(1, length(dates), 30)]
  
  #specify DA frequency to subset observation dataset
  DA_frequency <- dates
  
  #final obs dataset to be exported
  d_clean <- d_clean[d_clean$variable=="temperature" & d_clean$date %in% DA_frequency | d_clean$variable!="temperature",]

  readr::write_csv(d_clean, cleaned_insitu_file)
  
  return(cleaned_insitu_file)

}
       
