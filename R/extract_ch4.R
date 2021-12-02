extract_ch4 <- function(fname,
                        input_file_tz,
                        local_tzone,
                        focal_depths){

  d <- read_csv(fname, guess_max = 1000000,
                col_types = readr::cols()) %>%
    mutate(DateTime = as_datetime(DateTime),
           DateTime = force_tz(DateTime, tzone = input_file_tz),
           DateTime = DateTime + hours(12))%>%
    filter(Depth < 13.5) %>%
    mutate(CH4 = CAR_ch4 * 1000 * 0.001) %>%
    dplyr::select(DateTime, Depth,CH4) %>%
    group_by(DateTime,Depth) %>%
    summarise(CH4 = mean(CH4, na.rm = TRUE)) %>%
    ungroup() %>%
    rename("timestamp" = DateTime,
           "depth" = Depth) %>%
    pivot_longer(cols = -c(timestamp, depth), names_to = "variable", values_to = "value") %>%
    mutate(method = "grab_sample") %>%
    filter(!is.na(value)) %>%
    dplyr::select(timestamp , depth, value, variable, method)

  if(!is.na(focal_depths)){
    d <- d %>% filter(depth %in% focal_depths)
  }

  return(d)
}
