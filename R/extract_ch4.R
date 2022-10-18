extract_ch4 <- function(fname,
                        input_file_tz,
                        focal_depths){

  d <- read_csv(fname, guess_max = 1000000,
                col_types = readr::cols()) %>%
    dplyr::mutate(DateTime = as_datetime(DateTime),
           DateTime = force_tz(DateTime, tzone = input_file_tz),
           DateTime = DateTime + hours(12))%>%
    dplyr::filter(Depth < 13.5) %>%
    dplyr::mutate(CH4 = CAR_ch4 * 1000 * 0.001) %>%
    dplyr::select(DateTime, Depth,CH4) %>%
    dplyr::group_by(DateTime,Depth) %>%
    dplyr::summarise(CH4 = mean(CH4, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::rename("time" = DateTime,
           "depth" = Depth) %>%
    tidyr::pivot_longer(cols = -c(time, depth), names_to = "variable", values_to = "observed") %>%
    dplyr::mutate(method = "grab_sample") %>%
    dplyr::filter(!is.na(observed)) %>%
    dplyr::select(time , depth, observed, variable, method)

  if(!is.na(focal_depths)){
    d <- d %>% filter(depth %in% focal_depths)
  }

  return(d)
}
