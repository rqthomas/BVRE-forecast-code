combine_forecast_observation <- function(temp, obs, tol = 0.1) {

  obs <- obs[obs$Date %in% temp[[1]]$dates, c("Date", "depth", "value")]
  colnames(obs) <- c("date", "depth", "obs")


  idx_d <- c()
  depths <- unique(obs$depth)
  for(d in depths) {
    idx_d <- c(idx_d, which(temp[[1]]$depths >= d - tol & temp[[1]]$depths <= d + tol))
  }

  dep_table <- data.frame(depth_ref = idx_d, depth = depths)
  date_table <- data.frame(date_ref = 1:length(temp[[1]]$dates), date = temp[[1]]$dates)

  mlt <- reshape::melt(temp[[1]]$temp)
  colnames(mlt) <- c("date_ref", "depth_ref", "ens", "value")
  mlt <- mlt[mlt$depth_ref %in% idx_d, ]

  df2 <- merge(mlt, dep_table, by = "depth_ref")
  df3 <- merge(df2, date_table, by = "date_ref")

  df4 <- merge(df3, obs, by = c("date", "depth"))

  df4 <- df4[, c("date", "date_ref", "depth", "depth_ref", "ens", "value", "obs")]
  colnames(df4)[2] <- "horizon"
  df4$forecast_date <- as.character(lubridate::as_date(df4$date[1]) - lubridate::days(1))

  idx <- which(df4$depth %in% c(min(depths), max(depths)))

  df4$dens <- rLakeAnalyzer::water.density(df4$value)
  pct <- plyr::ddply(df4, "horizon", function(x) {
    surf <- x$dens[x$depth == min(x$depth)]
    bott <- x$dens[x$depth == max(x$depth)]
    data.frame(turnover_pct = sum((abs(surf - bott) < 0.1)) / max(x$ens) * 100)
  })

  df5 <- plyr::ddply(df4, c("forecast_date", "horizon", "depth"), function(x) {
    data.frame(mean = mean(x$value),
               sd = sd(x$value),
               obs = x$obs[1])
  })
  df5 <- merge(df5, pct, by = "horizon")
  return(df5)
}
