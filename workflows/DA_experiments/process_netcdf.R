#' FLARE-LER-MS Analysis
#' Updated for BVR FLARE forecasts on 29Sep 22

library(magrittr)
library(ggplot2)
library(rLakeAnalyzer)

lake_directory <- here::here()

da_freq <- c("weekly","daily", "fortnightly", "monthly") # Add names for your DA_freq
config_set_name <- "DA_experiments"
configure_run_file <- "configure_run.yml"

source(file.path(lake_directory, "R", "read_flare_temp.R"))
source(file.path(lake_directory, "R", "combine_forecast_observation.R"))

config <- FLAREr::set_configuration(configure_run_file, lake_directory,
                                    config_set_name = config_set_name)

# Read in FLARE observations ----
target_file <- file.path(config$file_path$qaqc_data_directory,
                         "bvre-targets-insitu.csv")
obs <- read.csv(target_file)
wtemp <- obs[obs$variable == "temperature", ] # Subset to temperature

sub <- wtemp[wtemp$date > "2021-01-01" & wtemp$date < "2022-01-02", ] # Subset to target dates - need to change for your experiment
sub$Date <- as.Date(sub$date) # Line
sub$dens <- rLakeAnalyzer::water.density(sub$value) # density for stratification

# Classifies phenology
phen <- plyr::ddply(sub, "Date", function(x) {
  surf <- x$dens[x$depth == min(x$depth)]
  bott <- x$dens[x$depth == max(x$depth)]
  if(nrow(x) == 1) return(data.frame(status = NA))
  status <- ifelse(abs(surf - bott) < 0.1, "Mixed", "Stratified")
  data.frame(status = status)
})

#round depths up
sub$depth <- floor(sub$depth)

#add thermocline depth
thermocline_depths <- sub %>% group_by(Date) %>% 
  summarize(thermo = thermo.depth(value,depth))

ggplot(thermocline_depths,aes(Date,thermo)) + geom_line() + theme_bw()

# Fig 3 - phenology plot w/ temp at different depths 
ggplot(sub) +   theme_bw() +
  geom_rect(data = phen, aes(fill = "Mixed"), xmin=-Inf ,xmax = as.Date("2021-03-12"), ymin = -Inf, ymax = Inf, inherit.aes = FALSE) + 
  geom_rect(data = phen, aes(fill = "Stratified"), xmin=as.Date("2021-03-13") ,xmax = as.Date("2021-11-07"), ymin = -Inf, ymax = Inf, inherit.aes = FALSE)+
  geom_rect(data = phen, aes(fill = "Mixed"), xmin=as.Date("2021-11-08") ,xmax = Inf, ymin = -Inf, ymax = Inf, inherit.aes = FALSE) +
  geom_line(aes(Date, as.numeric(value), color = factor(depth)), size=0.2) + ylab(expression("Temperature ("*~degree*C*")")) + xlab("") +
  theme(text = element_text(size=8), axis.text = element_text(size=6, color="black"), legend.position = c(0.11,0.59), legend.background = element_blank(),
        legend.key = element_blank(), legend.key.height = unit(0.3,"cm"), legend.key.width = unit(0.4,"cm"), legend.spacing.y = unit(0.01,"cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_fill_manual('', values = c('gray','white')) +
  guides(color = guide_legend("Depth (m)"), fill= guide_legend(order = 1, override.aes= list(color="black")))
#ggsave(file.path(lake_directory,"analysis/figures/2021_watertemp_mixedVstratified.jpg"))

#min(sub$value[which(sub$Date>="2021-11-08" | sub$Date<="2021-03-12")])
#max(sub$value[which(sub$Date>="2021-11-08" | sub$Date<="2021-03-12")]) #mixed
#min(sub$value[which(sub$Date>="2021-03-13" & sub$Date<="2021-11-09")])
#max(sub$value[which(sub$Date>="2021-03-13" & sub$Date<="2021-11-09")]) #mixed

for(m in seq_len(length(da_freq))) {

  fc_dir <- file.path(config$file_path$forecast_output_directory, config_set_name,"fixed_params" ,da_freq[m]) # Needs to be changed
  fc_files <- list.files(fc_dir, pattern = "*.nc", full.names = TRUE)

  for(f in fc_files) {

    tmp <- read_flare_temp(f)


    # Forecasts checks - some LER forecasts are bad files...
    if(tmp[[1]]$forecast_date == "No forecast") { 
      message("No forecast in ", basename(f))
      next
    }
    if(length(dim(tmp[[1]]$temp)) < 3) {
      message("Not enough dimensions in ", basename(f))
      next
    }

    res <- combine_forecast_observation(temp = tmp, obs = sub) # Combine forecasts & calculate turnover pct

    dir.create(file.path(config$file_path$analysis_directory,"summary_files","fixed_params", da_freq[m]), showWarnings = FALSE) # Create output dir for csv files
    write.csv(res, file.path(config$file_path$analysis_directory, "summary_files","fixed_params", da_freq[m], paste0("forecast_summary_", res$forecast_date[1], ".csv")),
              row.names = FALSE, quote = FALSE)
    message("[", Sys.time(), "] Finished ", basename(f))
  }
}

# end

