#creating figs using the summary csv files from analysis folder

#read in packages
pacman::p_load(dplyr,readr,ggplot2, FSA, AnalystHelper, rcompanion)

#set wd
lake_directory <- here::here()
setwd(lake_directory)

daily_forecasts <- list.files(file.path(lake_directory,"analysis/summary_files/daily"), pattern="csv", full.names=TRUE)
daily_forecasts <- lapply(daily_forecasts, read_csv) %>% bind_rows() %>% mutate(DA = "Daily")

day2_forecasts <- list.files(file.path(lake_directory,"analysis/summary_files/daily_2"), pattern="csv", full.names=TRUE)
day2_forecasts <- lapply(day2_forecasts, read_csv) %>% bind_rows() %>% mutate(DA = "2Day")

day5_forecasts <- list.files(file.path(lake_directory,"analysis/summary_files/daily_5"), pattern="csv", full.names=TRUE)
day5_forecasts <- lapply(day5_forecasts, read_csv) %>% bind_rows() %>% mutate(DA = "5Day")

weekly_forecasts <- list.files(file.path(lake_directory,"analysis/summary_files/weekly"), pattern="csv", full.names=TRUE)
weekly_forecasts <- lapply(weekly_forecasts, read_csv) %>% bind_rows() %>% mutate(DA = "Weekly")

fortnightly_forecasts <- list.files(file.path(lake_directory,"analysis/summary_files/fortnightly"), pattern="csv", full.names=TRUE)
fortnightly_forecasts <- lapply(fortnightly_forecasts, read_csv) %>% bind_rows() %>% mutate(DA = "Fortnightly")

monthly_forecasts <- list.files(file.path(lake_directory,"analysis/summary_files/monthly"), pattern="csv", full.names=TRUE)
monthly_forecasts <- lapply(monthly_forecasts, read_csv) %>% bind_rows() %>% mutate(DA = "Monthly")

detach(dplyr)
library(plyr)

all_DA_forecasts <- rbind(daily_forecasts, day2_forecasts, day5_forecasts, weekly_forecasts,
                          fortnightly_forecasts, monthly_forecasts)

#round depths to nearest m
all_DA_forecasts$depth <- ceiling(all_DA_forecasts$depth)

#add date column --> forecast_date + horizon
all_DA_forecasts$date <- all_DA_forecasts$forecast_date + all_DA_forecasts$horizon

#add a group number so that I can average horizons later on
all_DA_forecasts <- all_DA_forecasts %>% 
  mutate(group = case_when(all_DA_forecasts$horizon <= 5 ~ "1-5",
                        all_DA_forecasts$horizon <=10 & all_DA_forecasts$horizon > 5 ~ "6-10",
                        all_DA_forecasts$horizon <=15 & all_DA_forecasts$horizon > 10 ~ "11-15",
                        all_DA_forecasts$horizon <=20 & all_DA_forecasts$horizon > 15 ~ "16-20",
                        all_DA_forecasts$horizon <=25 & all_DA_forecasts$horizon > 20 ~ "21-25",
                        all_DA_forecasts$horizon <=30 & all_DA_forecasts$horizon > 25 ~ "26-30",
                        all_DA_forecasts$horizon <=35 & all_DA_forecasts$horizon > 30 ~ "31-35"))

strat_date<- "2021-11-14"

#add stratified vs mixed col
all_DA_forecasts$phen <- ifelse(all_DA_forecasts$date <= as.Date(strat_date) & 
                                  all_DA_forecasts$date >="2021-03-19","Stratified", "Mixed")

#------------------------------------------------------------------------------#
#calculate forecast skill metrics

forecast_skill_depth <- plyr::ddply(all_DA_forecasts, c("phen", "depth", "DA"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

#order DA frequencies
forecast_skill_depth$DA <- factor(forecast_skill_depth$DA, levels=c("Daily", "2Day", "5Day", "Weekly", "Fortnightly", "Monthly"))

#forecast skill for each depth and date
forecast_skill_depth_date <-  plyr::ddply(all_DA_forecasts, c("depth", "forecast_date", "DA"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

##add in mixed/stratified period
forecast_skill_depth_date$phen <- ifelse(forecast_skill_depth_date$forecast_date <= as.Date(strat_date) & 
                                           forecast_skill_depth_date$forecast_date >="2021-03-19","Stratified", "Mixed")


#averaging across depths and horizons
forecast_skill_horizon <- plyr::ddply(all_DA_forecasts, c("forecast_date", "horizon", "DA"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

#add date
forecast_skill_horizon$date <- forecast_skill_horizon$forecast_date + forecast_skill_horizon$horizon

##add in mixed/stratified period
forecast_skill_horizon$phen <- ifelse(forecast_skill_horizon$forecast_date <= as.Date(strat_date) & 
                                      forecast_skill_horizon$forecast_date >="2021-03-19","Stratified", "Mixed")

#order DA frequencies
forecast_skill_horizon$DA <- factor(forecast_skill_horizon$DA, levels=c("Daily", "2Day", "5Day", "Weekly", "Fortnightly", "Monthly"))

#df with all depths and horizons averaged
forecast_skill_avg <- plyr::ddply(all_DA_forecasts, c("date", "DA"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

##add in mixed/stratified period
forecast_skill_avg$phen <- ifelse(forecast_skill_avg$date <= as.Date(strat_date) & 
                                    forecast_skill_avg$date >="2021-03-19","Stratified", "Mixed")

#order DA frequencies
forecast_skill_avg$DA <- factor(forecast_skill_avg$DA, levels=c("Daily", "2Day", "5Day", "Weekly", "Fortnightly", "Monthly"))

#df with averaged forecast skill for all days (group by horizon, DA, and phen)
forecast_horizon_avg <- plyr::ddply(all_DA_forecasts, c("horizon", "DA", "phen"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

#order DA frequencies
forecast_horizon_avg$DA <- factor(forecast_horizon_avg$DA, levels=c("Daily", "2Day", "5Day", "Weekly", "Fortnightly", "Monthly"))


#df with averaged forecast skill for all days (group by horizon, DA, phen, and depth)
forecast_horizon_depth_avg <- plyr::ddply(all_DA_forecasts, c("horizon", "DA", "phen", "depth"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

#order DA frequencies
forecast_horizon_depth_avg$DA <- factor(forecast_horizon_depth_avg$DA, levels=c("Daily", "2Day", "5Day", "Weekly", "Fortnightly", "Monthly"))


#------------------------------------------------------------------------------#
#FIGURES

cb_friendly <- c("#117733", "#332288","#AA4499", "#44AA99", "#999933", "#661100")
cb_friendly_2 <- c("#8C510A", "#BF812D", "#DFC27D","#DEDEDE", "#C7EAE5", "#35978F")


#predicting turnover
#only select surface depth becuase other depths would just be repetitive
surf_temps <- plyr::ddply(all_DA_forecasts[all_DA_forecasts$depth==1,], c("date", "DA","group"), function(x) {
    data.frame(
        temp = mean(x$mean),
        sd = mean(x$sd),
        obs = mean(x$obs),
        turnover_pct = mean(x$turnover_pct)
    )
})
  
#order DA frequencies
surf_temps$DA <- factor(surf_temps$DA, levels = c("Daily", "2Day", "5Day", "Weekly", "Fortnightly", "Monthly"))
surf_temps$group <- factor(surf_temps$group, levels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35"))

strat_date <- "2021-11-14"

ggplot(subset(surf_temps, date>=as.Date(strat_date)-15 & date <=as.Date(strat_date)+15), 
       aes(date, turnover_pct, color=as.factor(group))) + geom_line() + facet_wrap(~DA) + theme_bw() +
  theme(panel.grid.minor = element_blank()) + scale_colour_viridis_d(option="C") +
  geom_vline(xintercept=as.Date(strat_date), linetype=4) +
  guides(color = guide_legend(title="Horizon (days)")) + xlab("") + ylab("% Turnover")
ggsave(file.path(lake_directory,"analysis/figures/Turnover_pred_DA_freq.jpg"))


#looking at how well each DA freq predicts turnover 
idx <- which(all_DA_forecasts$forecast_date >= (as.Date(strat_date) - lubridate::days(35)) &
               all_DA_forecasts$forecast_date <= (as.Date(strat_date)))

res <- plyr::ddply(all_DA_forecasts[idx, ], c("forecast_date", "DA"), function(x) {
  
  data.frame(turnover_pct = x$turnover_pct[x$date == strat_date])
  
})

#order based on DA 
res$DA <- factor(res$DA, levels = c("Daily", "2Day", "5Day", "Weekly", "Fortnightly", "Monthly"))

ggplot(res) +
  geom_line(aes(forecast_date, turnover_pct, color = DA)) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(values=cb_friendly)  +guides(color = guide_legend(title="DA frequency")) +
  xlab("") + ylab("% Turnover") + geom_hline(yintercept=60, linetype=4) 
#take-home - 2day and daily do pretty well, monthly misses it completely, others get it a few days before
ggsave(file.path(lake_directory,"analysis/figures/Turnover_oct-nov_DA_freq.jpg"))

#add a binary column to visualize another way
res$predict <- ifelse(res$turnover_pct>=60, "yes", "no")

ggplot(res, aes(forecast_date, predict, color=DA)) + geom_point() + theme_bw() + 
  facet_wrap(~DA) +scale_color_manual(values=cb_friendly) 

ggplot(res, aes(forecast_date, predict, color=DA)) + geom_boxplot() + theme_bw() + 
  facet_wrap(~DA) + scale_color_manual(values=cb_friendly) 

#------------------------------------------------------------------------------#
# Forecast Skill Figs and stats


#test significance of da frequencies in stratified period (anova)
qqnorm(forecast_horizon_avg$RMSE)
qqline(forecast_horizon_avg$RMSE) #data does not follow normal distribution
bartlett.test(forecast_horizon_avg$RMSE, forecast_horizon_avg$DA) #variances are not the same

#using kruskal wallis because data do not meet anova assumptions
kruskal.test(forecast_horizon_avg$RMSE[forecast_horizon_avg$phen=="Stratified"] ~ forecast_horizon_avg$DA[forecast_horizon_avg$phen=="Stratified"])
# p-value = 0.000026 so rmse DOES differ between DA frequencies
kruskal.test(forecast_horizon_avg$RMSE[forecast_horizon_avg$phen=="Mixed"] ~ forecast_horizon_avg$DA[forecast_horizon_avg$phen=="Mixed"])
#not significant

#now dunn test to determine which groups are different 
dunn_strat <- dunnTest(forecast_horizon_avg$RMSE[forecast_horizon_avg$phen=="Stratified"] ~ forecast_horizon_avg$DA[forecast_horizon_avg$phen=="Stratified"], method="bonferroni")
#daily-fortnightly, 2day-weekly, 5day-weekly, daily-weekly have p<0.05

rslt_strat=toupper(cldList(P.adj ~ Comparison, data=dunn_strat$res, threshold = 0.05)$Letter)


ann_text <- data.frame(DA=c("Daily","2Day","5Day","Weekly","Fortnightly","Monthly"),RMSE=c(2.38,2.15,2.3,1.8,1.85,1.95),lab = c("a","ab","ab","c","bc","abc"),
                       phen = factor("Stratified",levels = c("Mixed","Stratified")))

#horizon and depth averaged skill for mixed vs strat
ggplot(forecast_horizon_avg, aes(DA, RMSE, fill=DA)) +geom_boxplot() + xlab("") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  facet_wrap(~phen) + scale_fill_manual(values=cb_friendly_2) + guides(fill=guide_legend(title="DA frequency")) +
  geom_text(data = ann_text,label = as.factor(ann_text$lab), hjust = 0.5, vjust =-1.5)
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfreq_phen.jpg"))


ggplot(forecast_horizon_avg, aes(DA, CRPS, fill=DA)) + geom_boxplot() + xlab("") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  facet_wrap(~phen) + scale_fill_manual(values=cb_friendly_2) +   guides(fill=guide_legend(title="DA frequency"))
ggsave(file.path(lake_directory,"analysis/figures/pbiasvsDAfreq_phen.jpg"))


#skill across different horizons

#kruskal wallis and dunn tests for 1 day ahead
kruskal.test(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Stratified" & forecast_skill_horizon$horizon==1] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Stratified"& forecast_skill_horizon$horizon==1])
dunn_strat_1d <- dunnTest(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Stratified"& forecast_skill_horizon$horizon==1] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Stratified"& forecast_skill_horizon$horizon==1], method="bonferroni")
rslt_strat_1d=toupper(cldList(P.adj ~ Comparison, data=dunn_strat_1d$res, threshold = 0.05)$Letter)
ann_text_1d_strat <- data.frame(DA=c("2Day","5Day","Daily","Fortnightly","Monthly","Weekly"),RMSE=c(0.6,0.7,0.5,0.9,1.3,0.7),lab = c("ab","ac","b","d","e","c"),
                                phen = factor("Stratified",levels = c("Mixed","Stratified")))


kruskal.test(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Mixed" & forecast_skill_horizon$horizon==1] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Mixed"& forecast_skill_horizon$horizon==1])
dunn_mix_1d <- dunnTest(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Mixed"& forecast_skill_horizon$horizon==1] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Mixed"& forecast_skill_horizon$horizon==1], method="bonferroni")
rslt_mix_1d=toupper(cldList(P.adj ~ Comparison, data=dunn_mix_1d$res, threshold = 0.05)$Letter)
ann_text_1d_mixed <- data.frame(DA=c("2Day","5Day","Daily","Fortnightly","Monthly","Weekly"),RMSE=c(0.4,0.5,0.4,0.5,0.7,0.5),lab = c("ab","a","b","a","c","ab"),
                                phen = factor("Mixed",levels = c("Mixed","Stratified")))

ggplot(subset(forecast_skill_horizon, horizon==1), aes(DA, RMSE, fill=DA)) + geom_boxplot() +  xlab("") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  facet_wrap(~phen) + scale_fill_manual(values=cb_friendly_2) +   guides(fill=guide_legend(title="DA frequency")) +
geom_text(data = ann_text_1d_mixed,label = as.factor(ann_text_1d_mixed$lab), hjust = 1.7, vjust =-1.5) +
geom_text(data = ann_text_1d_strat,label = as.factor(ann_text_1d_strat$lab), hjust = 1.7, vjust =-1.5)
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfreq_phen_1day.jpg"))


#kruskal wallis and dunn tests for 7days ahead
kruskal.test(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Stratified" & forecast_skill_horizon$horizon==7] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Stratified"& forecast_skill_horizon$horizon==7])
dunn_strat_7d <- dunnTest(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Stratified"& forecast_skill_horizon$horizon==7] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Stratified"& forecast_skill_horizon$horizon==7], method="bonferroni")
rslt_strat_7d=toupper(cldList(P.adj ~ Comparison, data=dunn_strat_7d$res, threshold = 0.05)$Letter)
ann_text_7d_strat <- data.frame(DA=c("2Day","5Day","Daily","Fortnightly","Monthly","Weekly"),RMSE=c(1.4,1.6,1.5,1.4,1.6,1.2),lab = c("ab","ac","b","d","e","c"),
                                phen = factor("Stratified",levels = c("Mixed","Stratified")))


kruskal.test(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Mixed" & forecast_skill_horizon$horizon==7] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Mixed"& forecast_skill_horizon$horizon==7])
dunn_mix_7d <- dunnTest(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Mixed"& forecast_skill_horizon$horizon==7] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Mixed"& forecast_skill_horizon$horizon==7], method="bonferroni")
rslt_mix_7d=toupper(cldList(P.adj ~ Comparison, data=dunn_mix_7d$res, threshold = 0.05)$Letter)
ann_text_7d_mixed <- data.frame(DA=c("2Day","5Day","Daily","Fortnightly","Monthly","Weekly"),RMSE=c(1,0.9,0.9,0.9,1,0.8),lab = c("ab","a","b","a","c","ab"),
                                phen = factor("Mixed",levels = c("Mixed","Stratified")))

ggplot(subset(forecast_skill_horizon, horizon==7), aes(DA, RMSE, fill=DA)) + geom_boxplot() +  xlab("") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  facet_wrap(~phen) + scale_fill_manual(values=cb_friendly_2) +   guides(fill=guide_legend(title="DA frequency")) +
  geom_text(data = ann_text_7d_strat,label = as.factor(ann_text_7d_strat$lab), hjust = 1.7, vjust =-1.5) #mixed is not sig different
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfreq_phen_7day.jpg"))


#kruskal wallis and dunn tests for 30 days ahead
kruskal.test(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Stratified" & forecast_skill_horizon$horizon==30] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Stratified"& forecast_skill_horizon$horizon==30])
dunn_strat_30d <- dunnTest(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Stratified"& forecast_skill_horizon$horizon==30] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Stratified"& forecast_skill_horizon$horizon==30], method="bonferroni")
rslt_strat_30d=toupper(cldList(P.adj ~ Comparison, data=dunn_strat_30d$res, threshold = 0.05)$Letter)
ann_text_30d_strat <- data.frame(DA=c("2Day","5Day","Daily","Fortnightly","Monthly","Weekly"),RMSE=c(2.6,2.7,2.8,2.2,2.2,2.1),lab = c("a","b","b","cd","ac","d"),
                                phen = factor("Stratified",levels = c("Mixed","Stratified")))


kruskal.test(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Mixed" & forecast_skill_horizon$horizon==30] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Mixed"& forecast_skill_horizon$horizon==30])
dunn_mix_30d <- dunnTest(forecast_skill_horizon$RMSE[forecast_skill_horizon$phen=="Mixed"& forecast_skill_horizon$horizon==30] ~ forecast_skill_horizon$DA[forecast_skill_horizon$phen=="Mixed"& forecast_skill_horizon$horizon==30], method="bonferroni")
rslt_mix_30d=toupper(cldList(P.adj ~ Comparison, data=dunn_mix_30d$res, threshold = 0.05)$Letter)
ann_text_30d_mixed <- data.frame(DA=c("2Day","5Day","Daily","Fortnightly","Monthly","Weekly"),RMSE=c(1,0.9,0.9,0.9,1,0.8),lab = c("ab","a","b","a","c","ab"),
                                phen = factor("Mixed",levels = c("Mixed","Stratified")))

ggplot(subset(forecast_skill_horizon, horizon==30), aes(DA, RMSE, fill=DA)) + geom_boxplot() +  xlab("") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  facet_wrap(~phen) + scale_fill_manual(values=cb_friendly_2) + guides(fill=guide_legend(title="DA frequency")) +
  geom_text(data = ann_text_30d_strat,label = as.factor(ann_text_30d_strat$lab), hjust = 1.7, vjust =-1.5) #mixed is not sig different
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfreq_phen_30day.jpg"))

#depth forecasts
ggplot(forecast_skill_depth, aes(RMSE, depth, color=DA)) +geom_path(size=1.5) + facet_wrap(~phen)+
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + ylab("Depth (m)") +
  scale_y_reverse() + scale_color_manual(values=cb_friendly_2) + guides(color=guide_legend(title="DA frequency"))
ggsave(file.path(lake_directory,"analysis/figures/DepthvsRMSE_allfreqs_phen.jpg"))

ggplot(subset(forecast_skill_depth_date, depth==10), aes(DA, RMSE, fill=DA)) + geom_boxplot() +  xlab("10m") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  facet_wrap(~phen) + scale_fill_manual(values=cb_friendly_2) + guides(fill=guide_legend(title="DA frequency")) 
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfreq_10m.jpg"))

#depth by horizon (no stratified vs. mixed)
#ggplot(forecast_skill_depth_horizon, aes(RMSE, depth, color=DA, horizon==35)) +geom_path(size=1.5) +
#  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + ylab("Depth (m)") +
#  scale_y_reverse() + scale_color_manual(values=cb_friendly_2) + guides(color=guide_legend(title="DA frequency"))
#doesn't show anything different among horizons??

#horizon forecast figs
ggplot(forecast_horizon_avg, aes(horizon, RMSE, color=DA)) +geom_path(size=1.5) + facet_wrap(~phen)+
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_color_manual(values=cb_friendly_2) + guides(color=guide_legend(title="DA frequency")) +xlab("Horizon (days)")
ggsave(file.path(lake_directory,"analysis/figures/RMSEvshorizon_allfreqs_phen.jpg"))

##Final plots looking at 1-day ahead forecast skill 
ggplot(subset(forecast_horizon_avg, horizon==1), aes(DA, RMSE, color=DA, shape=phen)) + xlab("") + 
  theme_bw() + theme(text = element_text(size=14), axis.text = element_text(size=14, color="black"), legend.key.size = unit(0.5, "cm"),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_point(size=4) + guides(color = guide_legend(title="DA frequency"), shape=guide_legend(title="")) + scale_color_manual(values=cb_friendly_2)
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsfreq_phen_1day.jpg")) 

#round rmse to nearest 0.5 for tile plot below
forecast_horizon_avg$RMSE_bins <- round_any(forecast_horizon_avg$RMSE,0.5) 
forecast_horizon_depth_avg$RMSE_bins <- round_any(forecast_horizon_depth_avg$RMSE,0.5) 

#figure for horizon vs frequency to compare forecast skill
ggplot(forecast_horizon_avg, aes(DA, horizon, fill=RMSE_bins)) + geom_tile(color="black") + xlab("") +
  ylab("Horizon (days)") + theme_bw() + facet_wrap(~phen) +
  theme(text = element_text(size=14), axis.text = element_text(size=14, color="black"),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(title="RMSE")) +  scale_fill_gradientn(colors = hcl.colors(5, "BuPu")) 
ggsave(file.path(lake_directory,"analysis/figures/HorizonvsDA_tileplot.jpg")) 

#tile plot by depth
ggplot(subset(forecast_horizon_depth_avg,depth==9), aes(DA, horizon, fill=RMSE_bins)) + geom_tile(color="black") + xlab("9m") +
  ylab("Horizon (days)") + theme_bw() + facet_wrap(~phen) +
  theme(text = element_text(size=14), axis.text = element_text(size=14, color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(title="RMSE")) +  scale_fill_gradientn(colors = hcl.colors(5, "BuPu")) 
ggsave(file.path(lake_directory,"analysis/figures/HorizonvsDA_tileplot_9m.jpg")) 


#RMSE vs forecast period
ggplot(forecast_skill_avg, aes(date, RMSE, color=DA)) +geom_path(size=1.5) + xlab("") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  facet_wrap(~phen) + scale_color_manual(values=cb_friendly_2) + guides(fill=guide_legend(title="DA frequency"))
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsForecastperiod.jpg"))


#------------------------------------------------------------------------------------------------#
# Data assimilation figure
forecasts_daily_da <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily"), pattern="experiments.+.csv", full.names=TRUE) 
forecasts_daily_da <- lapply(forecasts_daily_da, read_csv) %>% bind_rows() %>% mutate(DA = "Daily")

forecasts_2day_da <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_2"), pattern="experiments.+.csv", full.names=TRUE)
forecasts_2day_da <- lapply(forecasts_2day_da, read_csv) %>% bind_rows() %>% mutate(DA = "2Day")

forecasts_5day_da <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_5"), pattern="experiments.+.csv", full.names=TRUE)
forecasts_5day_da <- lapply(forecasts_5day_da, read_csv) %>% bind_rows() %>% mutate(DA = "5Day")

forecasts_weekly_da <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/weekly"), pattern="experiments.+.csv", full.names=TRUE)
forecasts_weekly_da <- lapply(forecasts_weekly_da, read_csv) %>% bind_rows() %>% mutate(DA = "Weekly")

forecasts_fortnightly_da <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/fortnightly"), pattern="experiments.+.csv", full.names=TRUE)
forecasts_fortnightly_da <- lapply(forecasts_fortnightly_da, read_csv) %>% bind_rows() %>% mutate(DA = "Fortnightly")

forecasts_monthly_da <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/monthly"), pattern="experiments.+.csv", full.names=TRUE)
forecasts_monthly_da <- lapply(forecasts_monthly_da, read_csv) %>% bind_rows() %>% mutate(DA = "Monthly")

#combine all frequencies
DA <- rbind(forecasts_daily_da, forecasts_2day_da, forecasts_5day_da, forecasts_weekly_da, 
            forecasts_fortnightly_da, forecasts_monthly_da)

#change DA factor order
DA$DA <- factor(DA$DA, levels = c("Daily", "2Day", "5Day", "Weekly","Fortnightly","Monthly"))


ggplot(subset(DA, depth==1.5), aes(date, forecast_mean, color=DA)) + geom_line() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + geom_point(aes(date, observed), color="red") +
  ylab(expression("Temperature ("*~degree*C*")")) + xlab("")  + scale_color_manual(values=cb_friendly) +# scale_fill_viridis_d() +
  #geom_ribbon(aes(y = forecast_mean, ymin = forecast_mean-forecast_sd, ymax = forecast_mean+forecast_sd, color=DA, fill="gray"), alpha=0.2) +
  guides(fill = guide_legend(title="DA frequency"))
ggsave(file.path(lake_directory,"analysis/figures/AssimilationVSdatafreq.jpg")) 

test <- DA[DA$depth==1.5 & DA$DA=="Daily",]

plot(test$date, test$forecast_mean)
#------------------------------------------------------------------------------------------------#
#parameter evolution figs

source(file.path(lake_directory,"R/read_flare_params.R"))

forecasts_daily_nc <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily"), pattern=".nc", full.names=TRUE)[-c(1)] #ignoring first file because this is the DA period
forecasts_2day_nc <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_2"), pattern=".nc", full.names=TRUE)[-c(1)]
forecasts_5day_nc <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_5"), pattern=".nc", full.names=TRUE)[-c(1)]
forecasts_weekly_nc <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/weekly"), pattern=".nc", full.names=TRUE)[-c(1)]
forecasts_fortnightly_nc <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/fortnightly"), pattern=".nc", full.names=TRUE)[-c(1)]
forecasts_monthly_nc <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/monthly"), pattern=".nc", full.names=TRUE)[-c(1)]

#summary stats for first forecast of each
daily <- read_flare_params(files = forecasts_daily_nc, type = "forecast", summary = TRUE) %>% mutate(DA="Daily")
daily2 <- read_flare_params(files = forecasts_2day_nc, type = "forecast", summary = TRUE) %>% mutate(DA="2Day")
daily5 <- read_flare_params(files = forecasts_5day_nc, type = "forecast", summary = TRUE) %>% mutate(DA="5Day")
weekly <- read_flare_params(files = forecasts_weekly_nc, type = "forecast", summary = TRUE) %>% mutate(DA="Weekly")
fortnightly <- read_flare_params(files = forecasts_fortnightly_nc, type = "forecast", summary = TRUE) %>% mutate(DA="Fortnightly")
monthly <- read_flare_params(files = forecasts_monthly_nc, type = "forecast", summary = TRUE) %>% mutate(DA="Monthly")

#combine all parameter dfs
parameters <- rbind(daily, daily2, daily5, weekly, fortnightly, monthly)

#change DA factor order
parameters$DA <- factor(parameters$DA, levels = c("Daily", "2Day", "5Day", "Weekly","Fortnightly","Monthly"))

#rename zone1temp so more informative facet label in fig
parameters <- parameters %>% mutate(parameter = recode(parameter, "zone1temp" = "Sediment Temperature"))

#visualize how parameters change over time
ggplot(subset(parameters, parameter=="Sediment Temperature"), aes(datetime, mean, color=DA, group=DA)) + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +scale_color_manual(values=cb_friendly_2) +
  facet_wrap(~parameter, scales="free_y")  + scale_fill_manual(values=cb_friendly_2) + ylim(4.7,10.2) +
  scale_x_date(date_labels = "%b") + ylab(expression("Temp ("*~degree*C*")")) + xlab("")  +
  geom_ribbon(aes(y = mean, ymin = mean-sd, ymax = mean+sd, color=DA, fill=DA), alpha=0.5) +
  guides(fill = guide_legend(title="DA frequency"), color = guide_legend(title="DA frequency"))
ggsave(file.path(lake_directory,"analysis/figures/paramRMSEvsHorizon.jpg"))


#code for calculating strat/mixed periods
# bvr_temps <- temp_long %>% filter(temp_long$Variable=="temperature" & DateTime>= "2021-01-01") %>% select(DateTime, Reading, Depth) %>%
#   mutate(DateTime = as.Date(DateTime))  %>% mutate(Depth = round(Depth,0))
# 
# 
# bvr_surf_bot_temps <- bvr_temps %>% group_by(DateTime, Depth) %>% summarise(Temp = mean(Reading)) %>% 
#   group_by(DateTime)  %>% filter(Depth== min(Depth) | Depth== max(Depth)) %>%
#   mutate(diff = abs(last(Temp)-first(Temp))) %>% mutate(diff = round(diff,0))
# 
# plot(bvr_surf_bot_temps$DateTime, bvr_surf_bot_temps$diff)
# 
# mix <- bvr_surf_bot_temps[bvr_surf_bot_temps$diff<1,] 
# strat <- bvr_surf_bot_temps[bvr_surf_bot_temps$diff>=1,]





