inflow_qaqc <- function(inflow_file,
                        qaqc_file,
                        nutrients_file,
                        silica_file,
                        ghg_file,
                        cleaned_inflow_file,
                        local_tzone,
                        input_file_tz){

  
# read in calculated inflow csv, only select some cols
  # Updated inflow model using FCR met station precip and temp data: units in m3/d - need to convert to m3/s
  inflow <- read_csv(inflow_file) %>% 
    mutate(FLOW = Q_BVR_m3pd/86400) %>% select(-c(Q_BVR_m3pd,...1)) #convert flow to m3/s
  inflow$time = as.POSIXct(strptime(inflow$time,"%Y-%m-%d", tz="EST"))

  temp <- read.csv(qaqc_file) 
  temp$DateTime = as.POSIXct(strptime(temp$DateTime,"%Y-%m-%d", tz="EST"))
  temp <- temp %>% select(DateTime, WVWA_Temp_C) %>% 
    rename(time=DateTime, TEMP=WVWA_Temp_C) %>%
    dplyr::filter(time > as.POSIXct("2015-07-07") & time < as.POSIXct("2021-01-07")) %>% 
    group_by(time) %>% 
    summarise(TEMP=mean(TEMP)) #gives averaged daily temp in C
  
  # Merge inflow and inflow temp datasets
  inflow <- merge(inflow,temp,by="time",all=TRUE) 
  
  #only select data until last inflow obs so can infill the missing days
  inflow <- inflow[inflow$time<=last(temp$time),]
  
  #fill in missing days
  inflow <- inflow %>% mutate(TEMP=na.fill(na.approx(TEMP),"extend")) 
  
  # Add SALT column (salinty = 0 for all time points)
  inflow <- inflow %>% mutate(SALT = rep(0,length(inflow$time)))
  
  
  #### BRING IN THE NUTRIENTS

  if(!is.na(nutrients_file)){

    nutrients <- read_csv(nutrients_file) %>%
      select(Reservoir:DIC_mgL) %>%
      dplyr::filter(Reservoir=="BVR") %>%
      dplyr::filter(Site==100 | Site==200) %>%
      mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
      rename(time = DateTime)
      
    # Create nuts (randomly sampled from a normal distribution) for total inflow
    bvr_nuts <- as.data.frame(seq.Date(as.Date("2015/07/07"),as.Date("2021/12/01"), "days"))
    names(bvr_nuts)[1] <- "time"
    bvr_nuts$time<-as.POSIXct(strptime(bvr_nuts$time, "%Y-%m-%d", tz="EST"))
    bvr_nuts <- bvr_nuts %>% 
      mutate(TN_ugL = rnorm(2340,mean=mean(nutrients$TN_ugL, na.rm=TRUE),sd=sd(nutrients$TN_ugL, na.rm=TRUE))) %>% 
      mutate(TP_ugL = rnorm(2340,mean=mean(nutrients$TP_ugL, na.rm=TRUE),sd=sd(nutrients$TP_ugL, na.rm=TRUE))) %>% 
      mutate(NH4_ugL = rnorm(2340,mean=mean(nutrients$NH4_ugL, na.rm=TRUE),sd=sd(nutrients$NH4_ugL, na.rm=TRUE))) %>% 
      mutate(NO3NO2_ugL = rnorm(2340,mean=mean(nutrients$NO3NO2_ugL, na.rm=TRUE),sd=sd(nutrients$NO3NO2_ugL, na.rm=TRUE))) %>% 
      mutate(SRP_ugL = rnorm(2340,mean=mean(nutrients$SRP_ugL, na.rm=TRUE),sd=sd(nutrients$SRP_ugL, na.rm=TRUE))) %>% 
      mutate(DOC_mgL = rnorm(2340,mean=mean(nutrients$DOC_mgL, na.rm=TRUE),sd=sd(nutrients$DOC_mgL, na.rm=TRUE))) %>% 
      mutate(DIC_mgL = rnorm(2340,mean=mean(nutrients$DIC_mgL, na.rm=TRUE),sd=sd(nutrients$DIC_mgL, na.rm=TRUE)))
    
    # Make sure values are not negative!
    bvr_nuts <- bvr_nuts %>% 
      mutate(TN_ugL = ifelse(TN_ugL<=0.00, 0.00, TN_ugL)) %>% 
      mutate(TP_ugL = ifelse(TP_ugL<=0.00, 0.00, TP_ugL)) %>% 
      mutate(NH4_ugL = ifelse(NH4_ugL<=0.00, 0.00, NH4_ugL)) %>% 
      mutate(NO3NO2_ugL = ifelse(NO3NO2_ugL<=0.00, 0.00, NO3NO2_ugL)) %>% 
      mutate(SRP_ugL = ifelse(SRP_ugL<=0.00, 0.00, SRP_ugL)) %>%
      mutate(DOC_mgL = ifelse(DOC_mgL<=0.00, 0.00, DOC_mgL)) %>% 
      mutate(DIC_mgL = ifelse(DIC_mgL<=0.00, 0.00, DIC_mgL))
    
    #read in lab dataset of dissolved silica, measured by Jon in summer 2014 only
    silica <- read.csv(silica_file, header=T) %>%
      select(Date, Depth, DRSI_mgL) %>%
      mutate(Date = as.POSIXct(strptime(Date, "%Y-%m-%d", tz="EST"))) %>%
      dplyr::filter(Depth == 999) %>% #999 = weir inflow site
      select(Date, DRSI_mgL) %>%
      rename(time = Date)
    
    #only select dates until end of 2021
    inflow <- inflow %>% filter(time <= "2021-12-01")
    
    alldata<-merge(inflow, bvr_nuts, by="time", all.x=TRUE)
    
    #read in lab dataset of CH4 from 2015-2019
    # for BVR: Only have a handful of days w/ CH4 in inflows (BVR 100 and 200); aggregate all time points
    # and average CH4 - use average as CH4 input for the entier year
    ghg <- read.csv(ghg_file, header=T) %>%
      dplyr::filter(Reservoir == "BVR") %>%
      dplyr::filter(Depth_m == 100|Depth_m == 200) %>% #weir inflow
      select(DateTime, ch4_umolL) %>%
      mutate(DateTime = as.POSIXct(strptime(DateTime, "%d-%b-%y", tz="EST"))) %>%
      rename(time = DateTime, CAR_ch4 = ch4_umolL)
    
    # Calculate average for the BVR data points and mutate column to alldata
    alldata <- alldata %>% mutate(CAR_ch4 = mean(ghg$CAR_ch4))
    
    #need to convert mass observed data into mmol/m3 units for two pools of organic carbon
    total_inflow <- alldata %>% 
      mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
      mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
      mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
      mutate(OGM_doc = DOC_mgL*1000*(1/12.01)* 0.10) %>% #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
      mutate(OGM_docr = DOC_mgL*1000*(1/12.01)* 0.90) %>% #assuming 90% of total DOC is in labile DOC pool
      mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
      mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
      mutate(OGM_poc = 0.1*(OGM_doc+OGM_docr)) %>% #assuming that 10% of DOC is POC (Wetzel page 755)
      mutate(OGM_don = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.10) %>% #DON is ~5x greater than PON (Wetzel page 220)
      mutate(OGM_donr = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.90) %>% #to keep mass balance with DOC, DONr is 90% of total DON
      mutate(OGM_pon = (1/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>%
      mutate(OGM_dop = 0.3*(TP_ugL-PHS_frp)*0.10) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
      mutate(OGM_dopr = 0.3*(TP_ugL-PHS_frp)*0.90) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
      mutate(OGM_pop = 0.7*(TP_ugL-PHS_frp)) %>% 
      #mutate(PHS_frp_ads = PHS_frp) %>% #Following Farrell et al. 2020 EcolMod
      mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
    
    
    #creating OXY_oxy column using RMR package, assuming that oxygen is at 100% saturation in this very well-mixed stream
    # Obtained elevation from BVR DEM at BVR 100 inflow to the reservoir
    for(i in 1:length(total_inflow$TEMP)){
      total_inflow$OXY_oxy[i]<-(temp.C= Eq.Ox.conc(total_inflow$TEMP[i], elevation.m = 586,
                                                   bar.press = NULL, bar.units = NULL,
                                                   out.DO.meas = "mg/L",
                                                   salinity = 0, salinity.units = "pp.thou"))*1000*(1/32)
    }
    
    #clean it up and get vars in order
    total_inflow <- total_inflow %>%
      select(time, FLOW, TEMP, SALT, OXY_oxy, NIT_amm:CAR_dic, CAR_ch4) %>% 
      mutate(SIL_rsi = rep(median(silica$DRSI_mgL),length(total_inflow$time))) %>%
      mutate(SIL_rsi = SIL_rsi*1000*(1/60.08)) %>% #setting the Silica concentration to the median 2014 inflow concentration for consistency
      mutate_if(is.numeric, round, 4) #round to 4 digits 
    
    #estimate bvr inflow temp based on relationship between bvr and fcr inflows
    total_inflow$TEMP <- (1.5 * total_inflow$TEMP) - 9.21
  }
    
    #write file for inflow for the weir, with 2 pools of OC (DOC + DOCR)  
    readr::write_csv(total_inflow, cleaned_inflow_file)
  }
  


