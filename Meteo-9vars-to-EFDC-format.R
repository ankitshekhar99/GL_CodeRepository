# **********************************************************
# *** Script to transfer meteo-screening files in format ***
# *** for submission to EFDC                             ***
# **********************************************************
# *** Input: Meteo-Screening csv with 30 min resolution ***
# *** Output: csv-file with the format and naming conventions ***
# ***        needed for database upload ***
# *** Variables (9):  G, additional PPFD_IN/OUT, SW_IN/OUT, PotRad, LW_OUT, NETRAD, 
# ***                 more SWC, TS (SoilVue)
# *** needs a new BADM for 2021 since I decided to submit the "nighttime set to zero" variables for the Shortwave radiation variables (incl. netrad)
# *** last run: 2023-08-17
# ***************************************************************

rm(list = ls())
ID <- 'ID2022-07-08T170352'
year <- 2021

setwd('t:/18_Programming/11_R-Programs/EFDC_submissions/2023/Data_4_submission')

fo <- paste0('v:/CH-CHA_Chamau/10_meteo/',year,'/10_FINAL/',ID,'/')
fi <- paste0('CH-CHA_AGG_ALL_30min_',ID,'.csv')
output <- paste0('t:/18_Programming/11_R-Programs/EFDC_submissions/2023/Data_4_submission/CH-Cha_',year,'_Meteo-9M_20230817.csv')

# read meteo-screened data
dat <- read.csv(paste0(fo,fi), header = T)

# make timestamp start and end:
# reformat the time to get an EFDC Timestamp
dat$TIMESTAMP.ps <- as.POSIXct(dat$`X__TIMESTAMP__`, format='%Y-%m-%d %H:%M:%S', tz='Etc/GMT+1')
dat$TIMESTAMP <- format(dat$TIMESTAMP.ps, format='%Y%m%d%H%M') # this is the format they want

# new: Timestamp start and end is needed
# http://gaia.agraria.unitus.it/home/guidelines/how-to-submit-data/general-information
dat$TIMESTAMP.pss <- as.POSIXct(dat$`X__TIMESTAMP__`, format='%Y-%m-%d %H:%M:%S', tz='Etc/GMT+1')-1800
dat$TIMESTAMP_START <- format(dat$TIMESTAMP.pss, format='%Y%m%d%H%M')
dat$TIMESTAMP_END <- dat$TIMESTAMP

colsel <-
  c( 'TIMESTAMP_START','TIMESTAMP_END',
     'G_AVG_GF1_0.03_1',    # G_1_1_1 only until November then name/position changed
     'G_AVG_GF1_0.03_2',    # G_1_1_2 only until November then name/position changed
     'PPFD_IN_CORRECTED_SETTO_0_AVG_T1B2_2_1', # first replicate is second replicate in FLUXNET, offset corrected, nighttime set to zero
     'PPFD_OUT_CORRECTED_SETTO_0_AVG_T1B2_2_2', # second replicate is first replicate in FLUXNET, offset corrected, nighttime set to zero
     'NETRAD_CORRECTED_SETTO_0_AVG_T1B2_2_1', # attention this is calculated from the "corrected" SW_IN/OUT, 
     # with correction for nighttime offset, and nighttime SW set to zero
     'SW_IN_RAD_4', # potential incoming solar radiation for this latitude
     'SW_OUT_CORRECTED_SETTO_0_AVG_T1B2_2_1', # attention this is calculated from the "corrected" SW_IN/OUT, offset corrected, nighttime set to zero
     'LW_OUT_AVG_T1B2_2_1',
    # since January 2020 new soil profile (SoilVue)
    'SWC_AVG_GF1_0.05_3',   # SWC_3_1_1
    'SWC_AVG_GF1_0.1_3',    # SWC_3_2_1
    'SWC_AVG_GF1_0.2_3',    # SWC_3_3_1
    'SWC_AVG_GF1_0.3_3',    # SWC_3_4_1
    'SWC_AVG_GF1_0.4_3',    # SWC_3_5_1
    'SWC_AVG_GF1_0.5_3',    # SWC_3_6_1
    'SWC_AVG_GF1_0.6_3',    # SWC_3_7_1
    'SWC_AVG_GF1_0.75_3',   # SWC_3_8_1
    'SWC_AVG_GF1_1_3',      # SWC_3_9_1

     'TS_AVG_GF1_0.05_3',   # TS_3_1_1
     'TS_AVG_GF1_0.1_3',    # TS_3_2_1
     'TS_AVG_GF1_0.2_3',    # TS_3_3_1
     'TS_AVG_GF1_0.3_3',    # TS_3_4_1
     'TS_AVG_GF1_0.4_3',    # TS_3_5_1
     'TS_AVG_GF1_0.5_3',    # TS_3_6_1
     'TS_AVG_GF1_0.6_3',    # TS_3_7_1
     'TS_AVG_GF1_0.75_3',   # TS_3_8_1
     'TS_AVG_GF1_1_3'       # TS_3_9_1
)

newcols <- c('TIMESTAMP_START','TIMESTAMP_END',
             'G_1_1_1',
             'G_1_1_2',
             'PPFD_IN_1_1_2', # first replicate
             'PPFD_OUT_1_1_1',
             'NETRAD_1_1_1',
             'SW_IN_POT_1_1_1',
             'SW_OUT_1_1_1',
             'LW_OUT_1_1_1',
             # since January 2020 new soil profile (SoilVue)
             'SWC_3_1_1',    # SWC_AVG_GF1_0.05_3
             'SWC_3_2_1',    # SWC_AVG_GF1_0.1_3
             'SWC_3_3_1',    # SWC_AVG_GF1_0.2_3
             'SWC_3_4_1',    # SWC_AVG_GF1_0.3_3
             'SWC_3_5_1',    # SWC_AVG_GF1_0.4_3
             'SWC_3_6_1',    # SWC_AVG_GF1_0.5_3
             'SWC_3_7_1',    # SWC_AVG_GF1_0.6_3
             'SWC_3_8_1',    # SWC_AVG_GF1_0.75_3
             'SWC_3_9_1',    # SWC_AVG_GF1_1_3
             
             'TS_3_1_1',    # TS_AVG_GF1_0.05_3
             'TS_3_2_1',    # TS_AVG_GF1_0.1_3
             'TS_3_3_1',    # TS_AVG_GF1_0.2_3
             'TS_3_4_1',    # TS_AVG_GF1_0.3_3
             'TS_3_5_1',    # TS_AVG_GF1_0.4_3
             'TS_3_6_1',    # TS_AVG_GF1_0.5_3
             'TS_3_7_1',    # TS_AVG_GF1_0.6_3
             'TS_3_8_1',    # TS_AVG_GF1_0.75_3
             'TS_3_9_1'     # TS_AVG_GF1_1_3
)

# select columns needed for efdc file
dat.efdc <- dat[,colsel]

# rename columns
colnames(dat.efdc) <- newcols
print(cbind(colsel, newcols))

# replace NAs by -9999
dat.efdc[which(is.na(dat.efdc), arr.ind = T)] <- -9999 

# *****************************************
# **** # write the new file             ***
# *****************************************
write.table(dat.efdc,file=output,quote=F, sep=',',row.names = F)

# Visualization
# Plotting for checking the EFDC data, comment if you do not want that
for (i in 1:length(newcols)) {
  # create dummy variable for plotting where -9999 values are set to NA
  dd <- dat.efdc[,i]
  dd[dd==-9999] <- NA
  plot(dat$TIMESTAMP.ps,dd, ylab=newcols[i],xlab='', type='l')
}



