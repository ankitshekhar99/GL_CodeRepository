##########################################
# Prepare meteo file 3 plus (Rain TS SWC) for EFDC submission #
# in 2023, small adjustments compared to previous years
# last execution: 17.08.2023
##########################################

rm(list = ls())
fo <- 'v:/CH-CHA_Chamau/10_meteo/2021/10_FINAL/ID2022-07-08T170352/'
fi <- 'CH-CHA_AGG_ALL_30min_ID2022-07-08T170352.csv'

setwd('t:/18_Programming/11_R-Programs/EFDC_submissions/2023/Data_4_submission')

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

# which columns of the eddypro file go into the EFDC file?
#colsel <- c('TIMESTAMP_START','TIMESTAMP_END','LWin_1_1_1','PPFD_1_1_1','RH_1_1_1','Rg_1_1_1','Ta_1_1_1','Pa_1_1_1')


colsel <- c('TIMESTAMP_START','TIMESTAMP_END',
 # original name	  # name for FLUXNET
'P_RAIN_TOT_GF1_0.5_1', # P_1_1_1
'SWC_AVG_GF1_0.05_1', # SWC_1_1_1
'SWC_AVG_GF1_0.15_1', # SWC_1_2_1
'SWC_AVG_GF1_0.75_1', # SWC_1_3_1

#'TS_AVG_GF1_0.01_1',  # TS_1_1_1 doesnt exist anymore
'TS_AVG_GF1_0.04_1',  # TS_1_2_1
#'TS_AVG_GF1_0.07_1',  # TS_1_3_1 doesnt exist anymore
'TS_AVG_GF1_0.1_1',   # TS_1_4_1
'TS_AVG_GF1_0.15_1',  # TS_1_5_1
'TS_AVG_GF1_0.25_1',  # TS_1_6_1
'TS_AVG_GF1_0.4_1',   # TS_1_7_1
#'TS_AVG_GF1_0.95_1',  # TS_1_8_1 doesnt exist anymore

#'SWC_AVG_GF1_0.05_2', # SWC_1_4_1 doesnt exist anymore
#'SWC_AVG_GF1_0.1_2',  # SWC_1_5_1
'SWC_AVG_GF1_0.2_2',  # SWC_1_6_1 
#'SWC_AVG_GF1_0.3_2',  # SWC_1_7_1 doesnt exist anymore
#'SWC_AVG_GF1_0.5_2',  # SWC_1_8_1 doesnt exist anymore

'TS_AVG_GF1_0.025_2',  # TS_1_9_1
#'TS_AVG_GF1_0.05_2',   # TS_1_10_1 doesnt exist anymore
#'TS_AVG_GF1_0.1_2',    # TS_1_11_1 doesnt exist anymore
'TS_AVG_GF1_0.2_2')    # TS_1_12_1
#'TS_AVG_GF1_0.3_2',    # TS_1_13_1 doesnt exist anymore
#'TS_AVG_GF1_0.5_2'     # TS_1_14_1 doesnt exist anymore

# new data frame with selected columns
dat.efdc <- dat[,colsel]

# rename cols according to efdc variable codes
# check http://gaia.agraria.unitus.it/home/guidelines/how-to-submit-data/variables-codes for correct variable codes
newcols <- c('TIMESTAMP_START','TIMESTAMP_END',
             'P_1_1_1',   # 'P_RAIN_TOT_GF1_0.5_1'
             'SWC_1_1_1', # 'SWC_AVG_GF1_0.05_1'
             'SWC_1_2_1', # 'SWC_AVG_GF1_0.15_1'
             'SWC_1_3_1', # 'SWC_AVG_GF1_0.75_1'
             
             'TS_1_2_1', # 'TS_AVG_GF1_0.04_1'
             'TS_1_4_1', # 'TS_AVG_GF1_0.1_1'
             'TS_1_5_1', # 'TS_AVG_GF1_0.15_1'
             'TS_1_6_1', # 'TS_AVG_GF1_0.25_1'
             'TS_1_7_1', # 'TS_AVG_GF1_0.4_1'
             
             'SWC_1_6_1', # 'SWC_AVG_GF1_0.2_2'
             
             'TS_1_9_1', # 'TS_AVG_GF1_0.025_2'
             'TS_1_12_1'   # 'TS_AVG_GF1_0.2_2'
             )

# rename columns
colnames(dat.efdc) <- newcols
print(cbind(colsel, newcols))

# Problem that some of the soil variables got merge since same variable name, but the instruments (new profile) changed in November 2021
# set to NA from November onwards (Old soil profile was already removed on 15 September)
x1 <- which(dat.efdc$TIMESTAMP_END=='202111010000')
dat.efdc[x1:nrow(dat.efdc),c(-1,-2, -3)] <- -9999
# make a new file with a new BADM for the new sensors

# replace NAs by -9999
dat.efdc[which(is.na(dat.efdc), arr.ind = T)] <- -9999 

# write the new file 
# *****************************************
# **** (ADJUST FILENAME FOR YOUR SITE) ****
# *****************************************
write.table(dat.efdc,file='CH-Cha_2021_Meteo-3M_20230817.csv',quote=F, sep=',',row.names = F)

# Visualization
# Plotting for checking the EFDC data, comment if you do not want that
for (i in 1:length(newcols)) {
  # create dummy variable for plotting where -9999 values are set to NA
  dd <- dat.efdc[,i]
  dd[dd==-9999] <- NA
  plot(dat$TIMESTAMP.ps,dd, ylab=newcols[i],xlab='', type='l')
}

# check from which date onwards no data
# xx <- 12000:12500 
# plot(dat$TIMESTAMP.ps[xx],dat.efdc$TS_1_7_1[xx],type='l')

