##########################################
# Prepare meteo file 3 plus (Rain TS SWC) for EFDC submission #
# in 2023, small adjustments compared to previous years
# This is the program for the new soil profile, which was installed in November 2021.
# It was also at GF1, while the old sensors were taken out.
# Problem is, that there will be confusion about the variable names if we keep them the same.
# Hopefully it will be clear with a new BADM
# I need to submit a new BADM with the new profile variable names (valid from November 2021)
#
##########################################
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

plot(dat$SWC_AVG_GF1_0.05_1, ylim=c(0,100))
points(dat$SWC_AVG_GF1_0.1_1, col=2)
points(dat$SWC_AVG_GF1_0.2_1, col='orange')
points(dat$SWC_AVG_GF1_0.3_1, col='yellow')
points(dat$SWC_AVG_GF1_0.4_1, col='green')
points(dat$SWC_AVG_GF1_0.5_1, col='darkgreen')

plot(dat$TS_AVG_GF1_0.05_1, ylim=c(0,25))
points(dat$TS_AVG_GF1_0.1_1, col=2)
points(dat$TS_AVG_GF1_0.2_1, col='orange')
points(dat$TS_AVG_GF1_0.3_1, col='yellow')
points(dat$TS_AVG_GF1_0.4_1, col='green')
points(dat$TS_AVG_GF1_0.5_1, col='darkgreen')

colsel <- c('TIMESTAMP_START','TIMESTAMP_END',
 # original name	  # name for FLUXNET
'SWC_AVG_GF1_0.05_1', # SWC_1_1_1
'SWC_AVG_GF1_0.1_1', # SWC_1_2_1
'SWC_AVG_GF1_0.2_1', # SWC_1_3_1
'SWC_AVG_GF1_0.3_1', # SWC_1_4_1
'SWC_AVG_GF1_0.4_1', # SWC_1_5_1
'SWC_AVG_GF1_0.5_1', # SWC_1_6_1

'TS_AVG_GF1_0.05_1', # TS_1_1_1
'TS_AVG_GF1_0.1_1',  # TS_1_2_1
'TS_AVG_GF1_0.2_1',  # TS_1_3_1
'TS_AVG_GF1_0.3_1',  # TS_1_4_1
'TS_AVG_GF1_0.4_1',  # TS_1_5_1
'TS_AVG_GF1_0.5_1')  # TS_1_6_1


# new data frame with selected columns
dat.efdc <- dat[,colsel]

# rename cols according to efdc variable codes
# check http://gaia.agraria.unitus.it/home/guidelines/how-to-submit-data/variables-codes for correct variable codes
newcols <- c('TIMESTAMP_START','TIMESTAMP_END',
             
             'SWC_1_1_1', # 'SWC_AVG_GF1_0.05_1'
             'SWC_1_2_1', # 'SWC_AVG_GF1_0.1_1'
             'SWC_1_3_1', # 'SWC_AVG_GF1_0.2_1'
             'SWC_1_4_1', # 'SWC_AVG_GF1_0.3_1'
             'SWC_1_5_1', # 'SWC_AVG_GF1_0.4_1'
             'SWC_1_6_1', # 'SWC_AVG_GF1_0.5_1'
             
             'TS_1_1_1', # 'SWC_AVG_GF1_0.05_1'
             'TS_1_2_1', # 'SWC_AVG_GF1_0.1_1'
             'TS_1_3_1', # 'SWC_AVG_GF1_0.2_1'
             'TS_1_4_1', # 'SWC_AVG_GF1_0.3_1'
             'TS_1_5_1', # 'SWC_AVG_GF1_0.4_1'
             'TS_1_6_1') # 'SWC_AVG_GF1_0.5_1'

# rename columns
colnames(dat.efdc) <- newcols
# check
print(cbind(colsel, newcols))

# Old soil profile was already removed on 15 September
# New soil profile was installed in November, make a file only from this time onwards to not overwrite the old GF1 data
x1 <- which(dat.efdc$TIMESTAMP_END=='202111010030')
dat.efdc <- dat.efdc[x1:nrow(dat.efdc),]
# make a new file with a new BADM for the new sensors

# replace NAs by -9999
dat.efdc[which(is.na(dat.efdc), arr.ind = T)] <- -9999 

# write the new file 
# *****************************************
# **** (ADJUST FILENAME FOR YOUR SITE) ****
# *****************************************
write.table(dat.efdc,file='CH-Cha_2021_Meteo-newGF1_20230817.csv',quote=F, sep=',',row.names = F)

# Visualization
# Plotting for checking the EFDC data, comment if you do not want that
for (i in 1:length(newcols)) {
  # create dummy variable for plotting where -9999 values are set to NA
  dd <- dat.efdc[,i]
  dd[dd==-9999] <- NA
  plot(dat$TIMESTAMP.ps[x1:nrow(dat)],dd, ylab=newcols[i],xlab='', type='l')
}

# check from which date onwards no data
# xx <- 12000:12500 
# plot(dat$TIMESTAMP.ps[xx],dat.efdc$TS_1_7_1[xx],type='l')

