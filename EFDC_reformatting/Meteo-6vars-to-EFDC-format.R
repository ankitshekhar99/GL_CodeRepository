##########################################
# Prepare meteo file for EFDC submission #
##########################################
# for the 6 meteo variables used for the final fluxes
# in 2023, small adjustments compared to previous years, timestamp and capital letters for TA, PA
# file must contain EXACTLY the year you want to submit
# last execution for Chamau 2021: 17.08.2023 IF #
#################################################
rm(list = ls()) # clear workspace
# input folder path
fo <- 't:/18_Programming/11_R-Programs/EFDC_submissions/2023/input/'
fi <- 'CH-CHA_2021_meteo_aux_FF.csv' # take meteo file from final flux calculation 

setwd('t:/18_Programming/11_R-Programs/EFDC_submissions/2023/Data_4_submission')

dat <- read.csv(paste0(fo,fi), skip=2,header = F)
colnames(dat) <- read.csv(paste0(fo,fi), header = F, nrows = 1)

# make timestamp start and end:
# reformat the time to get an EFDC Timestamp
dat$TIMESTAMP.ps <- as.POSIXct(dat$TIMESTAMP_1, format='%Y-%m-%d %H%M', tz='Etc/GMT+1')
dat$TIMESTAMP <- format(dat$TIMESTAMP.ps, format='%Y%m%d%H%M') # this is the format they want

# new (since 2023): Timestamp start and end is needed
# http://gaia.agraria.unitus.it/home/guidelines/how-to-submit-data/general-information
dat$TIMESTAMP.pss <- as.POSIXct(dat$TIMESTAMP_1, format='%Y-%m-%d %H%M', tz='Etc/GMT+1')-1800 # minus half an hour to get the timestamp start
dat$TIMESTAMP_START <- format(dat$TIMESTAMP.pss, format='%Y%m%d%H%M')
dat$TIMESTAMP_END <- dat$TIMESTAMP

# which columns of the eddypro file go into the EFDC file?
colsel <- c('TIMESTAMP_START','TIMESTAMP_END','LWin_1_1_1','PPFD_1_1_1','RH_1_1_1','Rg_1_1_1','Ta_1_1_1','Pa_1_1_1')

# new data frame with selected columns
dat.efdc <- dat[,colsel]

# rename cols according to efdc variable codes, Attention: compared to 2020, Ta and Pa must be in capital letters.
# This means that also a new BADM is required
# check http://gaia.agraria.unitus.it/home/guidelines/how-to-submit-data/variables-codes for correct variable codes
newcols <- c('TIMESTAMP_START','TIMESTAMP_END','LW_IN_1_1_1','PPFD_IN_1_1_1','RH_1_1_1','SW_IN_1_1_1','TA_1_1_1','PA_1_1_1')

# rename columns
colnames(dat.efdc) <- newcols
print(cbind(colsel, newcols))

# na handling, no NAs, else put -9999 for NAs
print(which(is.na(dat.efdc)))

# write the new file 
# *****************************************
# **** (ADJUST FILENAME FOR YOUR SITE) ****
# *****************************************
write.table(dat.efdc,file='CH-Cha_2021_Meteo-6M_20230817.csv',quote=F, sep=',',row.names = F)

# Visualization
# Plotting for checking the EFDC data, comment if you do not want that
for (i in 1:length(newcols)) {
  # create dummy variable for plotting where -9999 values are set to NA
  dd <- dat.efdc[,i]
  dd[dd==-9999] <- NA
  plot(dat$TIMESTAMP.ps,dd, ylab=newcols[i],xlab='', type='l')
}
