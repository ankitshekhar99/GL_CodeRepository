####################################################################################
# Script to transfer EP_full_output file to the EFDC format for submission to EFDC #
####################################################################################
# Description : Script selects and renames variables from a final eddypro fluxfile 
#               and writes the data to a new file, which can be submitted to EFDC
# For further information check the variable codes at: http://gaia.agraria.unitus.it/home/guidelines/how-to-submit-data/variables-codes 
# last run and checked by Iris Feigenwinter, 10.08.2023
####################################################################################
# Example: Header from a previously submitted EFDC file (2019 CHA file)
# TIMESTAMP,CO2_1_1_1,FC_1_1_1,FC_SSITC_TEST_1_1_1,FETCH_70_1_1_1,FETCH_90_1_1_1,FETCH_MAX_1_1_1,H2O_1_1_1,H_1_1_1,H_SSITC_TEST_1_1_1,
# LE_1_1_1,LE_SSITC_TEST_1_1_1,SC_1_1_1,SH_1_1_1,SLE_1_1_1,TAU_1_1_1,TAU_SSITC_TEST_1_1_1,USTAR_1_1_1,WS_1_1_1
####################################################################################
# Program start
################
rm(list = ls())
# ************************************
# **** (ADJUST WORKING DIRECTORY) ****
# ************************************
setwd('t:/18_Programming/11_R-Programs/EFDC_submissions/2023')
# define folder and filename of eddypro final flux file 
# *********************************
# **** (ADJUST FOR YOUR SITE) ****
# ********************************
fo <- 'v:/CH-CHA_Chamau/20_ec_fluxes/2021/Level-1-2-3-4/R350-IRGA75_FF-202306/Level-2_QC_20230809-151524/'
fi <- 'CH-CHA_2021_Dataset_DIIVE-20230809-151524_Original-30T.diive.csv'

# read eddypro file, this takes a few minutes for a whole year...
dat <- read.csv(paste0(fo,fi), skip=2,header = F)
colnames(dat) <- read.csv(paste0(fo,fi), header = F, nrows = 1)

# reformat the time to get an EFDC Timestamp
dat$TIMESTAMP.ps <- as.POSIXct(dat$`_TIMESTAMP_END`, format='%Y-%m-%d %H:%M:%S', tz='Etc/GMT+1')
dat$TIMESTAMP <- format(dat$TIMESTAMP.ps, format='%Y%m%d%H%M') # this is the format they want

# new: Timestamp start and end is needed
dat$TIMESTAMP.pss <- as.POSIXct(dat$`_TIMESTAMP_END`, format='%Y-%m-%d %H:%M:%S', tz='Etc/GMT+1')-1800
dat$TIMESTAMP_START <- format(dat$TIMESTAMP.pss, format='%Y%m%d%H%M')
dat$TIMESTAMP_END <- dat$TIMESTAMP

# which columns of the eddypro file go into the EFDC file?
colsel <- c('TIMESTAMP_START','TIMESTAMP_END','co2_mole_fraction','co2_flux','qc_co2_flux','x_70%','x_90%','x_peak','h2o_mole_fraction','H','qc_H',
            'LE', 'qc_LE','co2_strg','H_strg','LE_strg','Tau','qc_Tau','u*','wind_speed', 'wind_dir')

# new data frame with selected columns
dat.efdc <- dat[,colsel]

# rename cols according to efdc variable codes
# check http://gaia.agraria.unitus.it/home/guidelines/how-to-submit-data/variables-codes for correct variable codes
newcols <- c('TIMESTAMP_START','TIMESTAMP_END','CO2_1_1_1','FC_1_1_1','FC_SSITC_TEST_1_1_1','FETCH_70_1_1_1','FETCH_90_1_1_1','FETCH_MAX_1_1_1','H2O_1_1_1','H_1_1_1','H_SSITC_TEST_1_1_1',
            'LE_1_1_1','LE_SSITC_TEST_1_1_1','SC_1_1_1','SH_1_1_1','SLE_1_1_1','TAU_1_1_1','TAU_SSITC_TEST_1_1_1','USTAR_1_1_1','WS_1_1_1',
            'WD_1_1_1')
# rename columns
colnames(dat.efdc) <- newcols

# write the new file 
# *****************************************
# **** (ADJUST FILENAME FOR YOUR SITE) ****
# *****************************************
write.table(dat.efdc,file='CH-CHA_2021_Fluxes_20230810.csv',quote=F, sep=',',row.names = F)

# Visualization
# Plotting for checking the EFDC data, comment if you do not want that
for (i in 1:length(newcols)) {
  # create dummy variable for plotting where -9999 values are set to NA
  dd <- dat.efdc[,i]
  dd[dd==-9999] <- NA
  plot(dat$TIMESTAMP.ps,dd, ylab=newcols[i],xlab='', type='l')
}
