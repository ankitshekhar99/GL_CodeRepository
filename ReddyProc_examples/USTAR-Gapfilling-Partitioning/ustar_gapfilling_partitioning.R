# CH-AWS
# Post-processing script for PI dataset CH-AWS_FP2022_2015-2021

# Script for USTAR threshold detection
# All NEE_QCF01 values were already quality-checked, with daytime data of high or OK quality (QCF = 0 or 1)
# and with nighttime data comprising values with high quality (QCF = 0) only.
# while for daytime data QCF = 0 and QCF = 1 were used.
# All LE_QCF01 values were already quality-checked and comprise values with quality flag
# QCF = 0 and QCF = 1 for *both* daytime and nighttime data.

library(ggplot2)
library(REddyProc)
library(caTools)
library(dplyr)
library(viridis)
library(readr)
library(segmented)



# RUN ID
run_id <- format(Sys.time(), "%Y%m%d%H%M%S",tz="GMT")   ## To give unique ID to saved output files



# SOURCE FILE 
# =========== 
file_fluxes_meteo <- "../2_input_data/formatForReddyProc_Dataset_DIIVE-20220202-095118_Original-30T.csv"
output_path <- getwd()
Sys.setenv(TZ = "GMT")



# DATA FROM FILE
# ==============
filedata <- read.csv(file_fluxes_meteo, header = 1)
filedata$TIMESTAMP <- as.POSIXct(filedata$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")
summary(filedata)
head(filedata)
colnames(filedata)



# DATA COLUMNS
# ============
EddyData.F <- filedata[FALSE]
EddyData.F$TIMESTAMP <- as.POSIXct(filedata$TIMESTAMP, format = '%m/%d/%Y %H:%M', tz = Sys.timezone())

# NEE w/ daytime QCF=0 and 1, and nighttime QCF=0, nighttime was defined as SW_IN < 20
EddyData.F$NEE <- as.numeric(as.character(filedata$NEE_QCF01))
EddyData.F$NEE[EddyData.F$NEE < -50] <- NA  # To remove -9999 missing values
EddyData.F$QCF_NEE <- as.numeric(as.character(filedata$QCF_NEE_IRGA7572))
EddyData.F$QCF_NEE[EddyData.F$QCF_NEE < -50] <- NA  # To remove -9999 missing values

EddyData.F$LE <- as.numeric(as.character(filedata$LE_QCF01))
EddyData.F$LE[EddyData.F$LE < -200] <- NA  # To remove -9999 missing values
EddyData.F$QCF_LE <- as.numeric(as.character(filedata$QCF_LE_IRGA7572 ))
EddyData.F$QCF_LE[EddyData.F$QCF_LE  < -50] <- NA  # To remove -9999 missing values

EddyData.F$Ustar <- as.numeric(as.character(filedata$USTAR_IRGA7572))
EddyData.F$Ustar[EddyData.F$Ustar < 0] <- NA  # To remove -9999 missing values

EddyData.F$Rg <- as.numeric(as.character(filedata$SW_IN))
EddyData.F$Rg[EddyData.F$Rg < -20] <- NA  # To remove -9999 missing values
EddyData.F$Rg[EddyData.F$Rg < 0] <- 0  # Below zero not accepted by ReddyProc
EddyData.F$Rg[EddyData.F$Rg > 1200] <- 1200  # Above 1200 not accepted by ReddyProc

EddyData.F$Tair <- as.numeric(as.character(filedata$TA))
EddyData.F$Tair[EddyData.F$Tair < -20] <- NA  # To remove -9999 missing values

EddyData.F$RH <- as.numeric(as.character(filedata$RH))
EddyData.F$RH[EddyData.F$RH < 0] <- NA  # To remove -9999 missing values
EddyData.F$RH[EddyData.F$RH > 100] <- 100  # Some values are 101

EddyData.F$VPD_FLUXNET <- as.numeric(as.character(filedata$VPD))  # Get available VPD
EddyData.F$VPD_FLUXNET[EddyData.F$VPD_FLUXNET < 0] <- NA  # To remove -9999 missing values
EddyData.F$VPD_PI <- fCalcVPDfromRHandTair(EddyData.F$RH, EddyData.F$Tair)  # Calc VPD
EddyData.F$VPD <- coalesce(EddyData.F$VPD_FLUXNET, EddyData.F$VPD_PI)

# cut the analysis period to the end of 2021
EddyData.F <- subset (EddyData.F, TIMESTAMP >= as.POSIXct('2015-01-01 00:30:00'))  # Date with first fluxes
EddyData.F <- subset (EddyData.F, TIMESTAMP <= as.POSIXct('2022-01-01 00:00:00'))

# # TEST DATA one year
# EddyData.F <- subset (EddyData.F, TIMESTAMP >= as.POSIXct('2021-01-01 00:30:00'))
# EddyData.F <- subset (EddyData.F, TIMESTAMP <= as.POSIXct('2022-01-01 00:00:00'))

summary(EddyData.F)



# Initialize R5 reference class
# ============================
EddyProc.C <-sEddyProc$new('CH-AWS',  EddyData.F,
                           c('NEE','LE', 'Rg','Tair','Ustar', 'VPD'), ColPOSIXTime = "TIMESTAMP")   
EddyProc.C$sSetLocationInfo(LatDeg = 46.583056, LongDeg = 9.790639, TimeZoneHour = 1)  # CH-AWS coordinates
str(EddyProc.C)
head(EddyProc.C$sDATA)
head(EddyProc.C$sTEMP)




# DEFINE SEASONS
# ==============

y <- rep(c(2015:2021), each=4)              # with static u*threshold dont need this
s <- rep(c(60, 152, 244, 335), times=4)
seasonStarts <- as.data.frame(cbind(s,y))
seasonFactor <- usCreateSeasonFactorYdayYear(EddyData.F$TIMESTAMP, starts=seasonStarts)
seasonStartsDate <-fConvertTimeToPosix(data.frame(Year=seasonStarts[, 2],
                                                  DoY=seasonStarts[, 1],
                                                  Hour=12),
                                       'YDH', Year = "Year", Day = "DoY", Hour = "Hour")

# # TEST SUBSET
# y <- rep(c(2021:2021), each=4)
# s <- rep(c(60, 152, 244, 335), times=1)
# seasonStarts <- as.data.frame(cbind(s,y))
# seasonFactor <- usCreateSeasonFactorYdayYear(EddyData.F$TIMESTAMP, starts=seasonStarts)
# seasonStartsDate <-fConvertTimeToPosix(data.frame(Year=seasonStarts[, 2],
#                                                   DoY=seasonStarts[, 1],
#                                                   Hour=12),
#                                        'YDH', Year = "Year", Day = "DoY", Hour = "Hour")



# USTAR FILTERING
# ===============
# Use QC0 for USTAR detection
EddyProc.C$sEstimateUstarScenarios(

  seasonFactor=seasonFactor,
  nSample = 200L,
  # probs = c(0.5),
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  NEEColName = "NEE",
  UstarColName = "Ustar",
  TempColName = "Tair",
  RgColName = "Rg",

  ctrlUstarEst = usControlUstarEst(

    ustPlateauFwd = 10,
    ustPlateauBack = 6,
    plateauCrit = 0.95,
    corrCheck = 0.5,
    firstUStarMeanCheck = 0.2,
    isOmitNoThresholdBins = TRUE,
    isUsingCPTSeveralT = FALSE,
    isUsingCPT = FALSE,
    minValidUStarTempClassesProp = 0.2,
    minValidBootProp = 0.4,
    minNuStarPlateau = 3L),

  ctrlUstarSub = usControlUstarSubsetting(
        taClasses = 7,
        UstarClasses = 20,
        swThr = 10,
        minRecordsWithinTemp = 100,
        minRecordsWithinSeason = 160,
        minRecordsWithinYear = 3000,
        isUsingOneBigSeasonOnFewRecords = TRUE)
  )

(uStarTh <- EddyProc.C$sGetEstimatedUstarThresholdDistribution())
EddyProc.C$useSeaonsalUStarThresholds()
# EddyProc.C$useAnnualUStarThresholds()
EddyProc.C$sGetUstarScenarios()
# thr = EddyProc.C$sGetUstarScenarios()
write.csv(uStarTh, file = paste("out_USTAR_thresholds_RP-", run_id,".csv",sep=""))



# USTAR plots
# outDir <- tempdir()
# EddyProc.C$sPlotNEEVersusUStarForSeason(levels(uStarTh$season)[0], dir = outDir)




# GAP FILLING
# ===============

## METEO
## -----
## is already nearly complete
EddyProc.C$sMDSGapFill('Tair', FillAll = FALSE)
EddyProc.C$sMDSGapFill('Rg', FillAll = FALSE)
EddyProc.C$sMDSGapFill('VPD', FillAll = FALSE)


# ## NEE w/ CONSTANT USTAR THRESHOLD
# ## -------------------------------
# uStar <- 0.287644
# EddyProc.C$sMDSGapFillAfterUstar('NEE',
#                                  uStarTh = uStar,
#                                  uStarSuffix = "CUT",
#                                  isFlagEntryAfterLowTurbulence = FALSE,
#                                  isFilterDayTime = TRUE)


## NEE IN USTAR SCENARIOS
## ----------------------
## daytime: QC0 and QC1, nighttime: only QC0
EddyProc.C$sMDSGapFillUStarScens(fluxVar = 'NEE',
                                 isFilterDayTime =TRUE,
                                 isFlagEntryAfterLowTurbulence=TRUE)
# grep("NEE_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
# grep("NEE_.*_fsd$",names(EddyProc.C$sExportResults()), value = TRUE)
# EddyProc.C$sPlotFingerprintY('NEE_U50_f', Year = 2010)


cat("  NEE successfully filled with u* filtering!\n")


## LE w/o USTAR THRESHOLD
## ----------------------
## (QC=0 and QC=1)
EddyProc.C$sMDSGapFill('LE', FillAll = TRUE, isVerbose = TRUE)


## ET
## --
## Calculate ET from LE (QC=0 and QC=1)
EddyProc.C$sTEMP$ET_f <- fCalcETfromLE(EddyProc.C$sTEMP$LE_f, EddyProc.C$sTEMP$Tair_f)


summary(EddyProc.C$sTEMP)



# COLLECT DATA AND EXPORT
# =======================
cat("Export data to standard data frame ...")
FilledEddyData.F <- EddyProc.C$sExportResults()
FilledEddyData.F$TIMESTAMP          <-EddyData.F$TIMESTAMP
write.csv(FilledEddyData.F, file = paste("CH-AWS_NEE_LE_ET_GAPF_RP-", run_id,".csv",sep="")) 



# PARTITIONING
# ============
EddyProc.C$sMRFluxPartition(suffix = 'U50')
EddyProc.C$sGLFluxPartition(suffix = 'U50')



# COLLECT DATA AND EXPORT
# =======================
cat("Export data to standard data frame ...")
FilledEddyData.F <- EddyProc.C$sExportResults()
FilledEddyData.F$TIMESTAMP          <-EddyData.F$TIMESTAMP
write.csv(FilledEddyData.F, file = paste("CH-AWS_NEE_LE_ET_GAPF_PART_RP-", run_id,".csv",sep="")) 






# grep("GPP|Reco",names(EddyProc.C$sExportResults()), value = TRUE)
# EddyProc.C$sPlotFingerprintY('GPP_f', Year = 1997)

# grep("_DT",names(EddyProc.C$sExportResults()), value = TRUE)
# EddyProc.C$sPlotFingerprintY('Reco', Year = 2004)



# PLOTS
# =====
# Save fingerprint plots as PDF
EddyProc.C$sPlotFingerprint('NEE_U50_f')
EddyProc.C$sPlotFingerprint('LE_f')
EddyProc.C$sPlotFingerprint('ET_f')
EddyProc.C$sPlotFingerprint('Tair_f')
EddyProc.C$sPlotFingerprint('Rg_f')
EddyProc.C$sPlotFingerprint('VPD_f')
EddyProc.C$sPlotFingerprint('GPP_U50_f')
EddyProc.C$sPlotFingerprint('Reco_U50')
EddyProc.C$sPlotFingerprint('GPP_DT_U50')
EddyProc.C$sPlotFingerprint('Reco_DT_U50')



# PLOT PER YEAR
# =============

FilledEddyData.F = FilledEddyData.F %>% mutate(
  Date = as.Date(TIMESTAMP,'%Y-%m-%d'), 
  Time = as.character(format(TIMESTAMP, '%H:%M')),
  Year = as.numeric(format(TIMESTAMP,'%Y')))

# NEE, GPP, RECO
# --------------
for (Yr in c(2015:2021))
{
  Year_Filled_Eddy_Data = FilledEddyData.F %>% filter(Year == Yr)
  summary(Year_Filled_Eddy_Data$TIMESTAMP)
  
  a = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = NEE_U50_f)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('NEE') + theme(axis.text.x=element_blank())
  
  b = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = GPP_U50_f)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('GPP (nighttime partitioning)') + theme(axis.text.x=element_blank())
  
  c = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = Reco_U50)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('RECO (nighttime partitioning)') + theme(axis.text.x=element_blank())  
  
  d = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = GPP_DT_U50)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('GPP (daytime partitioning)') + theme(axis.text.x=element_blank())
  
  e = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = Reco_DT_U50)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('RECO (daytime partitioning)') + theme(axis.text.x=element_blank())    
  
  gr = gridExtra::arrangeGrob(a,b,c,d,e, ncol = 5)
  
  ggsave(paste(as.character(Yr),"_CH-AWS_NEE_GPP_RECO_", run_id,".png",sep=""), gr ,height = 200, width = 450, units = 'mm')
  
  print(summary(Year_Filled_Eddy_Data$TIMESTAMP))
  print(Yr)
}


# NEE, LE, ET
# -----------
for (Yr in c(2015:2021))
{
  Year_Filled_Eddy_Data = FilledEddyData.F %>% filter(Year == Yr)
  summary(Year_Filled_Eddy_Data$TIMESTAMP)
  
  a = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = NEE_U50_f)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('NEE') + theme(axis.text.x=element_blank())
  
  b = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = LE_f)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('LE') + theme(axis.text.x=element_blank())
  
  c = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = ET_f)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('ET') + theme(axis.text.x=element_blank())
  
  gr = gridExtra::arrangeGrob(a,b,c, ncol = 3)
  
  ggsave(paste(as.character(Yr),"_CH-AWS_NEE_LE_ET_", run_id,".png",sep=""), gr ,height = 200, width = 450, units = 'mm')
  
  print(summary(Year_Filled_Eddy_Data$TIMESTAMP))
  print(Yr)
}



# SW_IN, TA, VPD
# --------------
for (Yr in c(2015:2021))
{
  Year_Filled_Eddy_Data = FilledEddyData.F %>% filter(Year == Yr)
  summary(Year_Filled_Eddy_Data$TIMESTAMP)
  
  a = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = Rg_f)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('SW_IN') + theme(axis.text.x=element_blank())
  
  b = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = Tair_f)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('TA') + theme(axis.text.x=element_blank())
  
  c = ggplot(Year_Filled_Eddy_Data, aes(x = Time, y = Date, fill = VPD_f)) + geom_tile() + scale_fill_viridis(option = "D") +
    ggtitle('VPD') + theme(axis.text.x=element_blank())
  
  gr = gridExtra::arrangeGrob(a,b,c, ncol = 3)
  
  ggsave(paste(as.character(Yr),"_CH-AWS_SWIN_TA_VPD_", run_id,".png",sep=""), gr ,height = 200, width = 450, units = 'mm')
  
  print(summary(Year_Filled_Eddy_Data$TIMESTAMP))
  print(Yr)
}







# DAILY AGGREGATION
# =================

## While daily aggregation only choosing QC = 0 and 1
FilledEddyData.F_daily = FilledEddyData.F %>% group_by(Date) %>% 
  summarise(NEE_U50_f_mean = mean(NEE_U50_f),
            GPP_U50_f_mean = mean(GPP_U50_f),
            Reco_U50_mean = mean(Reco_U50),
            GPP_DT_U50_mean = mean(GPP_DT_U50),
            Reco_DT_U50_mean = mean(Reco_DT_U50),
            LE_f_mean = mean(LE_f),
            ET_f_mean = mean(ET_f),
            VPD_f_mean = mean(VPD_f),
            TA_mean = mean(Tair_f),
            SW_IN_mean = mean(Rg_f)) %>% 
  mutate(Year = as.numeric(format(Date, '%Y')), Month = as.numeric(format(Date,'%m')), DOY = as.numeric(format(Date,'%j')))

summary(FilledEddyData.F_daily)

# write.csv(FilledEddyData.F_daily, 'CH_AWS_LE_ET_VPD_TA_SW_IN_daily.csv', row.names = FALSE)

## Plotting daily Fluxes

a = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = NEE_U50_f_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('NEE_f_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

b = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = GPP_U50_f_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('GPP_f_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

c = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = Reco_U50_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('RECO_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

d = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = GPP_DT_U50_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('GPP_DT_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

e = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = Reco_DT_U50_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('RECO_DT_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

f = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = LE_f_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('LE_f_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

g = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = ET_f_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('ET_f_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

h = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = VPD_f_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('VPD_f_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

i = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = TA_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('TA_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

j = ggplot(FilledEddyData.F_daily, aes(x = DOY, y = Year, fill = SW_IN_mean)) + geom_tile() + scale_fill_viridis(option = "D") +
  ggtitle('SW_IN_mean daily gapfilled') + theme(axis.text.x=element_blank()) + theme_bw()

gr = gridExtra::arrangeGrob(a,b,c,d,e,f,g,h,i,j, ncol = 5)

ggsave("CH-AWS_DOY_Daily.png", gr ,height = 300, width =600, units = 'mm')


#############------------------------------ End of Script --------------------------------------------------------------------##################
### Thank you for your patience.
