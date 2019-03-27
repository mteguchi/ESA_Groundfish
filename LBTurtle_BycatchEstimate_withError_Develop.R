#*******************************************************************************
# Leatherback Sea Turtle Biological Opinion Reporting
#  This code was developed to meet the reporting requirements of the 2014
#  Biological Opinion covering Leatherback Sea Turtles in the U.S. west coast
#  groundfish fisheries.
#_______________________________________________________________________________
#  5 June 2013 - Example leatherback entanglement risk in       Jim Caretta (NOAA/SWFSC)
#     groundfish fishery (simplified version used for DGN BiOp)
#         Turtle Projections based on observer program data
#         Rate is multiplied by expected number of sets fished annually (default = 1,500 sets). 
#         Resulting annual bycatch point estimate serves as annual 'lambda' from a Poisson process.  
#         Probability of entangling 0, 1, 2, 3... animals annually, is calculated with R-function 'dpois'
#         Probabilities are given as a 2-column table (n entanglements, p of observing n entanglements).
#-------------------------------------------------------------------------------
# Summer 2014 - Error added to model.                       Eric Ward (NOAA/NWFSC-CB)
#-------------------------------------------------------------------------------
# 25 Nov 2014 - Jannot develops original code from Carretta-Ward     Jannot(NOAA)
#               Bycatch Poisson model. Takes from example
#               to actual by incorporating actual WCGOP data.
#------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#TODO:
# 1. Create bycatch ratio
# 2. Develop annual estimation based on real data
#===============================================================================
#Introduction and Set-up
#------------------------
rm(list=ls())
library(stats)
library(foreign)
library(xtable)
library(ggplot2)
library(Hmisc)
library(plyr)
library(knitr)
library(tools)

drive1='~/observer/Input/'
drive2=paste("~/observer/Output/MMSBT/", substr(as.character(Sys.time()),1,4), "/", sep="")

source( paste(drive1,'match.f.r',sep=''))
source( paste(drive1,'functions.r',sep=''))
source( paste(drive1,'DataProcessingFunctions_2013DataFormatv17.r',sep=''))

#import data files
#turtle data
load(file=paste(drive2, "SeaTurtles_Take_ReviewAllYrs_2014-07-09.Rda", sep=""))#trt.analysis
unique(trt.analysis[, c("SIGHTING_YEAR", "sector", "gear")])
#SIGHTING_YEAR  sector        gear
#2008           OA Fixed Gear  Pot

#observer fishing data
oa0213 <- read.csv(file=paste(drive2
                              , "Bjorkland_OAFixedGear_4SeaTurtleBycatch_2003_2013_2014-07-02.csv"
                              , sep=""), header=T)
oa0213$gear <- gear.type(oa0213, "S", "S")
unique(oa0213$gear)

#fish ticket data
oa.ft0213 <-read.csv(file=paste(drive2
                                ,"Bjorkland_FTD_OAFixedGear_4SeaTurtleBycatch_2003_2013_2014-07-02.csv"
                                , sep=""), header=T)
#-------------------------------------------------------------------------------
# Stratification based on species - from MMSBT_2009.r
# Species           Season (RMONTHS)    AREA
#-----------------------------------------------
# Leather Turt.     6:11 present      twl-N/S 4010; fixed gears- coastwide
#-------------------------------------------------------------------------------
oa0213$season <- "absent"
oa0213$season[oa0213$SET_MONTH %in% c(6:11)] <- "present"

trt.analysis$season <- NA
trt.analysis$season[trt.analysis$SIGHTING_MONTH %in% c(6:11)] <- "present"

oa.ft0213$season <- "absent"
oa.ft0213$season[oa.ft0213$MONTH %in% c(6:11)] <- "present"

oa0213$area <- "N4010"
oa0213$area[oa0213$AVG_LAT< 40.16667] <- "S4010"

trt.analysis$area <- "N4010"
trt.analysis$area[trt.analysis$SIGHTING_LATITUDE<40.16667] <- "S4010"

oa.ft0213$area <- "N4010"
oa.ft0213$area[oa.ft0213$Latitude<40.16667] <- "S4010"

# #SEASON ONLY
# #Observed effort - using groundfish landings #
# ob.efrt <- ddply(oa0213, .(Year=RYEAR, Gear=gear, Season=season ) 
#                         , summarize
#                         , GFR=sum(GFR/2204.6, na.rm=T))
# 
# ob08.efrt<- ob.efrt$GFR[ob.efrt$Year==2008 & ob.efrt$Gear=="Pot" &
#                           ob.efrt$Season=="present" ] #&
#                           #ob.efrt$Area=="S4010"]
# 
# #Summarize TOTAL effort by strata  #, Season=season
# oa.efrt.season <- ddply(oa.ft0213, .(Year=YEAR, Gear=gear, Season=season)
#                         , summarize
#                         , MT=sum(MT, na.rm=T))
# 
# oa08.present.efrt <- oa.efrt.season$MT[oa.efrt.season$Year==2008 & oa.efrt.season$Gear=="Pot" &
#                                       oa.efrt.season$Season=="present"]# &
#                                         #oa.efrt.season$Area=="S4010"]

# #SEASON & AREA 
# #Observed effort - using groundfish landings #
# ob.efrt <- ddply(oa0213, .(Year=RYEAR, Gear=gear, Area=area, Season=season ) 
#                  , summarize
#                  , GFR=sum(GFR/2204.6, na.rm=T))
# 
# ob08.efrt<- ob.efrt$GFR[ob.efrt$Year==2008 & ob.efrt$Gear=="Pot" &
#                           ob.efrt$Season=="present" & ob.efrt$Area=="S4010"]
# 
# #Summarize TOTAL effort by strata  #
# oa.efrt.season <- ddply(oa.ft0213, .(Year=YEAR, Gear=gear, Area=area, Season=season)
#                         , summarize
#                         , MT=sum(MT, na.rm=T))
# 
# oa08.present.efrt <- oa.efrt.season$MT[oa.efrt.season$Year==2008 & oa.efrt.season$Gear=="Pot" &
#                                          oa.efrt.season$Season=="present" & oa.efrt.season$Area=="S4010"]

# #NO STRATA
# #Observed effort - using groundfish landings #
# ob.efrt <- ddply(oa0213, .(Year=RYEAR, Gear=gear) 
#                  , summarize
#                  , GFR=sum(GFR/2204.6, na.rm=T))
# 
# ob08.efrt<- ob.efrt$GFR[ob.efrt$Year==2008 & ob.efrt$Gear=="Pot" ] 
# 
# #Summarize TOTAL effort by strata  #, Season=season
# oa.efrt.season <- ddply(oa.ft0213, .(Year=YEAR, Gear=gear)
#                         , summarize
#                         , MT=sum(MT, na.rm=T))
# 
# oa08.present.efrt <- oa.efrt.season$MT[oa.efrt.season$Year==2008 & oa.efrt.season$Gear=="Pot" ]

# 5 year moving average based on page 123 of the Biological Opinion
#----------------------
#SEASON
#Observed effort - using groundfish landings #
ob.efrt <- ddply(oa0213, .(Year=RYEAR, Gear=gear, Season=season ) 
                 , summarize
                 , GFR=sum(GFR/2204.6, na.rm=T))

#subset and add LBT data
ob.efrt.LBT<- ob.efrt[ob.efrt$Gear=="Pot" & ob.efrt$Season=="present", c("Year"
                                                , "Gear", "Season", "GFR")]
ob.efrt.LBT$no.LBT <- 0
ob.efrt.LBT$no.LBT[ob.efrt.LBT$Year==2008] <- 1

ob.efrt.LBT$five.yrs <- "2003-2007"
ob.efrt.LBT$five.yrs[ob.efrt.LBT$Year %in% c(2008:2013)] <- "2008-2013"

ob.efrt.LBT.5yrave <- ddply(ob.efrt.LBT, .(five.yrs, Season), summarize
                         , GFR.5yr.ave = mean(GFR, na.rm=T)
                         , LBT.5yr.ave = mean (no.LBT, na.rm=T))


#Summarize TOTAL effort by strata  #
oa.efrt.season <- ddply(oa.ft0213, .(Year=YEAR, Gear=gear, Season=season)
                        , summarize
                        , MT=sum(MT, na.rm=T))

oa.efrt <- oa.efrt.season[oa.efrt.season$Gear=="Pot" & oa.efrt.season$Season=="present"
                          , c("Year", "Gear", "Season", "MT") ]

oa.efrt$five.yrs <- "2003-2007"
oa.efrt$five.yrs[oa.efrt$Year %in% c(2008:2013)] <- "2008-2013"

oa.efrt.5yrave <- ddply(oa.efrt, .(five.yrs, Season), summarize
                            , MT.5yr.ave = mean(MT, na.rm=T))


#-------------------------------------------------------------------------------
#Bycatch Estimation
#-------------------------------------------------------------------------------
# Bycatch Rate (generic lambda, with a numerator and denominator)

	number.killed.injured <- ob.efrt.LBT.5yrave$LBT.5yr.ave[2]
	observed.effort <- ob.efrt.LBT.5yrave$GFR.5yr.ave[2] #example from Caretta: 1000
	observed.bycatch.rate <- number.killed.injured/observed.effort
 
# Observed bycatch rate

	observed.bycatch.rate #SEASON + AREA = 0.2152333
                        #SEASON ONLY = 0.1942498
                        #NO STRATA = 0.09442227
                        # 5 yr ave with season only =  0.02844923

# Annual Fishing Effort for Entanglement Projections (user input required)

	annual.effort <- oa.efrt.5yrave$MT.5yr.ave[2] # example from Caretta: 1300
      lambda <- observed.bycatch.rate * annual.effort# observed.bycatch.rate * annual.effort
	
# Calculate Poisson probability of n entanglements (zero to 25 in this case), given lambda
        poisson.prob <- dpois(seq(0,25,1), lambda)

# Poisson probability of 0...1..2..3..and so on entanglements 
   
   z <- cbind(seq(0,25,1), poisson.prob); z <- as.data.frame(z); names(z) <- c("Number Entanglements", "Probability"); z[,2] <- round(z[,2],9); z 

# ERIC WARD ADDED THESE 3 LINES TO ILLUSTRATE HIERARCHICAL 
    estP = rbinom(1000000,round(observed.effort),observed.bycatch.rate)/observed.effort# Eric Ward added this
    lambda.2 <- estP * annual.effort# observed.bycatch.rate * annual.effort
	estBycatch = rpois(length(lambda.2),lambda.2)

#pdf(paste(drive2, "tst_turtle_output_2Dec2014_NOSTRATA.pdf"))
pdf(paste(drive2, "LBTurtle_CarettaWard_BycatchModeloutput_4Dec2014_5yrave.pdf"))
# ERIC WARD CHANGED THESE 3 LINES TO ILLUSTRATE COMPARISON BETWEEN THE TWO APPROACHES
par(mfrow = c(3,1), mai = c(0.5,0.5,0.2,0.2))
hist(rpois(length(lambda.2),lambda),breaks = length(seq(0,max(estBycatch),by=2)), xlim=c(0,60), col = "grey70", main = "No uncertainty in bycatch rate (J. Carretta)")
hist(estBycatch, breaks = length(seq(0,max(estBycatch),by=2)),xlim=c(0,60), col = "grey30", main = "Including uncertainty in bycatch rate (EW)")
plot(sort(unique(estBycatch)),1-cumsum(table(estBycatch))/length(estBycatch), xlim=c(0,60), xlab="Bycatch",type="l",ylab="Probability of exceeding",lwd=3)
plot(z[,1],z[,2], xlim=c(0,60), xlab="No.Entanglements",type="l",ylab="Probability of entanglements",lwd=3)
dev.off()    
    
    
    