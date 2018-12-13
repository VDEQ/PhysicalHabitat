--------------------------------------------------------------------------------------------------------
  
# PHYSICAL HABITAT CALCULATOR
# DESCRIPTION: This code calculates Physical Habitat parameters from Virginia DEQ Probablistic 
#    Monitoring Data read in from a Microsoft Access (.mdb) file and outputs results to a comma 
#    separated value (.csv) file and a table within the Access file names FinalSummary
#    This version outputs three Relative Bed Stability (LRBS) results, utilizing the most recent 
#    Kaufmann et.al. 2008 publication for residual pool and wood volume corrections (LRBS_fin)
# AUTHORS: Emma Jones, Jason Hill, Larry Willis: 
#    Virginia DEQ Water Monitoring Division, Blue Ridge Regional Office
# See README.txt for additional help and troubleshooting information
# LAST UPDATED: 11/30/2018 by Emma Jones (emma.jones@deq.virginia.gov)

--------------------------------------------------------------------------------------------------------

# Run in R 3.3.3
  
  
# Install packages (only if first time using tool)
#install.packages("reshape2")  
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("RODBC")
#install.packages("magrittr")
  
# Load packages
library(reshape2) #restructures data with melt(),dcast()
library(plyr) # data management functions
library(dplyr) # even better dataframe manipulation functions
library(RODBC) # connects to databases
library(magrittr) # piping package

# Restrict number of decimal places reported in output
options(digits=3)

# Set your working directory if you are not using a .Rproj
# Calculator will not run without correct file path
setwd("C:/Users/ktq89598/Documents/EmmaPHAB/March2018/")

# Connect to Access Database and read in Access tables
# Latest Access Database is available here: 
# https://www.deq.virginia.gov/Programs/Water/WaterQualityInformationTMDLs/WaterQualityMonitoring/ProbabilisticMonitoring/ProbMonDataSources.aspx
channel <- odbcConnectAccess("PHab_11-15-2018.mdb")
#channel <- odbcConnectAccess("PHab_11_27_2017.mdb")
reach <- sqlQuery(channel, "SELECT * FROM Reach ORDER BY SampleID")
thalweg <- sqlQuery(channel, "SELECT * FROM Thalweg ORDER BY SampleID")
wet <- sqlQuery(channel, "SELECT * FROM Wet ORDER BY SampleID")
bankfull <- sqlQuery(channel, "SELECT * FROM BankFull ORDER BY SampleID")
wood <- sqlQuery(channel, "SELECT * FROM Wood ORDER BY SampleID")
incision <- sqlQuery(channel, "SELECT * FROM Incision ORDER BY SampleID")
embed <- sqlQuery(channel, "SELECT * FROM Embeddedness ORDER BY SampleID")
substrate <- sqlQuery(channel, "SELECT * FROM Substrate ORDER BY SampleID")

## Residual Pool Calculations
# Combine columns from the Reach and Thalweg tables 
thalweg <- join(thalweg, reach[,c('SampleID','StationID','Date','ThalwegN','ReachLength','Interval'
                                  ,'Slope')],by=c('SampleID','StationID','Date')) 
# Identify columns to keep in 10 thalweg dataframe by determining which to drop and taking the inverse
keepNames <- grep('10|11|12|13|14',names(thalweg),invert=TRUE,value=TRUE)
# Divide thalweg dataframe into 10 and 15 thalweg dataframes
thalweg_10 <- subset(thalweg, ThalwegN=='10', select=keepNames) 
thalweg_15 <- subset(thalweg, ThalwegN=='15')
# Thalweg Dataframe Formatting Prior to Calculations
thalweg_10.1 <- melt(thalweg_10,id.vars=c('SampleID','StationID','Date','ThalwegN','ReachLength'
                                          ,'Interval','Slope')
                     ,variable.name='Location') %>% # Change dataframe to long format
  ddply(c('SampleID','StationID','Date','ThalwegN','ReachLength','Interval','Slope')
        ,summarise,Xdepth=mean(value,na.rm=T),Vdepth=sd(value, na.rm=T)) %>% # Calculate Xdepth, Vdepth
  mutate(Xdep_Vdep=Xdepth-Vdepth
         ,Stacks_Equation=(0.12 + 0.25*Slope)*Interval) # Calculate Xdep_Vdep, Stacks_Equation
thalweg_10loop <- merge(thalweg_10,thalweg_10.1,by=c('SampleID','StationID','Date','ThalwegN'
                                                     ,'ReachLength','Interval','Slope'))
# Append new calculations to thalweg_10 and create new dataframe to alter in Residual Pool Calculation loop
thalweg_15.1 <- melt(thalweg_15,id.vars=c('SampleID','StationID','Date','ThalwegN','ReachLength'
                                          ,'Interval','Slope')
                     ,variable.name='Location') %>% # Change dataframe to long format
  ddply(c('SampleID','StationID','Date','ThalwegN','ReachLength','Interval','Slope'),summarise
        ,Xdepth=mean(value,na.rm=T),Vdepth=sd(value, na.rm=T)) %>% # Calculate Xdepth, Vdepth
  mutate(Xdep_Vdep=Xdepth-Vdepth
         ,Stacks_Equation=(0.12 + 0.25*Slope)*Interval) # Calculate  Xdep_Vdep, Stacks_Equation
thalweg_15loop <- merge(thalweg_15,thalweg_15.1,by=c('SampleID','StationID','Date','ThalwegN'
                                                     ,'ReachLength','Interval','Slope'))
# Append new calculations to thalweg_15 and create new dataframe to alter in Residual Pool Calculation loop
# Create vector of measurement variable names for each loop
MeasList10 <- names(thalweg_10[,4:103])
MeasList15 <- names(thalweg_15[,4:153]) 

# Thalweg 10 Loop
for(i in 1:length(MeasList10)){ # Loop through measurement variable names (e.g. AB0,etc.)
  currMet <- MeasList10[i] # Create a variable to hold the current measurement name
  # If this is not first variable (AB0), also create a variable to hold the name of the residual of the
  # previous measurement
  if(i>1){
    prevRes <- paste('Residual',MeasList10[i-1],sep='_')
  }else{
    prevRes <- ''
  }
  if(currMet=='AB0'){
    min10.1 <- ddply(thalweg_10loop,c('SampleID'),summarise,Residual=min(Xdep_Vdep,AB0),Depth=AB0-Residual
                     ,Area=((Depth/100)*Interval))
  }else{
    x.10.1 <- subset(thalweg_10loop[,c('SampleID',prevRes,currMet,'Stacks_Equation','Interval')])
    x.10.1$prev <- x.10.1[[prevRes]]
    x.10.1$cur <- x.10.1[[currMet]]
    min10.1 <- ddply(x.10.1,c('SampleID'),summarise,Residual=min(prev + Stacks_Equation,cur,na.rm=TRUE)
                  ,Depth=cur-Residual,Area=((Depth/100)*Interval))
  }
  min10.1 <- plyr::rename(min10.1,c('Residual'=paste('Residual',currMet,sep='_')
                                    ,'Depth'=paste('Depth',currMet,sep='_')
                                    ,'Area'=paste('Area',currMet,sep='_')))
  min10.2 <- merge(thalweg_10loop,min10.1,by=c('SampleID'))
  
  thalweg_10loop <- min10.2
}
rm(thalweg_10.1);rm(x.10.1);rm(min10.1);rm(min10.2)# delete unnecessary dataframes

# Repeat for Thalweg 15 Loop
for(i in 1:length(MeasList15)){ # Loop through measurement variable names (e.g. AB0,etc.)
  currMet <- MeasList15[i] # Create a variable to hold the current measurement name
  # If this is not first variable (AB0), also create a variable to hold the name of the residual of 
  # the previous measurement
  if(i>1){prevRes <- paste('Residual',MeasList15[i-1],sep='_')}else{prevRes <- ''}
  if(currMet=='AB0'){
    min15.1 <- ddply(thalweg_15loop,c('SampleID'),summarise,Residual=min(Xdep_Vdep,AB0)
                     ,Depth=AB0-Residual,Area=((Depth/100)*Interval))
    }else{
    x.15.1 <- subset(thalweg_15loop[,c('SampleID',prevRes,currMet,'Stacks_Equation','Interval')])
    x.15.1$prev <- x.15.1[[prevRes]]
    x.15.1$cur <- x.15.1[[currMet]]
    min15.1 <- ddply(x.15.1,c('SampleID'),summarise,Residual=min(prev + Stacks_Equation,cur,na.rm=TRUE)
                     ,Depth=cur-Residual,Area=((Depth/100)*Interval))}
  min15.1 <- plyr::rename(min15.1,c('Residual'=paste('Residual',currMet,sep='_')
                                    ,'Depth'=paste('Depth',currMet,sep='_')
                                    ,'Area'=paste('Area',currMet,sep='_')))
  min15.2 <- merge(thalweg_15loop,min15.1,by=c('SampleID'))
  
  thalweg_15loop <- min15.2
} 
rm(thalweg_15.1);rm(x.15.1);rm(min15.1);rm(min15.2) # delete unnecessary dataframes
# Create a vector of names of area variables in each loop
areaNames10 <- names(thalweg_10loop)[substring(names(thalweg_10loop),1,4)=='Area']
areaNames15 <- names(thalweg_15loop)[substring(names(thalweg_15loop),1,4)=='Area']
# Calculate summary Area metrics
sumArea10 <- melt(thalweg_10loop,id.vars=c('SampleID','StationID','Date','Xdepth','Vdepth','Xdep_Vdep'
                                                ,'Stacks_Equation'),measure.vars=areaNames10) %>% # Convert to long format
  ddply(c('SampleID','StationID','Date','Xdepth','Vdepth','Xdep_Vdep','Stacks_Equation')
        ,summarise,AreaSum=sum(value))# Sum areas by sample
sumArea15 <- melt(thalweg_15loop,id.vars=c('SampleID','StationID','Date','Xdepth','Vdepth','Xdep_Vdep'
                                                ,'Stacks_Equation'),measure.vars=areaNames15) %>% # Convert to long format
  ddply(c('SampleID','StationID','Date','Xdepth','Vdepth','Xdep_Vdep','Stacks_Equation')
        ,summarise,AreaSum=sum(value))# Sum areas by sample
sumArea <- rbind(sumArea10, sumArea15)
thalweg_summary <- join(thalweg,sumArea,by=c('SampleID','StationID','Date')) %>%
  mutate(RP100=AreaSum/ReachLength*100)
thalweg_summary.alt <- thalweg_summary[,c(1:3,154:163)]
# Clean up workspace before going to substrate calculations
rm(sumArea);rm(sumArea10);rm(sumArea15);rm(thalweg_10);rm(thalweg_10loop);rm(thalweg_15);rm(thalweg_15loop)


## Substrate Calculations
substrate[,4:108] <- lapply(substrate[,4:108],as.character) # Convert all station data to character format
# Convert to long format to prepare for summary counts
substrate.long1 <- melt(substrate,id.vars=c('SampleID','StationID','Date'),variable.name='Location'
                       ,value.name='SubstrateType') %>%
  subset(!(SubstrateType %in% c('MM','OT','RC','WD','','MISSING','NA'))) %>% # Remove all MM, OT, RC, WD, blank, MISSING, and NA data to prevent them from being counted in totalcount
  ddply(c('SampleID','StationID','Date','SubstrateType'),summarise,Count=length(SubstrateType)) %>% # Count each substrate type per site
  ddply(c('SampleID','StationID','Date'),mutate,totalcount=sum(Count),PCT=Count/totalcount)# Count all substrate records and divide counts by totalcount
# Transform to wide format for QA step
substrate.wide1 <- dcast(substrate.long1,SampleID+StationID+Date~SubstrateType,value.var='PCT')
# Transform back to long format
substrate.long1.2 <- melt(substrate.wide1,id.vars=c('SampleID','StationID','Date')
                        ,value.name='PCT')%>%
  mutate(value=ifelse(is.na(PCT),0,PCT),variable=paste(variable,'_PCT',sep='')) # Remove NA's and rename variables 
# Transform back to wide format to align substrate percentages for final calculations
substrate.wide1.2 <- dcast(substrate.long1.2,SampleID+StationID+Date~variable,value.var='value')
# Calculate median particle size from substrate percentages
substrate_summary <- mutate(substrate.wide1.2,RR_PCT=RR_PCT+RS_PCT+HP_PCT
                            ,BL_PCT=XB_PCT+SB_PCT) %>% # Combine all bedrock and boulder percentages
  ddply(c('SampleID','StationID','Date'),mutate
        ,RRprop=RR_PCT*((log10(4000)+log10(8000))/2)
        ,BLprop=BL_PCT*((log10(250)+log10(4000))/2)
        ,CBprop=CB_PCT*((log10(64)+log10(250))/2)
        ,GCprop=GC_PCT*((log10(16)+log10(64))/2)
        ,GFprop=GF_PCT*((log10(2)+log10(16))/2)
        ,SAprop=SA_PCT*((log10(0.06)+log10(2))/2)
        ,FNprop=FN_PCT*((log10(0.001)+log10(0.06))/2)
        ,LSUB_DMM=RRprop+BLprop+CBprop+GCprop+GFprop+SAprop+FNprop
        ,SUB_DMM=10^(LSUB_DMM))
# Calculate substrate percentages including RC & WD
# Convert to long format to prepare for summary counts
substrate.long2 <- melt(substrate,id.vars=c('SampleID','StationID','Date'),variable.name='Location'
                        ,value.name='SubstrateType') %>%
  subset(!(SubstrateType %in% c('','MISSING','NA'))) %>% # Remove all blank, MISSING, and NA data to prevent them from being counted in totalcount
  ddply(c('SampleID','StationID','Date','SubstrateType'),summarise,Count=length(SubstrateType)) %>% # Count each substrate type per site
  ddply(c('SampleID','StationID','Date'),mutate,totalcount=sum(Count),PCT=Count/totalcount)# Count all substrate records and divide counts by totalcount
# Transform to wide format for QA step
substrate.wide2 <- dcast(substrate.long2,SampleID+StationID+Date~SubstrateType,value.var='PCT')
# Transform back to long format
substrate.long2.2 <- melt(substrate.wide2,id.vars=c('SampleID','StationID','Date')
                          ,value.name='PCT')%>%
  mutate(value=ifelse(is.na(PCT),0,PCT),variable=paste(variable,'_PCT',sep='')) # Remove NA's and rename variables 
# Transform back to wide format to align substrate percentages for final calculations
substrate.wide2.2 <- dcast(substrate.long2.2,SampleID+StationID+Date~variable,value.var='value')
# Final Substrate Percentage report
substratePCT <- ddply(substrate.wide2.2,c('SampleID','StationID','Date'),summarise
                      ,BR_PCT=(RR_PCT+RS_PCT)*100,HP_PCT=HP_PCT*100
                      ,RC_PCT=RC_PCT*100,BL_PCT=(XB_PCT+SB_PCT)*100
                      ,CB_PCT=CB_PCT*100,GC_PCT=GC_PCT*100,GF_PCT=GF_PCT*100
                      ,SA_PCT=SA_PCT*100,FN_PCT=FN_PCT*100
                      ,WD_PCT=WD_PCT*100,OT_PCT=OT_PCT*100
                      ,BL_CB_GR_PCT=(BL_PCT+CB_PCT+GC_PCT+GF_PCT)
                      ,SA_FN_PCT=(SA_PCT+FN_PCT)
                      ,TotSubstrate_PCT=BR_PCT+HP_PCT+RC_PCT+BL_PCT+CB_PCT+GC_PCT
                      +GF_PCT+SA_PCT+FN_PCT+WD_PCT+OT_PCT)
substrate_summary.alt <- join(substratePCT,select(substrate_summary, SampleID:Date, LSUB_DMM, SUB_DMM),by=c('SampleID','StationID','Date'))


## IN/OUT channel substrate calculations
# Subset substrate dataframe to separate inside and outermost channel observations
IN_substrate <- subset(substrate, select=grep('SampleID|StationID|Date|Left Middle|Middle|Right Middle'
                                              ,names(substrate),value=TRUE))
OUT_substrate <- subset(substrate, select=grep('Left Middle|Middle|Right Middle',names(substrate)
                                               ,invert=TRUE,value=TRUE))
# Convert to long format to prepare for summary counts
IN_substrate.long1 <- melt(IN_substrate,id.vars=c('SampleID','StationID','Date')
                           ,variable.name='Location',value.name='SubstrateType')%>%
  subset(!(SubstrateType %in% c('MM','OT','RC','WD','','MISSING','NA'))) %>% # Remove all MM, OT, RC, WD, blank, MISSING, and NA data to prevent them from being counted in totalcount
  ddply(c('SampleID','StationID','Date','SubstrateType'),summarise,Count=length(SubstrateType)) %>% # Count each substrate type per site
  ddply(c('SampleID','StationID','Date'),mutate,totalcount=sum(Count),PCT=Count/totalcount)# Count all substrate records and divide counts by totalcount
OUT_substrate.long1 <- melt(OUT_substrate,id.vars=c('SampleID','StationID','Date')
                            ,variable.name='Location',value.name='SubstrateType') %>%
  subset(!(SubstrateType %in% c('MM','OT','RC','WD','','MISSING','NA'))) %>% # Remove all MM, OT, RC, WD, blank, MISSING, and NA data to prevent them from being counted in totalcount
  ddply(c('SampleID','StationID','Date','SubstrateType'),summarise,Count=length(SubstrateType)) %>% # Count each substrate type per site
  ddply(c('SampleID','StationID','Date'),mutate,totalcount=sum(Count),PCT=Count/totalcount)# Count all substrate records and divide counts by totalcount
# Transform to wide format for QA step
IN_substrate.wide1 <- dcast(IN_substrate.long1,SampleID+StationID+Date~SubstrateType,value.var='PCT')
OUT_substrate.wide1 <- dcast(OUT_substrate.long1,SampleID+StationID+Date~SubstrateType,value.var='PCT')
# Transform back to long format
IN_substrate.long1.2 <- melt(IN_substrate.wide1,id.vars=c('SampleID','StationID','Date')
                             ,value.name='PCT') %>%
  mutate(value=ifelse(is.na(PCT),0,PCT),variable=paste(variable,'_PCT',sep='')) # Remove NA's and rename variables

OUT_substrate.long1.2 <- melt(OUT_substrate.wide1,id.vars=c('SampleID','StationID','Date')
                              ,value.name='PCT') %>%
  mutate(value=ifelse(is.na(PCT),0,PCT),variable=paste(variable,'_PCT',sep='')) # Remove NA's and rename variables
# Transform back to wide format to align substrate percentages for final calculations
IN_substrate.wide1.2 <- dcast(IN_substrate.long1.2,SampleID+StationID+Date~variable,value.var='value')
OUT_substrate.wide1.2 <- dcast(OUT_substrate.long1.2,SampleID+StationID+Date~variable,value.var='value')
# Calculate median particle size from substrate percentages
IN_substrate_summary <- mutate(IN_substrate.wide1.2,RR_PCT=RR_PCT+RS_PCT+HP_PCT
                               ,BL_PCT=XB_PCT+SB_PCT) %>% # Combine all bedrock and boulder percentages
  ddply(c('SampleID','StationID','Date'),mutate
        ,IN_RRprop=RR_PCT*((log10(4000)+log10(8000))/2)
        ,IN_BLprop=BL_PCT*((log10(250)+log10(4000))/2)
        ,IN_CBprop=CB_PCT*((log10(64)+log10(250))/2)
        ,IN_GCprop=GC_PCT*((log10(16)+log10(64))/2)
        ,IN_GFprop=GF_PCT*((log10(2)+log10(16))/2)
        ,IN_SAprop=SA_PCT*((log10(0.06)+log10(2))/2)
        ,IN_FNprop=FN_PCT*((log10(0.001)+log10(0.06))/2)
        ,IN_LSUB_DMM=IN_RRprop+IN_BLprop+IN_CBprop+IN_GCprop+IN_GFprop+IN_SAprop+IN_FNprop
        ,IN_SUB_DMM=10^(IN_LSUB_DMM))
# Calculate median particle size from substrate percentages
OUT_substrate_summary <- mutate(OUT_substrate.wide1.2,RR_PCT=RR_PCT+RS_PCT+HP_PCT
                                ,BL_PCT=XB_PCT+SB_PCT) %>% # Combine all bedrock and boulder percentages
  ddply(c('SampleID','StationID','Date'),mutate
        ,OUT_RRprop=RR_PCT*((log10(4000)+log10(8000))/2)
        ,OUT_BLprop=BL_PCT*((log10(250)+log10(4000))/2)
        ,OUT_CBprop=CB_PCT*((log10(64)+log10(250))/2)
        ,OUT_GCprop=GC_PCT*((log10(16)+log10(64))/2)
        ,OUT_GFprop=GF_PCT*((log10(2)+log10(16))/2)
        ,OUT_SAprop=SA_PCT*((log10(0.06)+log10(2))/2)
        ,OUT_FNprop=FN_PCT*((log10(0.001)+log10(0.06))/2)
        ,OUT_LSUB_DMM=OUT_RRprop+OUT_BLprop+OUT_CBprop+OUT_GCprop+OUT_GFprop
        +OUT_SAprop+OUT_FNprop
        ,OUT_SUB_DMM=10^(OUT_LSUB_DMM))
# Calculate substrate percentages including RC & WD
# Convert to long format to prepare for summary counts
IN_substrate.long2 <- melt(IN_substrate,id.vars=c('SampleID','StationID','Date'),variable.name='Location'
                           ,value.name='SubstrateType') %>%
  subset(!(SubstrateType %in% c('','MISSING','NA'))) %>% # Remove all blank, MISSING, and NA data to prevent them from being counted in totalcount
  ddply(c('SampleID','StationID','Date','SubstrateType'),summarise,Count=length(SubstrateType)) %>% # Count each substrate type per site
  ddply(c('SampleID','StationID','Date'),mutate,totalcount=sum(Count),PCT=Count/totalcount)# Count all substrate records and divide counts by totalcount
OUT_substrate.long2 <- melt(OUT_substrate,id.vars=c('SampleID','StationID','Date'),variable.name='Location'
                            ,value.name='SubstrateType') %>%
  subset(!(SubstrateType %in% c('','MISSING','NA'))) %>% # Remove all blank, MISSING, and NA data to prevent them from being counted in totalcount
  ddply(c('SampleID','StationID','Date','SubstrateType'),summarise,Count=length(SubstrateType)) %>% # Count each substrate type per site
  ddply(c('SampleID','StationID','Date'),mutate,totalcount=sum(Count),PCT=Count/totalcount)# Count all substrate records and divide counts by totalcount
# Transform to wide format for QA step
IN_substrate.wide2 <- dcast(IN_substrate.long2,SampleID+StationID+Date~SubstrateType,value.var='PCT')
OUT_substrate.wide2 <- dcast(OUT_substrate.long2,SampleID+StationID+Date~SubstrateType,value.var='PCT')
# Transform back to long format
IN_substrate.long2.2 <- melt(IN_substrate.wide2,id.vars=c('SampleID','StationID','Date')
                             ,value.name='PCT')%>%
  mutate(value=ifelse(is.na(PCT),0,PCT),variable=paste(variable,'_PCT',sep='')) # Remove NA's and rename variables 
OUT_substrate.long2.2 <- melt(OUT_substrate.wide2,id.vars=c('SampleID','StationID','Date')
                              ,value.name='PCT')%>%
  mutate(value=ifelse(is.na(PCT),0,PCT),variable=paste(variable,'_PCT',sep='')) # Remove NA's and rename variables 
# Transform back to wide format to align substrate percentages for final calculations
IN_substrate.wide2.2 <- dcast(IN_substrate.long2.2,SampleID+StationID+Date~variable,value.var='value')
OUT_substrate.wide2.2 <- dcast(OUT_substrate.long2.2,SampleID+StationID+Date~variable,value.var='value')
# Final Substrate Percentage report
IN_substratePCT <- ddply(IN_substrate.wide2.2,c('SampleID','StationID','Date'),summarise
                         ,IN_BR_PCT=(RR_PCT+RS_PCT)*100,IN_HP_PCT=HP_PCT*100
                         ,IN_RC_PCT=RC_PCT*100,IN_BL_PCT=(XB_PCT+SB_PCT)*100
                         ,IN_CB_PCT=CB_PCT*100,IN_GC_PCT=GC_PCT*100,IN_GF_PCT=GF_PCT*100
                         ,IN_SA_PCT=SA_PCT*100,IN_FN_PCT=FN_PCT*100
                         ,IN_WD_PCT=WD_PCT*100,IN_OT_PCT=OT_PCT*100
                         ,IN_BL_CB_GR_PCT=(IN_BL_PCT+IN_CB_PCT+IN_GC_PCT+IN_GF_PCT)
                         ,IN_SA_FN_PCT=(IN_SA_PCT+IN_FN_PCT)
                         ,IN_TotSubstrate_PCT=IN_BR_PCT+IN_HP_PCT+IN_RC_PCT+IN_BL_PCT+IN_CB_PCT
                         +IN_GC_PCT+IN_GF_PCT+IN_SA_PCT+IN_FN_PCT+IN_WD_PCT+IN_OT_PCT)
IN_substrate_summary.alt <- join(IN_substratePCT,select(IN_substrate_summary, SampleID:Date, IN_LSUB_DMM, IN_SUB_DMM),
                                 by=c('SampleID','StationID','Date'))
OUT_substratePCT <- ddply(OUT_substrate.wide2.2,c('SampleID','StationID','Date'),summarise
                          ,OUT_BR_PCT=(RR_PCT+RS_PCT)*100,OUT_HP_PCT=HP_PCT*100
                          ,OUT_RC_PCT=RC_PCT*100,OUT_BL_PCT=(XB_PCT+SB_PCT)*100
                          ,OUT_CB_PCT=CB_PCT*100,OUT_GC_PCT=GC_PCT*100,OUT_GF_PCT=GF_PCT*100
                          ,OUT_SA_PCT=SA_PCT*100,OUT_FN_PCT=FN_PCT*100
                          ,OUT_WD_PCT=WD_PCT*100,OUT_OT_PCT=OT_PCT*100
                          ,OUT_BL_CB_GR_PCT=(OUT_BL_PCT+OUT_CB_PCT+OUT_GC_PCT+OUT_GF_PCT)
                          ,OUT_SA_FN_PCT=(OUT_SA_PCT+OUT_FN_PCT)
                          ,OUT_TotSubstrate_PCT=OUT_BR_PCT+OUT_HP_PCT+OUT_RC_PCT+OUT_BL_PCT+OUT_CB_PCT
                          +OUT_GC_PCT+OUT_GF_PCT+OUT_SA_PCT+OUT_FN_PCT+OUT_WD_PCT+OUT_OT_PCT)
OUT_substrate_summary.alt <- join(OUT_substratePCT,select(OUT_substrate_summary, SampleID:Date, OUT_LSUB_DMM, OUT_SUB_DMM)
                                  ,by=c('SampleID','StationID','Date'))
# Combine all substrate summaries
INOUTsubstrate_summary.alt <- join_all(list(substrate_summary.alt,IN_substrate_summary.alt
                                            ,OUT_substrate_summary.alt),by=c('SampleID','StationID','Date'))


# Clean up workspace before proceeding
rm(substrate_summary);rm(substrate.long1.2);rm(substrate.long2)
rm(substrate.long2.2);rm(substrate.wide1);rm(substrate.wide1.2);rm(substrate.wide2)
rm(substrate.wide2.2);rm(substratePCT)
rm(IN_substrate_summary);rm(IN_substrate.long1);rm(IN_substrate.long1.2)
rm(IN_substrate.long2);rm(IN_substrate.long2.2);rm(IN_substrate.wide1)
rm(IN_substrate.wide1.2);rm(IN_substrate.wide2);rm(IN_substrate)
rm(IN_substrate.wide2.2);rm(IN_substratePCT);rm(IN_substrate_summary.alt)
rm(OUT_substrate_summary);rm(OUT_substrate.long1);rm(OUT_substrate.long1.2)
rm(OUT_substrate.long2);rm(OUT_substrate.long2.2);rm(OUT_substrate.wide1)
rm(OUT_substrate.wide1.2);rm(OUT_substrate.wide2);rm(OUT_substrate)
rm(OUT_substrate.wide2.2);rm(OUT_substratePCT);rm(OUT_substrate_summary.alt)


## Bankfull
XBKF_H <- apply(bankfull[, 4:14], 1, mean, na.rm=T)  # Average Bankfull Height
# na.rm=T adjusts denominator for missing data
XBKF_W <- apply(bankfull[,15:25], 1, mean, na.rm=T)  # Average Bankfull Width

## Wetted Width
Xwid <- apply(wet[, 4:24], 1, mean, na.rm=T) # Mean Wetted Width

## Incision
INC_H <- apply(incision[, 4:14], 1, mean, na.rm=T) # Average Incision Height

## Embeddedness
Xembed <- apply(embed[, 4:58], 1, mean, na.rm=T)
Vembed <- apply(embed[, 4:58], 1, sd, na.rm=T)

# IN/OUT Embeddedness- subset embed dataframe to separate inside and outermost channel observations
IN_embed <- subset(embed, select=grep('SampleID|StationID|Date|LM|M|RM',names(embed),value=TRUE))
OUT_embed <- subset(embed, select=grep('LM|M|RM',names(embed),invert=TRUE,value=TRUE))
# Embeddedness, inside and outside of channel
IN_Xembed <- apply(IN_embed[, 4:36], 1, mean, na.rm=T)
IN_Vembed <- apply(IN_embed[, 4:36], 1, sd, na.rm=T)
OUT_Xembed <- apply(OUT_embed[, 4:25], 1, mean, na.rm=T)
OUT_Vembed <- apply(OUT_embed[, 4:25], 1, sd, na.rm=T)

# Mean embeddedness of BL(XB+SB),CB,GR (GC+GF)
embed.long <- melt(embed,id.vars=c('SampleID','StationID','Date'),variable.name='Location'
                   ,value.name='EmbedPCT')
sub <- unique(substrate.long1[,c(1:6)])# Retrieve particle count and totalcount from substrate
sub.wide <- select(substrate,-matches("2")) # Remove mid thalweg substrate measures
names(sub.wide) <- c('SampleID','StationID','Date','AL','BL','CL','DL','EL','FL','GL','HL','IL','JL','KL'
                     ,'ALM','BLM','CLM','DLM','ELM','FLM','GLM','HLM','ILM','JLM','KLM'
                     ,'AM','BM','CM','DM','EM','FM','GM','HM','IM','JM','KM'
                     ,'ARM','BRM','CRM','DRM','ERM','FRM','GRM','HRM','IRM','JRM','KRM'
                     ,'AR','BR','CR','DR','ER','FR','GR','HR','IR','JR','KR')
embed.sub <- melt(sub.wide,id.vars=c('SampleID','StationID','Date'),variable.name='Location'
                  ,value.name='SubstrateType') %>% # Long format
  join(embed.long, by=c('SampleID','StationID','Date','Location')) %>% # Combine Embeddedness and Substrate
  filter(SubstrateType %in% c('XB','SB','CB','GC','GF')) # Identify only Boulders, Cobbles, and Gravels
embed.sub1 <- melt(sub.wide,id.vars=c('SampleID','StationID','Date'),variable.name='Location'
                     ,value.name='SubstrateType') %>%
  filter(!(SubstrateType %in% c('MISSING','NA'))) %>% # Remove all missing data
  ddply(c('SampleID','StationID','Date'),summarise,totalcount=length(StationID)) # Total particle count at transects
embed.sub1.5 <- ddply(embed.sub,c('SampleID','StationID','Date','SubstrateType')
                      ,summarise,Count=length(SubstrateType)) %>% # Count each substrate type per site
  ddply(c('SampleID','StationID','Date'),summarise,BL_CB_GRCount=sum(Count)) # count of BL,CB,GR at each site, not including mid channel measures
embed.sub2<- dcast(embed.sub,SampleID+StationID+Date~Location,value.var='EmbedPCT')
BL_CB_GRmeanEmbed <- apply(embed.sub2[,4:58],1,mean,na.rm=T)
BL_CB_GRmeanEmbed.df <- data.frame(cbind(embed.sub2[,1:3],BL_CB_GRmeanEmbed)) # Attach to dataframe
BL_CB_GRmeanEmbed.df <- join_all(list(embed.sub1,embed.sub1.5,BL_CB_GRmeanEmbed.df)
                                 ,by=c('SampleID','StationID','Date')) %>% # Join to dataframe with all sites
  mutate(BL_CB_GR_transectPCT=(BL_CB_GRCount/totalcount)*100) %>%
  select(-c(totalcount,BL_CB_GRCount)) # Clean up dataframe
# Clean up workspace
rm(embed.long);rm(embed.sub);rm(embed.sub1);rm(embed.sub1.5);rm(embed.sub2);rm(substrate.long1);rm(sub.wide)


## Wood
attach(reach)
wood_summary <- mutate(wood, XBKF_W=XBKF_W, ReachLength=ReachLength,wetsdsl=wetsdsl*0.058
                       ,wetsdml=wetsdml*0.182,wetsdll=wetsdll*0.438,wetmdsl=wetmdsl*0.333
                       ,wetmdml=wetmdml*1.042,wetmdll=wetmdll*2.501,wetldsl=wetldsl*0.932
                       ,wetldml=wetldml*2.911,wetldll=wetldll*6.988,wetxdsl=wetxdsl*3.016
                       ,wetxdml=wetxdml*9.421,wetxdll=wetxdll*22.620
                       ,VLW=wetsdsl+wetsdml+wetsdll+wetmdsl+wetmdml+wetmdll+wetldsl+wetldml+wetldll
                       +wetxdsl+wetxdml+wetxdll, VLW_msq=VLW/(ReachLength*XBKF_W))%>%
  select(SampleID,StationID,Date,ReachLength,VLW,VLW_msq)
wood_summary.alt<- mutate(wood_summary,XBKF_H=XBKF_H,Xwid=Xwid,INC_H=INC_H,Xembed=Xembed,Vembed=Vembed) %>%
  join(BL_CB_GRmeanEmbed.df,by=c('SampleID','StationID','Date'))
wood_summaryINOUT <- mutate(wood_summary.alt,IN_Xembed=IN_Xembed,IN_Vembed=IN_Vembed,OUT_Xembed=OUT_Xembed
                            ,OUT_Vembed=OUT_Vembed)
# Clean up workspace
rm(BL_CB_GRmeanEmbed.df)


## QA STEP: Search previous parameters for missing data
thalweg_10QA <- subset(thalweg, ThalwegN=='10', select=keepNames) %>%
  melt(id.vars=c('SampleID','StationID','Date','ThalwegN','ReachLength','Interval'
                 ,'Slope'),variable.name='Location') %>%
  ddply(c('SampleID','StationID','Date'), mutate,thalweg_NAcount=ifelse(is.na(value),1,0)) %>%
  ddply(c('SampleID','StationID','Date'), summarise
        ,thalweg_NAcount=sum(thalweg_NAcount)) %>% 
  mutate(thalwegPCT_NA=(thalweg_NAcount/100)*100) 
QA_thalweg <- subset(thalweg, ThalwegN=='15') %>%
  melt(id.vars=c('SampleID','StationID','Date','ThalwegN','ReachLength','Interval','Slope')
       ,variable.name='Location') %>% # Change dataframe to long format
  ddply(c('SampleID','StationID','Date'), mutate
        ,thalweg_NAcount=ifelse(is.na(value),1,0)) %>%
  ddply(c('SampleID','StationID','Date'), summarise
        ,thalweg_NAcount=sum(thalweg_NAcount)) %>%
  mutate(thalwegPCT_NA=(thalweg_NAcount/150)*100) %>%
  rbind(thalweg_10QA) %>% arrange(SampleID)
QA_bankfull <- melt(bankfull,c('SampleID','StationID','Date')) %>%
  ddply(c('SampleID','StationID','Date'),mutate,bankfull_NAcount=ifelse(is.na(value),1,0)) %>%
  ddply(c('SampleID','StationID','Date'),summarise,bankfull_NAcount=sum(bankfull_NAcount)) %>%
  mutate(bankfullPCT_NA=(bankfull_NAcount/22)*100)
QA_wet <- melt(wet,c('SampleID','StationID','Date')) %>%
  ddply(c('SampleID','StationID','Date'),mutate,wet_NAcount=ifelse(is.na(value),1,0)) %>%
  ddply(c('SampleID','StationID','Date'),summarise,wet_NAcount=sum(wet_NAcount)) %>%
  mutate(wetPCT_NA=(wet_NAcount/21)*100)
QA_incision <- melt(incision, c('SampleID','StationID','Date')) %>%
  ddply(c('SampleID','StationID','Date'),mutate,incision_NAcount=ifelse(is.na(value),1,0)) %>%
  ddply(c('SampleID','StationID','Date'),summarise,incision_NAcount=sum(incision_NAcount)) %>%
  mutate(incisionPCT_NA=(incision_NAcount/11)*100)
QA_embed <- melt(embed,c('SampleID','StationID','Date')) %>%
  ddply(c('SampleID','StationID','Date'),mutate,embed_NAcount=ifelse(is.na(value),1,0)) %>%
  ddply(c('SampleID','StationID','Date'),summarise,embed_NAcount=sum(embed_NAcount)) %>%
  mutate(embedPCT_NA=(embed_NAcount/55)*100)
QA_substrate <- melt(substrate,id.vars=c('SampleID','StationID','Date'),variable.name='Location'
                     ,value.name='SubstrateType') %>%
  ddply(c('SampleID','StationID','Date','SubstrateType'),mutate
                      ,substrate_NAcount=ifelse(is.na(SubstrateType),1,0)
                      ,substrate_MISSINGcount=ifelse(SubstrateType=='MISSING',1,0)
                      ,substrate_Blankcount=ifelse(SubstrateType=='',1,0)) %>%
  ddply(c('SampleID','StationID','Date'),summarise,substrateDataIssues=sum(substrate_NAcount)
        +sum(substrate_MISSINGcount)+sum(substrate_Blankcount)) %>%
  mutate(substratePCT_NA=(substrateDataIssues/105)*100)
QAsummary <- join_all(list(QA_thalweg,QA_bankfull,QA_wet,QA_incision,QA_embed,QA_substrate)
                      ,by=c('SampleID','StationID','Date')) %>%
  mutate(SiteFlag=ifelse(thalwegPCT_NA>50|bankfullPCT_NA>50|wetPCT_NA>50|incisionPCT_NA>50
                         |embedPCT_NA>50|substratePCT_NA>50,"Flag","Site Accepted")) # If any parameters (except wood) have >50% missing data flag site
# Clean Workspace
rm(QA_bankfull);rm(QA_embed);rm(QA_incision);rm(QA_substrate);rm(QA_thalweg);rm(QA_wet);rm(thalweg_10QA)
rm(sub);rm(thalweg_summary);rm(wood_summary)

# Final Data Summary
summary <- join_all(list(QAsummary[,c(1:3,16)], thalweg_summary.alt,substrate_summary.alt, wood_summary.alt))
INOUTsummary <- join_all(list(summary,INOUTsubstrate_summary.alt,wood_summaryINOUT))
# Establish constants for additional summary parameters
rho=998; rhosed=2650; g=9.807

# TMDL Workgroup Output 
TMDLSummary <- mutate(summary,radiusBKF=((Xdepth/100)+XBKF_H)/2,Dcbf=13.7*radiusBKF*Slope,RBS1=SUB_DMM/Dcbf
                       ,LRBS1=log10(SUB_DMM/Dcbf),RW=VLW_msq*1000, RP=(RP100*0.5)*10
                       ,RBFmm=((Xdepth*10)+(XBKF_H*1000))/2,R_bf=RBFmm-RW-RP,D_cbf=(13.7*R_bf)*(Slope/100)
                       ,LDMB=log10(D_cbf),LRBS2=LSUB_DMM-LDMB,XBKF_W=XBKF_W,BKF_depth_in_meters=(Xdepth/100)+XBKF_H
                       ,BKFW_BKFD=XBKF_W/BKF_depth_in_meters,incised_depth=(Xdepth/100)+INC_H) %>%
  select(-c(Vdepth,Xdep_Vdep,Stacks_Equation,AreaSum,SUB_DMM,VLW,XBKF_H,INC_H,Vembed,radiusBKF,Dcbf
            ,RBS1,LRBS1,RW,RP,RBFmm,R_bf,D_cbf,LDMB)) %>% # Remove select variables
  select(SampleID,StationID,Date,SiteFlag,ThalwegN,ReachLength,Interval,Slope,RP100,BR_PCT,HP_PCT,RC_PCT
         ,BL_PCT,CB_PCT,GC_PCT,GF_PCT,SA_PCT,FN_PCT,WD_PCT,OT_PCT,BL_CB_GR_PCT,SA_FN_PCT,TotSubstrate_PCT
         ,LSUB_DMM,VLW_msq,Xdepth,Xwid,XBKF_W,BKF_depth_in_meters,BKFW_BKFD,incised_depth,Xembed
         ,BL_CB_GR_transectPCT,BL_CB_GRmeanEmbed,LRBS2) # Reorder output fields

MasterSummary <- mutate(summary,radiusBKF=((Xdepth/100)+XBKF_H)/2,Dcbf=13.7*radiusBKF*Slope
                        ,RBS1=SUB_DMM/Dcbf,LRBS1=log10(SUB_DMM/Dcbf) # End LRBS1 calculations 
                        ,RW=VLW_msq*1000,RP=(RP100*0.5)*10,RBFmm=((Xdepth*10)+(XBKF_H*1000))/2
                        ,R_bf=RBFmm-RW-RP,D_cbf=(13.7*R_bf)*(Slope/100),LDMB=log10(D_cbf) 
                        ,LRBS2=LSUB_DMM-LDMB,XBKF_W=XBKF_W,BKF_depth_in_meters=(Xdepth/100)+XBKF_H
                        ,BKFW_BKFD=XBKF_W/BKF_depth_in_meters,incised_depth=(Xdepth/100)+INC_H
                        ,incised_height_BKFD=INC_H/BKF_depth_in_meters
                        ,BKFW_incised_depth=XBKF_W/incised_depth
                        ,incised_D_BKFD=incised_depth/BKF_depth_in_meters # End LRBS2 calculations
                        ,LS=log10(0.01+Slope),LRP=log10(0.1+RP100),LW=log10(0.0001+VLW_msq)
                        ,Dgm=10^LSUB_DMM,Dgm_m=(10^LSUB_DMM)/1000,Dbf_th=XBKF_H+(Xdepth/100)
                        ,Rb3=0.65*Dbf_th
                        ,Ct_rpwd=(1.21*(((RP100/100)^1.08)*(((RP100/100)+VLW_msq)^0.638)))/(Dbf_th^3.32)
                        ,LCt_rpwd=log10(Ct_rpwd),Cp3_mill=(1/8)*((2.03*log10(12.2*(Rb3/((10^LSUB_DMM)/1000))))^-2)
                        ,Cp3_mill_2=ifelse(Cp3_mill<0.002,0.002,Cp3_mill),LCp3_mill=log10(Cp3_mill)
                        ,Ct_rpwd_cl=Ct_rpwd,Ct_rpwd_cl_2=ifelse(Ct_rpwd<Cp3_mill_2,Cp3_mill_2,Ct_rpwd)
                        ,Ct_rpwd_cl_3= ifelse(Ct_rpwd_cl_2=='NA',Cp3_mill,Ct_rpwd_cl_2)
                        ,Cp3Ctrpwd_rat=Cp3_mill_2/Ct_rpwd
                        ,Cp3Ctrpwd_rat=ifelse(Cp3Ctrpwd_rat>1.0,1.0,Cp3Ctrpwd_rat)
                        ,Rrpw3=Rb3*(Cp3Ctrpwd_rat^0.3333),Rrpw3_RtRat=Rrpw3/Rb3
                        ,Reyp3=(((g*Rb3*(Slope/100))^0.5)*((10^LSUB_DMM)/1000))/0.00000102
                        ,LReyp3=log10(Reyp3)
                        ,Shld_Px3= 0.5*((0.22*(Reyp3^(-0.6)))+(0.06*(10^(-7.7*(Reyp3^(-0.6))))))
                        ,Shld_Px3=ifelse(is.na(LReyp3),NA,Shld_Px3),LShld_Px3=log10(Shld_Px3)
                        ,LShld_Px3=ifelse(LReyp3<=1.4166,(-1.3991-(0.24419*LReyp3)),LShld_Px3)
                        ,Shld_Px3=10^(LShld_Px3)
                        ,Dcbf_fin=1000*((rho*g*Rrpw3*(Slope/100))/(Shld_Px3*(rhosed-rho)*g))
                        ,LDcbf_fin=log10(Dcbf_fin),LRBS_fin=LSUB_DMM-LDcbf_fin) 

QAsummary <- join(QAsummary,MasterSummary,by=c('SampleID','StationID','Date','SiteFlag')) %>%
  select(SampleID,StationID,Date,thalweg_NAcount,thalwegPCT_NA,bankfull_NAcount,bankfullPCT_NA
         ,wet_NAcount,wetPCT_NA,incision_NAcount,incisionPCT_NA,embed_NAcount,embedPCT_NA
         ,substrateDataIssues,substratePCT_NA,SiteFlag,RBFmm,LS,LRP,LW,Dgm,Dgm_m,Dbf_th,Rb3
         ,Ct_rpwd,LCt_rpwd,Cp3_mill,Cp3_mill_2,LCp3_mill,Ct_rpwd_cl,Ct_rpwd_cl_2,Ct_rpwd_cl_3
         ,Cp3Ctrpwd_rat,Rrpw3,Rrpw3_RtRat,Reyp3,LReyp3,Shld_Px3,LShld_Px3,Dcbf_fin,LDcbf_fin)
MasterSummary <- select(MasterSummary,SampleID,StationID,Date,SiteFlag,ThalwegN,ReachLength,Interval
                        ,Slope,Xdepth,Vdepth,Xdep_Vdep,Stacks_Equation,AreaSum,RP100,BR_PCT,HP_PCT
                        ,RC_PCT,BL_PCT,CB_PCT,GC_PCT,GF_PCT,SA_PCT,FN_PCT,WD_PCT,OT_PCT,BL_CB_GR_PCT
                        ,SA_FN_PCT,TotSubstrate_PCT,LSUB_DMM,SUB_DMM,VLW,VLW_msq,XBKF_H,Xwid,INC_H
                        ,Xembed,Vembed,BL_CB_GRmeanEmbed,BL_CB_GR_transectPCT,radiusBKF,Dcbf,RBS1
                        ,LRBS1,RW,RP,R_bf,D_cbf,LDMB,LRBS2,XBKF_W,BKF_depth_in_meters,BKFW_BKFD,incised_depth
                        ,incised_height_BKFD,BKFW_incised_depth,incised_D_BKFD,LRBS_fin)
INOUT_MasterSummary <- INOUTsummary %>%
  select(c(SampleID,StationID,Date,SiteFlag,ThalwegN,ReachLength,Interval,Slope,Xdepth
           ,RP100,VLW_msq,IN_BR_PCT:OUT_Vembed)) %>%
  mutate(IN_radiusBKF=((Xdepth/100)+XBKF_H)/2,IN_Dcbf=13.7*IN_radiusBKF*Slope
         ,IN_RBS1=IN_SUB_DMM/IN_Dcbf, IN_LRBS1=log10(IN_SUB_DMM/IN_Dcbf) # End IN_LRBS1 calculations 
         ,RW=VLW_msq*1000, RP=(RP100*0.5)*10, RBFmm=((Xdepth*10)+(XBKF_H*1000))/2
         ,R_bf=RBFmm-RW-RP, D_cbf=(13.7*R_bf)*(Slope/100), LDMB=log10(D_cbf) 
         ,IN_LRBS2=IN_LSUB_DMM-LDMB # End IN_LRBS2 calculations
         ,LS=log10(0.01+Slope), LRP=log10(0.1+RP100), LW=log10(0.0001+VLW_msq)
         ,IN_Dgm= 10^IN_LSUB_DMM, IN_Dgm_m=(10^IN_LSUB_DMM)/1000
         ,Dbf_th=XBKF_H+(Xdepth/100),Rb3=0.65*Dbf_th
         ,Ct_rpwd=(1.21*(((RP100/100)^1.08)*(((RP100/100)+VLW_msq)^0.638)))/(Dbf_th^3.32)
         ,LCt_rpwd=log10(Ct_rpwd) 
         ,IN_Cp3_mill=(1/8)*((2.03*log10(12.2*(Rb3/((10^IN_LSUB_DMM)/1000))))^-2)
         ,IN_Cp3_mill_2=ifelse(IN_Cp3_mill<0.002,0.002,IN_Cp3_mill), IN_LCp3_mill=log10(IN_Cp3_mill)
         ,IN_Ct_rpwd_cl=Ct_rpwd, IN_Ct_rpwd_cl_2= ifelse(Ct_rpwd<IN_Cp3_mill_2,IN_Cp3_mill_2,Ct_rpwd)
         ,IN_Ct_rpwd_cl_3= ifelse(IN_Ct_rpwd_cl_2=='NA',IN_Cp3_mill,IN_Ct_rpwd_cl_2)
         ,IN_Cp3Ctrpwd_rat=IN_Cp3_mill_2/Ct_rpwd
         ,IN_Cp3Ctrpwd_rat=ifelse(IN_Cp3Ctrpwd_rat>1.0,1.0,IN_Cp3Ctrpwd_rat)
         ,IN_Rrpw3=Rb3*(IN_Cp3Ctrpwd_rat^0.3333), IN_Rrpw3_RtRat=IN_Rrpw3/Rb3
         ,IN_Reyp3=(((g*Rb3*(Slope/100))^0.5)*((10^IN_LSUB_DMM)/1000))/0.00000102
         ,IN_LReyp3=log10(IN_Reyp3)
         ,IN_Shld_Px3= 0.5*((0.22*(IN_Reyp3^(-0.6)))+(0.06*(10^(-7.7*(IN_Reyp3^(-0.6))))))
         ,IN_Shld_Px3=ifelse(is.na(IN_LReyp3),NA,IN_Shld_Px3), IN_LShld_Px3=log10(IN_Shld_Px3)
         ,IN_LShld_Px3=ifelse(IN_LReyp3<=1.4166,(-1.3991-(0.24419*IN_LReyp3)),IN_LShld_Px3)
         ,IN_Shld_Px3=10^(IN_LShld_Px3)
         ,IN_Dcbf_fin=1000*((rho*g*IN_Rrpw3*(Slope/100))/(IN_Shld_Px3*(rhosed-rho)*g))
         ,IN_LDcbf_fin=log10(IN_Dcbf_fin), IN_LRBS_fin=IN_LSUB_DMM-IN_LDcbf_fin 
         ,OUT_radiusBKF=((Xdepth/100)+XBKF_H)/2, OUT_Dcbf=13.7*OUT_radiusBKF*Slope
         ,OUT_RBS1=OUT_SUB_DMM/OUT_Dcbf, OUT_LRBS1=log10(OUT_SUB_DMM/OUT_Dcbf) # End OUT_LRBS1 calculations 
         ,OUT_LRBS2=OUT_LSUB_DMM-LDMB # End OUT_LRBS2 calculations
         ,OUT_Dgm= 10^OUT_LSUB_DMM, OUT_Dgm_m=(10^OUT_LSUB_DMM)/1000
         ,OUT_Cp3_mill=(1/8)*((2.03*log10(12.2*(Rb3/((10^OUT_LSUB_DMM)/1000))))^-2)
         ,OUT_Cp3_mill_2=ifelse(OUT_Cp3_mill<0.002,0.002,OUT_Cp3_mill)
         ,OUT_LCp3_mill=log10(OUT_Cp3_mill), OUT_Ct_rpwd_cl=Ct_rpwd
         ,OUT_Ct_rpwd_cl_2= ifelse(Ct_rpwd<OUT_Cp3_mill_2,OUT_Cp3_mill_2,Ct_rpwd)
         ,OUT_Ct_rpwd_cl_3= ifelse(OUT_Ct_rpwd_cl_2=='NA',OUT_Cp3_mill,OUT_Ct_rpwd_cl_2)
         ,OUT_Cp3Ctrpwd_rat=OUT_Cp3_mill_2/Ct_rpwd
         ,OUT_Cp3Ctrpwd_rat=ifelse(OUT_Cp3Ctrpwd_rat>1.0,1.0,OUT_Cp3Ctrpwd_rat)
         ,OUT_Rrpw3=Rb3*(OUT_Cp3Ctrpwd_rat^0.3333), OUT_Rrpw3_RtRat=OUT_Rrpw3/Rb3
         ,OUT_Reyp3=(((g*Rb3*(Slope/100))^0.5)*((10^OUT_LSUB_DMM)/1000))/0.00000102
         ,OUT_LReyp3=log10(OUT_Reyp3)
         ,OUT_Shld_Px3= 0.5*((0.22*(OUT_Reyp3^(-0.6)))+(0.06*(10^(-7.7*(OUT_Reyp3^(-0.6))))))
         ,OUT_Shld_Px3=ifelse(is.na(OUT_LReyp3),NA,OUT_Shld_Px3)
         ,OUT_LShld_Px3=log10(OUT_Shld_Px3)
         ,OUT_LShld_Px3=ifelse(OUT_LReyp3<=1.4166,(-1.3991-(0.24419*OUT_LReyp3)),OUT_LShld_Px3)
         ,OUT_Shld_Px3=10^(OUT_LShld_Px3)
         ,OUT_Dcbf_fin=1000*((rho*g*OUT_Rrpw3*(Slope/100))/(OUT_Shld_Px3*(rhosed-rho)*g))
         ,OUT_LDcbf_fin=log10(OUT_Dcbf_fin), OUT_LRBS_fin=OUT_LSUB_DMM-OUT_LDcbf_fin)%>%
  select(SampleID,StationID,Date,IN_BR_PCT,IN_HP_PCT,IN_RC_PCT,IN_BL_PCT,IN_CB_PCT,IN_GC_PCT,IN_GF_PCT
         ,IN_SA_PCT,IN_FN_PCT,IN_WD_PCT,IN_OT_PCT,IN_BL_CB_GR_PCT,IN_SA_FN_PCT,IN_TotSubstrate_PCT
         ,IN_LSUB_DMM,IN_SUB_DMM,OUT_BR_PCT,OUT_HP_PCT,OUT_RC_PCT,OUT_BL_PCT,OUT_CB_PCT,OUT_GC_PCT
         ,OUT_GF_PCT,OUT_SA_PCT,OUT_FN_PCT,OUT_WD_PCT,OUT_OT_PCT,OUT_BL_CB_GR_PCT,OUT_SA_FN_PCT
         ,OUT_TotSubstrate_PCT,OUT_LSUB_DMM,OUT_SUB_DMM,IN_Xembed,IN_Vembed,OUT_Xembed,OUT_Vembed
         ,IN_LRBS1,IN_LRBS2,IN_LRBS_fin,OUT_LRBS1,OUT_LRBS2,OUT_LRBS_fin)
rm(bankfull);rm(embed);rm(IN_embed);rm(incision);rm(INOUTsubstrate_summary.alt);rm(INOUTsummary);rm(OUT_embed)
rm(reach);rm(substrate);rm(substrate_summary.alt);rm(summary);rm(thalweg);rm(thalweg_summary.alt)
rm(wet);rm(wood);rm(wood_summary.alt);rm(wood_summaryINOUT);



# Change Date format and sort dataframe by SampleID for final output
TMDLSummary$Date <- as.character(TMDLSummary$Date)
TMDLSummary <- TMDLSummary[order(TMDLSummary$SampleID),]

MasterSummary$Date <- as.character(MasterSummary$Date)
MasterSummary <- MasterSummary[order(MasterSummary$SampleID),]

INOUT_MasterSummary$Date <- as.character(INOUT_MasterSummary$Date)
INOUT_MasterSummary <- INOUT_MasterSummary[order(INOUT_MasterSummary$SampleID),]

QAsummary$Date <- as.character(QAsummary$Date)
QAsummary <- QAsummary[order(QAsummary$SampleID),]

# Output summary data frame to CSV
write.csv(TMDLSummary, file=paste("TMDLsummary_",Sys.Date(),".csv",sep=""), row.names=FALSE)
write.csv(MasterSummary, file=paste("MasterSummary_",Sys.Date(),".csv",sep=""), row.names=FALSE)
write.csv(INOUT_MasterSummary, file=paste("INOUT_MasterSummary_",Sys.Date(),".csv",sep=""), row.names=FALSE)
write.csv(QAsummary, file=paste("QAsummary_",Sys.Date(),".csv",sep=""),row.names=FALSE)


# Output summary data frame to Access
# First ensure all previous table versions (or tables with identical names) are deleted of Access database
sqlSave(channel, TMDLSummary, tablename=paste("TMDLsummary_",Sys.Date(),sep=""), append=FALSE, rownames=FALSE, colnames=FALSE)
sqlSave(channel, MasterSummary, tablename=paste("MasterSummary_",Sys.Date(),sep=""), append=FALSE, rownames=FALSE, colnames=FALSE)
sqlSave(channel, INOUT_MasterSummary, tablename=paste("INOUT_MasterSummary_",Sys.Date(),sep=""), append=FALSE, rownames=FALSE, colnames=FALSE)
sqlSave(channel, QAsummary, tablename=paste("QAsummary_",Sys.Date(),sep=""), append=FALSE, rownames=FALSE, colnames=FALSE)
odbcClose(channel)
