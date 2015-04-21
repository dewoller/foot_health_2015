library(plyr)
################################################################################
processDirectory= function( 
			   path = "data/"
			   , 
			   pattern = '*.csv'
			   , 
			   searchPattern="^([A-Z]+)_.*"
			   ) {
    files =data.frame(list.files( path, pattern));
    names(files)=c("filename");
    files$base = gsub(searchPattern, "\\1", files$filename);
    files$fullPath= paste(path, files$filename, sep="");
  processFiles( files);
}
##############################################################################
processFilesFromCSV = function(basePath="~/mydoc/research/noraShields/carlon/",
                               file="temp.csv" 
) {
  # gather all the files
  
  files=read.csv(paste(basePath, file, sep=""),stringsAsFactors=FALSE)
  files$base=gsub("^DS_PRT_trial_(.*)_files_for_Dennis$","\\1",files$base,perl=TRUE)
  files$fullPath=paste(basePath, files$filename, sep="")
  processFiles(files)
}
############################################################################
processFiles= function( files ) {
  rv=list()
  
  sBouts=c(5,10,20)
  eBouts=c(9,19,9999999999)
  fileLevelSummaries=list()
  dayDetails=data.frame()
  for(i in 1:length(files[,1])){
    #browser()
    
    fileLevelSummary = processOneFile( files[i,] )
    fileLevelSummary=c(fileLevelSummary, files[i,])
    fileLevelSummaries=rbind(fileLevelSummaries, fileLevelSummary)
    if(!fileLevelSummary$isCompliant)
      next  # ignore it, skip to next data set
    
    #____________________________________________________________________________
    baseDataset=subset(fileLevelSummary$csv, subset=fileLevelSummary$csv$isCompliant)
    dayDetail = processFileDays( baseDataset, person=files[i,]$chunk, periodType=files[i,]$base, sBouts=sBouts, eBouts=eBouts)
    dayDetails=rbind(dayDetails, dayDetail)

    
    #_
  }
  rv$fileLevelSummaries=fileLevelSummaries
  rv$dayDetails=dayDetails
  rv$files=files
  rv
}

#############################################################################

#############################################################################
`processOneFile` = function( file) {
  
  print(paste("processing row",file$fullPath))
  
  # process this file;  first, see if it is compliant
  fileLevelSummary= markCompliant(file$fullPath,
                                  minuteThreshold=9*60 # number of minutes for a day to be compliant
                                  ,
                                  frame=60
                                  , 
                                  allowanceFrame=2  # number of minutes allowed
                                  ,
                                  dayThreshold=4
                                  ,
                                  weekendDayThreshold=0
  )               
  
  fileLevelSummary$filename=file$filename
  fileLevelSummary = within( fileLevelSummary, {
    NumberActiveMinutes =  sum( csv$isCompliant & csv$isWearing )
    totalActiveVM =  sum(csv[ csv$isCompliant & csv$isWearing, ]$counts)
    AverageVM =  totalActiveVM / NumberActiveMinutes
  })
  fileLevelSummary  
}

#############################################################################
`processBoutByDay` = function( baseDataset, person, periodType, sBout, eBout=99999999, level=40, label="") {

  dayDetails = data.frame()
  # find all active blocks for this threshold
  thisDataset=activeBlock(baseDataset, activityLevelBottom=level, 
                          frame=sBout, frameMax=eBout, cts="counts", 
                          allowanceFrame=2)
  
  # find the number of blocks for each day in the dataset
  chunks = unique(subset(thisDataset, 
                         select=c("day","chunkID"),
                         subset= thisDataset$isBlock & 
                                 thisDataset$isCompliant & 
                                 thisDataset$isWearing))
  dayDetail=ddply(chunks, .(day), summarise, N=length(day))
  
  # add in columns to identify these  day block counts, one record / day
  dayDetail=cbind(dayDetail, 
                  person,
                  paste("day-", sBout, "min",label,"-count", sep=""),
                  periodType
  )
  names(dayDetail)=c("day","count","person","measureType","periodType")
  # append these blocks to the accumulative daydetails
  dayDetails = rbind(dayDetails, dayDetail)
  
  
  # find the average count for this period
  dayDetail = ddply(subset(thisDataset, 
                           subset= thisDataset$isBlock & 
                                   thisDataset$isCompliant & 
                                   thisDataset$isWearing),
                    .(day), summarise, mean=mean(counts))
  
  # add in columns to identify these  day block counts, one record / day
  dayDetail=cbind(dayDetail, 
                  person,
                  paste("day-", sBout, "min",label,"-average", sep=""),
                  periodType
  )
  names(dayDetail)=c("day","count","person","measureType","periodType")
  # append these blocks to the accumulative daydetails
  dayDetails = rbind(dayDetails, dayDetail)
  dayDetails  
}

#############################################################################

#############################################################################
`processBoutByPerson` = function( baseDataset, person, periodType, sBout, eBout=99999999, level=40, label="") {
  
  dayDetails = data.frame()
  # find all active blocks for this threshold
  thisDataset=activeBlock(baseDataset, activityLevelBottom=level, 
                          frame=sBout, frameMax=eBout, cts="counts", 
                          allowanceFrame=2)
  
  # find the number of blocks for each day in the dataset
  chunks = unique(subset(thisDataset, 
                         select=c("day","chunkID"),
                         subset= thisDataset$isBlock & 
                           thisDataset$isCompliant & 
                           thisDataset$isWearing))
  dayDetail=ddply(chunks, .(), summarise, N=length(day))
  
  # add in columns to identify these  day block counts, one record / day
  dayDetail=cbind(dayDetail, 
                  person,
                  paste("personPeriod-", sBout, "min",label,"-count", sep=""),
                  periodType
  )
  names(dayDetail)=c("day","count","person","measureType","periodType")
  # append these blocks to the accumulative daydetails
  dayDetails = rbind(dayDetails, dayDetail)
  
  
  # find the average count for this period
  dayDetail = ddply(subset(thisDataset, 
                           subset= thisDataset$isBlock & 
                             thisDataset$isCompliant & 
                             thisDataset$isWearing),
                    .(), summarise, mean=mean(counts))
  
  # add in columns to identify these  day block counts, one record / day
  dayDetail=cbind(dayDetail, 
                  person,
                  paste("personPeriod-", sBout, "min",label,"-average", sep=""),
                  periodType
  )
  names(dayDetail)=c("day","count","person","measureType","periodType")
  # append these blocks to the accumulative daydetails
  dayDetails = rbind(dayDetails, dayDetail)
  dayDetails  
}


#############################################################################

#############################################################################
calculateActivityLevel = function(counts, isWearing) {
  brk = c(40,950)
  a=cbind(counts, isWearing)
  factor( 
    apply(a, 1,function( line ) { 
    ifelse(
      !line[2], 
      1, 
      2+length(which(line[1]>brk)))
  } )
    , 
   levels=seq(1,4), 
    labels= c("nowear", "light","sedentary", "vigorous")
  )
}


###########################################################
###########################################################
processFileDays = function( baseDataset, person, periodType, sBouts, eBouts)  {
  
  dayDetails=data.frame()
  # process 5, 10 and 20 minute bouts of any level activity
  for(j in 1: length(sBouts)){
    print(paste("Bout length", sBouts[j], "-", eBouts[j]))
    dayDetails=rbind(dayDetails,
                     processBoutByDay(baseDataset, sBout=sBouts[j], eBout=eBouts[j], level=40, person=person, periodType=periodType))
    dayDetails=rbind(dayDetails,
                     processBoutByPerson(baseDataset, sBout=sBouts[j], eBout=eBouts[j], level=40, person=person, periodType=periodType))
  }
  
  #____________________________________________________________________________
  # process 10 minute bouts of moderate-vigorous activity
  dayDetails=rbind(dayDetails,
                   processBoutByDay(baseDataset, sBout=10, label="-vigorous", level=950 , person=person, periodType=periodType))
  dayDetails=rbind(dayDetails,
                   processBoutByPerson(baseDataset, sBout=10, label="-vigorous", level=950 , person=person, periodType=periodType))

  # process overall Totals
  dayDetails=rbind(dayDetails,
                   processDayTotals(baseDataset, person=person, periodType=periodType))
  dayDetails=rbind(dayDetails,
                   processPersonPeriodTotals(baseDataset, person=person, periodType=periodType))

  dayDetails
}  

###########################################################
###########################################################
processDayTotals = function(baseDataset, person, periodType)   {
  dayDetails=data.frame()
  
  #____________________________________________________________________________
  # categorise all the minutes into overall activity levels
  baseDataset$activityLevel = calculateActivityLevel( baseDataset$counts, baseDataset$isWearing ) 
  
  dayDetail = ddply( baseDataset, .(day, activityLevel), summarise, count=length(activityLevel))
  # calc percents
  dayDetail1 = ddply( dayDetail, .(day), summarise, activityLevel=activityLevel, pct=count/sum(count))
  
  dayDetail=data.frame(
    cbind(dayDetail$day, dayDetail$count, 
          person,
          paste("day-", as.character(dayDetail$activityLevel), "-mins", sep=""),
          periodType
  ))
  
  names(dayDetail)=c("day","count","person","measureType","periodType")
  dayDetails=rbind(dayDetails, dayDetail)
  
  #____________________________________________________________________________
  # get percents
  dayDetail1=data.frame(
    cbind(dayDetail1$day, dayDetail1$pct, 
          person,
          paste("day-", as.character(dayDetail1$activityLevel), "-pct", sep=""),
          periodType
    )
  )
  names(dayDetail1)=c("day","count","person","measureType","periodType") 
  dayDetails=rbind(dayDetails, dayDetail1)
  #____________________________________________________________________________
  # get daily minutes wearing, and avg count while wearing, per day
  
  # capture the len and average
  dayDetail1 = ddply( 
    subset(baseDataset, subset=baseDataset$isWearing & baseDataset$isCompliant),
    .(day), summarise, dayMin=length(day), dayAvgCnt=mean(counts) )
  
  # create rows for the length
  dayDetail=data.frame(cbind(dayDetail1$day, dayDetail1$dayMin, 
                             person,
                             "day-wearingMins",
                             periodType
  )
  )
  names(dayDetail)=c("day","count","person","measureType","periodType") 
  dayDetails=rbind(dayDetails, dayDetail)
  
  # create rows for the average
  dayDetail=data.frame(cbind(dayDetail1$day, dayDetail1$dayAvgCnt, 
                             person,
                             "day-avg-cnt",
                             periodType
  )
  )
  names(dayDetail)=c("day","count","person","measureType","periodType") 
  dayDetails=rbind(dayDetails, dayDetail)
  dayDetails
} 


###########################################################
###########################################################
###########################################################
###########################################################
processPersonPeriodTotals= function(baseDataset, person, periodType)   {
  dayDetails=data.frame()
  
  #____________________________________________________________________________
  # categorise all the minutes into overall activity levels
  baseDataset$activityLevel = calculateActivityLevel( baseDataset$counts, baseDataset$isWearing ) 
  dayDetail = ddply( baseDataset, .(activityLevel), summarise, count=length(activityLevel))

  # calc percents from above
  dayDetail1 = ddply( dayDetail, .(), summarise, activityLevel=activityLevel, pct=count/sum(count))
  
  dayDetail=data.frame(
    cbind(NA, dayDetail$count, 
          person,
          paste("personPeriod-", as.character(dayDetail$activityLevel), "-mins", sep=""),
          periodType
    ))
  
  names(dayDetail)=c("day","count","person","measureType","periodType")
  dayDetails=rbind(dayDetails, dayDetail)
  
  #____________________________________________________________________________
  # store percents calculated above
  dayDetail1=data.frame(
    cbind(NA, dayDetail1$pct, 
          person,
          paste("personPeriod-", as.character(dayDetail1$activityLevel), "-pct", sep=""),
          periodType
    )
  )
  names(dayDetail1)=c("day","count","person","measureType","periodType") 
  dayDetails=rbind(dayDetails, dayDetail1)
  #____________________________________________________________________________
  # get daily minutes overall summaries, and avg count while wearing, per day
  
  # capture the len and average
  # create rows for the length
  wearingData = subset( baseDataset, subset=baseDataset$isWearing)
  dayDetail=data.frame(cbind(NA, dim(wearingData)[1], 
                             person,
                             "personPeriod-wearingMins",
                             periodType
  )
  )
  names(dayDetail)=c("day","count","person","measureType","periodType") 
  dayDetails=rbind(dayDetails, dayDetail)
  
  # create rows for the average
  dayDetail=data.frame(cbind(NA, mean(wearingData$counts),
                             person,
                             "personPeriod-avg-cnt",
                             periodType
  )
  )
  names(dayDetail)=c("day","count","person","measureType","periodType") 
  dayDetails=rbind(dayDetails, dayDetail)
  dayDetails
} 

#############################################################################

write.csv.Nora = function(rv, basename) {
  # write everything but the csv column
  write.csv( rv$fileLevelSummaries[,-9], paste(basename, "Summary.csv",sep=""))
  write.csv( rv$dayDetails, paste(basename, "Details.csv",sep=""))
}
  
#############################################################################
#############################################################################
