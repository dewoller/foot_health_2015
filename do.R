rm(list=ls())
library(plyr)
source("func.R")
################################################################################



instructions="

The issue with the way we summarised the data before is that we do not
calculate a per ourable for every eligible day's data to measure 
(1) the total amount of minutes the person spent in 
    moderate or vigorous level physical activity and 
(2) the amount of time spent in moderate or vigorous level physical activity 
    in bouts of at least 10 minutes.

So it's almost like we would have the above analysis for each eligible day of data ie 
The threshold was 275 (moderate level of physical activity or above)
Count of lines read  XXX (eligible time in a day when monitor was worn)
Count above threshold XXX 
  (total amount of at least moderate level physical activity 
    the participant did in that time)
Count above threshold in bouts of at least 10 minutes 
  (allowing for up to 2 minutes of readings below 275 in a bout)
Then we would re-run this for each eligible day of data collection 
  (between 4 and 7 readings for each participant- 
  we can discard the very first day of data collection as it is unlikely to be a 
  full days data anyway and was designed as a familiarisation day). 
  so for each participant we would have a line in an excel spreadsheet with 
  headings sometime like what I've attached to this email. 

When we have the data per day, 
we can then calculate the average per day 
but also the variability within daily activity (which I think will be high).

initial output columns:
#############################################################################
Participant  
Week (0/11/24)	
Compliant Day	
Threshold	
total or 10 minute bouts	
Value

#############################################################################
for each participant file
clean data
for each day
for each threshold
classify into 10 minute bouts
total value and 10 minute bouts

#############################################################################

"
#############################################################################
getFiles = function( 
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
    files
} 

processFilesNora = function(
#  basePath="/mnt/raid/home/dewoller/mydoc/research/noraShields/projects/foot_health_2015/data/",
  basePath="/tmp/a/"
  ,
  fileNameCSV=""
  ,
  defaultDateFormat="%m/%d/%Y"
  ) {
  rv=list()
  
  if (fileNameCSV == "") { 
    files = data.frame(list.files(basePath, "*.csv"))
    names(files) = c("filename")
  } else {
    files=read.csv(paste(basePath, fileNameCSV, sep=""),stringsAsFactors=FALSE)
  }
  files$base=files$filename
  files$chunk=files$filename
  files$fullPath=paste(basePath, files$filename, sep="")
  #thresholds=c(247,275,302,832,926,1018)
  thresholds=c(0, 52,1389, 2448)
  peopleRows=list()
  dayDetails=data.frame()
  for(i in 1:length(files[,1])){i
    cat(paste("processing row",i,files[i,]$fullPath, "\n"))
    # process this file
    df = defaultDateFormat
    if ("Reverse" %in% names(files)) {
      if( files[i,]$Reverse == "R") {
        df= "%m/%d/%Y" 
      } else {
        df="%d/%m/%Y"
      }
    }
    csv = readCountsDataRT3(files[i,]$fullPath , df )
    fileRow= markCompliant(csv, files[i,]$fullPath)
    fileRow$fileIndex=i  
    fileRow$filename=as.character( files[i,]$filename)
    peopleRows=rbind(peopleRows, fileRow)
    if(!fileRow$isCompliant)
      next
    baseDataset=fileRow$csv
    cat(paste("checking thresholds\n"))
    for(j in 1: length(thresholds)){
      cat(paste("Threshold", thresholds[j], "\n"))
      # find all active blocks
      dataset=activeBlock(baseDataset, activityLevelBottom=thresholds[j], frame=10, cts="counts", 
                          allowanceFrame=2, newColNameIsBlock="isBlock")
      blocksPerDay=ddply(dataset[dataset$isBlock & 
                                  dataset$isCompliant & 
                                   dataset$isWearing,], .(day), nrow)
      
      # check to make sure we got some data.  
      cat(paste( 'Checking for 10 minute data ', length(blocksPerDay[,1]), "\n"))
      if(length(blocksPerDay[,1]) > 0) {
        cat(paste( 'we have 10 minute data for ', length(blocksPerDay[,1]), "\n"))
        # add some more descriptive columns onto this
        blocksPerDay=cbind(blocksPerDay, 
                           files[i,]$chunk,
                           "dayTotalTenMinute",
                           files[i,]$base,
                           thresholds[j]
                           )
        names(blocksPerDay)=c("day","count","person","measureType","periodType", "threshold")
        dayDetails = rbind(dayDetails, blocksPerDay)
      }
      activePerDay=ddply(dataset[dataset$counts>=thresholds[j] & 
                                   dataset$isCompliant & 
                                   dataset$isWearing,], .(day), nrow)
      if(length(activePerDay[,1]) > 0) {
        activePerDay=cbind(activePerDay, 
                           files[i,]$chunk,
                           "dayTotal",
                           files[i,]$base,
                           thresholds[j]
                           )
        names(activePerDay)=c("day","count","person","measureType","periodType","threshold")
        dayDetails = rbind( dayDetails,  activePerDay)
      }
    }
  }
  rv$peopleRows=peopleRows
  rv$dayDetails=dayDetails
  rv$files=files
  write.csv.Nora(rv, basename="/tmp/output")
  return(rv)
}

#############################################################################
#############################################################################
#############################################################################

processFilesStaceyCarlon= function(
  basePath="~/mydoc/research/noraShields/carlon/"
  ,
  fileNameCSV="files2Process.csv"
  ) {
  rv=list()
  
  
  files=read.csv(paste(basePath, fileNameCSV, sep=""),stringsAsFactors=FALSE)
  files$base=gsub("^DS_PRT_trial_(.*)_files_for_Dennis$","\\1",files$base,perl=TRUE)
  files$fullPath=paste(basePath, files$filename, sep="")
  threshold_bottom=c(0,41,951)
  threshold_top=c(40,950,100000000)
  peopleRows=list()
  dayDetails=data.frame()
  for(i in 1:length(files[,1])){
    print(paste("processing row",i,files[i,]$fullPath))
    # process this file
    fileRow= markCompliant(files[i,]$fullPath,
             minuteThreshold=540 # number of minutes for a day to be compliant
                           ,
                           frame=60
                           , 
                           allowanceFrame=2  # number of minutes allowed
                           ,
                           dayThreshold=4
    )               
                           
    fileRow$fileIndex=i  
    fileRow$filename=files[i,]$filename
    peopleRows=rbind(peopleRows, fileRow)
    if(!fileRow$isCompliant)
      next
    baseDataset=fileRow$csv
    for(j in 1: length(thresholds)){
      cat(paste("Threshold", thresholds[j], "\n"))
      # find all active blocks
      thisDataset=activeBlock(baseDataset, activityLevelBottom=thresholds[j], frame=10, cts="counts", 
                          allowanceFrame=2, newColName="isBlock")
      blocksPerDay=ddply(thisDataset[thisDataset$isBlock & 
                                   thisDataset$isCompliant & 
                                  thisDataset$isWearing,], .(day), nrow)
      blocksPerDay=cbind(blocksPerDay, 
                         files[i,]$chunk,
                         "dayTotalTenMinute",
                         files[i,]$base,
                         thresholds[j]
                         )
      names(blocksPerDay)=c("day","count","person","measureType","periodType", "threshold")
      dayDetails = rbind(dayDetails, blocksPerDay)
      activePerDay=ddply(thisDataset[thisDataset$counts>=thresholds[j] & 
                                   thisDataset$isCompliant & 
                                   thisDataset$isWearing,], .(day), nrow)
      activePerDay=cbind(activePerDay, 
                         files[i,]$chunk,
                         "dayTotal",
                         files[i,]$base,
                         thresholds[j]
                         )
      names(activePerDay)=c("day","count","person","measureType","periodType","threshold")
        dayDetails = rbind( dayDetails,  activePerDay)
    }
  }
  rv$peopleRows=peopleRows
  rv$dayDetails=dayDetails
  rv$files=files
  return(rv)
}

#############################################################################

write.csv.Nora = function(rv, basename ) {
write.csv( rv$peopleRows[,which(!grepl("csv",colnames(rv$peopleRows)))], 
           paste(basename, "Summary.csv",sep=""))

  write.csv( rv$dayDetails, paste(basename, "Details.csv",sep=""))
}
  
#############################################################################
#############################################################################
