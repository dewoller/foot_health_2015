#!/usr/bin/R


library(plyr)
################################################################################
# find a block >= than frame length that has activity higher than activity level
# add a column into dataset with newColName containing a b where this block is true
# it makes no sense to have 10 minute blocks of > 0, becasue everything is >0

`activeBlock` <-
  function(dataset, 
           activityLevelBottom=0
           ,
           frame = 10
           ,
           cts = "counts"
           , 
           allowanceFrame= 2
           , 
           newColNameIsBlock = "isBlock"
           , 
           newColNameChunkID = "chunkID"
           )
  {
  
    # get the count vector; the activity counts
    ct = as.vector(dataset[,names(dataset) == cts])
        
    #all the NA's in the original counts data will be treated as 0 counts
    ct[is.na(ct)] = 0
    
    # length of datase
    size = dim(dataset)[1]
    
    # default to not block
    isBlock = rep(FALSE, size)
    chunkID = rep(0, size)
    
    #ct1_bool = (ct >= activityLevelBottom & ct <= activityLevelTop)
    #chunks0=findContigiousChunks( !ct1_bool )
  
    # chunks1 are not blocks if they are less than frame long
    # chunks0 are chunk1 if they are less than allowanceFrame long
    
    # for each recording
    startPos=-1
    endPos=-1
    chunkStart=c()
    chunkEnd=c()
    inChunk=FALSE
    lastGoodReading=-1

    # for every record on file
    for(i in 1:length(ct)) {
  
      if (ct[i] >= activityLevelBottom) {
        # a valid record
        if (!inChunk) {
          lowReadings=0   # reading that are too low, but within the chunk
          startPos=i
          inChunk=TRUE
        }  
        lastGoodReading=i
      } else {    

        # it is a bad reading
        if(inChunk) {
          if (lowReadings<allowanceFrame) {
            
            # we have a low reading, but it is within the allowable frame
            lowReadings = lowReadings+1
          } else {
            
            # we are at the end of the chunk
            endPos=i-1
            if (endPos-startPos+1 >= frame) {
              
              # the chunk is long enough, store it away
              chunkStart=c(chunkStart, startPos)
              chunkEnd=c(chunkEnd, endPos)
            }
            inChunk=FALSE
            lowReadings=0
          }
        } # if inchunk
      } # if activity level
    } # for r
    #
    
    if(length(chunkEnd)>0) {  
      for(w in 1: length(chunkEnd)){
        isBlock[chunkStart[w]:chunkEnd[w]] = TRUE 
        chunkID[chunkStart[w]:chunkEnd[w]] = w
      }
    }
  
    oldnames = names(dataset)
    rst = cbind(dataset, block = isBlock, blockID=chunkID)
    names(rst) = c(oldnames, newColNameIsBlock, newColNameChunkID)
    return(rst)
  }

############################################################################
############################################################################
############################################################################
############################################################################



dataCollapser= function (dataset, TS="TimeStamp", by=60, col="counts") 
{
  # dataset - input dataset, with columns TS and col
  # by = # of desired seconds between 
  ts = dataset[, TS]
  ct = as.numeric(dataset[, col])
  timeRange = range(as.vector(ts))
  # the current amount of time between measurements
  epoch = as.integer(difftime(as.POSIXlt(ts[2]), as.POSIXlt(ts[1]), units="secs"))
  # the number of input records that make 1 output record
  ratio = by/epoch
  # a vector of end points to translate from input to output
  newrange = c(0:(ceiling(length(ts)/ratio) - 1)) * by
  # create a base time
  step1 = as.POSIXlt(timeRange[1], origin="1970-01-01 00:00.00 UTC")
  # create destination time vector
  newts = newrange + step1
  newct = rep(NA, length(newrange))
  i = 1
  prevts = while (i <= length(newts)) {
    start = (i - 1) * ratio + 1
    end = i * ratio
    if (end > length(ct)) {
      end = length(ct)
    }
    newct[i] = sum(ct[start:end])
    i = i + 1
  }
  tf = data.frame(timestamp = newts, counts = newct)
  names(tf) = c(TS, col)
  return(tf)
}


############################################################################
############################################################################
############################################################################
############################################################################

`findContigiousChunks` <-
  function(ct_bool)
  {
    rowPos = nthOccurance (dataVct = ct_bool, value= TRUE)
    if (length(rowPos) <= 1) { return(NULL)}
    #get contigious inactivity section start and end positions
    startPos = rowPos[1]
    endPos = c()
    
    # traverse each moment of inactivity, discard contigious inactivity
    # store away endpoints of inactivity only
    for(q  in 2: (length(rowPos)))
    {
      if( rowPos[q] - rowPos[q-1]>1 )
      {
        # if there is an offending (ie 0) value between the current pos and the last pos
        # then this movement is not contigious
        # store away endpos, start new start chunk
        endPos = c(endPos, rowPos[q-1])
        startPos = c(startPos, rowPos[q])
      }
    }
    endPos = c(endPos, rowPos[q])
    
    
    #ele3 should be handled here on startPos/endpos level
    
    # what is duration for each windows of inactivity
    allowancewin = endPos-startPos+1
    return (data.frame( cbind(startPos, endPos, allowancewin)))
  }

################################################################################
################################################################################
markDays = function (dataset
,
 timestamp = "TimeStamp"
,
 startTime = "00:00:00"
,
 endTime = "23:59:59"
) 
{
  if (is.numeric(timestamp)) {
    cadval = dataset[, timestamp]
  } else {
    cadval = dataset[, c(names(dataset) == timestamp)]
  }

  daystart = as.POSIXlt(
    format(cadval[10], "%Y-%m-%d 00:00:00")
    , format="%Y-%m-%d %H:%M:%S")

   dayMarking = as.numeric(floor(difftime(cadval, daystart, units="days"))+1)
  
  temp = cbind(dataset, day = dayMarking)
  return(temp)
  
}
# we need to find if chunks1 are inactive enough to be called inactive
# go through chunks1;  for each segment in chunks 1, look at past 60 minutes
# of 

# for each window of inactivity, find out if this is a real window of inactivity, 
# and therefore, notwearing, 
# Algorithm;  eliminate the windows of inactivity that fulfill the criteria for wearing
# that is, less than 3 moments of possible activity in the past 60 minutes
# traverse each window of inactivity, counting possible activity

# we have 2 different possibilities;  get all the windows of activity, 
# or get all the windows of inactivity   

# there are 3 different windows;  inactive, possibly active, or definitely active
# possibly active becomes active if there is >3 counts in the period
# possibly active become inactive if it is quiet for 60 minutes beforehand

# break it into chunks;  inactive, possactive
# go through possactive chunks, see if there was either
# 1) activity up to 60 minutes beforehand
# or 2) >3 possactive chunks in prev 60 minutes
# if yes, eliminate, that is, convert to active
# go through remaining chunks, merge adjoining chunks
# eliminate all chunks <60 length


################################################################################
`markWearingNora` <-
  
  function(dataset, 
           frame = 60
           , 
           cts = "counts"
           , 
           allowanceFrame= 3
           , 
           newColName = "isWearing"
           )
  {
    
    # get the count vector
    ct = as.vector(dataset[,names(dataset) == cts])
        
    #all the NA's in the original counts data will be treated as 0 counts
    ct[is.na(ct)] = 0
    
    # length of dataset
    size = dim(dataset)[1]
    
    # default to not wearing
    wearing = rep(TRUE, size)
    
    #find the minutes which are possibly inactive 
    # as opposed to definitely inactive or definitely active
    # get rid of the 'maybe' case first
    notActiveLimit=10
    activeLimit=100
    chunks1=findContigiousChunks( (ct <activeLimit) & (ct>=notActiveLimit) )

  # chunks0 are inactive if they are 60 minutes long
    # chunks1 are chunk0 if they are less than 3 long
    
    #ct0_bool = vector of nodes definitely inactive
    ct0_bool = ct < notActiveLimit

    # for each possibly inactive chunks, if it is short enough 
    # mark it as inactive
    for(r in 1:length(chunks1$allowancewin))
    {
      #   If short enough
     #     make this chunk inactive in ct0_bool
      if (chunks1$allowancewin[r] <= allowanceFrame) {
        ct0_bool[ chunks1$startPos[r]:chunks1$endPos[r]] = TRUE
      }
    }

    # find contigious periods of inactivity, both real and theorized
    # get rid of all the chunks that are too short
    # chunks in chunks0
    chunks0=findContigiousChunks(ct0_bool)
    # for each chunks0
    for(r in 1:length(chunks0$allowancewin))
    {
      # mark as inactive if long enough
      #   delete chunk if length(chunk) < 60
      if(chunks0$allowancewin[r] < frame) {
        chunks0$startPos[r] = -1
      }
    }
    #
    # find active chunks, the inverse of chunks1
    chunks0 = chunks0[ chunks0$startPos>0, ]
    
    for(w in 1: length(chunks0$endPos)){
      wearing[chunks0$startPos[w]:chunks0$endPos[w]] = FALSE 
    }
    
    oldnames = names(dataset)
    rst = cbind(dataset, wearing = wearing)
    names(rst) = c(oldnames, newColName)
    return(rst)
  }







################################################################################
`OLDmarking` <-  # changed from marking to make sure no one is using this old code
  function(dataset, 
           frame=60, 
           cts = "counts", 
           streamFrame = 90, 
           allowanceFrame= 2, 
           newColName = "wearing",
           zeroLevel = 10)
    {
    
    # get the count vector
    ct = as.vector(dataset[,names(dataset) == cts])
    
    if(is.null(streamFrame)){
      streamFrame = round(0.5*frame)
    }
    
    #all the NA's in the original counts data will be treated as 0 counts
    ct1 = ct
    ct[is.na(ct)] = 0
    
    # length of dataset
    size = dim(dataset)[1]
    
    # default to not wearing
    wearing = rep("nw", size)
    
    #find the minutes which have counts
    ct_bool = ct > zeroLevel 
    
    # find me ALL the indexes in dataset where there is a value
    rowPos = nthOccurance (dataVct = ct_bool, value= TRUE)
    
    #getting section start and end positions
    startpos = rowPos[1]
    endpos = c()
    
    # traverse all the existing values, finding contigious chunks where there are values
    # store values in opposing values in startpos, endpos
    for(q  in 2: (length(rowPos)))
    {
      if( rowPos[q] - rowPos[q-1]>1 )
      {
        # if there is an offending (ie 0) value between the current pos and the last pos
        # then this movement is not contigious
        # store away endpos, start new start chunk
        endpos = c(endpos, rowPos[q-1])
        startpos = c(startpos, rowPos[q])
      }
    }
    endpos = c(endpos, rowPos[q])
    
    
    # what is duration for each windows of activity
    allowancewin = endpos-startpos
    
    # for each window of activity, find out if this is a real window of activity, 
    # and therefore, wearing, or is it an artifact, and therefore, nonwearing
    # Algorithm;  eliminate the windows of activity that fulfill the criteria for not wearing
    # that is, long enough, and also, no activity on BOTH sides, streamframe distance, of the activity 
    for(r in 1:length(allowancewin))
    {
      # if this chunk was smaller in size that the allowable size
      if(allowancewin[r] < allowanceFrame)
      {
        #upstream - is there activity prior to this allowable frame?
        usStart = startpos[r] - streamFrame
        usEnd = startpos[r] - 1
        if(usStart <=0) {usStart = 1}
        if(usEnd <= 0)  {usStart = 1}
        
        # if we are at the start, we were obvisiously non wearing
        if(usEnd-usStart == 0){
          usSignal = "nowearing"
        }else {
          
          # if there is at least one movement value in the upstream window
          if(sum(ct_bool[usStart:usEnd]) >0){
            usSignal = "wearing"
          }else {
            usSignal = "nowearing"    
          }
        }
        
        #downstream
        # is there activity after to this allowable frame?
        
        dsEnd = endpos[r] + streamFrame
        dsStart = endpos[r] + 1
        if(dsEnd >size)
        {dsEnd = size}
        if(dsStart > size)
        {dsStart = size}
        if(dsEnd-dsStart == 0){
          dsSignal = "nowearing"
        }else {
          if(sum(ct_bool[dsStart:dsEnd]) >0){
            dsSignal = "wearing"
          }else {
            dsSignal = "nowearing"    
          }
        }  
        
        if(usSignal == "nowearing" & dsSignal == "nowearing")
          # we had movement on BOTH sides of the frame, so, this allowable movement window
          # must have been real movement.  Throw it away
        {
          startpos[r] = -1
          endpos[r] = -1
        }      
      }#end of if/allowancewin
    }#end of for/r
    
    # get rid of all the segments disallowed above
    startpos = startpos[startpos != -1]
    endpos = endpos[endpos!=-1]
    #end of ele3
    
    #now get the non-wearing gap
    #frame is the gap allowed between time section.  ie if 90 minutes allowed
    #between two wearing sections, them frame = 90
    gap = startpos[-1] - endpos[1:length(endpos)-1]
    endgap = endpos[1:length(gap)]
    startgap = startpos[-1]
    endgap[gap<= frame] = NA
    startgap [gap <= frame] = NA
    startgap = c(startpos[1], startgap)
    endgap = c(endgap, endpos[length(gap)+1])
    
    newstartpos = startgap[!is.na(startgap)]
    newendpos = endgap[!is.na(endgap)]
    
    for(w in 1: length(newendpos)){
      wearing[newstartpos[w]:newendpos[w]] = "w"
    }
    
    tlen= length(wearing)
    wearing[tlen] = wearing[tlen-1]
    
    wearing[is.na(ct1)] = NA
    
    oldnames = names(dataset)
    rst = cbind(dataset, wearing = wearing)
    names(rst) = c(oldnames, newColName)
    return(rst)
  }
################################################################################
`nthOccurance` <-
  function(dataVct, value, nth = NA, reverse = FALSE)
  {
    # function to return the indexes of the nth value(s) from dataVct
    # reverse = TRUE, look from left side
    loc = c()
    if(reverse){
      dataVct = rev(dataVct)
    }
    
    #replace NA values in dataVct with string values NA
    if(is.na(value)){
      value = "NA"
      dataVct[is.na(dataVct)] = "NA"
    }
    
    # temp - indexes into dataVct
    temp = 1:length(dataVct)
    if(length(nth)==1){
      # look for a single value
      if( is.na(nth)){
        loc = temp[match(dataVct, value, nomatch = 0)==1]
      }else{
        loc = temp[match(dataVct, value, nomatch = 0)==1][nth]
      }
    }else{
      loc = temp[match(dataVct, value, nomatch = 0)==1][nth]
    }
    
    if(reverse){ 
      # reverse the found locations
      loc = length(dataVct) - loc +1
    }
    
    if(sum(is.na(loc)) == length(loc)){
      loc = 0
    }
    
    return(loc)
  }

################################################################################
################################################################################
################################################################################
plotData = function (data, day = NULL, start = 1, end = NULL) 
{
  findMidnight <- function(data) {
    n <- length(data[, 1])
    mm <- 0
    for (i in 1:(n - 1)) {
      mm <- c(mm, ifelse(data$days[i] == data$days[i +  1], 0, 1))
    }
    
    data.midnight <- data[mm == 1, ]
    first.rowname <- as.numeric(row.names(data[1, ]))
    midnightStart <- as.numeric(row.names(data.midnight)) - 
      first.rowname + 1
    return(midnightStart)
  }
  if (is.null(end) == 1) {
    end <- length(data[, 1])
  }
  if (is.null(day) != 1) {
    dd <- data[data$days == day, ]
    midnightMark <- findMidnight(dd)
  }
  else {
    dd <- data[start:end, ]
    midnightMark <- findMidnight(dd)
  }
  plot(dd$counts, type = "l", xlab = "Time", ylab = "Counts")
  abline(v = midnightMark, lty = 2, lwd = 1.5, col = 4)
  text(midnightMark, 0, pos = 1, "0 AM", cex = 0.8, col = 4)
}


############################################################################
############################################################################
############################################################################
############################################################################


################################################################################
################################################################################
readCountsDataRT3 = function (filename="", 
                              dateFormat="%d/%m/%Y"
                              ) 
{
  Tfile <- file(filename, "r")
  if (isOpen(Tfile, "r")) {
    seek(Tfile, 0, rw = "r")
    lines = readLines(Tfile)
    close(Tfile)
  }
  skipPos = grep("Entry", lines)[1]
  startTPos = grep("Start Time", lines)
  startTime = gsub("Start Time,", "", lines[startTPos])
  startTime = gsub(",", " ", startTime)
  startline = skipPos + 1
  endline = length(lines)
  rawdata = c()
  rst = c()
  pb <- txtProgressBar(max=endline)
  rvct=vector(mode="integer", length=(endline-startline+1))
  rvdt=vector(mode="character", length=(endline-startline+1))
  firstDatePart=vector(mode="integer", length=(endline-startline+1))
  secondDatePart=vector(mode="integer", length=(endline-startline+1))
  for (i in startline:endline) {
    pos = i - startline + 1
    setTxtProgressBar(pb, i)
    line = gsub('"','',strsplit(lines[i], ",") [[1]])
    if (length( line ) == 8 ) {
      rvct[ pos ] = as.integer(line[[6]])
      ts = stringr::str_replace_all( line[[2]], "-", "/" )
      parts = strsplit( ts, "/")[[1]]
      firstDatePart[ pos ] = parts[1]
      secondDatePart[ pos ] = parts[2]
      time=strsplit(line[[3]], ":")
      ts = paste(ts, paste(sprintf(fmt="%02d", as.integer(time[[1]])), collapse=":"), sep=" " )
      rvdt[ pos ] = ts 
    }
  }
  close(pb)
  cat("done\n")
  #timeline = rep(0:as.integer(length(rawdata)-1))*60
  #rst = timeline + as.POSIXlt(startTime, tz="", "%m/%d/%Y %H:%M:%s")
  if (length(unique( firstDatePart)) < length(unique( secondDatePart)))  {
                              dateFormat="%m/%d/%Y"
  } else {
                              dateFormat="%d/%m/%Y"
  }
    
  dtFormat = paste(dateFormat, "%H:%M:%S")
  
  if( is.na(as.POSIXlt(rvdt,format=dtFormat)[1])) {
    cat("\n")
    cat(filename)
    cat("\n")
    cat("ERROR")
    cat("\n")
    cat(dtFormat)
    cat("\n")
    cat(rvdt[1])
    cat("\n")
    stop("Error")
    
  }
  # create a data frame with 2 columns, TimStamp and counts 
  data.frame(TimeStamp = as.POSIXlt(rvdt,format=dtFormat)
                                    , counts = rvct)
}


################################################################################
################################################################################
wearingMarking = function (dataset, frame = 90, perMinuteCts = 60, TS = "TimeStamp", 
                           cts = "counts", streamFrame = NULL, allowanceFrame = 2, newColName = "wearing", 
                           getMinuteMarking = FALSE, dayStart = "00:00:00", dayEnd = "23:59:59", 
                           ...) 
{
  if (perMinuteCts != 1) {
    data2 = dataCollapser(dataset, TS = TS, by = 60, col = cts)
  }
  else {
    data2 = dataset
  }
  
  data3 = marking(data2, frame = frame, cts = cts, streamFrame = streamFrame, 
                  allowanceFrame = allowanceFrame, newColName = newColName)
  
  
  colName = names(data3)
  if (!getMinuteMarking) {
    dataset$key = substring(dataset[, names(dataset)[TS == 
                                                       names(dataset)]], 1, 16)
    data3$key = substring(data3[, names(data3)[TS == names(data3)]], 
                          1, 16)
    data4 = merge(dataset, data3[c(newColName, "key")], all.x = TRUE, 
                  by = "key")[c(colName)]
  }
  else {
    data4 = data3[c(colName)]
  }
  data4$weekday = weekdays(data4[, TS])
  markDays(data4, TS, dayStart, dayEnd)
}




# process read in a single RT3 file
# Compliant if XXX
# add in columns isWearing, isCompliant, and day
`markCompliant` <- function(csv, filename,
                            minuteThreshold=600 # number of minutes for a day to be compliant
                            ,
                            frame=60
                            , 
                            allowanceFrame=3  # number of minutes allowed
                            ,
                            dayThreshold=4
                            ) {
  # path - the path to the file to process
  row=list()
  row$filename=filename
  #csv=readCountsDataRT3(fileName)
  csv=markDays(csv)
  csv$isWeekEnd=(format(csv$TimeStamp, "%u")>=6)
  
  row$FirstReading=format(min(csv$TimeStamp), "%d/%m/%y")
  row$LastReading=format(max(csv$TimeStamp), "%d/%m/%y")
  row$DaysOnRecord=max(csv$day)
  row$MinutesOnRecord=length(csv[,1])
  row$TotalCompliantDays=0
  row$CompliantWeekendDays  = 0
  row$NumberActiveMinutes =  0
  row$totalActiveVM =  0
  row$AverageVM =  0
  row$csv = csv
  if (length(csv[csv$counts<10,1])==0) {
    cat(paste("ERROR:", fileName, "\n"))
    row$isCompliant	= FALSE
    stop
    return(row)
  }
  csv=markWearingNora(csv, frame=frame, allowanceFrame=allowanceFrame)
  wcsv = csv[csv$isWearing & csv$day>1,] # wearing values only and > first day
  
  # # of wearing minutes / day
  dayTotals=ddply(wcsv, ~day, summarise, totCount=length(day), isWeekEnd=min(isWeekEnd))
  compliantDays=dayTotals[dayTotals$totCount>=minuteThreshold,]$day
  csv$isCompliant=csv$day %in% compliantDays
  
  
  
  row$TotalCompliantDays=sum( dayTotals$totCount>=minuteThreshold )
  row$CompliantWeekendDays  = sum( dayTotals$totCount>=minuteThreshold & dayTotals$isWeekEnd)
  if( ( row$TotalCompliantDays >=dayThreshold & row$CompliantWeekendDays >0)) {
    row$isCompliant  = TRUE
  } else {
    row$isCompliant	= FALSE
  }
  row$NumberActiveMinutes =  sum( csv$isWearing & csv$isCompliant)
  row$totalActiveVM =	sum(csv[ csv$isWearing & csv$isCompliant, ]$counts)
  row$AverageVM =  row$totalActiveVM / row$NumberActiveMinutes
  row$csv = csv
  return (row)
}


