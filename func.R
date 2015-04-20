#!/usr/bin/R

library(plyr)
################################################################################
# find a block >= than frame length that has activity higher than activity level
# add a column into dataset with newColName containing a b where this block is true

`activeBlock` <-
  function(dataset, 
           activityLevelBottom,
           activityLevelTop=9999999999
           ,
           frame = 10
           ,
           frameMax = 99999999999 
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
    
    # default to not block, no chunk ID
    isBlock = rep(FALSE, size)
    chunkID = rep(0, size)
    
    
    # for each recording
    startPos=-1
    endPos=-1
    chunkStart=c()
    chunkEnd=c()
    inChunk=FALSE

    # for every record on file
    for(i in 1:length(ct)) {
  
      if (ct[i] >= activityLevelBottom) {
        # a valid record
        if (!inChunk) {
          lowReadings=0   # reading that are too low, but within the chunk
          startPos=i
          inChunk=TRUE
        }  
      } else {    

        # it is a bad reading
        if(inChunk) {
          if (lowReadings<allowanceFrame) {
            
            # we have a low reading, but it is within the allowable frame
            lowReadings = lowReadings+1
          } else {
            
            # we have gone past our limit, we are at the end of the chunk
            endPos=i-1
            chunkLength =  endPos-startPos+1 
            if ( chunkLength >= frame && chunkLength <= frameMax) {
              
              # the chunk is compliant, store it away
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
# findContigiousChunks` <-
# find contigious true values in ct_bool
# returns dataframe of startpos, endpos, and allowancewin (windowlength)

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
markDays = function (dataset, timestamp = "TimeStamp", startTime = "00:00:00", endTime = "23:59:59") 
{
  if (is.numeric(timestamp)) {
    cadval = dataset[, timestamp]
  }
  else {
    cadval = dataset[, c(names(dataset) == timestamp)]
  }
  daystart = as.POSIXlt(paste(substring(as.POSIXlt(cadval[1]), 
                                        1, 10), startTime))
  
  dayMarking = floor(as.integer(cadval-daystart)/24)+1
  
  return(dayMarking)
  
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
`markWearing` <-
  
  function(dataset, 
           frame = 60, 
           cts = "counts", 
           allowanceFrame= 3, 
           newColName = "isWearing")
  {
  
    # algorithm
    # find all the completely non wearing <10
    # find all the possibly non-wearing 10-100
    # for this latter set, if they are less than allowanceFrame long, mark them as completely non-wearing
    
    # get the count vector
    ct = as.vector(dataset[,names(dataset) == cts])
        
    #all the NA's in the original counts data will be treated as 0 counts
    ct[is.na(ct)] = 0
    
    # length of dataset
    size = dim(dataset)[1]
    
    # default to not wearing
    wearing = rep(TRUE, size)
    
    #find the minutes which are possibly inactive
    notActiveLimit=10
    activeLimit=100
    
    ct0_bool = ct < notActiveLimit
    chunks1=findContigiousChunks( (ct <activeLimit) & (ct>=notActiveLimit) )

    # chunks0 are inactive if they are 60 minutes long
    # chunks1 are chunk0 if they are less than 3 long
    
    # for each possibly active chunk, chunks1, put it 
    for(r in 1:length(chunks1$allowancewin))
    {
      #   If length < 3
      if(chunks1$allowancewin[r] <= allowanceFrame) {
        #cat(r, " ",  chunks1$startPos[r], " ",chunks1$endPos[r], "\n")
        ct0_bool[ chunks1$startPos[r]:chunks1$endPos[r]] = TRUE
      }
    }

    #     make this chunk inactive in ct0_bool
    # now that we have final set of non-wearing,  we can find the real non wearing chunks
    # that is, contigious chunks in chunks0
    chunks0=findContigiousChunks(ct0_bool)
    # for each chunks0
    for(r in 1:length(chunks0$allowancewin))
    {
      #   delete chunks that are too short, that is, if length(chunk) < frame length
      if(chunks0$allowancewin[r] < frame) {
        chunks0$startPos[r] = -1
      }
    }

    # get rid of all the chunks that are too short, that we eliminated above
    chunks0 = chunks0[ chunks0$startPos>0, ]
    
    # mark as non wearing as all the chunks found above
    for(w in 1: length(chunks0$endPos)){
      wearing[chunks0$startPos[w]:chunks0$endPos[w]] = FALSE 
    }
    
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
      mm <- c(mm, ifelse(data$days[i] == data$days[i + 
                                                     1], 0, 1))
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
readCountsDataRT3 = function (filename) 
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
  for (i in startline:endline) {
    rawdata = c(rawdata, strsplit(lines[i], ",") [[1]][6])
  }
  timeline = rep(0:as.integer(length(rawdata)-1))*60
  rst = timeline + as.POSIXlt(startTime, tz="", "%m/%d/%Y %H:%M:%s")
  data.frame(TimeStamp = rst, counts = as.numeric(as.vector(rawdata)))
}


################################################################################
################################################################################



# process read in a single RT3 file
# Compliant if XXX
# add in columns isWearing, isCompliant, and day
`markCompliant` <- function(fileName, 
                            minuteThreshold=600 # number of minutes for a day to be compliant
                            ,
                            frame=60
                            , 
                            allowanceFrame=3  # number of minutes allowed
                            ,
                            dayThreshold=4
                            ,
                            weekendDayThreshold =1
                            ) {

  csv=readCountsDataRT3(fileName)
  csv$day = markDays(csv)
  csv$isWeekEnd=(format(csv$TimeStamp, "%u")>=6)

  row=list()
  row$filename=fileName
  row$DaysOnRecord=max(csv$day)
  row$MinutesOnRecord=length(csv[,1])
  row$TotalCompliantDays=0 
  row$CompliantWeekendDays  = 0
  row$NumberActiveMinutes =  0
  row$totalActiveVM =  0
  row$AverageVM =  0
  row$csv = csv
  if (length(csv[csv$counts<10,1])==0) {
    print(paste("ERROR:", fileName))
    row$isCompliant	= FALSE
    return(row)
  }
  csv=markWearing(csv, frame=frame, allowanceFrame=allowanceFrame)
  
  # calculate # of wearing minutes / day and if this day a weekend
  dayTotals=ddply(
    subset( csv, subset=csv$isWearing & csv$day>1),
    .(day), summarise, totCount=length(day), isWeekEnd=min(isWeekEnd)
  )
  compliantDays=dayTotals[dayTotals$totCount>=minuteThreshold,]$day
  
  # mark all the minutes to say if they are a compliant day
  csv$isCompliant=csv$day %in% compliantDays

  # the number of compliant days, and compliant weekend days  
  row$TotalCompliantDays=sum( dayTotals$totCount>=minuteThreshold )
  row$CompliantWeekendDays  = sum( dayTotals$totCount>=minuteThreshold & dayTotals$isWeekEnd)
  
  # is this entire data file compliant?
  if( ( row$TotalCompliantDays >=dayThreshold & row$CompliantWeekendDays > weekendDayThreshold)) {
    row$isCompliant  = TRUE
  } else {
    row$isCompliant= FALSE
  }
  
  row$csv = csv
  return (row)
}


