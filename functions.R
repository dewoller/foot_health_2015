################################################################################
readCountsDataRT3 = function (filename) 
{
  Tfile <- file(filename, "r")
  if (isOpen(Tfile, "r")) {
    seek(Tfile, 0, rw = "r")
    lines = readLines(Tfile)
    close(Tfile)
  }
  skipPos = grep("Entry", lines[1:22])[1]
  startTPos = grep("Start Time", lines[1:22])
  startTime = gsub("Start Time,", "", lines[startTPos])
  startTime = gsub(",", " ", startTime)
  startline = skipPos + 1
  endline = length(lines)
  rawdata = c()
  for (i in startline:endline) {
    rawdata = c(rawdata, strsplit(lines[i], ",") [[1]][6])
  }
  timeline = rep(0:as.integer(length(rawdata)-1))*60
  rst = timeline + as.POSIXlt(startTime, tz="", "%m/%d/%Y %H:%M:%S")
rst = as.character(rst, "%Y-%m-%d %H:%M:%S")
  
  data.frame(TimeStamp = rst, counts = as.numeric(as.vector(rawdata)))
}
