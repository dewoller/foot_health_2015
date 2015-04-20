library("PhysicalActivity");
source("functions.R")
source("PhysicalActivity/R/nthOccurance.R")
source("/home/dewoller/mydoc/research/noraShields/students/carlon/stats/func.R")


a=readCountsDataRT3("test.csv")
markingCarlon=markWearing(a)
markingStandard=wearingMarking(a, 
                 perMinuteCts=1
                 )
b=wearingMarking(a, perMinuteCts=1)

dataset=a

