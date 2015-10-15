#####Settings#####
#where is the data?
path<-"fakeData/"
#where should the final plots go?
outputPath<-"output/"
#where does R keep it's libraries, "" should be fine for most systems
libPath<-""
#which data files to select (regular expressions ok)
search<-"br[0-9][^.]+[0-9]\\.txt"
#Max count if not logged else NULL

if (libPath!=""){
.libPaths(libPath)
}
if (!require('date')|!require('hexbin')|!require('maps')){
	stop(simpleError("This program requires data, hexbin and maps library"))
}

if (file.exists('functions.R')){
	source('functions.R')
}else{
	stop(simpleError("Please provide the file functions.R. (It should be included in zip file.)"))
}
homepath<-getwd()

#read the data output from Douglas filter
douglasData<-readDouglas(path,search,ignoreErrors=FALSE,assumeJuv=TRUE,extraColumns=c('sex','stage','nester'))

#backup all the data
allDouglasData<-douglasData

limits<-hexPlot(allDouglasData,paste(outputPath,'all.ps',sep=""),hexPerDegree=4,logCount=TRUE)
#get the maximum hex count
hexMax<-limits[5]
#to zoom in on a certain portion
zoomLimits<-c(-100,-65,25,45)

plotSeason(allDouglasData,outputPath,'all',limits,zoomLimits,hexMax=hexMax,logCount=TRUE)
hexPlot(allDouglasData,paste(outputPath,'allzoom.ps',sep=""),limits=zoomLimits,hexPerDegree=4,hardLimit=zoomLimits,hexMax=hexMax,logCount=TRUE)

#Can filter the data for various attributes. For example:
douglasData<-allDouglasData[allDouglasData$file=="XX_sample_br9d9lc9.txt",]
hexPlot(douglasData,paste(outputPath,'XX.ps',sep=""),limits=limits,hexPerDegree=4,hexMax=hexMax,logCount=TRUE)
plotSeason(douglasData,outputPath,'XX',limits,zoomLimits,hexMax=hexMax,logCount=TRUE)


douglasData<-allDouglasData[allDouglasData$file=="YY_sample_br9d9lc9.txt",]
hexPlot(douglasData,paste(outputPath,'YY.ps',sep=""),limits=limits,hexPerDegree=4,hexMax=hexMax,logCount=TRUE)
plotSeason(douglasData,outputPath,'YY',limits,zoomLimits,hexMax=hexMax,logCount=TRUE)
