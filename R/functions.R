#' Functions to plot hexbins in GMT
#'
#' Produce a visual representation of position data with hexbins using the [General Mapping Tool](LINK)
#'
#' The main function is:
#'      \describe{
#'        \item{\code{\link{hexPlot}}:}{to produce a hexmap from a bunch of position data}
#'      }
#'
#' And main helper functions are:
#'      \describe{
#'        \item{\code{\link{readDouglas}}:}{to read data filtered by the Douglas filter}
#'      }
#'
#' @docType package
#' @name GMTHexBinR
#' @author Scott Sherrill-Mix, \email{shescott@@upenn.edu}
NULL


#' Read in Douglas filtered files
#'
#' Sources code in filter.R after reading in br files for custom filtering (see example file)
#' @param path System path to directory containing Douglas outputted files
#' @param search Regular expression to select files (defaults to br files)
#' @param ignoreErrors If true does not check for correct deployment information (lc94=DP)
#' @param extraColumns A vector of extra column names (present in the br files) that should also be returned (should be consistent over a day [e.g. deployday not time])
#' @return Data frame with median position for each day present in br file with columns c('animal', 'ptt', 'date', 'month', 'year', 'rdate', 'season', 'dir','file','deployday', extraColumns)
#' @import date
#' @export
readDouglas<-function(path=".",search="br[0-9][^.]+[0-9]\\.txt",ignoreErrors=FALSE,extraColumns=NULL){
	dayColumns<-c('animal','ptt','date','month','year',extraColumns)
	wantedColumns<-c('animal','ptt','date','latitude','longitud','month','year','thetime','lc94',extraColumns)
	#Read in br files
	files<-list.files(path,pattern=search,full.names=TRUE,recursive=TRUE)

	if (length(files)==0){
		stop(simpleError(paste("Please put some files matching ",search," in ",path,sep="")))
	}else message(paste('Found ',length(files),' br files:\n\t',paste(files,collapse=" "),sep=''))

	found<-FALSE
	for (i in 1:length(files)) {
		thisDir=dirname(files[i])
		thisFile=basename(files[i])
		thisBr<-read.table(files[i],header=TRUE,sep=",",as.is=TRUE)
		missingCol<-wantedColumns[!(wantedColumns %in% colnames(thisBr))]
		if(length(missingCol)>0){
			thisBr<-read.table(files[i],header=TRUE,sep="\t",as.is=TRUE)
			if(!all(wantedColumns %in% colnames(thisBr))){
				if(length(wantedColumns[!(wantedColumns %in% colnames(thisBr))])<length(missingCol)) missingCol<-wantedColumns[!(wantedColumns %in% colnames(thisBr))]
				warning(paste("The file ",thisFile," is missing the columns ",paste(missingCol,collapse=" "),sep=""))
				next()
			}
		}
		thisBr<-thisBr[,wantedColumns]
		thisBr$dir<-thisDir
		thisBr$file<-thisFile
		if (found) brs<-rbind(brs,thisBr)
		else{
			brs<-thisBr
			found<-TRUE
		}
	}

	#Remove any without lat or lon
	brs<-brs[!is.na(brs$latitude)&!is.na(brs$longitud),]

	#Check for ptt and deployment weirdness
	dpsPtts<-sort(brs[brs$lc94=="DP",'animal'])
	ptts<-sort(unique(brs$animal))
	if(!identical(dpsPtts,ptts)&!ignoreErrors){
		if (length(dpsPtts)>length(ptts)) stop(simpleError(paste("There appears to be more deployments (",length(dpsPtts),") than unique animals (",length(ptts),"). Please fix this.\n Missing animals: ",paste(dpsPtts[!dpsPtts%in%ptts],collapse=" "),sep="")))
		if (length(dpsPtts)<length(ptts)) stop(simpleError(paste("There appears to be more unique animals (",length(ptts),") than deployments (",length(dpsPtts),"). Please fix this.\n Missing deployments: ",paste(ptts[!ptts%in%dpsPtts],collapse=" "),sep="")))
		stop(simpleError(paste("Unique animal ids and animal ids from deployments do not match. Please fix this.",sep="")))
	}
	message(paste('Found ',length(ptts),' unique animals',sep=''))
	brs$rdate<-date::as.date(brs$date)
	dps<-brs[brs$lc=="DP",c('animal','rdate')]
	colnames(dps)<-c('animal','depdate')
	brs<-merge(brs,dps,all=TRUE)
	if((any(is.na(brs$animal))|any(is.na(brs$depdate)))&!ignoreErrors){
		stop(simpleError(paste("Deploy dates and animal ids did not match. This is weird.",sep="")))
	}

	brs<-brs[order(brs$animal,brs$rdate,brs$thetime,decreasing=FALSE),]
	brs$lagAnimal<-c(NA,brs$animal[1:(length(brs$animal)-1)])
	if (any(!is.na(brs$lagAnimal) & ((brs$lagAnimal!=brs$animal & brs$lc94!='DP') | (brs$lagAnimal==brs$animal & brs$lc94=="DP")))&!ignoreErrors){
		problemAnimals<-brs[!is.na(brs$lagAnimal) & ((brs$lagAnimal!=brs$animal & brs$lc94!='DP') | (brs$lagAnimal==brs$animal & brs$lc94=="DP")),'animal']
		stop(simpleError(paste(length(problemAnimals)," deployments not at start of track after sorting by animal, date and time (",paste(problemAnimals,collape=" "),"). Please fix this.",sep="")))
	}

	brs$deployday<-brs$rdate-brs$depdate

	brs$season<-NA
	brs[brs$month >= 1 & brs$month < 4,'season']<-1
	brs[brs$month >= 4 & brs$month < 7,'season']<-2
	brs[brs$month >= 7 & brs$month < 9, 'season']<-3
	brs[brs$month >= 10 & brs$month < 13, 'season']<-4

	brs$animalDate<-paste(brs$animal,"_",as.numeric(brs$rdate),sep="")
	avglon<-tapply(brs$longitud,brs$animalDate,median)
	avglat<-tapply(brs$latitude,brs$animalDate,median)
	daybrs<-unique(brs[,c(dayColumns,'rdate','season','animalDate','dir','file','deployday')])
	if(length(daybrs$animalDate)!=length(avglat)|length(daybrs$animalDate)!=length(avglon)){
		stop(simpleError(paste("Somehow the number of average lats or lons does not match the number of animals and days.",sep="")))
	}
	#Not actually necessary but better safe...
	daybrs<-daybrs[order(daybrs$animalDate),]
	avglon<-avglon[order(rownames(avglon))]
	avglat<-avglat[order(rownames(avglat))]
	if(!all(daybrs$animalDate==rownames(avglon))|!all(daybrs$animalDate==rownames(avglat))){
		stop(simpleError(paste("Somehow the number of avgerage lats or lons does not match the number of animals and days.",sep="")))
	}
	daybrs$lat<-avglat[rownames(daybrs)]
	daybrs$lon<-avglon[rownames(daybrs)]

	return(daybrs)
}


#' Generate hexmaps from position data
#'
#' Generates various intermediary files (detailed in other functions) for use in plotting
#' If proportionByAnimal writes weights for each animal to 'proportionWeights.csv'
#' Generates postscript hexmap plot in outFile
#' @param daybrs A data frame containing 'animal','rdate','lat','lon' (e.g. the output from readDouglas)
#' @param outFile File location to write postscript map generated by GMT to
#' @param hexPerDegree How many hexs should fit horizontally in 1 degree of longitude at the equator
#' @param limits A vector of (southernMostLatitude,northernMostLatitude,westernMostLongitude,easternMostLongitude) for final hexMap or NULL to select automatically
#' @param extraCmd If not NULL, vector of system commands to run after other GMT commands and before scale (e.g. extra labels with pstext)
#' @param landMaskCmd If not NULL, vector of system commands to run after hex output and before pscoast (e.g. polygons over hexes)
#' @param hardLimit A vector of (southernMostLatitude,northernMostLatitude,westernMostLongitude,easternMostLongitude) for cropping hexs or NULL to not crop
#' @param hexMax Sets hexs with counts > hexMax to hexMax (for creating maps with the highest hex counts being something like "100+")
#' @param logCounts  If true, base color scale on logged hex counts
#' @param debug If true, browser() before returning from makeHexs()
#' @param maxInterp Maximum number of missing days to fill between two data points (missing days<=maxInterp)
#' @param gmtDir System directory for calling GMT functions (e.g. if pscoast is in /home/bin/ and /home/bin/ isn't in path then gmtDir="/home/bin/")
#' @param contourFile If not NULL, run pscontour at contourDepth meters based on this GMT elevation file
#' @param contourDepth Depth to draw contour in meters
#' @param dayScale Divide day totals by this number (useful for intervals less than 1 day)
#' @param interp number of positions per day to interpolate
#' @param addNumberToName Add number of days and turtles to file name?
#' @param proportion Divide counts by sum(counts)
#' @param showMax show maximum on scale ticks
#' @param proportionByAnimal weight points to add up to one for each animal
#' @param seasonLimit When using proportionByAnimal, if less than seasonLimit points then total animal weight = # points/seasonLimit otherwise 1
#' @param uniqueAnimals Count unique occurrences of animal in a hex
#' @param outlineCount Draw an outline around all hexes with count >= outlineCount
#' @param propCount Draw an outline around enough hexs to cover propCount proportion of the data (overrides outlineCount)
#' @param addPlus Add a "+" to maximum label in scale (if using hexMax)
#' @param animalWeights Vector (with names of animals) of custom weighting for animals if proportionByAnimal is TRUE (e.g. monthly plots of proportion)
#' @param ... Any other arguments to be passed to runGMT
#' @return Vector of (lowXLim,highXlim,lowYLim,highYLim,maxHexCount) can be stored and passed as limits={return}, hardLimit={return} to further hexPlot calls to use the same range of latitude and longitude
#' @export
hexPlot<-function(daybrs,outFile,hexPerDegree=1,limits=NULL,extraCmd=NULL,landMaskCmd=NULL,hardLimit=NULL,hexMax=NULL,logCounts=FALSE,debug=FALSE,maxInterp=7,gmtDir="",contourFile=NULL,contourDepth=-200,dayScale=1,interp=1,addNumberToName=TRUE,proportion=FALSE,showMax=TRUE,proportionByAnimal=FALSE,seasonLimit=0,uniqueAnimals=FALSE,outlineCount=NULL,propCount=NULL,addPlus=TRUE,animalWeights=NULL,...){
	#Interpolate between days
	if(nrow(daybrs)>0){
		interps<-fill.missing.days(daybrs$animal,daybrs$rdate,daybrs$lat,daybrs$lon,maxInterp,interp)
		colnames(interps)<-c('animal','rdate','lat','lon')
		daybrs$interp<-FALSE
		if (length(interps$animal)>0){
			interps$interp<-TRUE
			message(paste('Merging ',length(interps$animal),' interpolated animal-days with ',length(daybrs$interp),' original animal-days',sep=""))
			finalbrs<-rbind(daybrs[,c('animal','rdate','lat','lon','interp')],interps)
		}else finalbrs<-daybrs[,c('animal','rdate','lat','lon','interp')]
		finalbrs<-finalbrs[order(finalbrs$animal,finalbrs$rdate),]
		if(proportionByAnimal){
			if(!is.null(animalWeights)){
				weights<-as.numeric(ave(finalbrs$animal,finalbrs$animal,FUN=function(x)animalWeights[x[1]]))
			}else{
				seasonCount<-ave(finalbrs$lat,finalbrs$animal,FUN=length)
				weights<-1/seasonCount*ifelse(seasonCount>=seasonLimit,1,seasonCount/seasonLimit)
			}
			animalWeightsOut<-data.frame('weight'=weights[!duplicated(finalbrs$animal)],'animal'=finalbrs$animal[!duplicated(finalbrs$animal)])
			write.csv(animalWeightsOut,'proportionWeights.csv',row.names=FALSE)
		}else{
			weights<-rep(1,nrow(finalbrs))
		}
		if(uniqueAnimals)uniqueCounter<-finalbrs$animal
		else uniqueCounter<-NULL
		newlimits<-makeHexs(finalbrs$lat,finalbrs$lon,file="hexs.dat",hexPerDegree=hexPerDegree,hardLimit=hardLimit,hexMax=hexMax,logCounts=logCounts,debug=debug,scale=dayScale,proportion=proportion,showMax=showMax,weights=weights,uniqueCounter=uniqueCounter,addPlus=addPlus)
		hexs<-newlimits[[4]]
		hexPoints<-newlimits[[5]]

		newlimits<-unlist(newlimits[-c(4,5)])
		
		if(!is.null(outlineCount)|!is.null(propCount)){
			if(!is.null(propCount)){
				uniqueCounts<-unique(hexs$count)
				countProps<-sapply(uniqueCounts,function(x,y)sum(hexs$count[hexs$count>=x])/y,sum(hexs$count))
				outlineCount<-uniqueCounts[countProps==min(countProps[countProps>=propCount])]
				message('Selected cutoff ',outlineCount,'. Covering ',sum(hexs$count>=outlineCount),' out of ',length(hexs$count),' hexs.')
			}
			if(!require(adehabitat))stop(simpleError('Outlining counts requires the package adehabitat'))

			goodHexs<-hexs[hexs$count>=outlineCount,]
			goodPoints<-hexPoints[hexPoints$count>=outlineCount&hexPoints$selector!=1,]
			if(nrow(goodPoints)>0){
				hexPolys<-by(goodPoints,rep(1:(nrow(goodPoints)/6),each=6),function(x)as(round(x[,c('coordx','coordy')],4),'gpc.poly'))
				for(poly in hexPolys){
					if(!exists('totalPoly'))totalPoly<-poly
					else totalPoly<-union(totalPoly,poly)
				}
				polyOut<-c()
				for(poly in totalPoly@pts){
					if(!poly$hole)polyOut<-c(polyOut,'>',paste(poly$x,poly$y,sep='\t'))
				}
				writeLines(polyOut,'countOutline.dat')
				landMaskCmd<-c(landMaskCmd,"psxy -R -JM -O -K -W4,0 -M -L <countOutline.dat>>")
			}
		}
		message('Hexs in plot sum to ',sum(hexs$count),'. Total data sum to ',sum(weights))
	}else{
		writeLines('','hexs.dat')
		newlimits<-c(limits,0)
	}
	if(is.null(limits)) limits<-newlimits
	outFileTemp<-strsplit(outFile,'\\.')[[1]]
	animals<-length(unique(daybrs$animal))
	days<-ceiling(length(daybrs$animal)/dayScale)
	outFileTemp[1]<-paste(outFileTemp[1],'_',animals,'t_',round(days),'d',sep="")
	if(addNumberToName)outFile<-paste(outFileTemp,collapse=".")
	runGMT(limits,outFile=outFile,dataFile="hexs.dat",gmtDir=gmtDir,contourFile=contourFile,contourDepth=contourDepth,extraCmd=extraCmd,landMaskCmd=landMaskCmd,...)
	return(newlimits)
}


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#---------------------------------------#
#########################################
#########Simple Base Functions###########
#Probably not necessary to call directly#
#########################################
#---------------------------------------#
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


#' Lag a vector
#'
#' Return a vector lagged one to the right
#' @param data A vector
#' @return A vector of same length as data with each datum moved one to the right and an NA in position 1
lagger<-function(data){
	if (length(data)==1) return(NA)
	if (length(data)==0) return(c())
	return(c(NA,data[1:(length(data)-1)]))
}

#' Sends concatenated command to system
#'
#' Concatenates arguments and sends to system() for executionj
#' @param ... Various parameters to be concatenated into a system command
#' @param debug If true prints command to screen (and still calls system)
#' @return Return code returned by OS
#' @seealso \code{\link{paste}}, \code{\link{system}}
#Side Effect: 
systemOut<-function(...,debug=FALSE){
	cmd<-paste(...,sep='')
	if (debug)message(cmd)
	returnCode<-system(cmd)
	return(returnCode)
}

#' Fill in missing days
#'
#' Interpolate between known positions to fill in missing days
#' @param id Vector of length N of animal identifiers (so the last day of one animal is not interpolated to the first day of the next)
#' @param date Vector of length N of either an rdate or some other integer
#' @param lat Vector of length N of latitudes
#' @param lon Vector of length N of longitudes
#' @param maxinterp Maximum number of missing days to fill between two data points (<=maxinterp)
#' @param interp number of positions per day to interpolate
#' @return Data frame of interpolated date/positions with columns id, date, lat, lon (note: original positions are not returned)
fill.missing.days <- function(id,date,lat,lon,maxinterp=7,interp=1){
	if (length(lat)!=length(date)|length(date)!=length(lon)|length(date)!=length(id)){
		stop(simpleError(paste("Lengths of id, date, lat and lon not equal.",sep="")))
	}
	maxinterp<-maxinterp*interp
	data<-data.frame(id,date,lat,lon)
	minDate<-min(date)
	data$date<-(data$date-minDate)*interp
	if(nrow(data)<1)browser()
	data$lagid<-lagger(id)
	data$lagdate<-lagger(data$date)
	data$laglat<-lagger(lat)
	data$laglon<-lagger(lon)
	data$daysdiff<-data$date-data$lagdate
	data$latstep<-(data$lat-data$laglat)/data$daysdiff
	data$lonstep<-(data$lon-data$laglon)/data$daysdiff

	#Find all points with same id as last row but a gap of more than 1 and <=maxinterp between dates
	fillers<-data[!is.na(data$lagid) & data$id==data$lagid & data$daysdiff>1 & data$daysdiff<=maxinterp,]
	output<-data[0,c('id','date','lat','lon')]
	for (i in 2:maxinterp) {
		interps<-fillers[fillers$daysdiff>=i,c('id','date','lat','lon','lonstep','latstep')]
		interps$date<-interps$date-(i-1)
		interps$lat<-interps$lat-interps$latstep*(i-1)
		interps$lon<-interps$lon-interps$lonstep*(i-1)
		output<-rbind(output,interps[,c('id','date','lat','lon')])
	}
	message('Interpolated ',length(output$id),' missing points between points <= ',maxinterp/interp,' days apart with a spacing of ',round(1/interp,3),' days')
	output$date<-output$date/interp+minDate
	return(output)
}



#' Generate hexs
#'
#' Writes lat/lon color information to file specified by file parameter in a format ready to use in GMT (but probably easy to parse by other programs too)
#' Makes test map of hex positions and data in testhex.eps
#' Makes GMT color table scale.cpt for later use
#' Calls makeTicks() which will output tick position for scalebar for later use
#' @param lat A vector of latitudes of length N
#' @param lon A vector of longitudes of length N
#' @param lonBase Longitude for centering calculations 
#' @param hexPerDegree How many hexs should fit horizontally in 1 degree of longitude at the equator
#' @param border How many degrees of space to leave around the hexs (if hardLimit is NULL)
#' @param file File to lat/lon of hex positions to
#' @param hardLimit A vector of (southernMostLatitude,northernMostLatitude,westernMostLongitude,easternMostLongitude) (or NULL) to filter hexs 
#' @param hexMax Sets hexs with counts > hexMax to hexMax (for creating maps with the highest hex counts being something like "100+")
#' @param logCounts If true base color scale on logged hex counts
#' @param debug If true, browser() before returning
#' @param scale Divide day totals by this number (useful for intervals less than 1 day)
#' @param proportion Divide counts by sum(counts)
#' @param showMax show maximum on scale ticks?
#' @param weights Weight for each point
#' @param uniqueCounter If not NULL, count unique occurrences of uniqueCounter in a hex
#' @param addPlus Add a "plus" to maximum label in scale if using hexMax
#' @import maps hexbin
#' @return Vector for use in further functions of (lowXLim,highXlim,lowYLim,highYLim,maxHexCount)
makeHexs<-function(lat,lon,lonBase=-45,hexPerDegree=1,border=5,file="hexs.dat",hardLimit=NULL,hexMax=NULL,logCounts=FALSE,debug=FALSE,scale=1,proportion=FALSE,showMax=FALSE,weights=rep(1,length(lat)),uniqueCounter=NULL,addPlus=TRUE){
	xlim <- range(lon)
	xlim[1] <- round(xlim[1] - border)
	xlim[2] <- round(xlim[2] + border)
	ylim <- range(lat)
	ylim[1] <- round(ylim[1] - border)
	ylim[2] <- round(ylim[2] + border)  
	convLon<-(lon-lonBase)*cos(lat/180*pi)#Convert to standard degrees 
	convXlim <- range(convLon)
	convXlim[1]<-floor(convXlim[1])
	convXlim[2]<-ceiling(convXlim[2])
	horHexRadius<-1/hexPerDegree/2
	xspread<-diff(convXlim)
	yspread<-diff(ylim)
	convYlim<-ylim
	if (xspread>yspread){
		convYlim[2]<-convYlim[2]+xspread-yspread
	}else{
		convXlim[2]<-convXlim[2]+yspread-xspread
	}
	xhexs<-diff(convXlim)*hexPerDegree
	shape<-1
	bin <- hexbin(convLon, lat, xbins = xhexs,xbnds=convXlim,ybnds=convYlim,shape=shape,IDs=TRUE)
	cellxy<-hcell2xy(bin)
	message("Outputted test hex positions to testhex.eps")
	postscript("testhex.eps")
		plot(cellxy$x/cos(cellxy$y/180*pi)+lonBase,cellxy$y,cex=.25,col="red",ylab="Latitude",xlab="Longitude",xlim=c(-180,180),ylim=c(-70,70))
		points(lon,lat,cex=.1,col="grey")
		maps::map(add=TRUE)
	dev.off()

	proportionScale<-1
	if(!is.null(uniqueCounter)) weightSum<-tapply(uniqueCounter,bin@cID,function(x)length(unique(x)))
	else weightSum<-tapply(weights,bin@cID,sum)
	if(any(names(weightSum)!=bin@cell))stop(simpleError('Problem setting bin counts'))
	bin@count<-as.vector(weightSum)
	if(any(weights!=1)){
		proportionScale<-10000
		bin@count<-ceiling(bin@count*proportionScale)
	}
	if(proportion){
		proportionScale<-10000
		bin@count<-ceiling(bin@count/sum(bin@count)*proportionScale)
	}
	if(scale!=1){bin@count<-ceiling(bin@count/scale)}
	if (!is.null(hexMax)){
		message(paste("Capping hex counts at",hexMax))
		bin@count[bin@count>hexMax]<-hexMax
	}
	output<-data.frame('x'=cellxy$x,'y'=cellxy$y,'count'=bin@count)
	r<-180/pi
	vertHexRadius<-horHexRadius/sin(60/r)
	output$topy<-output$y+vertHexRadius
	output$midtopy<-output$y+vertHexRadius/2
	output$midbottomy<-output$y-vertHexRadius/2
	output$bottomy<-output$y-vertHexRadius
	output$topx<-output$x/cos(output$topy/r)+lonBase
	output$topleftx<-(output$x-horHexRadius)/cos(output$midtopy/r)+lonBase
	output$bottomleftx<-(output$x-horHexRadius)/cos(output$midbottomy/r)+lonBase
	output$bottomx<-output$x/cos(output$bottomy/r)+lonBase
	output$bottomrightx<-(output$x+horHexRadius)/cos(output$midbottomy/r)+lonBase
	output$toprightx<-(output$x+horHexRadius)/cos(output$midtopy/r)+lonBase
	#Filter out out of bounds hexs
	if (!is.null(hardLimit)){
		outOfBounds<-output$topy<hardLimit[3]|output$bottomy>hardLimit[4]|(output$bottomrightx<hardLimit[1]&output$toprightx<hardLimit[1])|(output$bottomleftx>hardLimit[2]&output$topleftx>hardLimit[2])
		output<-output[!outOfBounds,]
	}
	if(nrow(output)>0){
		#make color scale
		colormin<-1
		maximum<-hexMax
		if (logCounts){
			output$count<-round(log(output$count)*100);
			colormin<-0;
			if (!is.null(hexMax)) maximum<-round(log(maximum)*100)
		}
		if (is.null(hexMax))countcolors<-red2bluehsl(max(output$count),colormin)
		else countcolors<-red2bluehsl(maximum,colormin)
		cptoutput<-cbind(countcolors[1:max((nrow(countcolors)-1),1),],countcolors[min(2,nrow(countcolors)):length(countcolors$count),])
		write.table(c("#	cpt file created by: R","#COLOR_MODEL = RGB","#"),'scale.cpt',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
		if (cptoutput[1,1]==0)cptoutput[1,1]<-0.00001	
		write.table(cptoutput,'scale.cpt',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)

		makeTicks(output$count,logCounts,hexMax=hexMax,adjustScale=proportionScale,showMax=showMax,addPlus=addPlus)
		numCheck<-length(output$x)
		output<-merge(output,countcolors)
		if (numCheck!=length(output$x)){
			stop(simpleError(paste("Merging hexcounts with colors got messed up somehow",sep="")))
		}
		output$id<-1:length(output$x)
		stackoutput<-rbind(output,output,output,output,output,output,output)
		stackoutput<-stackoutput[order(stackoutput$id),]
		stackoutput$coordx<-NA
		stackoutput$coordy<-NA
		stackoutput$text<-NA
		stackoutput$selector<-rep(1:7,length(output$x))
		#Color
		selector<-stackoutput$selector==1
		#Color
		stackoutput[selector,'text']<-paste(">-W1/255 -G",stackoutput[selector,'red'],"/",stackoutput[selector,'green'],"/",stackoutput[selector,'blue'],sep="")
		#Top
		selector<-stackoutput$selector==2
		stackoutput[selector,'coordx']<-stackoutput[selector,'topx']
		stackoutput[selector,'coordy']<-stackoutput[selector,'topy']
		#Topleft
		selector<-stackoutput$selector==3
		stackoutput[selector,'coordx']<-stackoutput[selector,'topleftx']
		stackoutput[selector,'coordy']<-stackoutput[selector,'midtopy']
		#BotLeft
		selector<-stackoutput$selector==4
		stackoutput[selector,'coordx']<-stackoutput[selector,'bottomleftx']
		stackoutput[selector,'coordy']<-stackoutput[selector,'midbottomy']
		#Bottom
		selector<-stackoutput$selector==5
		stackoutput[selector,'coordx']<-stackoutput[selector,'bottomx']
		stackoutput[selector,'coordy']<-stackoutput[selector,'bottomy']
		#BotRight
		selector<-stackoutput$selector==6
		stackoutput[selector,'coordx']<-stackoutput[selector,'bottomrightx']
		stackoutput[selector,'coordy']<-stackoutput[selector,'midbottomy']
		#TopLeft
		selector<-stackoutput$selector==7
		stackoutput[selector,'coordx']<-stackoutput[selector,'toprightx']
		stackoutput[selector,'coordy']<-stackoutput[selector,'midtopy']
		if (!is.null(hardLimit)){
			stackoutput$coordx[stackoutput$coordx<hardLimit[1]]<-hardLimit[1]
			stackoutput$coordx[stackoutput$coordx>hardLimit[2]]<-hardLimit[2]
			stackoutput$coordy[stackoutput$coordy<hardLimit[3]]<-hardLimit[3]
			stackoutput$coordy[stackoutput$coordy>hardLimit[4]]<-hardLimit[4]
		}
		stackoutput[is.na(stackoutput$text),'text']<-paste(stackoutput[is.na(stackoutput$text),'coordx'],stackoutput[is.na(stackoutput$text),'coordy'])
		write.table(stackoutput$text,file,quote=FALSE,row.names=FALSE,col.names=FALSE)
	}else{
		message('No points in limits')
		writeLines('',file)
	}
	message(paste("Wrote hex data to file",file,sep=""))
	#Return x and ylims for next function
	output$count<-output$count/proportionScale
	stackoutput$count<-stackoutput$count/proportionScale
	if (debug) browser()
	return(list(xlim,ylim,max(bin@count),output,stackoutput))
}

#' Generate hsl color red-blue color scale
#' 
#' Generate color scale for 1:n colors in the hsl color scheme going evenly from hue=.31 to 1 (s=1,v=.5)
#' @param n Maximum count
#' @param min Minimum count
#' @return Data frame with rows 1:n and columns 'count','red','green','blue' (rgb columns in 0-255 scale) 
red2bluehsl<-function (n,min=1){
	if(min==n)return(data.frame('count'=n,'red'=255,'green'=0,blue=0))
	count<-min:n
	output<-data.frame(count)
	output$red<-NA;output$green<-NA;output$blue<-NA;output$hue<-NA
	huestart<-.31
	output$hue<-(count-min(count))/(max(count)-min(count))*(1-huestart)+huestart
	for (i in 1:length(output$hue)) {
		output[i,c('red','green','blue')]<-hsl2rgb(output$hue[i],1,.5)
	}
	return(output[,c('count','red','green','blue')])
}

#' Convert hsl to rgb
#'
#' Convert hsl color to rgb 
#' @param hue Hue 0-1 in hsl color scale
#' @param saturation Saturation 0-1 in hsl color scale
#' @param lightness Lightness 0-1 in hsl color svale
#' @return Vector of red green blue in 0-255 scale
hsl2rgb<-function (hue,saturation,lightness){
	if (saturation == 0){
		red <- lightness * 255
		green <- lightness * 255
		blue <- lightness * 255
	} else {
		if (lightness < 0.5) var2 <- lightness * (1 + saturation)
		else var2 <- (lightness + saturation) - (saturation * lightness);
		var1 <- 2 * lightness - var2;
		red <- 255 * hue2rgb(var1,var2,hue + (1 / 3));
		green <- 255 * hue2rgb(var1,var2,hue - (1 / 3));
		blue <- 255 * hue2rgb(var1,var2,hue);
	}
	return(round(c(red,green,blue)));
}

#Function: blue <- 255 * hue2rgb(var1,var2,hue)
#For internal use in hsl2rgb
hue2rgb<-function(v1,v2,vh){
	if (vh < 0) vh <-vh+ 1
	else {
		if (vh > 1) vh<-vh- 1
	}
	if ((6 * vh) < 1) output<-v1 + (v2 - v1) * 6 * vh
	else {
		if ((2 * vh) < 1) output<-v2
		else{
			if ((3 * vh) < 2) output<-v1 + (v2 - v1) * ((2 / 3 - vh) * 6)
			else output<-v1
		}
	}
	return(output);
}

#' Makes ticks for use in GMT scale bar
#'
#' Writes tick positions to ticks.dat for later use in runGMT
#' Writes labels and label positions to labels.dat for later use in runGMT
#' @param counts Vector of counts to make ticks for
#' @param isLog If true counts have previously been transformed round(log(counts)*100)
#' @param showMax Add the maximum hex count to the scale and filter out any labels too close to it
#' @param hexMax Counts have previously been capped at hexMax. Also adds "+" to {hexMax} label (e.g. "100+") if isLog is false
#' @param adjustScale Multiply the scale by this e.g. adjustScale=100 would give percent
#' @param addPlus Add a "+" to {hexMax} label?
#' @param addMin If true makes sure the minimum scale value is at most 1 (0 if isLog is TRUE)
#' @return Invisible NULL
makeTicks<-function(counts,isLog=FALSE,showMax=TRUE,hexMax=NULL,addMin=TRUE,adjustScale=1,addPlus=TRUE){
	high<-.4
	low<-.36
	lower<-.33
	if (isLog){
		scaler<-100
		if(addMin) counts<-c(counts,0)
		if (!is.null(hexMax)) counts<-c(counts,log(hexMax)*scaler)
		ticks<-c(1:9,seq(10,90,10),seq(100,900,100),seq(1000,9000,1000))
		labels<-c(1,5,10,50,100,500,1000,5000)
		ticks<-ticks[ticks>=exp(min(counts)/100)&ticks<=exp(max(counts)/scaler)]
		suppress<-NULL
		if (showMax){
			selector<-max(counts)-log(labels)*scaler<.6*scaler&round(exp(max(counts)/scaler))>labels
			if (any(selector)) suppress<-labels[selector]
			if (!any(labels==round(exp(max(counts)/scaler))))labels<-c(labels,round(exp(max(counts)/scaler)))
			if (!any(ticks==round(exp(max(counts)/scaler))))ticks<-c(ticks,round(exp(max(counts)/scaler)))
		}
	}else{
		if (addMin) counts<-c(counts,1)
		if (!is.null(hexMax)){
			counts<-c(counts[counts<=hexMax],hexMax)
			maxCount<-max(c(counts,hexMax))
		}else{
			maxCount<-max(counts)
		}
		ticks<-pretty(counts/adjustScale)
		ticks<-ticks[ticks<=maxCount/adjustScale]
		if (!any(ticks==maxCount/adjustScale)&showMax) ticks<-c(ticks,maxCount/adjustScale)
		if (any(ticks==0)&any(ticks>1)) ticks<-c(1,ticks[ticks!=0])
		labels<-ticks
		max1<-max(labels)
		max2<-max(labels[labels!=max1])
		if(max1-max2<diff(range(labels))*.015*nchar(max1))labels<-labels[labels!=max2]

		suppress<-NULL
	}
	ticks<-sort(c(ticks,ticks,ticks))
	ticks<-data.frame(ticks)
	ticks$y<-rep(c(NA,high,low),length(ticks$ticks)/3)
	ticks$y[ticks$ticks %in% labels&ticks$y==low]<-lower
	if (isLog){
		ticks$logticks<-log(ticks$ticks)*scaler
		ticks$x<-round((ticks$logticks-min(counts))/max(counts-min(counts)),4)
		ticks$x[ticks$x>1&ticks$x<1.001]<-1 #deal with minor rounding error
	}else{
		ticks$x<-round((ticks$ticks-min(counts)/adjustScale)/max(counts/adjustScale-min(counts)/adjustScale),4)
	}
	ticks$text<-NA
	ticks$text[is.na(ticks$y)]<-">"
	ticks$text[!is.na(ticks$y)]<-paste(ticks[!is.na(ticks$y),c('x')],ticks[!is.na(ticks$y),c('y')])
	labelers<-ticks[ticks$ticks %in% labels&is.na(ticks$y)&!ticks$ticks %in% suppress,]
	labelers$y<-.3
	labelers$text<-paste(labelers$x,labelers$y,"10 0 1 CT",format(labelers$ticks,digits=2,drop0trailing=TRUE))
	if(addPlus)labelers$text[labelers$ticks==hexMax]<-paste(labelers$text[labelers$ticks==hexMax],"+",sep="")
	write.table(ticks$text,'ticks.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(labelers$text,'labels.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
	invisible(NULL)
}

#' Run GMT
#'
#' Takes files generated by previous functions and calls mapping program GMT to generate output postscript file
#' IMPORTANT: You must have GMT installed for this function to work
#' @param limits Vector of (lowXLim,highXlim,lowYLim,highYLim) probably from makeHexs or passed in from hexPlot
#' @param outFile File location to output GMT postscript to
#' @param width Width of output figure in inches
#' @param dataFile File location of hex color and lat/lon data previously generated by makeHexs
#' @param gmtDir System directory for calling GMT functions (e.g. if pscoast is in /home/bin/ and /home/bin/ isn't in path then gmtDir="/home/bin/")
#' @param scale If TRUE make a scale based on files output by makeTicks (called in makeHexs)
#' @param contourFile If not NULL, run pscontour at contourDepth meters based on this GMT elevation file
#' @param contourDepth Depth to draw contour in meters
#' @param extraCmd If not NULL, vector of system commands to run after other GMT commands and before scale (e.g. extra labels with pstext)
#' @param landMaskCmd If not NULL, vector of system commands to run prior to final psCoast (e.g. polygons overlaid by coast)
#' @param landMask If TRUE, hexs overlapping land are covered by land else hexs overlap land and only the land border is plotted over them
#' @param ps2eps If TRUE, after all other commands calls the program ps2eps to convert .ps to .eps (Make sure you have ps2eps installed and in path if you use this)
#' @param scaleLabel Label for scale if scale is TRUE
#' @param contourPen A string or vector of strings specifying pen attributes for grd contour to trace the contour file
#' @param concatenate Add output to existing file?
#' @param xpos Shift plot origin right xpos inches
#' @param ypos Shift plot origin up ypos inches
#' @param posOptions Options for position (e.g. -O)
#' @param gmtsetOptions Options for gmtset
#' @param annotateBorders A string like "WSen" specifying which borders to annotate as in psbasemap
#' @param borderColor color for border of hexes
#' @param pscoastOptions command line options for pscoast
#' @return Invisible NULL
runGMT<-function(limits,outFile="output.ps",width=6,dataFile="hexs.dat",gmtDir="",scale=TRUE,contourFile=NULL,contourDepth=-200,extraCmd=NULL,landMaskCmd=NULL,landMask=TRUE,ps2eps=FALSE,scaleLabel="Days Turtles Recorded in Hex",contourPen='1/120ta',concatenate=FALSE,xpos=3,ypos=1.8,posOptions=ifelse(concatenate,'-O',''),gmtsetOptions=NULL,annotateBorders='WSen',borderColor=255,pscoastOptions=''){
	if(!is.null(gmtsetOptions))systemOut(gmtDir,'gmtset ',gmtsetOptions)
	xlim<-limits[1:2]
	ylim<-limits[3:4]
	xdif<-abs(diff(xlim))
	ydif<-abs(diff(ylim))
	xanot<-2
	if (xdif>10) xanot<-5
	if (xdif>20) xanot<-10
	if (xdif>50) xanot<-20
	yanot<-2
	if (ydif>10) yanot<-5
	if (ydif>20) yanot<-10
	if (ydif>50) yanot<-20

	pscoastRes<-'l'
	if(ydif<=30)pscoastRes<-'i'
	if(ydif<=15)pscoastRes<-'h'

	#starting gmt grapphing
	#read in graph parameters here
	#width of final map in inches
	dataFile<-"hexs.dat"	
	if(concatenate)adder<-'>>'
	else adder<-'>'
	pos<-sprintf('%s -X%fi -Y%fi',posOptions,xpos,ypos)
	systemOut(gmtDir,"psbasemap -R",paste(c(xlim,ylim),collapse="/")," -JM",width,"i -G255 -P -K ",pos,adder,outFile)
	
	if (!landMask) systemOut(gmtDir,"pscoast -R -JM -O -K -D",pscoastRes," -W1/110 -G220 -A2000/0/2 ",pscoastOptions," >>",outFile) 

	#output to gmthex.dat
	if(!is.null(borderColor))borderCmd<-sprintf("-W1/%s",borderColor)
	else borderCmd<-''
	systemOut(gmtDir,"psxy ",dataFile," -R -JM -O -K -A -M -G0 ",borderCmd," -V>>",outFile)
	if (!is.null(landMaskCmd)){
		for (i in 1:length(landMaskCmd)) {
			systemOut(gmtDir,landMaskCmd[i],outFile)
		}
	}
	if (!landMask){
		systemOut(gmtDir,"pscoast -R -JM -O -K -D",pscoastRes," -W1/0 -A0/0/2 ",pscoastOptions," >>",outFile)
		systemOut(gmtDir,"pscoast -R -JM -O -K -D",pscoastRes," -W1/0 -A2000/0/2 ",pscoastOptions," >>",outFile)
	}else{
		systemOut(gmtDir,"pscoast -R -JM -O -K -D",pscoastRes," -W1/0 -A0/0/2 -G220 ",pscoastOptions," >>",outFile)
		systemOut(gmtDir,"pscoast -R -JM -O -K -D",pscoastRes," -W1/0 -A2000/0/2 -G220 ",pscoastOptions," >>",outFile)
	}
	#Redo border since coast covers up
	systemOut(gmtDir,"psbasemap -R -JM -B",xanot/2,"a",xanot,"/",yanot/2,"a",yanot,annotateBorders," -P -O -K>>",outFile)
	if (!is.null(contourFile)){
		#Generate contour at contourDepth meters
		write.table(c(paste(contourDepth,' c',sep="")),'contour.cont',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
		sapply(contourPen,function(x)systemOut(gmtDir,'grdcontour ',contourFile,' -R -JM -O -K -Ccontour.cont -A+k120+s5 -W',x,' -Q800 >>',outFile))
	}
	if (!is.null(extraCmd)){
		for (i in 1:length(extraCmd)) {
			systemOut(gmtDir,extraCmd[i],outFile)
		}
	}

	if (scale){
		makeScale(width,outFile,scaleLabel,gmtDir=gmtDir)
	}
		
	if (ps2eps){
		systemOut("ps2eps -f -B -l ",outFile)
	}
	invisible(NULL)
}

#' Make a scale in GMT
#'
#' Makes scale based on ticks.dat and labels.dat
#' @param width width of plot area
#' @param outFile plot file
#' @param scaleLabel title of scale
#' @param margin space for margin on either side of scale
#' @param scaleHeight height of scale
#' @param xpos Shift plot origin right xpos inches (balance with margin and scaleHeight)
#' @param ypos Shift plot origin up ypos inches (balance with margin and scaleHeight)
#' @param gmtDir System directory for calling GMT functions (e.g. if pscoast is in /home/bin/ and /home/bin/ isn't in path then gmtDir="/home/bin/")
#' @return Invisible NULL
makeScale<-function(width,outFile,scaleLabel="Days Turtles Recorded in Hex",margin=1,scaleHeight=.8,xpos=margin/2,ypos=-scaleHeight*1.5,gmtDir=''){
		scaleWidth<-width-margin
		systemOut(gmtDir,'psbasemap -R0/1/0/1 -JX',scaleWidth,'i/',scaleHeight,'i -X',xpos,'i -Y',ypos,'i -K -G255 -O>>',outFile)
		systemOut(gmtDir,'psscale -D',scaleWidth/2,'i/',scaleHeight*.6,'i/',scaleWidth,'i/',scaleHeight*.2,'ih -O -Cscale.cpt -K  -B100000::/::>>',outFile)
		write.table(paste('.5 0.7 12 0 1 CB ',scaleLabel,sep=""),'scalelab.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
		systemOut(gmtDir,'pstext scalelab.dat -R -JX -O -K>>',outFile)
		systemOut(gmtDir,'psxy ticks.dat -JX -R -O -K -M -A -W1.5/0>>',outFile)
		systemOut(gmtDir,'pstext labels.dat -JX -R -O -K -N>>',outFile)
		invisible(NULL)
}
