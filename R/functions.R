# Copyright (c) 2008, Scott Sherrill-Mix
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the organization nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY SCOTT SHERRILL-MIX ``AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
#

#---------------------------------------#
#########################################
##############User Functions#############
###########The Important Stuff###########
#########################################
#---------------------------------------#


#Function: brData<-readDouglas('datadir/',search="br[0-9][^.]+[0-9]\\.txt",extraColumns<-c('sex','adult'))
#Parameters:
	#path: System path to Douglas outputted files
	#search: Regular expression to select files (defaults to br files)
	#ignoreErrors: If true does not check for correct deployment information (lc94=DP)
	#extraColumns: A vector of extra column names (present in the br files) that should also be returned (should be consistent over a day [e.g. deployday not time])
	#assumeJuv: If true and br file is missing the 3 columns sex, stage and nester, will assume U, J, No respectively (A pretty specific convenience parameter for myself)
	#twoWeekRemove: If true removes days < 14 days into the track
#Side Effects: Sources code in filter.R after reading in br files for custom filtering (see example file)
#Return: Data frame with median position for each day present in br file with columns c('animal', 'ptt', 'date', 'month', 'year', 'rdate', 'season', 'dir','file','deployday', extraColumns)
readDouglas<-function(path=".",search="br[0-9][^.]+[0-9]\\.txt",ignoreErrors=FALSE,extraColumns=NULL,assumeJuv=FALSE,twoWeekRemove=FALSE){
	dayColumns<-c('animal','ptt','date','month','year',extraColumns)
	wantedColumns<-c('animal','ptt','date','latitude','longitud','month','year','thetime','lc94',extraColumns)
	homepath<-getwd()
	print(path)
	#Read in br files
	setwd(path)
	files<-list.files(full.names=TRUE,recursive=TRUE)
	files<-files[grep(search,files)]

	if (length(files)==0){
		stop(simpleError(paste("Please put some files matching ",search," in ",path,sep="")))
	}else message(paste('Found ',length(files),' br files:\n\t',paste(files,collapse=" "),sep=''))

	found<-FALSE
	for (i in 1:length(files)) {
		thisDir=strsplit(files[i],'/')[[1]]
		thisFile=thisDir[length(thisDir)]
		thisDir=thisDir[length(thisDir)-1]
		thisBr<-read.table(files[i],header=TRUE,sep=",",as.is=TRUE)
		missingCol<-wantedColumns[!(wantedColumns %in% colnames(thisBr))]
		if (identical(missingCol,c('sex','stage','nester'))&assumeJuv){
			message(paste("Assuming sex is U and stage is J for br and nester is No",thisFile))
			thisBr$sex<-"U"
			thisBr$stage<-"J"
			thisBr$nester<-"No"
			missingCol=NULL
		}
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
	setwd(homepath)

	#filter out any crappy ones or custom 
	if(file.exists('filter.R')){
		message('NOTE: Filtering data frame "brs" based on code in filter.R')
		source('filter.R')
		brs<-brFilter(brs)
	}else message('If you would like to filter any points out of the total br file (or add points from non standard files) put R code modifying the data frame "brs" into filter.R')

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
	brs$rdate<-as.date(brs$date)
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

	#Remove first two weeks if desired
	brs$deployday<-brs$rdate-brs$depdate
	if (twoWeekRemove){
		brs<-brs[brs$deployday>=14,]
	}

	brs$season<-NA
	brs[brs$month >= 1 | brs$month <= 3,'season']<-1;
	brs[brs$month >= 4 & brs$month <= 6,'season']<-2;
	brs[brs$month >= 7 & brs$month <= 9, 'season']<-3;
	brs[brs$month >= 9 & brs$month <= 12, 'season']<-4;

	brs$animalDate<-paste(brs$animal,"_",as.numeric(brs$rdate),sep="")
	avglon<-tapply(brs$longitud,brs$animalDate,median)
	avglat<-tapply(brs$latitude,brs$animalDate,median)
	daybrs<-unique(brs[,c(dayColumns,'rdate','season','animalDate','dir','file','deployday')])
	if(length(daybrs$animalDate)!=length(avglat)|length(daybrs$animalDate)!=length(avglon)){
		stop(simpleError(paste("Somehow the number of avgerage lats or lons does not match the number of animals and days.",sep="")))
	}
	#Not actually necessary but better safe...
	daybrs<-daybrs[order(daybrs$animalDate),]
	avglon<-avglon[order(rownames(avglon))]
	avglat<-avglat[order(rownames(avglat))]
	if(!all(daybrs$animalDate==rownames(avglon))|!all(daybrs$animalDate==rownames(avglat))){
		stop(simpleError(paste("Somehow the number of avgerage lats or lons does not match the number of animals and days.",sep="")))
	}
	daybrs$lat<-avglat
	daybrs$lon<-avglon

	#Specific for fixing sex stage and nester
	if (all(c('sex','stage','nester') %in% wantedColumns)){
		daybrs$convertSex<-NA
		daybrs$convertSex[daybrs$sex %in% c('m','M')]<-"M"
		daybrs$convertSex[daybrs$sex %in% c('F','FALSE','f')]<-"F"
		daybrs$convertSex[daybrs$sex %in% c('U','UN','u')]<-"U"
		daybrs$convertStage<-NA
		daybrs$convertStage[daybrs$stage %in% c('Adult','adult','A')]<-"A"
		daybrs$convertStage[daybrs$stage %in% c('immature','juv','J')]<-"J"
		daybrs$convertStage[daybrs$stage %in% c('U','u','uk')]<-"U"
		daybrs$convertNester<-NA
		daybrs$convertNester[daybrs$nester %in% c('N','no','n','No')]<-"N"
		daybrs$convertNester[daybrs$nester %in% c('Y','ye','y')]<-"Y"
		daybrs$convertNester[daybrs$nester %in% c('U','uk')]<-"U"
		if (any(is.na(daybrs$convertSex)))stop(simpleError("Some sexs not understood"))
		if (any(is.na(daybrs$convertStage)))stop(simpleError("Some stages not understood"))
		if (any(is.na(daybrs$convertNester)))stop(simpleError("Some nesters not understood"))
		daybrs$sex<-daybrs$convertSex
		daybrs$stage<-daybrs$convertStage
		daybrs$nester<-daybrs$convertNester
	}
	return(daybrs)
}

#Function: limits<-hexPlot(alldaybrs,'all.ps',hexPerDegree=2,hexMax=100)
#Parameters:
	#daybrs:Data frame containing 'animal','rdate','lat','lon' (e.g. the output from readDouglas)
	#outFile: File location to write postscript map generated by GMT to
	#hexPerDegree: How many hexs should fit horizontally in 1 degree of longitude at the equator
	#limits: A vector of (southernMostLatitude,northernMostLatitude,westernMostLongitude,easternMostLongitude) for final hexMap or NULL to select automatically
	#extraCmd: If not NULL, vector of system commands to run after other GMT commands and before scale (e.g. extra labels with pstext)
	#landMaskCmd: If not NULL, vector of system commands to run after hex output and before pscoast (e.g. polygons over hexes)
	#hardLimit: A vector of (southernMostLatitude,northernMostLatitude,westernMostLongitude,easternMostLongitude) for cropping hexs or NULL to not crop
	#hexMax: Sets hexs with counts > hexMax to hexMax (for creating maps with the highest hex counts being something like "100+")
	#histPrefix: If not NULL, create histogram files of hex counts in {histPrefix}_hist.ps 
	#stateCount: If true, add commands generated by stateOutput() based on column 'stateDep' for deployment state of each animal (pretty specific command for this project but could be modified for other projects)
	#logCounts:  If true, base color scale on logged hex counts
	#debug: If true, browser() before returning from makeHexs()
	#maxInterp: Maximum number of missing days to fill between two data points (missing days<=maxInterp)
	#gmtDir: System directory for calling GMT functions (e.g. if pscoast is in /home/bin/ and /home/bin/ isn't in path then gmtDir="/home/bin/")
	#contourFile: If not NULL, run pscontour at contourDepth meters based on this GMT elevation file
	#contourDepth: Depth to draw contour in meters
	#dayScale: Divide day totals by this number (useful for intervals less than 1 day)
	#interp: # of positions per day to interpolate
	#addNumberToName: Add number of days and turtles to file name?
	#allOneColor: All hexes a single color e.g. '20/20/20'
	#proportion: Divide counts by sum(counts)
	#showMax: show maximum on scale ticks
	#proportionByAnimal: weight points to add up to one for each animal
	#seaonLimit: When using proportionByAnimal, if less than seasonLimit points then total animal weight = # points/seasonLimit otherwise 1
	#uniqueAnimals: Count unique occurrences of animal in a hex
	#outlineCount: Draw an outline around all hexes with count >= outlineCount
	#outlineCount: Draw an outline around enough hexs to cover propCount proportion of the data (overrides outlineCount)
	#addPlus: Add a "+" to maximum label in scale (if using hexMax)
	#animalWeights: Vector (with names of animals) of custom weighting for animals if proportionByAnimal is TRUE (e.g. monthly plots of proportion)
	#...: Any other arguments to be passed to runGMT
#Side Effects: 
	#Generates various intermediary files (detailed in other functions) for use in plotting
	#If histPrefix is not NULL writes hex count histograms to {histPrefix}_hist{XXX}.eps
	#If daybrs contains column 'stateDep' generates animalTable.csv and dayTable.csv showing number of animal and animal-days from each state
	#If proportionByAnimal writes weights for each animal to 'proportionWeights.csv'
	#Generates postscript hexmap plot in outFile
#Return: Vector of (lowXLim,highXlim,lowYLim,highYLim,maxHexCount) can be stored and passed as limits={return}, hardLimit={return} to further hexPlot calls to use the same range of latitude and longitude
hexPlot<-function(daybrs,outFile,hexPerDegree=1,limits=NULL,extraCmd=NULL,landMaskCmd=NULL,hardLimit=NULL,hexMax=NULL,histPrefix=NULL,stateCount=FALSE,logCounts=FALSE,debug=FALSE,maxInterp=7,gmtDir="",contourFile=NULL,contourDepth=-200,dayScale=1,interp=1,addNumberToName=TRUE,allOneColor=NULL,proportion=FALSE,showMax=TRUE,proportionByAnimal=FALSE,seasonLimit=0,uniqueAnimals=FALSE,outlineCount=NULL,propCount=NULL,addPlus=TRUE,animalWeights=NULL,...){
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
		if ('stateDep' %in% colnames(daybrs)){
			interpStates<-merge(finalbrs,unique(daybrs[,c('animal','stateDep')]))
			if (length(interpStates$animal)!=length(finalbrs$animal)) stop(simpleError("Merging state deployments and post-interpolated data didn't work"))
			if (is.null(limits)) append<-FALSE
			else append<-TRUE
			fileName<-gsub("\\.[^.]*$","",gsub("^[^/]*/","",outFile))
			outTable<-as.data.frame(t(c(table(interpStates$stateDep))))
			outTable<-cbind(fileName,outTable)
			write.table(outTable,"dayTable.csv",append=append,sep=",",quote=TRUE,row.names=FALSE,col.names=!append)
			outTable<-as.data.frame(t(c(table(unique(interpStates[,c('animal','stateDep')])$stateDep))))
			outTable<-cbind(fileName,outTable)
			write.table(outTable,"animalTable.csv",append=append,sep=",",quote=TRUE,row.names=FALSE,col.names=!append)
			if (stateCount){
				exCmds<-stateOutput(interpStates,limits=hardLimit)
				extraCmd<-c(extraCmd,"psxy states.dat -R -JM -O -K -A -M>>","pstext stateStats.dat -JM -R -O -K>>",exCmds)	
			}
		}
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
		newlimits<-makeHexs(finalbrs$lat,finalbrs$lon,file="hexs.dat",hexPerDegree=hexPerDegree,hardLimit=hardLimit,hexMax=hexMax,histPrefix=histPrefix,logCounts=logCounts,debug=debug,scale=dayScale,allOneColor=allOneColor,proportion=proportion,showMax=showMax,weights=weights,uniqueCounter=uniqueCounter,addPlus=addPlus)
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
			#k=0 should be minimum convex hull
			#homeRanges<-NNCH(unique(goodHexs[,c('x','y')]), a=5,duplicate='ignore')
			#date();homeRanges<-NNCH(unique(goodPoints[,c('coordx','coordy')]), a=5,duplicate='ignore');date()
			#polys<-homeRanges[[1]]$polygons
			#getting 100% poly
			#thisPoly<-polys[[length(polys)]]@pts
			#polyOut<-c()
			#for(j in thisPoly){
			#	if(!j$hole)polyOut<-c('>',paste(j$x,j$y,sep='\t'))
			#}
			#writeLines(polyOut,'countOutline.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
			#extraCmd<-c(extraCmd,"psxy -R -JM -O -K -W4,0 -G<countOutline.dat>>")

			#write.table(homeRanges[,c('X','Y')],'countOutline.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)

			#extraCmd<-c(extraCmd,"psxy -R -JM -O -K -W4,0<countOutline.dat>>")
			#homeRanges<-mcp(goodPoints[,c('coordx','coordy')],rep(1,nrow(goodPoints)),percent=100)

			#write.table(homeRanges[,c('X','Y')],'countOutline.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
			#extraCmd<-c(extraCmd,"psxy -R -JM -O -K -W4,0<countOutline.dat>>")
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
############Specifc Functions############
#####Probably only useful as examples####
#########################################
#---------------------------------------#
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#Function: plotSeason(alldaybrs,'output/','all',limits,zoomLimits,hexMax=100)
#Parameters:
	#data: Data frame containing 'animal','rdate','lat','lon' (e.g. the output from readDouglas)
	#outputPath: Directory location to write the multiple season postscript map generated by GMT 
	#outPutID: Prefix to append to map file names 
	#limits: A vector of (southernMostLatitude,northernMostLatitude,westernMostLongitude,easternMostLongitude) for final hexMap or NULL to select automatically
	#hardLimit: A vector of (southernMostLatitude,northernMostLatitude,westernMostLongitude,easternMostLongitude) for cropping hexs or NULL to not crop
	#hexMax: Sets hexs with counts > hexMax to hexMax (for creating maps with the highest hex counts being something like "100+")
	#stateCount: If true, add commands generated by stateOutput() based on column 'stateDep' for deployment state of each animal (pretty specific command for this project but could be modified for other projects)
	#logCounts:  If true, base color scale on logged hex counts
	#debug: If true, browser() before returning from makeHexs()
	#hexPerDegree: How many hexs should fit horizontally in 1 degree of longitude at the equator
	#seasons: Number of seasons. Currently only accepts 2 ('Nov-Apr','May-Oct') or 4 ('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')
	#maxInterp: Maximum number of missing days to fill between two data points (missing days<=maxInterp)
	#contourFile: If not NULL, run pscontour at contourDepth meters based on this GMT elevation file
	#contourDepth: Depth to draw contour in meters
	#extraCmd: Extra commands to be called from shell e.g. pstext psxy
	#...: Any other arguments passed to hexPlot
#Side Effects: 
	#Generates various intermediary files (detailed in other functions) for use in plotting
	#If daybrs contains column 'stateDep' generates animalTable.csv and dayTable.csv showing number of animal and animal-days from each state
	#Generates postscript hexmap plots in outputPath
#Return: Nothing
plotSeason<-function(data,outputPath="",outputID="",limits=NULL,hardLimit=NULL,hexMax=NULL,stateCount=FALSE,logCounts=FALSE,debug=FALSE,hexPerDegree=4,seasons=4,maxInterp=7,contourFile=NULL,contourDepth=-200,extraCmd=NULL,...){
	if (!seasons %in% c(2,4)) stop(simpleError("Only 2 and 4 seasons currently programmed"))
	seasonNames<-c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')
	if (seasons==2){
		seasonNames<-c('Nov-Apr','May-Oct')
		data$season<-data$season2
	}
	for (i in 1:seasons) {
		message(paste("Working on season",seasonNames[i]))
		selector<-data$season==i
		if (!any(selector)){ 
			message(paste("Skipping season",seasonNames[i]))
			next()
		}
		daybrs<-data[selector,]
		if (is.null(hardLimit)){
			lablat<-limits[4]-1/100*diff(limits[3:4])
			lablon<-limits[1]+1/100*diff(limits[1:2])
		}else{
			lablat<-hardLimit[4]-1/100*diff(hardLimit[3:4])
			lablon<-hardLimit[1]+1/100*diff(hardLimit[1:2])
			limits<-hardLimit
		}
		write.table(c(paste(lablon,lablat,"12 0 1 TL",seasonNames[i],sep=' ')),'month.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
		command<- c(extraCmd,"pstext month.dat -JM -R -O -K -W255o>>")
		hexPlot(daybrs,paste(outputPath,outputID,'season_',i,'.ps',sep=""),hexPerDegree=hexPerDegree,limits=limits,extraCmd=command, hardLimit=hardLimit,hexMax=hexMax,stateCount=stateCount,logCounts=logCounts,debug=debug,maxInterp=maxInterp,contourFile=contourFile,contourDepth=contourDepth,...)
	}
}


#Function:exCmds<-stateOutput(interpStates,limits=hardLimit)
#Generate file for GMT pstext to label states (specific to this project)
#Parameters:
	#data: Data frame containing animal id in 'animal' and state of deployment (only "FL-E", "FL-W", "GA", "LA", "NC", "NY", "SC", "TX", "VA") in {stateCol}
	#stateCol: Column in data containg state of deployment
	#limits: If not NULL, increases font size and moves NY label (specific to this project)
	#showAnimals: If true, label states with "{animals}|{animal-days}" else "{animal-days}"
#Side Effects:
	#If necessary, writes line data for psxy for labelling (floridaWLine.dat) and (floridaLine.dat) dividing Florida and labelling NY (nyLine.dat)
	#Writes animal-days to stateStats.dat for use in GMT pstext
#Return: Extra commands to be passed to runGMT (e.g. extraCmd={output})
stateOutput<-function(data,stateCol="stateDep",limits=NULL,showAnimals=FALSE){
	extraCmds<-NULL
	pos<-data.frame(state=c("FL-E", "FL-W", "GA", "LA", "NC", "NY", "SC", "TX", "VA"), lat=c(29.2,32.3,32.7,30.9,35.9,42.9,34.5,30.3,37.5),lon=c(-81.2,-89.3,-84.5,-93.2,-80,-77,-82.2,-97.8,-79.9),angle=c(-70,0,0,0,0,0,0,0,0)) #FL-W 29.8 -82.9
	if (is.null(limits)){
		size<-6
	}else{
		size<-11
		pos[pos$state=="NY",c('lat','lon')]<-c(41.3,-78.5)
	}
	days<-tapply(data[,stateCol],data[,stateCol],length)
	animals<-tapply(data[,'animal'],data[,stateCol],function(x){length(unique(x))})
	output<-data.frame(state=row.names(days),days,animals)
	output<-merge(output,pos)
	output<-output[!is.na(output$animals),]
	if (showAnimals) output$text<-paste(output$lon,' ',output$lat," ",size," ",output$angle," 1 TL ",output$animals,'|',output$days,sep="")
	else output$text<-paste(output$lon,' ',output$lat," ",size," ",output$angle," 1 TL ",output$days,sep="")
	write.table(output$text,'stateStats.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
	if (any(output$state %in% c("FL-E","FL-W"))){
		write.table(c("-82.7 30.2","-80.85 25.8"),'floridaLine.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
		extraCmds<-c("psxy floridaLine.dat -JM -R -O -K -M -A -W1.5,0,->>")
		if(any(output$state == "FL-W")){
			write.table(c("-87.8 31.5","-83.5 30.15"),'floridaWLine.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
			extraCmds<-c(extraCmds,"psxy floridaWLine.dat -JM -R -O -K -M -A -W1,0>>")
		}
	}
	if (any(output$state == "NY") & !is.null(limits)){
		write.table(c("-77.5 41.3","-74 41.7"),'nyLine.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
		extraCmds<-c(extraCmds,"psxy nyLine.dat -JM -R -O -K -M -A -W1,0>>")
	}
	return(extraCmds)
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


#Function: lagx<-lagger(x)
#Parameters:
	#data: A vector
#Return: A vector of same length as data with each datum moved one to the right and an NA in position 1
lagger<-function(data){
	if (length(data)==1) return(NA)
	if (length(data)==0) return(c())
	return(c(NA,data[1:(length(data)-1)]))
}

#Function: returnCode<-systemOut("ls>>",outputFile,debug=TRUE)
#Parameters:
	#...:Various parameters to be concatenated into a system command
	#debug: If true prints command to screen (and still calls system)
#Return: Return code returned by OS
#Side Effect: Sends concatenated command to system
systemOut<-function(...,debug=FALSE){
	cmd<-paste(...,sep='')
	if (debug)message(cmd)
	returnCode<-system(cmd)
	return(returnCode)
}

#Function: interpolatedDays<-fill.missing.days(animal,date,lats,lons)
#Parameters:
	#id: Vector of length N of animal identifiers (so the last day of one animal is interpolated to the first day of the next)
	#date: Vector of length N of either an rdate or some other integer
	#lat: Vector of length N of latitudes
	#lon: Vector of length N of longitudes
	#maxinterp: Maximum number of missing days to fill between two data points (<=maxinterp)
#Return: Data frame of interpolated date/positions with columns id, data, lat, lon (note: original positions are not returned)
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


#Function: limits<-makeHexs(lats,lons,file="hexs.dat",hexPerDegree=4,hardLimit=c(-60,-40,30,45),hexMax=100)
#Parameters:
	#lat: A vector of latitudes of length N
	#lon: A vector of longitudes of length N
	#hexPerDegree: How many hexs should fit horizontally in 1 degree of longitude at the equator
	#borders: How many degrees of space to leave around the hexs (if hardLimit is NULL)
	#file: File to lat/lon of hex positions to
	#hardLimit: A vector of (southernMostLatitude,northernMostLatitude,westernMostLongitude,easternMostLongitude) (or NULL) to filter hexs 
	#hexMax: Sets hexs with counts > hexMax to hexMax (for creating maps with the highest hex counts being something like "100+")
	#logCounts: If true base color scale on logged hex counts
	#histPrefix: If not NULL, create histogram files of hex counts in {histPrefix}_hist.ps
	#debug: If true, browser() before returning
	#scale: Divide day totals by this number (useful for intervals less than 1 day)
	#allOneColor: Make all hexes a single color e.g. '20/20/20'
	#proportion: Divide counts by sum(counts)
	#showMax: show maximum on scale ticks?
	#weights: Weight for each point
	#uniqueCounter: If not NULL, count unique occurrences of uniqueCounter in a hex
	#addPlus: Add a "plus" to maximum label in scale if using hexMax
#Side Effects: 
	#Writes lat/lon color information to file specified by file parameter in a format ready to use in GMT (but probably easy to parse by other programs too)
	#If histPrefix is not NULL writes histograms to {histPrefix}_hist{XXX}.eps
	#Makes test map of hex positions and data in testhex.eps
	#Makes GMT color table scale.cpt for later use
	#Calls makeTicks() which will output tick position for scalebar for later use
#Return: Vector for use in further functions of (lowXLim,highXlim,lowYLim,highYLim,maxHexCount)
makeHexs<-function(lat,lon,lonBase=-45,hexPerDegree=1,border=5,file="hexs.dat",hardLimit=NULL,hexMax=NULL,logCounts=FALSE,histPrefix=NULL,debug=FALSE,scale=1,allOneColor=NULL,proportion=FALSE,showMax=FALSE,weights=rep(1,length(lat)),uniqueCounter=NULL,addPlus=TRUE){
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
		map(add=TRUE)
	dev.off()
	if (!is.null(histPrefix)){
		postscript(paste(histPrefix,"_hist.eps",sep=""),horizontal=FALSE)
			hist(bin@count,breaks=30,xlab="Turtle-Days in Hex",main=paste(hexPerDegree,"Hexs Per Degree"))
		dev.off()
		postscript(paste(histPrefix,"_under100hist.eps",sep=""),horizontal=FALSE)
			hist(bin@count[bin@count<=100],breaks=100,xlab="Turtle-Days in Hex",main=paste(hexPerDegree,"Hexs Per Degree (Under 100 Days)"))
		dev.off()
		if (any(bin@count>100)){
			postscript(paste(histPrefix,"_over100hist.eps",sep=""),horizontal=FALSE)
				hist(bin@count[bin@count>100],breaks=100,xlab="Turtle-Days in Hex",main=paste(hexPerDegree,"Hexs Per Degree (Over 100 Days)"))
			dev.off()
		}
	}

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
		if(!is.null(allOneColor)) stackoutput[selector,'text']<-paste(">-W0 -G",allOneColor,sep="")
		else stackoutput[selector,'text']<-paste(">-W1/255 -G",stackoutput[selector,'red'],"/",stackoutput[selector,'green'],"/",stackoutput[selector,'blue'],sep="")
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

#Function:countColors<-red2bluehsl(maxCount,minCount)
#Generate color scale for 1:n colors in the hsl color scheme going evenly from hue=.31 to 1 (s=1,v=.5)
#Parameters:
	#n: Maximum count
	#min: Minimum count
#Return: Data frame with rows 1:n and columns 'count','red','green','blue' (rgb columns in 0-255 scale) 
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

#Function:output[i,c('red','green','blue')]<-hsl2rgb(output$hue[i],1,.5)
#Parameters:
	#hue: Hue 0-1 in hsl color scale
	#saturation: Saturation 0-1 in hsl color scale
	#lightness: Lightness 0-1 in hsl color svale
#Return: Vector of red green blue in 0-255 scale
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

#Function:makeTicks(output$count,FALSE,hexMax=100)
#Makes ticks for use in GMT scale bar
#Parameters:
	#counts: Vector of counts to make ticks for
	#isLog: If true counts have previously been transformed round(log(counts)*100)
	#showMax: Add the maximum hex count to the scale and filter out any labels too close to it
	#hexMax: Counts have previously been capped at hexMax. Also adds "+" to {hexMax} label (e.g. "100+") if isLog is false
	#addPlus: Add a "+" to {hexMax} label?
	#addMin: If true makes sure the minimum scale value is at most 1 (0 if isLog is TRUE)
#Side Effects:
	#Writes tick positions to ticks.dat for later use in runGMT
	#Writes labels and label positions to labels.dat for later use in runGMT
#Return: Nothing
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
}

#Function:runGMT(c(-60,-40,30,45),outFile="hexmap.ps",dataFile="hexs.dat",gmtDir="/bin/",contourFile="/data/depth/Gebco_SandS_blend.grd")
#Takes files generated by previous functions and calls mapping program GMT to generate output postscript file
#IMPORTANT: You must have GMT installed for this function to work
#Parameters:
	#limits: Vector of (lowXLim,highXlim,lowYLim,highYLim) probably from makeHexs or passed in from hexPlot
	#outFile: File location to output GMT postscript to
	#width: Width of output figure in inches
	#dataFile: File location of hex color and lat/lon data previously generated by makeHexs
	#gmtDir: System directory for calling GMT functions (e.g. if pscoast is in /home/bin/ and /home/bin/ isn't in path then gmtDir="/home/bin/")
	#scale: If TRUE make a scale based on files output by makeTicks (called in makeHexs)
	#contourFile: If not NULL, run pscontour at contourDepth meters based on this GMT elevation file
	#contourDepth: Depth to draw contour in meters
	#extraCmd: If not NULL, vector of system commands to run after other GMT commands and before scale (e.g. extra labels with pstext)
	#landMaskCmd: If not NULL, vector of system commands to run prior to final psCoast (e.g. polygons overlaid by coast)
	#landMask: If TRUE, hexs overlapping land are covered by land else hexs overlap land and only the land border is plotted over them
	#ps2eps: If TRUE, after all other commands calls the program ps2eps to convert .ps to .eps (Make sure you have ps2eps installed and in path if you use this)
	#scaleLabel: Label for scale if scale is TRUE
	#concatenate: Add output to existing file?
	#xpos: Shift plot origin right xpos inches
	#ypos: Shift plot origin up ypos inches
	#posOptions: Options for position (e.g. -O)
	#gmtsetOptions: Options for gmtset
	#borderColor: color for border of hexes
	#pscoastOptions: command line options for pscoast
#Return: Nothing
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
}

#Makes scale based on ticks.dat and labels.dat
#width: width of plot area
#outFile: plot file
#scaleLabel: title of scale
#margin: space for margin on either side of scale
#scaleHeight: height of scale
makeScale<-function(width,outFile,scaleLabel="Days Turtles Recorded in Hex",margin=1,scaleHeight=.8,xpos=margin/2,ypos=-scaleHeight*1.5,gmtDir=''){
		scaleWidth<-width-margin
		systemOut(gmtDir,'psbasemap -R0/1/0/1 -JX',scaleWidth,'i/',scaleHeight,'i -X',xpos,'i -Y',ypos,'i -K -G255 -O>>',outFile)
		systemOut(gmtDir,'psscale -D',scaleWidth/2,'i/',scaleHeight*.6,'i/',scaleWidth,'i/',scaleHeight*.2,'ih -O -Cscale.cpt -K  -B100000::/::>>',outFile)
		write.table(paste('.5 0.7 12 0 1 CB ',scaleLabel,sep=""),'scalelab.dat',sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
		systemOut(gmtDir,'pstext scalelab.dat -R -JX -O -K>>',outFile)
		systemOut(gmtDir,'psxy ticks.dat -JX -R -O -K -M -A -W1.5/0>>',outFile)
		systemOut(gmtDir,'pstext labels.dat -JX -R -O -K -N>>',outFile)
}
