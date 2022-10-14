# Version 7.0 (March 8, 2011).  Changes:
# 1. Change approach used for path variable.
# 2. Deal with polar day and night situations.
# 3. Correct sep argument in calls to paste.

AstroCalc4R <- function(day,month,year,hour,timezone,
                      lat,lon,withinput=F,path=getwd())
	{
####
# Be sure all arguments have the same length
	if ((length(day)!=length(month))|(length(month)!=length(year))|
		(length(year)!=length(hour))|(length(hour)!=length(timezone))|
		(length(timezone)!=length(lat))|(length(lat)!=length(lon)))
	stop("\n Input vectors are not the same length \n")

# Save n records	
	times<-length(day)
	
####
# Check for NA values
	na.c <- function(x) return(sum(is.na(x))) #simple function to simplify 
	if (sum(na.c(day),na.c(month),na.c(year),na.c(hour),na.c(timezone),na.c(lat),na.c(lon)) > 0) 
		stop("\n NA values in input data \n")

####		
# Check for obviously incorrect data 

# Month and date (considering leap years)
	logic1 <- year < 0
	if (sum(logic1)>0) stop(cat("\n Error in year for record numbers:",(1:times)[logic1]," \n\n"))

	is.leap <- function(x) return ((((x %% 4==0)&(x %% 100 !=0)))|(x %% 400 ==0))
	date.list <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
	
	logic1 <- abs(month-6) > 6
	if (sum(logic1) > 0) stop(cat("\n Error in month for record numbers:",(1:times)[logic1]," \n\n"))
	
	logic1 <- day > (date.list[month] + is.leap(year)*(month==2))
	logic2 <- day <= 0
	if ((sum(logic1) > 0) |(sum(logic2) >0)) stop(cat("\n Incorrect month-day-year combination for record numbers: ",(1:times)[logic1|logic2]," \n\n"))
	
	logic1 <- abs(hour-12) > 12
	if (sum(logic1) > 0) stop(cat("\n Error in hour for record numbers:",(1:times)[logic1]," \n\n"))
	
	logic1 <- abs(timezone)>12
	if (sum(logic1)>0) stop(cat("\n Error in time zone for record numbers:",(1:times)[logic1]," \n\n"))

	logic1 <- abs(lon) > 180
	if (sum(logic1)>0) stop(cat("\n Error in longitude for record numbers:",(1:times)[logic1]," \n\n"))
	
	logic1 <- abs(lat) > 90
	if (sum(logic1)>0) stop(cat("\n Error in latitude for record numbers:",(1:times)[logic1]," \n\n"))	
	

####
# Check to see if the compiled C library is where it should be
	OS <- .Platform$OS.type
	cmd <- dir(path)
	if (OS=="unix") 
		{
				
		if (! "AstroCalc4R.so" %in% cmd) stop(paste("\n AstroCalc4R.so not in path",path))
		} else # windows situ
		{
		
		if (! "AstroCalc4R.dll" %in% cmd) stop(paste("\n AstroCalc4R.dll not in path",path))
		}	

####
# Load the library if it is not already loaded	
	if (!is.loaded("AstroCalc4R"))
		{
		if (OS=="unix") dyn.load(paste(path,"/AstroCalc4R.so",sep="")) else dyn.load(paste(path,"/AstroCalc4R.dll",sep=""))
		}
	
####
# Call the C function	
	x <- .C("AstroCalc4R",nrec=times,
               tzone=as.integer(timezone),
               day=as.integer(day),
               month=as.integer(month),
               year=as.integer(year),
               hhour=as.numeric(hour),
               xlat=as.numeric(lat),
               xlon=as.numeric(lon),
               noon=rep(0.,times),
               sunrise=rep(0.,times),
               sunset=rep(0.,times),
               azimuth=rep(0.,times),
               zenith=rep(0.,times),
               eqtime=rep(0.,times),
               declin=rep(0.,times),
               daylight=rep(0.,times),
               PAR=rep(0.,times) )
               
	logic1 <- is.na(x$sunrise)
	if (sum(logic1)>0) {
    msg<-cat("Warning: Polar day/night (daylength 0 or 24 hrs, PAR=0) in record numbers:",(1:times)[logic1],
             " \n Check input data (i.e. latitude)? ")
    message(msg)
  }
	x$daylight[(1:times)[logic1]] <- ifelse(x$PAR[(1:times)[logic1]]>0,24,0)
	
	if (withinput) return(as.data.frame(x)[,2:17]) else return(as.data.frame(x)[,9:17])
	}

##example:
#setwd("C:\\New_Papers\\DielRefDoc-1\\AstroCalc_C_code\\Version1_0")
#dyn.load("AstroCalc4R.dll")	
#x <- AstroCalc4R(day=c(1,3,27,28),month=c(1,12,2,2),
#               year=c(1900,1950,2000,2010),hour=c(0,7,12,23),
#               timezone=c(-7,-9,-9,-12),lat=c(3,60,-89.9,0),
#               lon=c(-105,-120,0,100),withinput=T)
#print(x)
