library(stringr)
library(lubridate)
library(RNCEP)
library(chron)
library(maptools)
library(lme4) 
library(MuMIn)
library(data.table)
library(mgcv)
library(OpenStreetMap)
library(sp)
library(raster)	
library(rgdal)
library(atlastools)
library(sp)
library(sf)
library(ggplot2)
library(dplyr)

        ### no scientific notation and represent locations with precision
        options(scipen=999, digits=10)
        
        
        
        ###########################################################################
        ############################################################################
        ###                                                                      ###
        ###            MATCH CLIMATIC CONDITIONS TO DEPARTURE TIMES              ###
        ###                   & RESOURCE SELECTION FUNCTION                      ###
        ###                                                                      ###
        ############################################################################
        ############################################################################
        
        
        
        
### Functions

    
    high_tide <- function(d){
      setDT(d)
      d[tides, tide_id := tideID , on = .(departure_time >= high_start_time, departure_time < high_end_time)]
      d <- merge.data.table(d, tides[,c("tideID","high_start_time", "low_time", "high_end_time")], by.x="tide_id", by.y="tideID", all.x=T, all.y=F)
      d$outgoing <- d$departure_time <= d$low_time
      
      d$diff_time_tide <- ifelse(d$outgoing == "TRUE", difftime(d$departure_time, d$high_start_time, units="mins"), difftime(d$departure_time, d$high_end_time, units="mins"))
      d$diff_low <- as.numeric(d$departure_time - d$low_time) 
      d$diff_low <- d$diff_low/60                                                                       
      d
    }


### Data
        
    Sys.setenv(TZ='UTC') # change time zone for session
    options(scipen=999, digits=10)
    
    #Wind
    weather <- read.csv("weather_analyzed_manualgapfill1.csv")
    weather$Date_Time <- as.POSIXct(weather$Date_Time, format="%Y-%m-%d %H:%M")
    
    #Rain
    Rain <- read.csv("Rain_Ter.csv", as.is=TRUE, fileEncoding="UTF-8-BOM")
    Rain$date <- as.POSIXct(paste(Rain$YYYYMMDD, Rain$HH), format="%Y%m%d %H")
    Rain$date <- as.POSIXct(Rain$date)
    
    #Cloud
    cloud <- read.csv("Cloud_vlie.csv")
    cloud$date <- as.POSIXct(paste(cloud$YYYYMMDD, cloud$HH), format="%Y%m%d %H")
    cloud$date <- as.POSIXct(cloud$date)
    
    #Tidal data
    tides <- read.csv("Tides-Griend.csv", fileEncoding="UTF-8-BOM")
    tides$high_start_time<- as.POSIXct(tides$high_start_time, format="%m/%d/%Y %H:%M")
    tides$high_start_time<- as.POSIXct(tides$high_start_time, format="%m/%d/%Y %H:%M")
    tides$high_end_time <- as.POSIXct(tides$high_end_time, format="%m/%d/%Y %H:%M")
    tides$low_time <- as.POSIXct(tides$low_time, format="%m/%d/%Y %H:%M")
    setDT(tides)
  
    #tracking data
    west_ws <- read.csv("WATLAS19+20.csv")
    #selecting only those that actually departed from the western Wadden sea
    west_ws <- west_ws[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42),]
    west_ws$departure_h <- as.POSIXct(west_ws$departure_time)
    west_ws$departure_h <- format(round(west_ws$departure_h, units="hours"), format="%Y-%m-%d %H:%M")
    new_df <- data.frame(west_ws[,c("tag", "departure_5", "departure_h", "departure_time")], real = 1)   #new dataframe with only necessary variables
    new_df$departure_h <- as.POSIXct(new_df$departure_h)
    new_df$departure_5 <- as.POSIXct(new_df$departure_5)
    new_df$departure_time <- as.POSIXct(new_df$departure_time)
    
    #creating new dataframe for our resource selection model
    original_df <- new_df
    random_days = 4 #number of days before departure where random departure times are chosen from
    
    fake <- data.frame() 
    new_df <- original_df
  
    
    for(j in new_df$tag){
      temp <- data.frame(tag=rep(j,each = 30),
                         departure_h = sample(seq(as.numeric(new_df[new_df$tag==j,"departure_h"])-(3600*24*random_days),
                                                  as.numeric(new_df[new_df$tag==j,"departure_h"])-3600, 60),30),
                         real=0)
      
      
      
      fake <- rbind(fake,temp)
    }
    
    fake$departure_time <- as.POSIXct(fake$departure_h, origin = "1970-01-01")
    new_df <- new_df[,c("real", "tag", "departure_time", "departure_5")]
    fake$departure_5 <- round_date(fake$departure_time, unit="10 minutes")
    fake$departure_5 <- as.POSIXct(fake$departure_5, origin = "1970-01-01")
    fake <- fake[,c("real", "tag", "departure_time", "departure_5")]
    
    new_df <- rbind(new_df,fake)
    
    # create new departure_h column 
    new_df$departure_h <- format(round(new_df$departure_5, units="hours"), format="%Y-%m-%d %H:%M")
    new_df$departure_h <- as.POSIXct(new_df$departure_h)

  
  
#### Matching departure conditions to departure times   ####### 
  
    row_id <- match(new_df$departure_h, cloud$date)
    new_df$cloud <- cloud$N[row_id] #cloud cover
    
    row_id <- match(new_df$departure_h, weather$Date_Time)
    new_df$ws <- as.numeric(as.character(weather$Wind_Speed)[row_id]) #wind speed
    
    new_df$winddir <- weather$wd[row_id]
    new_df$atm_pressure <- as.numeric(as.character(weather$Barometer)[row_id]) #atmospheric pressure
    ## change between last hour 
    departure_min1 <- new_df$departure_h-(60*60)
    weatherdep <- as.character(weather$Date_Time)
    westdep <- as.character(departure_min1)
    row_id <- match(westdep, weatherdep)
    atm_pressure1 <- as.numeric(as.character(weather$Barometer)[row_id])
    new_df$atm_pressure <- as.numeric(as.character(new_df$atm_pressure))
    new_df$change_atm1 <- new_df$atm_pressure - atm_pressure1
    
    row_id <- match(new_df$departure_h, Rain$date)
    new_df$rain <- Rain$Rain[row_id] #rain 
    
  
    tide <- high_tide(new_df)
    tidedep <- as.character(tide$departure_time)
    westdep <- as.character(new_df$departure_time)
    row_id <- match(westdep, tidedep)
    new_df$time_to_lowtide <- tide$diff_low[row_id] #low-tide
    new_df$time_to_hightide <- tide$diff_time_tide[row_id] #high-tide
    
       ### Wind assistance according to Shamoun-Baranes et al. 2007 
        row_id <- match(new_df$tag, west_ws$tag)
        new_df$flight_dir <- west_ws$flight_dir[row_id]
        
        ## add ground speed to west_ws
        tag <- as.character(new_df$tag)
        groundtag <- as.character(west_ws$tag)
        row_id <- match(tag, groundtag)
        new_df$ground_speed <- west_ws$ground_speed[row_id]
        
        #CALCULATE HEADING FROM WIND DIRECTION AND FLIGHT DIRECTION
        winddir <- as.numeric(as.character(new_df$winddir))
        wind_direction_radian <- winddir*(pi/180) #conversion to radians
        flight_dir_radian <- new_df$flight_dir*(pi/180)
        
        sflight<- sin(flight_dir_radian)#calculate sinus and cosinus of track and wind direction
        cflight <- cos(flight_dir_radian)
        swind <- sin(wind_direction_radian)
        cwind <- cos(wind_direction_radian)
        
        
        #use formulas from the Shamoun-Baranes et al. 2007 to calculate xa and ya components that define
        #vector of heading and airspeed
        new_df$ws <- as.numeric(as.character(new_df$ws))
        xa <- (new_df$ground_speed*sflight)-(new_df$ws*swind)
        ya <- (new_df$ground_speed*cflight)-(new_df$ws*cwind)
        
        heading <- atan2(y=ya,x=xa)
        
        airspeed<-sqrt((xa^2)+(ya^2)) #calculate  airspeed (m/s)
        heading <- heading*(180/pi)#formula for conversion back to angles
        
        #directional measurements
        heading <- ifelse(heading<0, 360+heading, heading)
        
        ## calculate u and v components of wind based on speed + direction
        u_wind <- new_df$ws*cos(wind_direction_radian)
        v_wind <- new_df$ws*sin(wind_direction_radian)
        wind_assistance <- NCEP.Tailwind(u_wind,v_wind,heading,airspeed)
        new_df$wind_assistance <- wind_assistance[,1]
        new_df[new_df$ws==0 | is.na(new_df$wind_assistance),"wind_assistance"] <-0 
        
            #year of tracking
            tag <- as.character(new_df$tag)
            groundtag <- as.character(west_ws$tag)
            row_id <- match(tag, groundtag)
            new_df$year <- west_ws$db[row_id]
            
                  ### Minutes after sunset
                  new_df$departure_time <- format(new_df$departure_time,format='%Y%m%d %H:%M:%S')
                  new_df$time <- str_split(new_df$departure_time, " ", simplify=TRUE)[,2]
                  new_df$time <- as.times(new_df$time)
                  new_df$hours <- hours(new_df$time)
                  new_df$departure_time <- as.POSIXct(new_df$departure_time, format="%Y%m%d %H:%M:%S")
                  new_df$new_dep <-  ifelse(new_df$hours < 12, new_df$departure_time-86400, new_df$departure_time)
                  new_df$new_dep <- as.POSIXct(new_df$new_dep, origin="1970-01-01")
                  crds <- SpatialPoints(matrix(c(5.254321,53.251235), nrow=1),proj4string=CRS("+proj=longlat +datum=WGS84"))
                  sun <- sunriset(crds, new_df$new_dep, direction = "sunset", POSIXct.out = TRUE)
                  new_df$solarpos<- solarpos(crds,new_df$new_dep, POSIXct.out = TRUE)[,2]
                  new_df$sunrise_time <- sun$time
                  #match toevoegn
                  new_df$min_after_sun <- difftime(new_df$departure_time, new_df$sunrise_time, units="mins")
                  new_df$min_after_sun <- as.numeric(new_df$min_after_sun)
                  
  
    ## add weights
    new_df$w <- 1000^(1-new_df$real)
    
    #new dataframe with only necessary variables
    ws_df <- new_df[ , c("real", "w", "tag", "year", "departure_time", "wind_assistance", "time_to_lowtide", "time_to_hightide", "cloud", "rain", "change_atm1", "min_after_sun")]
    
    #change NA to 0 
    ws_df[is.na(ws_df)] <- 0 

    
    
    
##### MODEL ###### 

model <- gam(real~ year + wind_assistance + cloud + change_atm1+
               s(time_to_hightide, k=12) + s(min_after_sun)  + rain,
             family=binomial(link="logit"),
             select=T,
             weights=w, 
             data=ws_df, 
             method="REML") 
    
    
    

    
    
##### RESOURCE SELECTION FUNCTION PLOTS ####### 
    
    par(mfrow=c(2,3)) 
    
    
    
    
    ########################################
    #######   MINUTES AFTER SUNSET  ########
    ########################################
    
    
    x="min_after_sun"				# column original predictor 
    dframe=ws_df		# original uad dataframe
    Ylim=NA			# limit for y-axis
    ylab=NULL		# label for y-axis	
    xlab=NULL		# label for x-axis	
    panel="A"		# panel label of plot
    EXP=T			# back transform by taking the exponent
    CEX=1			# size of symbols in plot 
    scale=T 		# scale response between 0 and 1
    
    if(is.null(ylab)){ylab="Resource selection"}
    if(is.null(xlab)){xlab=x}
    
    N<-100	# number of prediction points
    pred.frame.m<-data.frame(id=1:N)
    pred.frame.m <- expand.grid(wind_assistance = median(ws_df$wind_assistance),  
                                year="atlas2019", 
                                cloud = 8, 
                                rain= median(ws_df$rain), 
                                change_atm1 = median(ws_df$change_atm1),
                                min_after_sun = seq(min(dframe[,"min_after_sun"]), 
                                                    max(dframe[,"min_after_sun"]), length.out=N), 
                                time_to_hightide = median(ws_df$time_to_hightide))
    
    
    tmp<-predict(model, pred.frame.m, type="link", se.fit=TRUE)
    
    pred.frame.m$pred2<-tmp$fit - coef(model)[1] 
    pred.frame.m$se<-tmp$se.fit  
    pred.frame.m$ul<-(pred.frame.m$pred2 + 1*pred.frame.m$se)
    pred.frame.m$ll<-(pred.frame.m$pred2 - 1*pred.frame.m$se)  
    
    # take exponent
    if(EXP==T){
      pred.frame.m$pred2e<-exp(pred.frame.m$pred2)
      pred.frame.m$ule<-exp(pred.frame.m$ul)
      pred.frame.m$lle<-exp(pred.frame.m$ll)
    }
    
    if(scale==T){
      MAX<-max(pred.frame.m$ule)
      pred.frame.m$pred2s<-(pred.frame.m$pred2e)/MAX
      pred.frame.m$uls<-(pred.frame.m$ule)/MAX
      pred.frame.m$lls<-(pred.frame.m$lle)/MAX
    }
    

    ## make plot
    collint<-"grey"
    RGBi<-rgb(col2rgb(collint)[1,1],col2rgb(collint)[2,1],col2rgb(collint)[3,1], alpha=90, max=255)
    
    if(is.na(Ylim[1])){
      Ylim=c(min(pred.frame.m$lls),max(pred.frame.m$uls))
    }
    
    #plot main region
    plot((pred.frame.m$uls) ~ pred.frame.m[,"min_after_sun"], xaxt="n", type="n", ylim=c(0,1),xlab="Time relative to sunset (min)", ylab=ylab, xpd=NA, cex.lab=1.6, cex.axis=1.6, family="serif")
    tickslab<-pretty(ws_df$min_after_sun)	
    axis(1,at=tickslab, cex.lab=4, family="serif", cex.axis=1.6)
    polygon(x = c((pred.frame.m$min_after_sun) ,rev(pred.frame.m$min_after_sun)), y = c(pred.frame.m$lls, rev(pred.frame.m$uls)), col = RGBi, border = RGBi)
    
    
    lines((pred.frame.m$pred2s)~pred.frame.m$min_after_sun, lwd=2)
    
    mtext(panel,padj=2,adj=0.98, family="serif")
    
    axis(3,at=ws_df$min_after_sun[ws_df$real==1],labels=NA)
   
    ########################################
    #######     WIND ASSISTANCE     ########
    ########################################
    
    
    x="wind_assistance"				# column original predictor (e.g. "time_to_high_tide")
    dframe=ws_df		# original uad dataframe
    Ylim=NA			# limit for y-axis
    ylab=NULL		# label for y-axis	
    xlab=NULL		# label for x-axis	
    panel="B"		# panel label of plot
    EXP=T			# back transform by taking the exponent
    CEX=1			# size of symbols in plot 
    scale=T 		# scale response between 0 and 1
    
    
    n=2	# zodat orthog. polynomials goed gemaakt worden
    if(is.null(ylab)){ylab="Resource selection"}
    if(is.null(xlab)){xlab=x}
    
    # require(AICcmodavg)	# voor de functie: 
    N<-100	# number of prediction points
    pred.frame.w<-data.frame(id=1:N)
    pred.frame.w<- expand.grid(year="atlas2019", 
                               cloud = 8, 
                               rain= median(ws_df$rain), 
                               change_atm1 = median(ws_df$change_atm1),
                               min_after_sun = median(ws_df$min_after_sun), 
                               time_to_hightide = median(ws_df$time_to_hightide),
                               wind_assistance = seq(min(dframe[,"wind_assistance"]), 
                                                     max(dframe[,"wind_assistance"]), length.out=N))
    
    
    tmp<-predict(model, pred.frame.w, type="link", se.fit=TRUE)
    
    pred.frame.w$pred2<-tmp$fit - coef(model)[1] 
    pred.frame.w$se<-tmp$se.fit  
    pred.frame.w$ul<-(pred.frame.w$pred2 + 1*pred.frame.w$se) 
    pred.frame.w$ll<-(pred.frame.w$pred2 - 1*pred.frame.w$se)  
    
    
    # take exponent
    if(EXP==T){
      pred.frame.w$pred2e<-exp(pred.frame.w$pred2)
      pred.frame.w$ule<-exp(pred.frame.w$ul)
      pred.frame.w$lle<-exp(pred.frame.w$ll)
    }
    
    if(scale==T){
      MAX<-max(pred.frame.w$ule)
      pred.frame.w$pred2s<-(pred.frame.w$pred2e)/MAX
      pred.frame.w$uls<-(pred.frame.w$ule)/MAX
      pred.frame.w$lls<-(pred.frame.w$lle)/MAX
    }
    
    
    
    
    ## make plot
    collint<-"grey"
    RGBi<-rgb(col2rgb(collint)[1,1],col2rgb(collint)[2,1],col2rgb(collint)[3,1], alpha=90, max=255)
    
    if(is.na(Ylim[1])){
      Ylim=c(min(pred.frame.w$lls),max(pred.frame.w$uls))
      #  Ylim=c(min(pred.frame.m$lle),max(pred.frame.m$ule))
    }
    
    #plot main region
    plot((pred.frame.w$uls) ~ pred.frame.w$wind_assistance, xaxt="n", type="n", ylim=c(0,1), xlim=c(-8,8), xlab="Wind assistance (m/s)", ylab=ylab, xpd=NA, cex.lab=1.6, cex.axis=1.6, family="serif")
    tickslab<-pretty(ws_df$wind_assistance)	# pretty original x-axis values
    #ticks<-poly(tickslab, n, coefs = attr(polymodel, "coefs")) # get transformed x-axis values using earlier ploynomial model
    # axis(1,at=ticks[,1], labels=tickslab)
    axis(1,at=tickslab, family="serif", cex.axis=1.6)
    
    polygon(x = c((pred.frame.w$wind_assistance),rev(pred.frame.w$wind_assistance)), y = c(pred.frame.w$lls, rev(pred.frame.w$uls)), col = RGBi, border = RGBi)
    
    # polygon(x = c(pred.frame.w[,paste(x,1,sep="")],rev(pred.frame.w[,paste(x,1,sep="")])), y = c(pred.frame.w$lls, rev(pred.frame.w$uls)), col = RGBi, border = RGBi)
    # lines((pred.frame.w$pred2s)~pred.frame.w[,paste(x,1,sep="")], lwd=2)
    # 
    lines((pred.frame.w$pred2s)~pred.frame.w$wind_assistance, lwd=2)
    
    
    mtext(panel,padj=2,adj=0.98, family="serif")
    
    
    #lines(x=c(7.73, 7.73, 7.73, 7.73, 7.73, 7.73, 7.73, 7.73, 7.73, 7.73, 7.73), y=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7, 0.8, 0.9, 1), type="l", lty=2, col="red")
    axis(3, at=ws_df$wind_assistance[ws_df$real==1], labels=NA)
    
    #########################
    #######   CLOUD  ########
    #########################
    
    x="cloud cover"				# column original predictor (e.g. "time_to_high_tide")
    dframe=ws_df		# original uad dataframe
    Ylim=NA			# limit for y-axis
    ylab=NULL		# label for y-axis	
    xlab=NULL		# label for x-axis	
    panel="C"		# panel label of plot
    EXP=T			# back transform by taking the exponent
    CEX=1			# size of symbols in plot 
    scale=T 		# scale response between 0 and 1
    
    
    if(is.null(ylab)){ylab="Resource selection"}
    if(is.null(xlab)){xlab=x}
    
    N<-100	
    pred.frame.c<-data.frame(id=1:N)
    pred.frame.c <- expand.grid(wind_assistance = median(ws_df$wind_assistance),  
                                year="atlas2019", 
                                cloud = seq(min(dframe[,"cloud"]), 
                                            max(dframe[,"cloud"]), length.out=N),
                                rain= median(ws_df$rain), 
                                change_atm1 = median(ws_df$change_atm1),
                                min_after_sun = median(ws_df$min_after_sun), 
                                time_to_hightide = median(ws_df$time_to_hightide))
    
    
    
    tmp<-predict(model, pred.frame.c, type="link", se.fit=TRUE)
    
    pred.frame.c$pred2<-tmp$fit - coef(model)[1] 	# 
    pred.frame.c$se<-tmp$se.fit  
    pred.frame.c$ul<-(pred.frame.c$pred2 + 1*pred.frame.c$se) ##95% CI interval
    pred.frame.c$ll<-(pred.frame.c$pred2 - 1*pred.frame.c$se)  ##95% CI
    
    
    # take exponent
    if(EXP==T){
      pred.frame.c$pred2e<-exp(pred.frame.c$pred2)
      pred.frame.c$ule<-exp(pred.frame.c$ul)
      pred.frame.c$lle<-exp(pred.frame.c$ll)
    }
    
    if(scale==T){
      MAX<-max(pred.frame.c$ule)
      pred.frame.c$pred2s<-(pred.frame.c$pred2e)/MAX
      pred.frame.c$uls<-(pred.frame.c$ule)/MAX
      pred.frame.c$lls<-(pred.frame.c$lle)/MAX
    }
    
  
    ## make plot
    collint<-"grey"
    RGBi<-rgb(col2rgb(collint)[1,1],col2rgb(collint)[2,1],col2rgb(collint)[3,1], alpha=90, max=255)
    
    if(is.na(Ylim[1])){
      Ylim=c(min(pred.frame.c$lls),max(pred.frame.c$uls))

    }
    
    #plot main region
    plot((pred.frame.c$uls) ~ pred.frame.c$cloud, xaxt="n", type="n", ylim=c(0,1),xlab="Cloud cover", ylab=ylab, xpd=NA, cex.lab=1.6, cex.axis=1.6, family="serif")
    tickslab<-pretty(ws_df$cloud)	# pretty original x-axis values
    axis(1,at=tickslab, family="serif", cex.axis=1.6)
    polygon(x = c(pred.frame.c$cloud,rev(pred.frame.c$cloud)), y = c(pred.frame.c$lls, rev(pred.frame.c$uls)), col = RGBi, border = RGBi)
    lines((pred.frame.c$pred2s)~pred.frame.c$cloud, lwd=2) 
    mtext(panel,padj=2, adj=0.98, family="serif")
 
    
    axis(3, at=cloud$cover1, labels=NA)
  
    
    
    
    
    ########################################
    #######     TIME TO HIGHTIDE    ########
    ########################################
    
    
    x="time_to_hightide"				# column original predictor (e.g. "time_to_high_tide")
    dframe=ws_df		# original uad dataframe
    Ylim=NA			# limit for y-axis	
    ylab=NULL		# label for y-axis	
    xlab=NULL		# label for x-axis	
    panel="D"		# panel label of plot
    EXP=T			# back transform by taking the exponent
    CEX=1			# size of symbols in plot 
    scale=T 		# scale response between 0 and 1
    
    
    #n=2	# zodat orthog. polynomials goed gemaakt worden
    if(is.null(ylab)){ylab="Resource selection"}
    if(is.null(xlab)){xlab=x}
    
   
    N<-100	# number of prediction points
    pred.frame.t<-data.frame(id=1:N)
    
    
    pred.frame.t <- expand.grid(wind_assistance = median(ws_df$wind_assistance),  year="atlas2019", 
                                cloud = 8, 
                                rain= median(ws_df$rain), 
                                change_atm1 = median(ws_df$change_atm1),
                                min_after_sun = median(ws_df$min_after_sun), 
                                time_to_hightide = seq(min(dframe[,"time_to_hightide"]), max(dframe[,"time_to_hightide"]), length.out=N)) 
    
    
    
    tmp<-predict(model, pred.frame.t, type="link", se.fit=TRUE)
    
    pred.frame.t$pred2<-tmp$fit - coef(model)[1] 	# zonder de intercept of niet?
    pred.frame.t$se<-tmp$se.fit  
    
    pred.frame.t$ul<-(pred.frame.t$pred2 + 1*pred.frame.t$se)
    pred.frame.t$ll<-(pred.frame.t$pred2 - 1*pred.frame.t$se)
    
    
    # take exponent
    if(EXP==T){
      pred.frame.t$pred2e<-exp(pred.frame.t$pred2)
      pred.frame.t$ule<-exp(pred.frame.t$ul)
      pred.frame.t$lle<-exp(pred.frame.t$ll)
    }
    
    if(scale==T){
      MAX<-max(pred.frame.t$ule)
      pred.frame.t$pred2s<-(pred.frame.t$pred2e)/MAX
      pred.frame.t$uls<-(pred.frame.t$ule)/MAX
      pred.frame.t$lls<-(pred.frame.t$lle)/MAX
    }
    
    
    collint<-"grey"
    RGBi<-rgb(col2rgb(collint)[1,1],col2rgb(collint)[2,1],col2rgb(collint)[3,1], alpha=90, max=255)
    
    
    Ylim=c(min(pred.frame.t$lls),max(pred.frame.t$uls))
    
    plot((pred.frame.t$uls) ~ pred.frame.t[,"time_to_hightide"], xaxt="n", type="n", ylim=c(0,1),xlab="Time relative to hightide (min)", ylab=ylab, xpd=NA, cex.lab=1.6, cex.axis=1.6, family="serif")
    tickslab<-pretty(ws_df$time_to_hightide)
    axis(1,at=tickslab, family="serif", cex.axis=1.6)
    polygon(x = c(pred.frame.t[,"time_to_hightide"],rev(pred.frame.t[,"time_to_hightide"])), y = c(pred.frame.t$lls, rev(pred.frame.t$uls)), col = RGBi, border = RGBi)
    lines((pred.frame.t$pred2s)~pred.frame.t[,"time_to_hightide"], lwd=2, type="l")
    mtext(panel,padj=2,adj=0.98, family="serif")
    
    
    axis(3, at=ws_df$time_to_hightide[ws_df$real==1], labels=NA)
    
    
    
    
    ################################################
    ####### CHANGE IN ATMOSPHERIC PRESSURE  ########
    ################################################
    
    
    x="Change Atmospheric pressure (1 hour)"				# column original predictor (e.g. "time_to_high_tide")
    dframe=ws_df		# original uad dataframe
    Ylim=NA			# limit for y-axis
    ylab=NULL		# label for y-axis	
    xlab=NULL		# label for x-axis	
    panel="E"		# panel label of plot
    EXP=T			# back transform by taking the exponent
    CEX=1			# size of symbols in plot 
    scale=T 		# scale response between 0 and 1
    
    
    
    if(is.null(ylab)){ylab="Resource selection"}
    if(is.null(xlab)){xlab=x}
    
    
    N<-100	# number of prediction points
    pred.frame.a<-data.frame(id=1:N)
    pred.frame.a <- expand.grid(wind_assistance = median(ws_df$wind_assistance),  
                                year="atlas2019", 
                                cloud = 8,
                                rain= median(ws_df$rain), 
                                change_atm1 = seq(min(dframe[,"change_atm1"]), 
                                                   max(dframe[,"change_atm1"]), length.out=N),
                                min_after_sun = median(ws_df$min_after_sun), 
                                time_to_hightide = median(ws_df$time_to_hightide))
    
    
    
    tmp<-predict(model, pred.frame.a, type="link", se.fit=TRUE)
    
    pred.frame.a$pred2<-tmp$fit - coef(model)[1] 	# zonder de intercept of niet?
    pred.frame.a$se<-tmp$se.fit  
    pred.frame.a$ul<-(pred.frame.a$pred2 + 1*pred.frame.a$se) ##95% CI interval
    pred.frame.a$ll<-(pred.frame.a$pred2 - 1*pred.frame.a$se)  ##95% CI
    
    
    
    # take exponent
    if(EXP==T){
      pred.frame.a$pred2e<-exp(pred.frame.a$pred2)
      pred.frame.a$ule<-exp(pred.frame.a$ul)
      pred.frame.a$lle<-exp(pred.frame.a$ll)
    }
    
    if(scale==T){
      MAX<-max(pred.frame.a$ule)
      pred.frame.a$pred2s<-(pred.frame.a$pred2e)/MAX
      pred.frame.a$uls<-(pred.frame.a$ule)/MAX
      pred.frame.a$lls<-(pred.frame.a$lle)/MAX
    }
    
    
    
    
    ## make plot
    collint<-"grey"
    RGBi<-rgb(col2rgb(collint)[1,1],col2rgb(collint)[2,1],col2rgb(collint)[3,1], alpha=90, max=255)
    
    if(is.na(Ylim[1])){
      Ylim=c(min(pred.frame.a$lls),max(pred.frame.a$uls))
      #  Ylim=c(min(pred.frame.m$lle),max(pred.frame.m$ule))
    }
    
    #plot main region
    plot((pred.frame.a$uls) ~ pred.frame.a$change_atm1, xaxt="n", type="n", ylim=c(0,1),xlab=xlab, ylab=ylab, cex.lab=1.6, cex.axis=1.6, family="serif")
    tickslab<-pretty(ws_df$change_atm1)	# pretty original x-axis values
    #ticks<-poly(tickslab, n, coefs = attr(polymodel, "coefs")) # get transformed x-axis values using earlier ploynomial model
    axis(1,at=tickslab, family="serif", cex.axis=1.6)
    polygon(x = c(pred.frame.a$change_atm1,rev(pred.frame.a$change_atm1)), y = c(pred.frame.a$lls, rev(pred.frame.a$uls)), col = RGBi, border = RGBi)
    lines((pred.frame.a$pred2s)~pred.frame.a$change_atm1, lwd=2, type="l", lty=2)
    mtext(panel,padj=2,adj=0.98, family="serif")
    
    axis(3, at=ws_df$change_atm1[ws_df$real==1], labels=NA)
    
    
    
    #########################
    #######   RAIN   ########
    #########################
    
    x="Rain"				# column original predictor (e.g. "time_to_high_tide")
    dframe=ws_df		# original uad dataframe
    Ylim=NA			# limit for y-axis
    ylab=NULL		# label for y-axis	
    xlab=NULL		# label for x-axis	
    panel="F"		# panel label of plot
    EXP=T			# back transform by taking the exponent
    CEX=1			# size of symbols in plot 
    scale=T 		# scale response between 0 and 1
    
    
    
    if(is.null(ylab)){ylab="Resource selection"}
    if(is.null(xlab)){xlab=x}
    
    N<-100	# number of prediction points
    pred.frame.r<-data.frame(id=1:N)
    pred.frame.r<- expand.grid(wind_assistance = median(ws_df$wind_assistance),  
                               year="atlas2019", 
                               cloud = 8,
                               rain= seq(min(dframe[,"rain"]), max(dframe[,"rain"]), 
                                         length.out=N), 
                               change_atm1 = median(ws_df$change_atm1),
                               min_after_sun = median(ws_df$min_after_sun), 
                               time_to_hightide = median(ws_df$time_to_hightide))
    
    
    
    
    tmp<-predict(model, pred.frame.r, type="link", se.fit=TRUE)
    
    pred.frame.r$pred2<-tmp$fit - coef(model)[1] 	# zonder de intercept of niet?
    pred.frame.r$se<-tmp$se.fit  
    pred.frame.r$ul<-(pred.frame.r$pred2 + 1.*pred.frame.r$se) ##95% CI interval
    pred.frame.r$ll<-(pred.frame.r$pred2 - 1.*pred.frame.r$se)  ##95% CI
    
    
    
    # take exponent
    if(EXP==T){
      pred.frame.r$pred2e<-exp(pred.frame.r$pred2)
      pred.frame.r$ule<-exp(pred.frame.r$ul)
      pred.frame.r$lle<-exp(pred.frame.r$ll)
    }
    
    if(scale==T){
      MAX<-max(pred.frame.r$ule)
      pred.frame.r$pred2s<-(pred.frame.r$pred2e)/MAX
      pred.frame.r$uls<-(pred.frame.r$ule)/MAX
      pred.frame.r$lls<-(pred.frame.r$lle)/MAX
    }
    
    
    
    
    ## make plot
    collint<-"grey"
    RGBi<-rgb(col2rgb(collint)[1,1],col2rgb(collint)[2,1],col2rgb(collint)[3,1], alpha=90, max=255)
    
    if(is.na(Ylim[1])){
      Ylim=c(min(pred.frame.r$lls),max(pred.frame.r$uls))

    }
    
    #plot main region
    plot((pred.frame.r$uls) ~ pred.frame.r$rain, xaxt="n", type="n", ylim=Ylim,xlab="Rain (mm/h)", ylab=ylab, xpd=NA, cex.lab=1.6, cex.axis=1.6, family="serif")
    tickslab<-pretty(ws_df$rain)
    axis(1,at=tickslab, family="serif", cex.axis=1.6)
    polygon(x = c(pred.frame.r$rain,rev(pred.frame.r$rain)), y = c(pred.frame.r$lls, rev(pred.frame.r$uls)), col = RGBi, border = RGBi)
    lines((pred.frame.r$pred2s)~pred.frame.r$rain, lwd=2, type="l", lty=2)
    mtext(panel,padj=2,adj=0.98, family="serif")
    
    axis(3, at=ws_df$rain[ws_df$real==1], labels=NA)
    
    
    
    
    
    
