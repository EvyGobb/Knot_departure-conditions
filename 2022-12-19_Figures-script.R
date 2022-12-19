
###########################################################################
############################################################################
###                                                                      ###
###                                FIGURES                               ###
###                                                                      ###
###                                                                      ###
############################################################################
############################################################################
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


#### Wind-rose with windsspeed 

        setwd("C:\\Users\\egobb\\Documents\\WUR\\NIOZ\\Data\\Output\\") 
        ws_df <- read.csv("Wind-rose-data.csv", as.is=TRUE)
        real <- ws_df[ws_df$real==1,]
        fake <- ws_df[ws_df$real==0,]
        
        library(openair)
        
##Figure 1A        
          windr <- windRose(real, ws="ws", wd="winddir",
                          paddle=FALSE,
                          breaks=c(0,2,4,6,8),
                          annotate=FALSE,
                          grid.line = 10,
                          key=list(labels=c("0-2",
                                            "2-4",
                                            "4-6",
                                            "6-8")),
                          cols=c("#D3D3D3", "#808080","#696969","#708090"),
                          main="Wind speed and direction at flight departure times",
                          key.position="bottom")
        
## Figure 1B       
            windf <- windRose(fake, ws="ws", wd="winddir",
                          paddle=FALSE,
                          breaks=c(0,2,4,8,10,12),
                          annotate=FALSE,
                          grid.line = 10,
                          key=list(labels=c("0-2",
                                            "2-4",
                                            "4-6",
                                            "6-8",
                                            "8-10",
                                            "10-12")),
                          cols=c("#D3D3D3", "#A9A9A9","#808080","#696969", "#708090", "#2F4F4F"),
                          main="Wind speed and direction at random times",
                          key.position="bottom")
        


### Figure 2
##### RESOURCE SELECTION FUNCTION PLOTS ####### 
        
        par(mfrow=c(2,3)) 
        
        
        #### First run Gobbens_Final-R-script, end on line 704: 
        # model <- gam(real~ year + wind_assistance + cloud + change_atm1+
        # s(time_to_hightide, k=12) + s(min_after_sun)  + rain,
        # family=binomial(link="logit"),
        # select=T,
        # weights=w, 
        # data=ws_df, 
        # method="REML") 
        
        
        
        
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
        
        
        
## Figure 1C        
## Westward departures map 
## tracking data available upon request 
        
        
        # source("~\\WUR\\NIOZ\\R scripts\\Allert\\function-auxiliary.r")
        # 
        # ### get path and files 
        # 
        # ##laptop
        # files_all <- list.files(path, pattern="*.csv", full.names=TRUE)
        # files<-files_all
        # ldf19 <- lapply(files, fread, skip=0, header=TRUE, na.strings="NA")
        # ## remove empty tracks
        # min_locs<-1 	# select only if at least this many locations 
        # ldf_n <- lapply(ldf19, nrow)
        # n<-unlist(ldf_n)
        # ldf19<-ldf19[n>=min_locs]
        # 
        # 
        # for(i in 1:length(ldf19)){
        #   ldf19[[i]]$TIME <- (ldf19[[i]][["TIME"]]/1000)
        # }
        # 
        # for(i in 1:length(ldf19)){
        #   ts <- as.POSIXct(ldf19[[i]][["TIME"]], origin="1970-01-01")
        #   ldf19[[i]]$ts <- ts
        # }
        # 
        # ## Get tagID of only 3 numbers    
        # for(i in 1:length(ldf19)){
        #   ldf19[[i]]$id <- (ldf19[[i]][["TAG"]]-31001000000)
        # }
        # 
        # 
        # ############ FILTER DATA ####################### 
        # 
        # ## Remove attraction box points ##
        # 
        # filtered_data <- function(d){
        #   d <- atl_filter_bounds(data = d ,
        #                          x = "X", y = "Y",
        #                          x_range = c(639470 , 639471), y_range = c(5887143, 5887144),
        #                          remove_inside = TRUE)
        #   d
        # }
        # ldf19_filter <- lapply(ldf19, filtered_data)    
        # 
        # ## Calculate speed
        # 
        # get_speed <- function(d){
        #   d$speed <- atl_get_speed(d,
        #                            x = "X", y = "Y", 
        #                            time = "ts", 
        #                            type = c("in"))
        #   d
        # }
        # ldf19_speed <- lapply(ldf19_filter, get_speed)    
        # 
        # ## Speed threshold of 100 m/s, SD threshold of 150
        # 
        # filter_speed <- function(d){
        #   d <- atl_filter_covariates(data=d, 
        #                              filters=c("speed<100", 
        #                                        "SD < 150"))
        #   d
        # }
        # 
        # 
        # 
        # ldf19_speedfilter <- lapply(ldf19_speed, filter_speed)
        # 
        # ## Some dataframes do not meet these criteria, and therefore all datapoints were removed
        # ## Delete all dataframes that have less than 1 line 
        # min_locs<-1 	# select only if at least this many locations 
        # ldf_n <- lapply(ldf19_speedfilter, nrow)
        # n<-unlist(ldf_n)
        # ldf19<-ldf19_speedfilter[n>=min_locs]
        # 
        # ## median smoother of 5
        # 
        # med_smooth <- function(d){
        #   d <- atl_median_smooth(data = d , 
        #                          x = "X", y = "Y",
        #                          time = "ts",
        #                          moving_window = 5)
        #   d
        # }
        # ldf19_f <- lapply(ldf19, med_smooth)
        # 
        # ## Extract last 20 minutes of data 
        # 
        # last_20min <- function(d){
        #   end <- max(d$TIME)
        #   start <- end-1200
        #   sub <- subset(d, d$TIME>start)
        # }
        # ldf_20min <- lapply(ldf19_f, last_20min)
        # 
        # 
        # last_hour <- function(d){
        #   end <- max(d$TIME)
        #   start <- end-3600
        #   sub <- subset(d, d$TIME>start)
        # }
        # ldf_hour <- lapply(ldf19_f, last_hour)
        # 
        # ## Loading in data  
        # path<-"~\\WUR\\NIOZ\\Data\\2020\\"
        # files<- list.files(path, pattern="*.csv", full.names=TRUE)
        # 
        # ldf20 <- lapply(files, fread, skip=0, header=TRUE, na.strings="NA")
        # 
        # ## Selecting only the dataframes with at least 2 rows of data, since we cannot calculate speed with one point    
        # min_locs<-2 	# select only if at least this many locations 
        # ldf_n <- lapply(ldf20, nrow)
        # n<-unlist(ldf_n)
        # ldf20<-ldf20[n>=min_locs]
        # 
        # 
        # ## Time column needs to be divided by 1000    
        # for(i in 1:length(ldf20)){
        #   ldf20[[i]]$TIME <- (ldf20[[i]][["TIME"]]/1000)
        # }
        # 
        # 
        # ## Convert time column to timestamp     
        # for(i in 1:length(ldf20)){
        #   ts <- as.POSIXct(ldf20[[i]][["TIME"]], origin="1970-01-01")
        #   ldf20[[i]]$ts <- ts
        # }
        # 
        # ## Get tagID of only 3 numbers    
        # for(i in 1:length(ldf20)){
        #   ldf20[[i]]$id <- (ldf20[[i]][["TAG"]]-31001000000)
        # }
        # 
        # 
        # ############ FILTER DATA ####################### 
        # 
        # ## Remove attraction box points ##
        # 
        # filtered_data <- function(d){
        #   d <- atl_filter_bounds(data = d ,
        #                          x = "X", y = "Y",
        #                          x_range = c(639470 , 639471), y_range = c(5887143, 5887144),
        #                          remove_inside = TRUE)
        #   d
        # }
        # ldf20_filter <- lapply(ldf20, filtered_data)    
        # 
        # ## Calculate speed
        # 
        # get_speed <- function(d){
        #   d$speed <- atl_get_speed(d,
        #                            x = "X", y = "Y", 
        #                            time = "ts", 
        #                            type = c("in"))
        #   d
        # }
        # ldf20_speed <- lapply(ldf20_filter, get_speed)    
        # 
        # ## Speed threshold of 100 m/s, SD threshold of 150
        # 
        # filter_speed <- function(d){
        #   d <- atl_filter_covariates(data=d, 
        #                              filters=c("speed<100", 
        #                                        "SD < 150"))
        #   d
        # }
        # 
        # 
        # 
        # ldf20_speedfilter <- lapply(ldf20_speed, filter_speed)
        # 
        # # ## Some dataframes do not meet these criteria, and therefore all datapoints were removed
        # # ## Delete all dataframes that have less than 1 line 
        # # min_locs<-1 	# select only if at least this many locations 
        # # ldf_n <- lapply(ldf20_speedfilter, nrow)
        # # n<-unlist(ldf_n)
        # # ldf20<-ldf20_speedfilter[n>=min_locs]
        # 
        # ## median smoother of 5
        # 
        # med_smooth <- function(d){
        #   d <- atl_median_smooth(data = d , 
        #                          x = "X", y = "Y",
        #                          time = "ts",
        #                          moving_window = 5)
        #   d
        # }
        # ldf20_f <- lapply(ldf20_speedfilter, med_smooth)
        # 
        # ## Extract last 20 minutes of data 
        # 
        # last_20min <- function(d){
        #   end <- max(d$TIME)
        #   start <- end-1200
        #   sub <- subset(d, d$TIME>start)
        # }
        # ldf_20min <- lapply(ldf20_f, last_20min)
        # 
        # #last 1000 points
        # # ldf_t <- lapply(ldf20_f, tail,1000)
        # 
        # #last hour 
        # last_hour <- function(d){
        #   end <- max(d$TIME)
        #   start <- end-3600
        #   sub <- subset(d, d$TIME>start)
        # }
        # 
        # ldf_20h <- lapply(ldf20_f, last_hour)
        # 
        # 
        # ## combine two lists ldf_20h and ldf_hour
        # 
        # ldf_new <- c(ldf_hour, ldf_20h)
        # 
        # #only select westward departures from griend 
        # 
        # ldf_west <- ldf_new[c(94, 45, 163, 127, 126, 89, 90, 9, 147, 43, 177, 17, 116, 155, 215, 129, 184, 185, 210, 156, 186, 175, 198, 275, 321, 323, 330, 339, 342, 346, 347, 357, 382, 385, 408, 417, 454, 467,474, 488)]
        # 
        # ## convert to spatial data frames
        # ## adjust make_spatial function (with small x and y) 
        # make_spatial<-function(d, crs){
        #   library(sp)
        #   # coordinates(d) <- ~ X+Y
        #   utm<-"+proj=utm +zone=31 +datum=WGS84"
        #   # proj4string(d) <- CRS(utm)
        #   d <- SpatialPointsDataFrame(coords = d[,c("X","Y")], data = d, proj4string = CRS(utm))
        #   d
        # }
        # 
        # 
        # ## convert to spatial data frames
        # ## adjust make_spatial function (with small x and y) 
        # make_spatial<-function(d, crs){
        #   library(sp)
        #   # coordinates(d) <- ~ X+Y
        #   utm<-"+proj=utm +zone=31 +datum=WGS84"
        #   # proj4string(d) <- CRS(utm)
        #   d <- SpatialPointsDataFrame(coords = d[,c("X","Y")], data = d, proj4string = CRS(utm))
        #   d
        # }
        # 
        # 
        # ### make spatial	
        # ldf_utm <- lapply(ldf_west, make_spatial)	# for easy analyses
        # ldf_osm <- lapply(ldf_utm, spTransform, osm()) # for easy plotting
        # 
        # 
        # ##################
        # ### get bounding box, load map and subset tracking data with BBox
        # 
        # Bbox<-lapply(ldf_osm,BBOX,buffer=2000)
        # xrange<-range(unlist(lapply(Bbox, `[`,1,)))
        # yrange<-range(unlist(lapply(Bbox, `[`,2,)))
        # Bbox<-cbind(rev(yrange), xrange)
        # Bbox <- rbind(c(52.86, 4.35), c(53.43, 5.41))
        # ## load map with specified bbox
        # map <- openmap(Bbox[1,],Bbox[2,],type='bing')
        # 
        # ## plot all tracks in one map
        # plot_map(map)
        # # get colours for different individuals
        # COL=rainbow(length(ldf_osm))	
        # ## plot tracks
        # mapply(plot_fun, d = ldf_osm, Pch=19, Cex=0.4, Lwd=1, col = COL, Type="o", endpoint=TRUE) # type = "o" for lines thorugh points	 
        # ## add legend
        # # legend("topleft", tags, col=COL, pt.bg=COL,pch=rep(21,length(COL)),text.col="white", cex=0.75,pt.cex=1.5,bty = "n")	
        # ## add scalear
        # fr=0.02	# custum position of scalebar (in fraction of plot width) 
        # ydiff<-diff(par('usr')[3:4])
        # xdiff<-diff(par('usr')[1:2])
        # xy_scale<-c(par('usr')[1]+xdiff*fr, par('usr')[3] + ydiff*fr)
        # scalebar(20000, xy_scale,type='line', divs=2, lwd=3, col="white", label="10 km")
        # north.arrow(2,2,4,5)
        # #dev.off()
        # 
        # 
        
        
        
        
        
        
