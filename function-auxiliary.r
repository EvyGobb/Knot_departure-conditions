
### script by Allert Bijleveld
### version 27 March 2018

### this file contains differet kinds of small function used for processing and plotting data 	
	
### set functions

## make x,y data spatial
make_spatial<-function(d, crs){
	library(sp)
	# coordinates(d) <- ~ X+Y
	utm<-"+proj=utm +zone=31 +datum=WGS84"
	# proj4string(d) <- CRS(utm)
	d <- SpatialPointsDataFrame(coords = d[,c("X","Y")], data = d, proj4string = CRS(utm))
	d
	}

## get bounding box from spatial data in LatLong)		
BBOX<-function(d, buffer){
	library(OpenStreetMap)
	library(sp)
	# buffer is in meters x and then y
	bbox<-as.data.frame(t(bbox(d)+matrix(c(-buffer,buffer,-buffer,buffer),nrow=2,byrow=TRUE)))
	names(bbox)<-c("X","Y")
	### make bbox spatial  
	coordinates(bbox) <- ~ X+Y
	proj4string(bbox) <- osm()
	## transform to LL
	LL<- "+init=epsg:4326" #LL
	bbox<-spTransform(bbox,LL)
	bbox<-bbox@bbox
	}
	
## plot tracks from lists
plot_fun = function(d, Pch=19, Cex=0.25, Lwd=1, col, Type="b",endpoint=FALSE) {
	points(d, col=col, pch=Pch, cex=Cex, lwd=Lwd, type=Type)
	if(endpoint){points(d[nrow(d),], col="magenta", pch=19, cex=Cex*2)}
}

## get tower data and make spatial
get_towers<-function(path){
	library(sp)
	towers<-read.csv(path, header=FALSE)
	names(towers)<-c("ID", "X", "Y", "Z")
	coordinates(towers) <- ~ X+Y
	utm<-"+proj=utm +zone=31 +datum=WGS84"
	proj4string(towers) <- CRS(utm)
	towers<-spTransform(towers,osm())	
	towers
	}
	
## plot map base don size oof map
plot_map<-function(map, ppi=96){
	## map=osm map; ppi=pixels per inch resolution for plot
	## get size of plot
	px_width  <- map$tiles[[1]]$yres[1]
	px_height <- map$tiles[[1]]$xres[1]
	## initiate plotting window 
	win.graph(width=px_width/ppi, height=px_height/ppi)
	par(bg="black")
	par(xpd=TRUE)	
	## make plot
	plot(map)
	}
	
#### not used anymore
# calculate appromixate standard deviation on position fix by uning the max variabce estimate (assumes circular instead of ellipsoid standard error around position. appromixation that falls within 1.4 of real estimate (read sivan's e-mail)  
# maxSD <- function(d){
	# tmp<-as.numeric(apply(d[,c("VARX", "VARY", "COVXY")],1, function(x) (x[names(x)=="VARX"] + x[names(x)=="VARY"] + 2*x[names(x)=="COVXY"]) ) )
	# ifelse(tmp<0, 0, sqrt(tmp) )
		## old conservative circular estimate on maximum variance estiamate
		# sqrt(as.numeric(apply(d[,c("VARX", "VARY")],1, max, na.rm = TRUE)))
	## for spatial points dataframe
	#sqrt(as.numeric(apply(d@data[,c("VARX", "VARY")],1, max, na.rm = TRUE)))
# }
	
## plot individual tag from one 
plot_tag<-function(d, id, color_by="T", path=NULL, towers=NULL, mapID=map, Legend="topleft"){
		# BboxID<-lapply(d,BBOX,buffer=500)
			# xrange<-range(unlist(lapply(BboxID, `[`,1,)))
			# yrange<-range(unlist(lapply(BboxID, `[`,2,)))
			# BboxID<-cbind(rev(yrange), xrange)
		# load map with bbox
		# mapID <- openmap(BboxID[1,],BboxID[2,],type='bing')
		
		tag<-d@data[1,1]
		
		if(color_by=="NBS")	{ color_by2 <-d$NBS; color_by_title<-paste("aantal base stations", '\n', "tag ",tag,sep="")}
		if(color_by=="SD")	{ color_by2 <-d$SD; color_by_title<-paste("positioning error (SD)", '\n', "tag ",tag,sep="")}
		if(color_by=="T")	{ color_by2 <-as.numeric(difftime(d$ts, min(d$ts)), unit="hours"); color_by_title<-paste("time since start (h)", '\n', "tag ",tag,sep="")}
		
		### if path is not NULL then save plots 
			if(!is.null(path)){
				dir.create(file.path(path), showWarnings = FALSE)
				## get size of plot
				px_width  <- mapID$tiles[[1]]$yres[1]
				px_height <- mapID$tiles[[1]]$xres[1]
				## initiate plotting window 
				png(filename = paste(path,"\\", tag, "-",color_by, ".png", sep=""), width = px_width, height = px_height, units = "px")
				par(bg="black")
				par(xpd=TRUE)   
				## make plot
				plot(mapID)
				}else{
				plot_map(mapID)## plot with normal graphics device
				}
			
		# add title 
			mtext(paste("from ", min(d$ts)," \nto ", max(d$ts), sep=""), line = 2, col="white")
		# add towers
			if(!is.null(towers)){points(towers$X,towers$Y,pch=23, cex=2,col=2,bg=1)}
		## make color scale
			# rbPal <- colorRampPalette(c('green', 'yellow', 'orange', 'red', 'dark red'))
			rbPal <- colorRampPalette(c('white', 'light yellow', 'yellow', 'orange', 'dark orange','red', 'dark red'))
			n<-100 #number of color classes
			cuts<-cut(color_by2,breaks = n)
			colramp<-rbPal(n)
			COLID <- colramp[as.numeric(cuts)]	
		## plot positions
			points(d, pch=3, cex=0.3, lwd=0.5, col = COLID, type="b")#type="o" 
		## add scalear
			fr=0.02	# custum position of scalebar (in fraction of plot width) 
			ydiff<-diff(par('usr')[3:4])
			xdiff<-diff(par('usr')[1:2])
			xy_scale<-c(par('usr')[1]+xdiff*fr, par('usr')[3] + ydiff*fr)
			scalebar(25000, xy_scale,type='line', divs=4, lwd=3, col="white", label="25 km")
		## add legend 
				legend_cuts<-pretty(color_by2, n=5)
				legend_cuts_col<-colramp[seq(1,n, length=length(legend_cuts))]
				legend(Legend, legend=legend_cuts, col =legend_cuts_col, pch=15, bty="n", text.col = "white", title=color_by_title, inset=c(0.01, 0.02),y.intersp = 0.8)
		
		if(!is.null(path)){dev.off()} # close grapichs device if saved to file
			
			}


### filter tracking data with attrcation point
	rm_loc<-function(x, xmin, xmax, ymin, ymax){
		x<-x[ !(x$X>xmin & x$X<xmax & x$Y>ymin & x$Y<ymax),]
		}


	
### subset tracking data with BBox
	CROP<-function(x, Bbox){
		require(rgeos)
		sp_Bbox<-data.frame(Bbox)
		names(sp_Bbox)<-c("Y", "X")
		coordinates(sp_Bbox) <- ~ X+Y
		sp_Bbox <- as(extent(sp_Bbox), "SpatialPolygons")
		proj4string(sp_Bbox) <- CRS("+init=epsg:4326")
		sp_Bbox<-spTransform(sp_Bbox,osm())
		# gIntersection(x, sp_Bbox)	
		row_id<-as.vector(gIntersects(x, sp_Bbox, byid=TRUE))
		x[row_id,]
}


	