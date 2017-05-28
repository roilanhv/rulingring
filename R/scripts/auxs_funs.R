
translate.vector <- function(vector, foreign_lang, native_lang){
    vector <- as.vector(vector)
    for(i in 1:length(foreign_lang) ){
        indexes <- grep(x = vector,pattern = foreign_lang[i])
        vector[indexes] <- gsub(vector[indexes],
                                pattern = foreign_lang[i],
                                replacement = native_lang[i]) }
    return(vector)
}


maxlim <- function(i,max_=1,min_=0){ sapply(i,function(i) min(max(i,min_),max_) )  }


to.daylight <- 
    function(date,lon = -47.63,lat = -21.61, timezone = -3){
        require(REddyProc)
        if(lon == -47.63 & lat == -21.61 & timezone == -3) 
            message("Warning: This data are only valid for PdG site. RHV")
        
        date1 = format(date, "%j") %>% as.numeric()
        date2 = format(date, "%H") %>% as.numeric()
        
        ifelse(fCalcPotRadiation(DoY.V.n = date1,
                                 Hour.V.n = date2,  
                                 Lat_deg.n = lat,
                                 Long_deg.n = lon,
                                 TimeZone_h.n = timezone,    
                                 useSolartime.b = TRUE ) > 0 , 
               "day","night")
        
    }

bestParams <- function(ParGen, ObjVals){
    dist <- lapply(1:ncol(ObjVals), function(i){
        (ObjVals[,i]- min(ObjVals[,i]))^2
    }) 
    dist <- as.data.frame(dist) 
    names(dist) <- paste0("val",1:ncol(ObjVals))
    
    dist$dist <- sapply(1:nrow(ObjVals),function(i){
        sqrt(sum(dist[i,]))
    } )
    
    index.best <- which(dist$dist == min(dist$dist))
    
    message(paste(round( ObjVals[index.best,1:ncol(ObjVals)],3), collapse = ", "))
    
    best <- ParGen[ index.best ,]
    
    if(!is.vector(best)){
        best <-  best[1,]
    }
    best
}


normal <- function(serie){
    (serie - min(serie,na.rm = TRUE))/ (max(serie,na.rm = TRUE)-min(serie,na.rm = TRUE))
}


to8bitword <- function(x=157,nbits = 8){
    sapply(x,function(i){ 
        paste(rev(as.integer(intToBits(i))), collapse="") %>% substr((nbits-1),32)
    }) %>% as.vector
    # return()
}

multiplot <- function(..., plotlist=NULL, cols) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    # plots <- c(list(ggtar,ggtar2), plotlist)
 
    numPlots = length(plots)
    
    # Make the panel
    plotCols = cols                          # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
    
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}


timeCumsum <- function(mydata = sib2in %>% select(date,LE,eci),
                       ave.time = "month", setNA=TRUE){
  
  require(plyr);require(dplyr)
  
  if(ave.time == "year") form = "%Y"
  if(ave.time == "month") form = "%Y-%m"
  if(ave.time == "day") form = "%Y-%m-%d"
  if(ave.time == "hydroyear") {
    cat("Warning: Fixed hydrological year for southern hemisphere\n",
        "Warning: Still not implemented for data without complete years.\n")
    
    start.year <- sapply(unique(format(mydata$date,"%Y")), function(i){
        which(format(mydata$date,"%Y-%m") == paste0(i,"-07"))[1]
    }) %>% as.integer
    
    start.year <- c( 1,start.year, length(mydata$date)+1)
    names(start.year) <- (as.numeric(format(mydata$date,"%Y")[1] %>% unique)-1):
                         (as.numeric(format(mydata$date,"%Y")[length(mydata$date)] %>% unique)+1)
    
    new.mydata <- llply(1:(length(start.year)-1),
                         function(i){ # i <- period[3]
                             
        rows.period <- start.year[i]:(start.year[i+1]-1)
        subs <- cumsum(mydata[rows.period,-which(names(mydata)=="date")])
        if(setNA) subs[1,] <- NA
        mydata[rows.period,-which(names(mydata)=="date")] <- subs
        mydata$id <- names(start.year[i])
        return(mydata[rows.period,])
    })
    # subset(mydatamonts[1]
    
  } else if(ave.time == "season") {
      
    cat("Warning: Fixed for southern hemisphere")
    
    mydata <- cutData(mydata,type = "season",hemisphere = "southern")
      period <- expand.grid(unique(format(mydata$date,"%Y")), unique(mydata$season))
        n.period <- nrow(period)
        
     new.mydata <- llply(1:n.period, function(i){ # i <- 2
          
          if(period[i,2] == "summer (DJF)"){
            
            rows.period <- c(which(format(mydata$date,"%Y") == as.numeric(as.vector(period[i,1])) -1 
                                 &
                                 format(mydata$date,"%m") == 12 
                                 &
                                 mydata$season == period[i,2])
                             ,
                             which(format(mydata$date,"%Y") == as.numeric(as.vector(period[i,1])) 
                                 &
                                 format(mydata$date,"%m") %in% c("01","02") 
                                 &
                                 mydata$season == period[i,2])
            )
              
          } else { 
            
          rows.period <- which(format(mydata$date,"%Y") == period[i,1] 
                               &
                              mydata$season == period[i,2])
          }
       
       
            subs <- cumsum(mydata[rows.period,-which(names(mydata) %in% c("date","season"))])
          subs[1,] <- NA
          mydata[rows.period,-which(names(mydata) %in% c("date","season"))] <- subs
          mydata$id <- period[i,2]
          mydata$season <- NULL
          return(mydata[rows.period,])
        })     
    
  } else {
  
        period <- unique(format(mydata$date,form))
        n.period <- length(period)
        
        new.mydata <- llply(period,.progress = "text", function(i){ # i <- period[3]
          rows.period <- which(format(mydata$date,form) == i)
          subs <- cumsum(mydata[rows.period,-which(names(mydata)=="date")])
          subs[1,] <- NA
          mydata[rows.period,-which(names(mydata)=="date")] <- subs
          mydata$id <- i
          return(mydata[rows.period,])
        })
  
  }

  my.data <- bind_rows(new.mydata)
  return(my.data)
  
}


make.barplot <- function(bars = comp.le,
                         sequential = rainbow(4),
                         main = "",
                         beside = FALSE){
  
  barplot(t(bars[,-1]), 
          names.arg = bars$date %>% format("%Y-%m"),
          cex.names = 0.8,
          col = sequential, 
          beside = beside,
          width = 1, 
          las = 2,
          space = ifelse(beside,c(1,1),0.2),
          xlim = c(0, (length(bars$date)+18)+ beside*ncol(bars)),
          main = main,
          axes = FALSE
  )
  box(bty = "l")
   axis(2,padj = 1)
  legend(ifelse(beside,"top","bottomright"),
         bty = "n",cex = 0.75,ncol = ifelse(beside,ncol(bars)-1,1),
         legend = names(bars[,-1]), #in order from top to bottom
         fill = sequential, # 6:1 reorders so legend order matches graph
         title = "LE comp")
  
  
}

# Function to convert from Wm-2 to mm for nhour*3600s 
wm2.to.mm <- function(var,nhour=1){
  if("date" %in% names(var)) {
    var[,-which(names(var) == "date")] <- var[,-which(names(var) == "date")]*nhour*3600/2454000
  } else {
    var <- var*nhour*3600/2454000
  }
  return(var)}
