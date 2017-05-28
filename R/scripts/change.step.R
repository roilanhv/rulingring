
change.time.step.albedo <- 
  function(sim,
           var = "albedoPAR",
           step = "15 day",
           date = FALSE){
    # Carregando pacote 'openair'
    require(openair)
    require(plyr)
    require(dplyr)
    require(REddyProc) 
    require(magrittr)
    require(hydroGOF)
  
    sim %<>% mutate(daytime = ifelse( fCalcPotRadiation( DoY.V.n = format(date,"%j") %>% as.numeric,
                                                         Hour.V.n = format(date,"%H") %>% as.numeric,
                                                         Lat_deg.n = -21.61,
                                                         Long_deg.n = -47.63,
                                                         TimeZone_h.n = -3,
                                                         useSolartime.b = TRUE) > 0 , 
                                      "day","night"))
    sim[which(sim$daytime == "night"),var] <- NA
    sim %<>% mutate(daytime = NULL)
    
    sim.mly <- timeAverage(sim, avg.time = step)
    if(date)return(sim.mly)
    if(!date)return(sim.mly[,var])
  }

getcycleAvgTS <- function(x, each8day = 24,na.rm=TRUE) {
    # x=timeAverage(Measurement$MeasData[,c("date",Extra$gof.VAR[fobj])],
    #               avg.time = "month")
    
    aa <- rep(1:each8day,length(x)/each8day)
    if("date" %in% names(x)) x <- x[,names(x) !=  "date"]
    
    viclima <- c(t(unlist(lapply(split(x,aa), mean, na.rm=T))))
                 
    return(viclima)
}



change.time.step <- 
    function(sim,
             obs, 
             step = "15 day",
             gof.FUN = "NSE",
             gof.Ini="2009-07-02",
             gof.Fin="2012-06-30", 
             gof.FUN.args = list()){
        # Carregando pacote 'openair'
        require(openair)
        require(plyr)
        require(dplyr)
        require(REddyProc) 
        require(magrittr)
        require(hydroGOF)
        
        date <- seq.POSIXt(from = as.POSIXct(gof.Ini,tz = "GMT",format= "%Y-%m-%d" ),
                           to = as.POSIXct(gof.Fin,tz = "GMT",format= "%Y-%m-%d" ) + 3600*23,
                           by = "hour")
        
        sim <- data.frame(date = date,albedo = sim)
        
        sim %<>% mutate(daytime = ifelse( fCalcPotRadiation( DoY.V.n = format(date,"%j") %>% as.numeric,
                                                             Hour.V.n = format(date,"%H") %>% as.numeric,
                                                             Lat_deg.n = -21.61,
                                                             Long_deg.n = -47.63,
                                                             TimeZone_h.n = -3,
                                                             useSolartime.b = TRUE) > 0 , 
                                          "day","night"))
        
        sim %<>% mutate(albedo = ifelse(daytime == "day",albedo,NA))
        sim %<>% mutate(daytime = NULL)
        
        obs <- data.frame(date=date, albedo = obs)
        obs %<>% mutate(daytime = ifelse( fCalcPotRadiation( DoY.V.n = format(date,"%j") %>% as.numeric,
                                                             Hour.V.n = format(date,"%H") %>% as.numeric,
                                                             Lat_deg.n = -21.61,
                                                             Long_deg.n = -47.63,
                                                             TimeZone_h.n = -3,
                                                             useSolartime.b = TRUE) > 0 , 
                                          "day","night"))
        obs %<>% mutate(albedo = ifelse(daytime == "day",albedo,NA))
        obs %<>% mutate(daytime = NULL)
        
        obs.mly <- timeAverage(obs, avg.time = step)
        sim.mly <- timeAverage(sim, avg.time = step)
        
        
        out <- do.call(what = gof.FUN,
                       args = list(sim = sim.mly$albedo, obs = obs.mly$albedo, na.rm = TRUE) ) 
        return(out)
    }