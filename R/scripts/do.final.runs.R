

runs <- 1:nrow(ParGen)
vars <- c("LE","H","swc02")
b4.calib <- calib.params 
param2run <- ParGen[1,]

require(parallel)        
        
out <- mclapply(runs, function(i){
 
    out <- do.call(sib2,
                   as.list(modifyList(formals(sib2), 
                                      c(list(vars_out = vars,
                                             infile2 = infile2,
                                             green = 0.5,
                                             id = paste0(i,"RN"),
                                             dir_out = dir_out,
                                             multicamadas = "on"),
                                        calib.params,
                                        as.list(ParGen[i,]))))
    )  
    id <- rep(paste0("RUN",i), nrow(out))
    cbind(id, out)
    
}, mc.cores = 8) %>% bind_rows
    out$id <- as.factor(out$id)
    
    # out_daily <- timeAverage(out,avg.time = "day")
    
    out_daily <- lapply(unique(out$id), function(i){
        out <- timeAverage(subset(out, id == i) ,avg.time = "day")
        id <- rep(i, nrow(out))
        cbind(id, out)
    })%>% bind_rows
    
    ggplot(out_daily, aes(x=date, y = LE, color = id)) +
        geom_line() +
        theme_bw()
    
    
    out_period <- lapply(unique(out_daily$id), function(i){
        out <- selectByDate(out_daily, start = "2009-07-02",
                            end = "2012-06-30")
        # id <- rep(i, nrow(out))
        # cbind(id, out)
    })%>% bind_rows
    
    ggplot(out_period, aes(x=date, y = LE, color = id)) +
    geom_line() +
        theme_bw()

    ggplot(out_period, aes(x=date, y = swc02, color = id)) +
        geom_line() +
        theme_bw()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    