
gg_hour <-    function(data_hour,seas,vari, scaley=FALSE,siz.text,limits,limdec){
    #seas ="inverno"
    inv.tab <- with(data_hour %>% 
                        filter(season == seas) %>% 
                        select(variable, Mean) %>%  
                        arrange(variable), gof_new(obs = Mean[1:24], sim = Mean[25:48], na.rm = TRUE))
    
    plot.gg <- data_hour %>% 
        filter(season == seas) %>% 
        select(variable, hour,Mean) %>% 
        ggplot(aes(x=hour, y = Mean, colour = variable)) + 
        geom_line(size = 1.) +
        scale_color_manual(values = c("black","red")) +
        theme_bw() +
        theme(legend.position="none") 
    
    if(scaley) plot.gg <- plot.gg  + scale_y_continuous(name = vari,limits = limits)
    if(!scaley)plot.gg <- plot.gg + scale_y_continuous(name = element_blank(),limits = limits)+ 
        theme(axis.text.y = element_blank())
    
    
    plot.gg <- plot.gg+ scale_x_continuous(name = element_blank())+
        labs(title=seas) +
        annotate(geom = "text",x = 5.2, y = limits[2], 
                 label = paste(names(inv.tab[1,]),inv.tab[1,]),size = siz.text)+
        annotate(geom = "text",x = 5.2, y = limits[2]-limdec,
                 label = paste(names(inv.tab[2,]),inv.tab[2,]),size = siz.text)+
        annotate(geom = "text",x = 5.2, y = limits[2]-2*limdec,
                 label = paste(names(inv.tab[3,]),inv.tab[3,]),size = siz.text)+
        annotate(geom = "text",x = 5.2, y = limits[2]-3*limdec,
                 label = paste(names(inv.tab[4,]),inv.tab[4,]),size = siz.text)+
        annotate(geom = "text",x = 5.2, y = limits[2]-4*limdec,
                 label = paste(names(inv.tab[5,]),inv.tab[5,]),size = siz.text)+
        annotate(geom = "text",x = 5.2, y = limits[2]-5*limdec,
                 label = paste(names(inv.tab[6,]),inv.tab[6,]),size = siz.text)
    
    plot.gg
}


make_plot_series <- function(vari, obs, sim1,sim2 = NULL,
                             limits_hour = c(0,350),
                             size_hour = 2.5,hourly=TRUE,
                             limdec_hour = 20){
    require(scales)
    
    minaxes <- min(sim1[,2], na.rm = TRUE)
    maxaxes <- max(sim1[,2], na.rm = TRUE)
    # inter <- as.integer( (maxaxes -  minaxes)/30)
    
    limits <- timeAverage(sim1) %>% select(-date) %>% as.vector %>% range()
    limdec <- 
        timeAverage(sim1) %>% select(-date) %>% as.vector %>% range() %>% diff() %>% divide_by(6)
    siz.text <- 4.0
    
    obs1 <- obs %>% select_("date",vari) %>% setNames(c("date",paste0(vari,".obs")))
    sim1 <- sim1  %>% select_("date",vari) %>% setNames(c("date",paste0(vari,".sim")))
    
    if(!is.null(sim2)){
        sim2 <- sim2 %>% select_("date",vari) %>% setNames(c("date",paste0(vari,".sim2")))
        sim1 <- left_join(x = sim1,y = sim2, by= "date")
    } 
    
    gof.tab <- gof_new(sim = sim1[,2],obs = obs1[,2], na.rm = TRUE)
    date.plus <- as.POSIXct("2012-10-01")
    
    data2plot <- 
        left_join(x = obs1 ,y = sim1, by = "date") %>% 
        timeAverage(avg.time = "day") %>%
        gather(vars, value, -date) %>% 
        ggplot(aes(x = date, y = value, colour = vars)) +
        geom_line() + 
        theme_bw() + 
        scale_y_continuous(name = paste(vari),breaks = pretty_breaks(5),
                           minor_breaks = pretty_breaks(20)) +
        scale_x_datetime(name = element_blank(),
                         date_breaks = "3 month",
                         limits = c(as.POSIXct("2009-07-01"),as.POSIXct("2013-01-01")),
                         date_labels = "%b\n%Y", expand = c(0,0)
        ) +
        scale_color_manual(values = c("black","red", "blue")) +
        theme(legend.title = element_blank(), legend.position = c(0.2, 0.9),
              legend.key=element_blank() , legend.direction = "horizontal")  +
        theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
        annotate(geom = "text",x = date.plus, y = limits[2], 
                 label = paste(names(gof.tab[1,]),gof.tab[1,]),size = siz.text)+
        annotate(geom = "text",x = date.plus, y = limits[2]-limdec,
                 label = paste(names(gof.tab[2,]),gof.tab[2,]),size = siz.text)+
        annotate(geom = "text",x = date.plus, y = limits[2]-2*limdec,
                 label = paste(names(gof.tab[3,]),gof.tab[3,]),size = siz.text)+
        annotate(geom = "text",x = date.plus, y = limits[2]-3*limdec,
                 label = paste(names(gof.tab[4,]),gof.tab[4,]),size = siz.text)+
        annotate(geom = "text",x = date.plus, y = limits[2]-4*limdec,
                 label = paste(names(gof.tab[5,]),gof.tab[5,]),size = siz.text)+
        annotate(geom = "text",x = date.plus, y = limits[2]-5*limdec,
                 label = paste(names(gof.tab[6,]),gof.tab[6,]),size = siz.text)
    
    vars2plot <- paste0(vari, c(".obs",".sim"))
    
    if(hourly){
        data2plot_hourly <-
            left_join(x = obs1 ,y = sim1, by = "date") %>%
            timeVariation(pollutant =  vars2plot,type = "season", hemisphere = "southern")
        
        data2plot_hourly_all <-
            left_join(x = obs1 ,y = sim1, by = "date") %>%
            timeVariation(pollutant =  vars2plot)
        
        hourly_ggplot <-
            (data_hour <- rbind(data2plot_hourly_all$data$hour %>% mutate(default = NULL,
                                                                          season = "total") %>%
                                    dplyr::select(variable, hour,season,Mean,Lower,Upper,ci),
                                data2plot_hourly$data$hour %>%
                                    separate(col = season,c("season","months")) %>%
                                    mutate(months = NULL) %>%
                                    mutate(season = ifelse(season == "winter","inverno",season)) %>%
                                    mutate(season = ifelse(season == "summer","verão",season)) %>%
                                    mutate(season = ifelse(season == "autumn","outono",season)) %>%
                                    mutate(season = ifelse(season == "spring","primavera",season))
            ) )%>%
            ggplot(aes(x = hour, y = Mean ,ymin =  Lower, ymax = Upper, colour = variable)) +
            geom_line() +
            facet_wrap(facets = ~season,nrow = 1,ncol = 5) +
            theme_bw() +
            scale_color_manual(values = c("black","red")) +
            theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
            theme(legend.title = element_blank(),
                  legend.key=element_blank() ) +
            scale_x_continuous(name = element_blank())
        
        
        
        hourly_ggplot <-  grid.arrange(gg_hour(data_hour = data_hour,
                                               seas = "total",
                                               siz.text = size_hour,
                                               limits = limits_hour,
                                               limdec = limdec_hour,
                                               vari = vari, 
                                               scaley = T),
                                       gg_hour(data_hour = data_hour,
                                               seas = "inverno",
                                               siz.text = size_hour,
                                               limits = limits_hour,
                                               limdec = limdec_hour),
                                       gg_hour(data_hour = data_hour,
                                               seas = "primavera",
                                               siz.text = size_hour,
                                               limits = limits_hour,
                                               limdec = limdec_hour),
                                       gg_hour(data_hour = data_hour,
                                               seas = "verão",
                                               siz.text = size_hour,
                                               limits = limits_hour,
                                               limdec = limdec_hour),
                                       gg_hour(data_hour = data_hour,
                                               seas = "outono",
                                               siz.text = size_hour,
                                               limits = limits_hour,
                                               limdec = limdec_hour),
                                       ncol=5)
        
        return(list(data2plot, hourly_ggplot))
        
    }
    return(list(data2plot))
    
}


make.Coef.Escoam <- function(sim,
                             vazao_file = "./OutrosDados/monthly_data_vazao_pauliceia.rds",
                             run_id
){
    
    best.calib_med <- sim %>% 
        mutate(LE = wm2.to.mm(LE,nhour = 1)) %>% 
        select(date, LE, tprec, roff) %>%  
        timeAverage(avg.time="month",statistic = "sum") 
    
    #%>%
    # mutate(ce = roff/tprec, fe = LE/tprec)# %>% 
    # select(date, tprec, LE, roff, ce, fe) %>% 
    # group_by(year = month(date)) %>%
    # summarise_each(funs(mean))
    
    qmon_obs <- readRDS(vazao_file)
    qmon <- left_join(qmon_obs, best.calib_med) 
    
    coef.graph <- timePlot(data.frame(qmon), 
                           c("tprec","roff", "vazao_mm","LE"), #,"prec_t"
                           group = TRUE, 
                           lwd =3, lty = 1
                           #avg.time = "year", statistic = "sum", data.thresh = 80
    )
    
    coef.table <- qmon[complete.cases(qmon),] %>%
        group_by(year = day(date)) %>%
        select(-date) %>%
        summarise_each(funs(sum)) %>%
        mutate(ce = roff/tprec, 
               fe = LE/tprec,
               ce_obs = vazao_mm/tprec,
               ce_obs2 = vazao_mm/prec_t) %>% 
        select(tprec, LE, roff, ce, ce_obs, ce_obs2, fe) %>% 
        t %>% as.data.frame() %>% 
        setNames(run_id)
    # 
    
    
    # # horarrios de vazao
    # qh <- readRDS("./OutrosDados/vazaoh_psx_pauliceia.rds")
    # timePlot(qh, c("prec_t", "vazao"))
    # 
    return(list(coef.table = coef.table,coef.graph = coef.graph))
}


gof_new <- function(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                    j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), 
                    lQ.thr=0.7, hQ.thr=0.2, digits=2, ...){
    
    method   <- match.arg(method)
    
    ME     <- me(sim, obs, na.rm=na.rm)
    MAE    <- mae(sim, obs, na.rm=na.rm)
    MSE    <- mse(sim, obs, na.rm=na.rm)
    RMSE   <- rmse(sim, obs, na.rm=na.rm) 
    NRMSE  <- nrmse(sim, obs, na.rm=na.rm, norm=norm)
    RSR    <- rsr(sim, obs, na.rm=na.rm, ...)
    rSD    <- rSD(sim, obs, na.rm=na.rm)     
    PBIAS  <- pbias(sim, obs, na.rm=na.rm, ...)
    NSE    <- NSE(sim, obs, na.rm=na.rm, ...)
    mNSE   <- mNSE(sim, obs, na.rm=na.rm, j=j, ...)
    rNSE   <- rNSE(sim, obs, na.rm=na.rm, ...)
    d      <- d(sim, obs, na.rm=na.rm, ...)
    md     <- md(sim, obs, na.rm=na.rm, ...)
    rd     <- rd(sim, obs, na.rm=na.rm, ...)
    cp     <- cp(sim, obs, na.rm=na.rm, ...)
    #r      <- hydroGOF:::.rPearson(sim, obs)
    bR2    <- br2(sim, obs, na.rm=na.rm, ...)     
    KGE    <- KGE(sim, obs, na.rm=na.rm, s=s, method=method, out.type="single", ...) 
    VE     <- VE(sim, obs, na.rm=na.rm, ...)     
    
    # sim = data2plot$LE; obs = data2plot$LE.obs
    reg <- lm(sim ~obs)
    coef_ang <- coef(reg)[["obs"]]
    coef_lin <- coef(reg)[["(Intercept)"]]
    
    
    # 'R2' is the Coefficient of Determination
    # The coefficient of determination, R2, is useful because it gives the proportion of
    # the variance (fluctuation) of one variable that is predictable from the other variable.
    # It is a measure that allows us to determine how certain one can be in making
    # predictions from a certain model/graph.
    # The coefficient of determination is the ratio of the explained variation to the total
    # variation.
    # The coefficient of determination is such that 0 <  R2 < 1,  and denotes the strength
    # of the linear association between x and y. 
    R2 <- summary(reg)[["r.squared"]]
    
    if (do.spearman) {
        r.Spearman <- cor(sim, obs, method="spearman", use="pairwise.complete.obs") 
        
        # if 'sim' and 'obs' were matrixs or data.frame, then the correlation
        # between observed and simulated values for each variable is given by the diagonal of 'r.Pearson' 
        if ( is.matrix(r.Spearman) | is.data.frame(r.Spearman) ) {
            r.Spearman        <- diag(r.Spearman)
        } # IF end
        
    } # IF end
    
    if (do.pbfdc) { pbfdc  <- pbiasfdc(sim, obs, na.rm=na.rm, lQ.thr=lQ.thr, hQ.thr=hQ.thr, plot=FALSE, ...) }
    
    #gof <- rbind(ME, MAE, MSE, RMSE, NRMSE, PBIAS, RSR, rSD, NSE, mNSE, rNSE, d, md, rd, cp, r, R2, bR2, KGE, VE)
    gof <- rbind(RMSE, MAE, PBIAS, NSE, R2, coef_ang, coef_lin)     
    
    #rownames(gof)[5] <- "NRMSE %"
    rownames(gof)[3] <- "PBIAS %"    
    rownames(gof)[6] <- "COEF_ANG"    
    rownames(gof)[7] <- "COEF_LIN"
    
    if (do.spearman) { gof <- rbind(gof, r.Spearman) }
    
    if (do.pbfdc) { 
        gof <- rbind(gof, pbfdc) 
        rownames(gof)[length(rownames(gof))] <- "pbiasFDC %"
    } # IF end
    
    # Rounding the final results, ofr avoiding scientific notation
    gof <- round(gof, digits)
    
    return(gof)
}
