

FLUX.acum <- function(sim1, obs1, var, lim_inf = c(500,800)){
    require(scales)
    
    qq <- left_join(obs1,sim1) %>%
        mutate_each(funs(wm2.to.mm), -date) %>%
        mutate_each(funs(cumsum), -date) 
    acums <- qq[nrow(qq),-1]
    rownames(acums) <- NULL

     qqgg <- qq %>%   # mutate_each(funs(fill), -date) %>%
        gather(vars, value, -date) %>%
        ggplot(aes(x = date, y = value, color = vars)) +
        theme_bw()+
        geom_line(size = 1.) +
        scale_color_manual(values = c("black","red")) +
        theme(legend.position="none")+
        scale_y_continuous(name = var,breaks = pretty_breaks(8), minor_breaks = pretty_breaks(20) )+
        scale_x_datetime(name = element_blank(),
                         date_breaks = "6 month", 
                         date_labels = "%b\n%Y")+
        annotate(geom = "text",x = as.POSIXct("2012-01-01"),y = lim_inf[1],label = paste0("Obs = ",round(acums[1,1],2) ))+
        annotate(geom = "text",x = as.POSIXct("2012-01-01"),y = lim_inf[2],label = paste0("Sim = ",round(acums[1,2],2) ))
        qqgg 
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

ggPareto <- function(Obj = ObjVals, cols = 1:2,funsnames, Iter){
    require(ggplot2)
    Obj <- as.data.frame(Obj)[cols]
    names(Obj) <- paste0("x",1:length(cols))
    ggplot(Obj,
           aes(x = x1,y = x2)) +
        geom_point() +
        theme_bw() +
        labs(title = paste0("Iteração: ",Iter), 
             x = funsnames[cols[1]], y = funsnames[cols[2]])
    
}

make_Pareto_analises <- function(run_dir,
                                 amalout_file,
                                 model.FUN.args, 
                                 gof.FUN, 
                                 gof.VAR,
                                 obs, 
                                 run_id,
                                 new_=FALSE){
    
  message("Buscando :",paste0(run_dir,run_id,".rds"))
    
  if(!file.exists(paste0(run_dir,run_id,".rds")) | new_ ){
    amalOUT <- readRDS(paste0(run_dir,amalout_file))
    message("[ 1- Lendo saída do AMALGAM ..")
    ParSet <- amalOUT[[1]] 
    Iter <- rownames(ParSet)
    
    output <- amalOUT[[2]] %>%
        as.data.frame    
    
    ParGen <- ParSet[(nrow(ParSet)-output[1,1]+1):nrow(ParSet),-((ncol(ParSet)-3):ncol(ParSet))]
    ObjVals <- ParSet[(nrow(ParSet)-output[1,1]+1):nrow(ParSet),(ncol(ParSet)-2):ncol(ParSet)] %>%
        set_rownames(NULL) %>%
        set_colnames( paste0(gof.FUN,gof.VAR)) %>%
        as.data.frame
    
    print(ggPareto(Obj = ObjVals, cols = 1:2,funsnames = gof.VAR, Iter = run_id ))
    
    message("[ 2- Selecionando melhor combinação de parâmetros ..")
    best <-  bestParams(ParGen = ParGen , ObjVals = ObjVals )
    
    message("[ 3- Rodando o SiB2 ..")
    
    best.calib <- do.call(sib2,
                          as.list(modifyList(model.FUN.args, 
                                             c(list(vars_out = c("em","tm","um","tprec","ki",
                                                                 "swc20","swc02","swc01",
                                                                 "vwc20","vwc02","vwc01",
                                                                 "www1","www2","www3",
                                                                 "LE","H","assimn","rn",
                                                                 "eci","ect","egi","egs",
                                                                 "tc","tg","G" ,"shf","chf",
                                                                 "roff","roff1","roff2","roff3",
                                                                 "ustar","ra",
                                                                 "albedo","albedoPAR","ldwn","lupw")),
                                               as.list(best))))
    )
    
    best.calib_save <-  best.calib
    best.calib %<>% selectByDate(start = "2009-07-02",end = "2012-06-30")
    best.calib$assimn <- -best.calib$assimn 
    names(best.calib)[-1] <- paste0(names(best.calib)[-1], ".amalgam")
    calib_run <- merge(best.calib, obs, by = "date")
    calib_run$assimn.amalgam[is.na(calib_run$assimn)] <- NA
    
    message("[ 3- Calculando estatística ..")
    GOf_stats <- with(calib_run,
                      data.frame(vwc20 = gof(obs =  vwc20, sim = vwc20.amalgam, digits = 5),
                                 vwc01 = gof(obs =  vwc01, sim = vwc01.amalgam, digits = 5),
                                 vwc02 = gof(obs =  vwc02, sim = vwc02.amalgam, digits = 5),
                                 swc20 = gof(obs =  swc20, sim = swc20.amalgam, digits = 5),
                                 swc01 = gof(obs =  swc01, sim = swc01.amalgam, digits = 5),
                                 swc02 = gof(obs =  swc02, sim = swc02.amalgam, digits = 5),
                                 LE    = gof(obs =  LE, sim = LE.amalgam, digits = 5),
                                 H     = gof(obs =  H, sim = H.amalgam, digits = 5),
                                 RN    = gof(obs =  rn, sim = rn.amalgam, digits = 5),
                                 ustar = gof(obs =  ustar, sim = ustar.amalgam, digits = 5) ))
    
    list(GOf_stats =GOf_stats,
         model.FUN.args = model.FUN.args,
         ParSet = ParSet, 
         ParGen = ParGen,
         ObjVals = ObjVals,
         gof.FUN = gof.FUN,
         gof.VAR =gof.VAR,
         best = best,
         best.calib_save = best.calib_save,
         calib_run = calib_run) -> all.info
    
    saveRDS(all.info, paste0(run_dir,run_id,".rds"))
  }else{
    all.info <- readRDS(paste0(run_dir,run_id,".rds"))
}
    return(all.info)
}

make_runs_satcap <- function(Spars,obs,model.FUN.args,best,gof.VAR,run_dir, run_id){ 
    
    teste_satcap <- lapply(1:nrow(Spars), function(i){ #i =2
        message("Simulação #",i)
        simul <- do.call(sib2,
                         as.list(modifyList(model.FUN.args, 
                                            c(list(vars_out = c("swc20","swc02","swc01",
                                                                "vwc20","vwc02","vwc01",
                                                                "www1","www2","www3",
                                                                "LE","H","assimn","rn",
                                                                "eci","ect","egi","egs",
                                                                "tc","tg","G" ,
                                                                "shf","chf","tprec",
                                                                "roff","roff1","roff2","roff3",
                                                                "ustar","ra","ldwn","lupw")),
                                              as.list(best),
                                              as.list(Spars[i,1:2]) )))
        )
        
        combin <-  left_join(x = obs,y = simul,by = "date")
        
        message("  Estatística...")
        if("swc02" %in% gof.VAR){
            gof_ww <- with(combin, 
                           data.frame(swc02 = gof_new(obs = swc02.x,sim = swc02.y, na.rm = TRUE,digits = 4),
                                      LE    = gof_new(obs = LE.x   ,sim = LE.y   , na.rm = TRUE,digits = 4),
                                      H     = gof_new(obs = H.x    ,sim = H.y    , na.rm = TRUE,digits = 4)))
            
        }
        if("vwc"   %in% gof.VAR){
            gof_ww <- with(combin,
                           data.frame(vwc20 = gof_new(obs = vwc20.x,sim = vwc20.y, na.rm = TRUE,digits = 4),
                                      vwc01 = gof_new(obs = vwc01.x,sim = vwc01.y, na.rm = TRUE,digits = 4),
                                      vwc02 = gof_new(obs = vwc02.x,sim = vwc02.y, na.rm = TRUE,digits = 4),
                                      LE    = gof_new(obs = LE.x   ,sim = LE.y   , na.rm = TRUE,digits = 4),
                                      H     = gof_new(obs = H.x    ,sim = H.y    , na.rm = TRUE,digits = 4)))
            
        }
        
        # gof_ww <- gof_ww[which(rownames(gof_ww) %in% c("PBIAS %", "NSE")),] 
        
        
        out <- list(gof_ww = gof_ww,
                    simul  = simul) 
        message("     Returning Funs e Estats")
        return(out)
    })
  
        names(teste_satcap)  <- Spars$id_sim
        saveRDS(teste_satcap,paste0(run_dir,run_id,"_SgSc_All.rds") )
        teste_satcap
}

GOf_satcap_fun <-function(teste_satcap, id = Spars$id_sim, gof.VAR){
    
    sapply(1:length(teste_satcap), function(i){ #  i = 3
        
        if("vwc" %in% gof.VAR){
            # teste_satcap[[i]]$gof_ww["PBIAS %",] <-  abs(teste_satcap[[i]]$gof_ww["PBIAS %",]  )
            teste_satcap[[i]]$gof_ww$vwc <-  with(teste_satcap[[i]]$gof_ww, (vwc20+vwc01+vwc02)/3 )
            
            n1 <- names(teste_satcap[[i]]$gof_ww)
            v1 <- teste_satcap[[i]]$gof_ww[3,] %>% setNames(paste0("PBIAS_",n1)) %>% set_rownames(NULL)
            v2 <- teste_satcap[[i]]$gof_ww[5,] %>% setNames(paste0("NSE_",n1)) %>% set_rownames(NULL)
            
        } else if("swc02" %in% gof.VAR){
            
            # teste_satcap[[i]]$gof_ww["PBIAS %",] <-  abs(teste_satcap[[i]]$gof_ww["PBIAS %",]  )
            
            n1 <- names(teste_satcap[[i]]$gof_ww)
            v1 <- teste_satcap[[i]]$gof_ww[3,] %>% setNames(paste0("PBIAS_",n1)) %>% set_rownames(NULL)
            v2 <- teste_satcap[[i]]$gof_ww[5,] %>% setNames(paste0("NSE_",n1)) %>% set_rownames(NULL)
    
        }
        
        return(c(v1,v2) %>% unlist) 
    }) %>% t  %>%  as.data.frame %>% mutate(id_sim = id)

}

order_vars <- function(data){
    # data = gof_vars
    www <- c("H", "LE", "vwc","vwc20", "vwc01", "vwc02")
    if(all(www %in% unique(data$variable))) {
        data <- mutate(data, variable = ordered(variable, labels = www))    
    }  
    return(data)
}

make.satcap.analises <- function(GOf_satcap, run_id){

    gof_vars <- GOf_satcap %>% 
        gather(stat, value, -id_sim) %>%
        separate(stat, c("indice", "variable"), sep = "_") %>%
        spread(indice, value) %>% 
        separate(id_sim, c("Sg", "Sc"), sep = "_")
    
    gof_vars <- gof_vars %>% order_vars()
        
        scatterPlot(mydata = gof_vars, x = "NSE", 
                    y = "PBIAS", 
                    type = c("variable", "Sc"), 
                    group = "Sg",cex = 1.2,
                    ref.y = list(h = 0, col = "gray50", lty = 2),
                    main = paste0("Rodada ",run_id)#,
                    # yscale.components = yscale.components.subticks
                    # scales = list(y = list(alternating = 1,relation = "free"),
                    #               x = list(alternating = 1))
                    )
    
}

select.correction.satcap <- function(Spars,teste_satcap, Sg, Sc, run_dir , run_id){
   
  # index <- with(Spars, which(satcap_g %>% as.numeric == Sg & satcap_c %>% as.numeric  == Sc))
    # index <-
    # which( Spars$satcap_g %>% as.numeric == as.numeric(Sg) ) 
    # which(Spars$satcap_c %>% as.numeric  == 0.0001 ) 
    # 
    aSg <- grep(Spars$id_sim,pattern = as.character(sprintf("%.4f",Sg)))
    aSc <- grep(Spars$id_sim,pattern = as.character(sprintf("%.4f",Sc)))
    index <- aSg[aSg %in% aSc]
            message(index)
    SCap_correction <- teste_satcap[c(1,index)] 
 
        saveRDS(SCap_correction, paste0(run_dir , run_id,"SgSc_best.rds"))
 teste_satcap[[index]][[2]] %>% as.data.frame
#  SCap_correction

}



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
    require(gridExtra)
    
    minaxes <- min(sim1[,2], na.rm = TRUE)
    maxaxes <- max(sim1[,2], na.rm = TRUE)
    # inter <- as.integer( (maxaxes -  minaxes)/30)
    
    limits <- timeAverage(sim1,avg.time = "day") %>% select(-date) %>% as.vector %>% range()
    limdec <- 
        timeAverage(sim1) %>% 
        select(-date) %>% 
        as.vector %>% range() %>% 
        diff() %>% divide_by(6)
    
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
                             obs,
                             vazao_file = "~/Dissertação/RODADAS_ROILAN/OutrosDados/monthly_data_vazao_pauliceia.rds",
                             run_id
                             ){
    
    best.calib_med <- sim %>% 
        mutate(LE = wm2.to.mm(LE,nhour = 1)) %>% 
        mutate(LE.obs = wm2.to.mm(obs$LE,nhour = 1)) %>%
        select(date, LE,LE.obs, tprec, roff) %>%  
        timeAverage(avg.time="month",statistic = "sum") 
    
    #%>%
    # mutate(ce = roff/tprec, fe = LE/tprec)# %>% 
    # select(date, tprec, LE, roff, ce, fe) %>% 
    # group_by(year = month(date)) %>%
    # summarise_each(funs(mean))
    
    qmon_obs <- readRDS(vazao_file)
    qmon <- left_join(qmon_obs, best.calib_med) 
    
    coef.graph <- timePlot(data.frame(qmon), 
                           c("tprec","roff", "vazao_mm","LE","LE.obs"), #,"prec_t"
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
               fe.obs = LE.obs/tprec,
               ce_obs = vazao_mm/tprec,
               ce_obs2 = vazao_mm/prec_t) %>% 
        select(tprec, LE, roff, ce, ce_obs, ce_obs2, fe,fe.obs) %>% 
        t %>% as.data.frame() %>% 
        setNames(run_id)
    # 
    
    
    # # horarrios de vazao
    # qh <- readRDS("./OutrosDados/vazaoh_psx_pauliceia.rds")
    # timePlot(qh, c("prec_t", "vazao"))
    # 
    return(list(coef.table = coef.table,coef.graph = coef.graph))
}























