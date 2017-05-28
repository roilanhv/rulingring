####
####
####   GERAR GRENNNES

        torre <- fread(input = "~/Dropbox/Dissertacao/TORRE.txt",header = TRUE)
        
        line_type <- c(LAI="dotted",N_0="dotted")
        
        N_LAI <- 
            torre %>% 
            mutate(date = as.POSIXct(nymd %>% as.character, format= "%Y%m%d%H", tz = "GMT"),
                   nymd = NULL, sta_id = NULL)  %>%
            select(date,green,zlt) %>% rename(LAI = zlt, N_0 = green) %>%
            data.frame() %>%
            timeAverage(avg.time = "15 day") %>%
            selectByDate(year=2009:2012) %>%
            mutate(N_0.3 = N_0 * (0.98 - 0.3) + 0.3,
                   N_0.5 = N_0 * (0.98 - 0.5) + 0.5,
                   N_0.75 = N_0 * (0.98 - 0.75) + 0.75,
                   LAI_N = LAI * N_0.5) %>%
            gather(vars,value,-date) %>%
            ggplot(aes(x=date,y=value, color = vars, linetype = vars))+
            geom_line(size = 0.9) +
            theme_bw() +
            # scale_linetype(value=line_type)+
            scale_y_continuous(name = "LAI, LAI_N, N", 
                               breaks = pretty_breaks(10), 
                               minor_breaks = pretty_breaks(20))+
            scale_x_datetime(name = element_blank(), 
                             date_breaks = "3 month",
                             date_minor_breaks =  "1 month",
                             date_labels = "%b\n%Y",expand = c(0,0) )+
            theme(legend.title= element_blank()
                  # legend.position = c(2.0,3.0), 
                  # legend.direction = "horizontal"
                  )
            
        
        pdf("~/Dropbox/Dissertacao/DOCUMENTO/figuras/LAI_N.pdf",width = 8, height = 3)
            N_LAI
        dev.off()
        # 
        # torre %>% 
        #     mutate(date = as.POSIXct(nymd %>% as.character, format= "%Y%m%d%H", tz = "GMT"),
        #            nymd = NULL, sta_id = NULL)  %>%
        #     select(date,zlt) %>% rename(LAI = zlt) %>%
        #     data.frame() %>%
        #     timeAverage(avg.time = "15 day") %>%
        #     selectByDate(year=2009:2012) %>%
        #     gather(vars,value,-date) %>%
        #     ggplot(aes(x=date,y=value, color = vars))+
        #     geom_line() +
        #     theme_bw()
        #     
        GloMcNzArmHR_best <- 
            subset(rung_best %>% as.data.frame(),
                   rownames(rung_best %>% as.data.frame()) %in% rownames(run3_best %>% as.data.frame()))
        
        
        PARAMS <- cbind(TcArm = run2_best %>% as.data.frame(), 
                        McNzArm = run6_best %>% as.data.frame(),
                        McNzArmHR = run5_best %>% as.data.frame(),
                        McDzArmHR = run3_best %>% as.data.frame(),
                        GloMcNzArmHR = GloMcNzArmHR_best)
        
        names(PARAMS) <- c("TcArm","McNzArm","McNzArmHR", "McDzArmHR","GloMcNzArmHR")

            kable(PARAMS, format = "latex")
            