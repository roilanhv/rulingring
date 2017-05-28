##
##    Script para gerar arqivo de dados para Calibração na 
##    bacia do córrego Pauliceia
##    
##    
require(REddyProc)
require(openair)
require(plyr)
require(dplyr)
require(magrittr)
require(RcppRoll)
source("~/Dropbox/Dissertacao/scripts/cleanQuantis.R")


## Criando arquivo de LAI E GREEN segundo Humberto 
## 
## 
LAI <- c(4.00,  4.00,  4.00,  4.00,  4.00,  3.50,  3.00,  1.50,  1.50,  2.00,  3.50 , 4.00)
GREEN <- c(0.90 , 0.90,  0.90,  0.85,  0.80,  0.75,  0.55,  0.40,  0.40 , 0.40,  0.60,  0.80)


df <- data.frame(month = 1:12,LAI, GREEN)

dt <- data.frame(date = lubridate::ymd(as.Date(do.call("paste",c(expand.grid(2001:2014, 1:12, 1), sep = "-")))))
dt$month <- lubridate::month(dt$date)
fj <- full_join(dt,df)
str(fj)
fj_lai_green <- timeAverage(fj, avg.time = "hour")
fj_lai_green <- fj_lai_green %>% 
    fill(LAI, GREEN) %>% 
    select(-month)
fj_lai_green <- fj_lai_green %>% 
    slice(-nrow(fj_lai_green))
fj_lai_green
    

do.call("paste", c(tmp, sep = ""))

torre <- read.table("/home/hidro2roilan/Dropbox/Dissertacao/TORRE.txt", header = TRUE)
    torre$zlt <- fj_lai_green$LAI
    torre$green <- fj_lai_green$GREEN
    
    date = data.frame(date = seq.POSIXt(from = as.POSIXct("2001010102",tz = "GMT","%Y%m%d%H")-3600,
                                        to =  as.POSIXct("2015010101",tz = "GMT","%Y%m%d%H")-3600,
                                        by = "hour"))
    
    nrow(date[1])

    head(torre)

write.table(x = torre, file = "/home/hidro2roilan/Dropbox/Dissertacao/TORRE_HUMB.txt", 
            sep = "\t",row.names = FALSE,col.names = TRUE,quote = FALSE)



























dataOriginal <- readRDS("~/Dropbox/Dissertacao/data2sens&calib_salva.rds") %>%
    select(-NEE)

        head(dataOriginal)

Nee_hor <- readRDS("~/Dropbox/Dissertacao/NEE_1h_pdg_2009_2014_filt.rds")
    head(Nee_hor) 
    timePlot(Nee_hor, "NEE", avg.time = "day")

    dataNew <- merge(dataOriginal, Nee_hor %>% select(date, NEE), by = "date", all.x = TRUE)
head(dataNew)
range(dataNew$date)
        saveRDS(dataNew, "~/Dropbox/Dissertacao/data2sens&calib.rds")

# Converter Cont de Água a umidade
        data <- readRDS("~/Dropbox/Dissertacao/data2sens&calib.rds")
        head(data)
        
data %<>% mutate(vwc20 = swctop / 200,
                 vwc01 = (swc1m ) / 1000,
                 vwc02 = (swc2m - swc1m ) / 1000)

timePlot(data, c("vwc20", "vwc01","vwc02"), group =F, lty = 1, lwd = 2)
timePlot(data, c("swctop", "swc1m","swc2m"), group = T, lty = 1, lwd = 2)

saveRDS(data, "~/Dropbox/Dissertacao/data2sens&calib.rds")





getcycleAvgTS <- function(x, each8day) {
    viclima <- round(rep(c(t(unlist(lapply(split(x,each8day), mean, na.rm=T)))),
                         length(x)/max(each8day)), 2)
    data2fill <- viclima[is.na(x)]
    return(data2fill)
}

##----------------------------------------------------##
## Consertar horário de verão nas observações
## 
dataOriginal <- readRDS("~/Dropbox/Dissertacao/data2sens&calib.rds")
dataOriginal %>% filter(is.na(LE) | is.na(H) | is.na(rn))
# Datas de troca de horário de verão:
# 2009-10-18 00:00:00
a1 <- subset(dataOriginal , 
           date >= as.POSIXct("2009-10-18 00:00:00", tz="GMT")  & 
           date <= as.POSIXct("2010-02-22 00:00:00", tz="GMT")) %>% 
    select(-date)
        l1_a1 <- rbind(a1[-1,c("LE","H","rn")],a1[nrow(a1),c("LE","H","rn")])
        nrow(l1_a1)

start <- which(dataOriginal$date == as.POSIXct("2009-10-18 00:00:00", tz="GMT"))
end <- which(dataOriginal$date == as.POSIXct("2010-02-22 00:00:00", tz="GMT"))

dataOriginal[start:end, c("LE","H","rn")] <- l1_a1

# 2010-10-17 00:00:00
a2 <- subset(dataOriginal , 
             date >= as.POSIXct("2010-10-17 00:00:00", tz="GMT")  & 
                 date <= as.POSIXct("2011-02-2 00:00:00", tz="GMT"))

l1_a2 <- rbind(a2[-1,c("LE","H","rn")],a2[nrow(a2),c("LE","H","rn")])
nrow(l1_a2)

start <- which(dataOriginal$date == as.POSIXct("2010-10-17 00:00:00", tz="GMT"))
end <- which(dataOriginal$date == as.POSIXct("2011-02-2 00:00:00", tz="GMT"))

dataOriginal[start:end, c("LE","H","rn")] <- l1_a2

# 2011-10-16 00:00:00
a3 <- subset(dataOriginal , 
            date >= as.POSIXct("2011-10-16 00:00:00", tz="GMT")  & 
            date <= as.POSIXct("2012-02-19 00:00:00", tz="GMT"))

l1_a3 <- rbind(a3[-1,c("LE","H","rn")],a3[nrow(a3),c("LE","H","rn")])
nrow(l1_a3)

start <- which(dataOriginal$date == as.POSIXct("2011-10-16 00:00:00", tz="GMT"))
end <- which(dataOriginal$date == as.POSIXct("2012-02-19 00:00:00", tz="GMT"))

dataOriginal[start:end, c("LE","H","rn")] <- l1_a3

saveRDS(dataOriginal, "~/Dropbox/Dissertacao/data2sens&calib_hourok.rds")
## 
#####################################################################################################
# torre <- read.table("~/Dropbox/Dissertacao/TORRE.txt", head = TRUE )
#             n=3*15*24
#     LAI$X30 <- lead(LAI$X30,n=n)
#         timePlot(LAI, "X30",avg.time = "15 day", 
#                  date.breaks = 24)
#         torre$zlt <- LAI$X30
#         torre$zlt[is.na(torre$zlt)] <- mean(torre$zlt,na.rm = TRUE)
#             torre$zlt <- round(torre$zlt,2)
# write.table(x = torre #%>% select(-date),
#            ,file = "~/Dropbox/Dissertacao/TORRE_lead.txt",
#             sep = "\t",row.names = FALSE,col.names = TRUE,quote = FALSE)
# 
######################################################################################################

#####################################################################################################
n=2*15*24
n
torre <- read.table("~/Dropbox/Dissertacao/TORRE.txt", head = TRUE ) %>% 
  mutate(date = as.POSIXct(nymd %>% as.character, tz = "GMT" , format = "%Y%m%d%H"),
         zltlag = dplyr::lead(zlt,n) )

timePlot(torre %>% selectByDate(year=2009:2012), c("zlt","zltlag"), group = TRUE)

torre$zlt <- torre$zltlag
torre$zltlag <- NULL
torre$date <- NULL
torre$zlt[is.na(torre$zlt)] <- mean(torre$zlt,na.rm = TRUE)
torre$zlt <- round(torre$zlt,2)

write.table(x = torre #%>% select(-date),
            ,file = paste0("~/Dropbox/Dissertacao/TORRE_lead",n,".txt"),
            sep = "\t",row.names = FALSE,col.names = TRUE,quote = FALSE)
######################################################################################################
## LAI DO NDVI
#####################################################################################################
n=0
Lai_ndvi <- readRDS("~/Dropbox/Dissertacao/LAI_ndvi_bacia_filtr_2001_2014_v2.rds") %>%
  select(X30) 
torre <- read.table("~/Dropbox/Dissertacao/TORRE.txt", head = TRUE ) %>% 
  mutate(date = as.POSIXct(nymd %>% as.character, tz = "GMT" , format = "%Y%m%d%H"),
         zlt_ndvi = Lai_ndvi %>% t %>% c,
         zlt_ndvi = dplyr::lead(zlt_ndvi,n))

timePlot(torre %>% selectByDate(year=2009:2012), c("zlt","zlt_ndvi"), 
         group = TRUE,lty =1,date.breaks = 16, date.format = "%b\n%Y",
         ref.x = list(v = as.POSIXct(c("2010-05-01","2011-02-01","2011-05-01","2012-02-01"),
                                     tz = "GMT",format="%Y-%m-%d")))

torre$zlt <- torre$zlt_ndvi
  torre$zlt_ndvi <- NULL
  torre$date <- NULL
torre$zlt[is.na(torre$zlt)] <- mean(torre$zlt,na.rm = TRUE)
torre$zlt <- round(torre$zlt,2)
write.table(x = torre #%>% select(-date),
            ,file = "~/Dropbox/Dissertacao/TORRE_ndvi.txt",
            sep = "\t",row.names = FALSE,col.names = TRUE,quote = FALSE)
######################################################################################################




N <- readRDS("~/Dropbox/Dissertacao/N_ndvi_bacia_filtr_2001_2014_v2.rds")
torre$green <- round(N$X30,2) 

scatterPlot(torre,x = "zlt",y = "green",linear = TRUE)

lai_fpar_OBS <- readRDS("~/Dropbox/Dissertacao/lai_torre_obs_2001_2014.rds") 
lai_fpar_OBS %<>% mutate(FPAR_FILT = 1-(LAI.FILT/0.0038)^(-1/1.84))



plot(torre$zlt[70000:length(torre$zlt)], type = "l")
lines(lai_fpar_OBS$LAI.OBS[70000:length(torre$zlt)],lwd = 2, col = "red")
lines(lead(torre$zlt[70000:length(torre$zlt)], n = 2*15*24),lwd = 2, col = "blue")
# fpar <- readRDS("~/Dropbox/Dissertacao/fpar_bacia_filtr_2001_2014.rds")
torre$fpar <- lai_fpar_OBS$FPAR_FILT
#-------------------------------------------------------------------------------------------------

data <- readRDS("~/Dropbox/Dissertacao/data2sens&calib.rds")
nee <- readRDS("~/Dropbox/Dissertacao/nee_hh_r_qc_flags_pdg.rds") %>% 
    select(date,NEE,lai)

nee %<>% timeAverage(avg.time = "hour") %T>%
timePlot("NEE")

data <- merge(data,nee,by = "date",all.x = TRUE)

head(data)
tail(data)


saveRDS(data,"~/Dropbox/Dissertacao/data2sens&calib.rds")

##################################################################################################
#    CALCULANDO GREEN A PARTIR DO FPAR OBSERVADO

all.data <- readRDS("~/Dropbox/Dissertacao/OK_1hr_torre_2001_2014_comfalhas.rds") %>%   
    select(date,PAR_IN,PAR_OUT) %>%
    cleanQuantis(vars = c("PAR_IN","PAR_OUT"),qqts = c(0.01,0.99),times = "%H") %>% 
    mutate(albedoPAR = PAR_OUT/PAR_IN) %>%
    mutate(daytime = ifelse( fCalcPotRadiation( DoY.V.n = format(date,"%j") %>% as.numeric,
                                                Hour.V.n = format(date,"%H") %>% as.numeric,
                                                Lat_deg.n = -21.61,
                                                Long_deg.n = -47.63,
                                                TimeZone_h.n = -3,
                                                useSolartime.b = TRUE) > 0 , 
                             "day","night") ) %>%
    mutate(albedoPAR = ifelse(daytime == "day",albedoPAR,NA) ) %>% 
    mutate(albedoPAR = roll_mean(albedoPAR, n = 15*24,fill = c(0),align = "center",na.rm = TRUE)) %>%
    mutate(albedoPAR = ifelse( (albedoPAR > 0.1 | albedoPAR < 0.02 ), NA, albedoPAR)) %>%
    mutate(albedoPAR = ifelse(is.na(PAR_OUT),NA,albedoPAR)) %>%
    filter(daytime == "day") %T>% 
    timePlot(c("PAR_IN","PAR_OUT","albedoPAR")) %>%
    select(date,albedoPAR) %>% 
    timeAverage(avg.time = "day") %T>%
    timePlot("albedoPAR")

choosen <- seq.POSIXt(from = as.POSIXct("2010-01-01",tz = "GMT",format="%Y-%m-%d"),
           to = as.POSIXct("2010-12-31",tz = "GMT",format="%Y-%m-%d"),
           by = "day")

all.data <- all.data %>% 
    filter(format(date,"%Y") %in% 2001:2014) %>%
    filter(format(date,"%m-%d") %in% format(choosen,"%m-%d"))

timePlot(all.data.15days,"albedoPAR")


getcycleAvgTS <- function(x, each8day) {
    viclima <- round(rep(c(t(unlist(lapply(split(x,each8day), mean, na.rm=T)))),
                         length(x)/max(each8day)), 4)
    data2fill <- viclima[is.na(x)]
    return(data2fill)
}

fills <- getcycleAvgTS(x = all.data$albedoPAR, each8day = rep(1:365,14))

all.data$albedoPAR[is.na(all.data$albedoPAR)] <- fills


timePlot(all.data,"albedoPAR")

all.data.15days <- all.data %>% mutate(albedo_roll = roll_mean(albedoPAR,
                                                    n = 15,
                                                    align = "center",
                                                    fill = mean(albedoPAR)),
                          fpar = 1 - albedo_roll) %T>% 
    timePlot("fpar")

choosen2 <- seq.POSIXt(from = as.POSIXct("2010-01-01",tz = "GMT",format="%Y-%m-%d"),
                      to = as.POSIXct("2010-12-31",tz = "GMT",format="%Y-%m-%d"),
                      by = "15 day")

all.data.15days %<>%
    filter(format(date,"%m-%d") %in% format(choosen2,"%m-%d"))

timePlot(all.data.15days,c("albedoPAR","fpar"), 
         ref.x = c(as.POSIXct("2009-07-01",format="%Y-%m-%d"),
                   as.POSIXct("2012-06-30",format="%Y-%m-%d")))

all.data.15days %<>% mutate(green = (fpar-min(fpar))/(max(fpar)-min(fpar))) %T>%
    timePlot("green") 

###########
########### LLEVANDO LOS DADOS A HORARIOS
########### 

green.hly <- data.frame(date = seq.POSIXt(from =as.POSIXct("2001-01-01 00:00",
                                                               tz="GMT",
                                                               format="%Y-%m-%d %H:%M"),
                                              to = as.POSIXct("2014-12-31 23:00",
                                                              tz="GMT",
                                                              format="%Y-%m-%d %H:%M"),
                                              by="hour"  ))
green.hly <- cbind(green.hly,green = NA)

breakdata <- unique(format(all.data.15days$date, "%Y-%m-%d"))

for(i  in  1:length(breakdata)){ # i=length(breakdata)
    cat(".")
    if(i < length(breakdata)){   
        sus.rows <- which(format(green.hly$date,"%Y-%m-%d") >= breakdata[i] &
                              format(green.hly$date,"%Y-%m-%d") < breakdata[i+1]) 
    } else {
        sus.rows <- which(format(green.hly$date,"%Y-%m-%d") >= breakdata[i])
        cat("\n")
    }
    green.hly[sus.rows,-1] <- all.data.15days[which(format(all.data.15days$date,"%Y-%m-%d")  == breakdata[i]),
                                              "green"]
}

timePlot(green.hly,"green")

        amplitud = 0.75
green.hly %<>% mutate(final = green*(0.98-amplitud) + amplitud) %T>%
    timePlot(c("green","final"), group = TRUE)
                          
torre <- read.table("~/Dropbox/Dissertacao/TORRE.txt", head = TRUE )                          
torre$green <- round(green.hly$green,2)
write.table(x = torre #%>% select(-date),
            ,file = "~/Dropbox/Dissertacao/TORRE.txt",
            sep = "\t",row.names = FALSE,col.names = TRUE,quote = FALSE)

######################################
######  LAI OBSERVADO

lai_fpar_OBS <- readRDS("~/Dropbox/Dissertacao/lai_torre_obs_2001_2014.rds") 
    timePlot(lai_fpar_OBS, c("LAI.OBS","LAI.FILT"))
    
    
    data <- readRDS("~/Dropbox/Dissertacao/data2sens&calib.rds") %>% 
        select(date,lai) %T>%
        timePlot("lai") %>% 
        mutate(lai = roll_mean(x = lai,n = 30*15, align = "center",na.rm = TRUE,fill = mean(lai))) %T>%
        timePlot("lai") %>%
        timeAverage(avg.time = "day") %T>%
        timePlot("lai") %>%
        filter(format(date,"%Y-%m-%d") %in% breakdata)
    
    new.breakdata <- as.character(data$date[!is.na(data$lai)])

for(i  in  1:(length(new.breakdata)-1)){ # i=2
    cat(".")
    if(i < length(new.breakdata)){   
        sus.rows <- which(format(lai_fpar_OBS$date,"%Y-%m-%d") >= new.breakdata[i] &
                              format(lai_fpar_OBS$date,"%Y-%m-%d") < new.breakdata[i+1]) 
    } else {
        sus.rows <- which(format(lai_fpar_OBS$date,"%Y-%m-%d") >= new.breakdata[i])
        cat("\n")
    }
    lai_fpar_OBS[sus.rows,"LAI.FILT"] <- data[which(format(data$date,"%Y-%m-%d")  == new.breakdata[i]),
                                              "lai"]
}    
    timePlot(lai_fpar_OBS %>% selectByDate(year = 2009:2012),"LAI.FILT")
    
    
    torre <- read.table("~/Dropbox/Dissertacao/TORRE.txt", head = TRUE )                          
    torre$zlt <- round(lai_fpar_OBS$LAI.FILT,2)
    write.table(x = torre #%>% select(-date),
                ,file = "~/Dropbox/Dissertacao/TORRE.txt",
                sep = "\t",row.names = FALSE,col.names = TRUE,quote = FALSE)                          
                          
                          
                          
                          
                          
                      
                      
                      