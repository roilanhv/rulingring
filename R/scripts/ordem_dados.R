# para saber qye dados temos

setwd("~/Dropbox/Dissertacao/")
list.files(pattern = ".rds$")
data <- readRDS("data2sens&calib.rds")
otherdata <- readRDS("nee_hh_r_qc_flags_pdg.rds") %>% 
  select(date,NEE,Ustar,swctop,swc1m,swc2m,qc_Ustar_fr,qc_NEE_fr) %>%
  selectByDate(start = as.POSIXct("200907020100",tz="GMT",format= "%Y%m%d%H%M"),
               end = as.POSIXct("201206302330",tz="GMT",format= "%Y%m%d%H%M"))

timePlot(otherdata,"NEE")
timePlot(otherdata,c("Ustar","qc_Ustar_fr"))

data <- merge(data, 
      otherdata %>% select(date,Ustar) %>% timeAverage(avg.time = "hour"),by = "date",all.x = TRUE)

timePlot(data, c("Ustar", "LE"))

data <- merge(data, 
              otherdata %>% 
                select(date,swctop,swc1m,swc2m) %>% 
                timeAverage(avg.time = "hour"),by = "date",all.x = TRUE)

timePlot(data, c("Ustar", "LE","swctop","swc1m","swc2m"))
names(data)[names(data) == "Ustar"] <-  "ustar"
saveRDS(data, "data2sens&calib.rds")
