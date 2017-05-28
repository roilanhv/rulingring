
require(plyr)
require(lubridate)
require(dplyr)

nee_qc4 <- readRDS("~/Dropbox/Dissertacao/NEE_1h_pdg_2009_2014_filt.rds")
head(nee_qc4)

nee_obs <- nee_qc4 %>% 
    dplyr::select(date, NEE) %>% 
    dplyr::rename(NEE_obs = NEE)


nee_sim <- nee %>% dplyr::rename(NEE_sim = assimn.pad)

dim(nee_sim)
dim(nee_obs)

## 
## 
## 
NSE_nee <- function(sim, obs){
    nee_sim = sim
    nee_obs = obs
nee_j <- left_join(nee_sim, nee_obs) %>% tbl_df
nee_j <- nee_j[complete.cases(nee_j), ]

nee_j_dly <- timeAverage(nee_j, avg.time = "day")
#timePlot(nee_j_dly, names(nee_j_dly)[-1], group = TRUE)

NSE_dly <- NSE.default(sim = nee_j_dly$NEE_sim, obs = nee_j_dly$NEE_obs, na.rm = TRUE)

nee_med_mon_hly <- 
    nee_j %>%
    group_by(month = month(date),hour = hour(date)) %>%
    select(-date) %>%
    summarise_each(funs(med = mean(., na.rm = TRUE), N = total_valid(.))) %>%
    ungroup() 

NSE_hly <- NSE.default(sim = nee_med_mon_hly$NEE_sim_med, obs = nee_med_mon_hly$NEE_obs_med, na.rm = TRUE)

NSE_tot <- 0.5*NSE_hly + 0.5 * NSE_dly
return(NSE_tot)    

}

total_valid <- function(x) sum(!is.na(x))

#NSE_nee(nee_sim, nee_obs)

    


#nee_med_mon_hly  %>% data.frame()

hydroGOF::NSE.default
hydroGOF:::valindex