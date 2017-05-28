



### Script para criar dados de entrada a partir do arquivo sib2diag

diag.2.input <- function(sib2diag = sib2diag,
                         name.file = "FROM_SiB2Diag.txt",
                         dir_out = "~/Dropbox/Dissertacao/"
                         ){   
    
  atm.vars <- sib2diag %>% select(date,ki,rn,em, tm,um,tprec,lai,ldwn,fpar)
summaryPlot(atm.vars %>% mutate(tm = tm-273.15))

dbhm_vars <- data.frame(sta_id = 90009,
                        nymd = paste0(atm.vars %$% date %>% format("%Y%m%d"),
                                      ifelse((hour <-  atm.vars %$% date %>% 
                                                format("%H") %>% 
                                                as.numeric %>%
                                                add(1)) < 10 , paste0("0",hour),hour)  ),
                        ki = atm.vars %$% ki %>% round(2),
                        rn = atm.vars %$% rn %>% round(2),
                        em = atm.vars %$% em %>% round(2),
                        tm = atm.vars %$% tm %>% round(2),
                        um = atm.vars %$% um  %>% round(2) ,
                        prec = atm.vars %$% tprec %>% round(2),
                        zlt = atm.vars %$% lai %>% round(2),
                        fpar = atm.vars %$% fpar %>% round(2),
                        zlwad = atm.vars %$% ldwn %>% round(2))

write.table(dbhm_vars,
            file = paste0(dir_out,name.file),
            quote = FALSE,
            sep = "\t",
            na = "-9999",
            row.names = FALSE,
            col.names = TRUE)
return(cat("File created > ",paste0(dir_out,name.file),"\n" ))
}

