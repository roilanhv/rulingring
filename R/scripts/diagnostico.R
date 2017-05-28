






















do_basic_serie_plot <- function(data=met_data, var=variable_){
    
    # NOTA: modificar aqui para incluir 
    tdy.met_data <- 
        data %>% 
        gather(variables,values,-date) %>% 
        as_tibble()
    
    serie_natural <- 
        tdy.met_data %>% 
        filter(variables == var) %>%
        ggplot(aes(x=date, y = values,colour=variables) ) +
        geom_line(color = "red") + theme_bw() + ylab(label = var) +
        scale_x_datetime(name = element_blank(),
                         breaks = pretty_breaks(n = 15),minor_breaks = pretty_breaks(n = 15),
                         date_labels = "%b\n%Y") 
        
    ciclo_diario <- 
        tdy.met_data %>% 
        filter(variables == var) %>%
        group_by(month = month(date), hour = hour(date)) %>%
        summarise_each(funs(mean),c(-date,-variables)) %>%
        ggplot(aes(x= hour, y=values)) +
        geom_line(color = "red") + theme_bw() + facet_wrap(~month, ncol = 12)  
        
    decomp_serie <-
        tdy.met_data %>% 
        decompose()
        
    
}
###############################################################################
##                PREPARATION FOR GRAPHICS
##                       LabPHI (2016)
###############################################################################
##     SOME SPECIFICATIONS MADED BY AUTHORS
##  1- Initial construct: Roilan HV (2016-07)
##    -
##    -  
###############################################################################      
    # Loading packages:
    packs <- c("dplyr", "readr", "magrittr","openair","tidyr", "tibble","ggplot2",
               "gridExtra","lubridate","scales","xts")
    invisible(sapply(packs,require,character.only = TRUE,quietly = TRUE))


    # Here load the data:
    met_data <- read_rds("~/Dissertação/PAMPA/DADOS/Dados_SM_Nov2013Set2015_30min_posFBE.rds") %>%
        mutate(date = as.POSIXct(date))
    head(met_data)    
 
    # Choose ONE variable to work on:
    variable_ <- "Tar_inmet"

    # met_data %>% 
        





        
