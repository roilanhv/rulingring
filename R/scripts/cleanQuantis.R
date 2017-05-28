

cleanQuantis <- function(mydata = mydata,
                         vars = "PELE",
                         qqts = c(0.1,0.99),
                         times = "%H:%M",
                         verbose = FALSE
                         ){
    
time.step <- unique(format(mydata$date,times))

for(i in vars){
     for(j in time.step){
         
            qqtiles <- quantile(mydata[which(format(mydata$date,times)==j),i],
                                probs=qqts,
                                na.rm = TRUE) %>% as.vector
            
            mydata[which(format(mydata$date,times)==j),i] <- ifelse(mydata[which(format(mydata$date,times)==j),i] < qqtiles[1],
                                                                    NA,
                                                                    mydata[which(format(mydata$date,times)==j),i])
            
            mydata[which(format(mydata$date,times)==j),i] <- ifelse(mydata[which(format(mydata$date,times)==j),i] > qqtiles[2],
                                                                    NA,
                                                                    mydata[which(format(mydata$date,times)==j),i])
            if(verbose) cat(j," ",qqtiles,"\n")

    } 
}

return(mydata)
}
