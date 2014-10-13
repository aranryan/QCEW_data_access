

indlist <- c("10 (Total, all industries)",
             "1011 (Natural resources and mining)",
             "1012 (Construction)",
             "1013 (Manufacturing)",
             "1021 (Trade, transportation, and utilities)",
             "1022 (Information)",
             "1023 (Financial activities)",
             "1024 (Professional and business services)",
             "1025 (Education and health services)",
             "1026 (Leisure and Hospitality)", 
             "1027 (Other services)",
             "1028 (Public administration)",
 #            "1029 (Unclassified)",
              "481 (Air transportation)",
              "4811 (Scheduled air transportation)",
              "71 (Arts, entertainment, and recreation)",
             "7211 (Traveler accommodation)",
             "72111 (Hotels and motels, except casino hotels)",
             "72112 (Casino hotels)")

# "31-33 (Manufacturing)",
# "51 (Information)",
# "52 (Finance and insurance)",
# "54 (Professional and technical services)",
# "55 (Management of companies and enterprises)",
# "72 (Accommodation and food services)",
             
# creates empty data frame
data <- data.frame()
for(n in indlist){            
  fname <- paste("input_data/1990.annual.by_industry/1990.annual ", n, ".csv", sep="")
  new_data <- read.csv(fname, header = TRUE, dec =".", sep=",")
  data <- rbind(data, new_data)
  
  fname <- paste("input_data/2000.annual.by_industry/2000.annual ", n, ".csv", sep="")
  new_data <- read.csv(fname, header = TRUE, dec =".", sep=",")
  data <- rbind(data, new_data)
  
  
  fname <- paste("input_data/2013.annual.by_industry/2013.annual ", n, ".csv", sep="")
  new_data <- read.csv(fname, header = TRUE, dec =".", sep=",")
  data <- rbind(data, new_data)
  
  head(data)
}

# drops county level data 
data <- subset(data, agglvl_code < 70 | agglvl_code > 80) 

write.csv(data, file="output_data/out_data.csv", row.names=FALSE)
