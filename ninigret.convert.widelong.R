####Convert 2016 Ninigret Gillnet data Wide - Long####

setwd("Z:/Will's Drive/R_wd/fhe/FHE_Fish_Sampling")

library('reshape2')

dat <- read.csv('2016_ninigret_gillnet_data.csv')

dat.long <- melt(dat, na.rm = T, id = c('Date','Site','Mesh.size'))
dat.long <- dat.long[-which(dat.long$value == ""), ]

write.csv(dat.long, "2016_ninigret_gillnet_longdata.csv")
