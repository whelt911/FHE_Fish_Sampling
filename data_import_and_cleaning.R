####Import and Save FHE Fish Sampling Data to Working Directory####
# By Will Helt -- 2016-09-15

#Working Directory
setwd("H:/Will's Drive/R_wd/fhe/FHE_Fish_Sampling")

#Load Packages
library('stringr')
library('lubridate')
library('data.table')     #Required for renaming column setnames()

#Read in Access Exports
ep_dat <- read.csv("EelPot_Query.txt")
gn_dat <- read.csv("Gillnet_Query.txt")
mt_dat <- read.csv("MinnowTrap_Query.txt")

#Merge with site data to show empty hauls
ep_mt_site_dat <- read.csv("EelPot_MinnowTrap_SiteData.txt")
gn_site_dat <- read.csv("Gillnet_SiteData.txt")

ep_dat <- merge(ep_dat, ep_mt_site_dat, by = c("Date","Time.In","Time.Out","Pond","Site.ID"), all.y = T )
mt_dat <- merge(mt_dat, ep_mt_site_dat, by = c("Date","Time.In","Time.Out","Pond","Site.ID"), all.y = T )
gn_dat <- merge(gn_dat, gn_site_dat, by = c("Date","Time.In","Time.Out","Pond","Site.ID"), all.y = T )


#Clean up date and time
ep_dat$Date <- as.Date(ep_dat$Date, format='%m/%d/%Y')
gn_dat$Date <- as.Date(gn_dat$Date, format='%m/%d/%Y')
mt_dat$Date <- as.Date(mt_dat$Date, format='%m/%d/%Y')

ep_dat$Time.In <- strptime(ep_dat$Time.In, format = '%m/%d/%Y %H:%M:%S')
gn_dat$Time.In <- strptime(gn_dat$Time.In, format = '%m/%d/%Y %H:%M:%S')
mt_dat$Time.In <- strptime(mt_dat$Time.In, format = '%m/%d/%Y %H:%M:%S')

ep_dat$Time.Out <- strptime(ep_dat$Time.Out, format = '%m/%d/%Y %H:%M:%S')
gn_dat$Time.Out <- strptime(gn_dat$Time.Out, format = '%m/%d/%Y %H:%M:%S')
mt_dat$Time.Out <- strptime(mt_dat$Time.Out, format = '%m/%d/%Y %H:%M:%S')

#add soak time column (hours)
ep_dat$Soak.Time <- difftime(ep_dat$Time.Out, ep_dat$Time.In)

gn_dat$Time.Out <- gn_dat$Time.Out + days(1) 
gn_dat$Soak.Time <- difftime(gn_dat$Time.Out, gn_dat$Time.In)

mt_dat$Soak.Time <- difftime(mt_dat$Time.Out, mt_dat$Time.In)

#add Month column
ep_dat$Month <- month(ep_dat$Date)
gn_dat$Month <- month(gn_dat$Date)
mt_dat$Month <- month(mt_dat$Date)

#add Year column
ep_dat$Year <- year(ep_dat$Date)
gn_dat$Year <- year(gn_dat$Date)
mt_dat$Year <- year(mt_dat$Date)

#add Impact column (Before/After)
ep_dat$Impact <- ifelse(ep_dat$Date > "2016-01-01", "After","Before")
gn_dat$Impact <- ifelse(gn_dat$Date > "2016-01-01", "After","Before")
mt_dat$Impact <- ifelse(mt_dat$Date > "2016-01-01", "After","Before")

#Target Species (Yes/No)
##Target Species: Black Sea Bass, Winter Flounder, Summer Flounder, Tautog, Scup
ep_dat$Target.Species <- ifelse(ep_dat$Species == "141","Yes",
                                ifelse(ep_dat$Species == "177","Yes",
                                       ifelse(ep_dat$Species == "143","Yes",
                                              ifelse(ep_dat$Species == "103", "Yes",
                                                     ifelse(ep_dat$Species == "106","Yes","No")))))
gn_dat$Target.Species <- ifelse(gn_dat$Species == "141","Yes",
                                ifelse(gn_dat$Species == "177","Yes",
                                       ifelse(gn_dat$Species == "143","Yes",
                                              ifelse(gn_dat$Species == "103", "Yes",
                                                     ifelse(gn_dat$Species == "106","Yes","No")))))
mt_dat$Target.Species <- ifelse(mt_dat$Species == "141","Yes",
                                ifelse(mt_dat$Species == "177","Yes",
                                       ifelse(mt_dat$Species == "143","Yes",
                                              ifelse(mt_dat$Species == "103", "Yes",
                                                     ifelse(mt_dat$Species == "106","Yes","No")))))
 
#Rename COMMON.NAME to Common.Name
setnames(ep_dat, "COMMON.NAME", "Common.Name")
setnames(gn_dat, "COMMON.NAME", "Common.Name")
setnames(mt_dat, "COMMON.NAME", "Common.Name")

#add Treatment column (control, seeded, unseeded) <- will eventually require ARC, GH, and NR
nin_ep_dat <- subset(ep_dat, ep_dat$Pond == "Ninigret Pond")
nin_gn_dat <- subset(gn_dat, gn_dat$Pond == "Ninigret Pond")
nin_mt_dat <- subset(mt_dat, mt_dat$Pond == "Ninigret Pond")

##Create matrix to equate Site.ID with Treatment
mat1 <- matrix(nrow = 12, ncol = 2)
vec1 <- c('1C','2C','3C','4C','1U','2U','3U','4U','1S','2S','3S','4S')
vec2 <- c('Control','Control','Control','Control',
          'Unseeded','Unseeded','Unseeded','Unseeded',
          'Seeded','Seeded','Seeded','Seeded')
mat1[,1] <- vec1
mat1[,2] <- vec2
colnames(mat1) <- c("Site.ID","Treatment")

##Merge two matrices
nin_ep_dat <- merge(nin_ep_dat, mat1, by= "Site.ID")
nin_gn_dat <- merge(nin_gn_dat, mat1, by= "Site.ID")
nin_mt_dat <- merge(nin_mt_dat, mat1, by= "Site.ID")

#Replace 0's in abiotic data columns(temp, sal, DO, pH) to NA
is.na(nin_ep_dat[,10:13])<- !nin_ep_dat[,10:13]
is.na(nin_mt_dat[,10:13])<- !nin_mt_dat[,10:13]

#Save objects
save(ep_dat, file = "ep_dat")
save(gn_dat, file = "gn_dat")
save(mt_dat, file = "mt_dat")

save(nin_ep_dat, file = "nin_ep_dat")
save(nin_gn_dat, file = "nin_gn_dat")
save(nin_mt_dat, file = "nin_mt_dat")

####End Data Import & Cleaning####

# ####Create CSV files for sharing####
# write.csv(nin_ep_dat, file =  'C:/Users/William.helt/Desktop/nin_ep_dat.csv')
# write.csv(nin_gn_dat, file =  'C:/Users/William.helt/Desktop/nin_gn_dat.csv')
# write.csv(nin_mt_dat, file =  'C:/Users/William.helt/Desktop/nin_mt_dat.csv')
