####Compare size dist. of species caught in FHE vs CPS in Ninigret Pond####

# by: Will Helt
# Date Modified: 1/11/17

# Load Packages
library('ggplot2')
library('plyr')

# Load datasets
load('nin_ep_dat')
load('nin_mt_dat')
load('nin_gn_dat')

####Size distribution####

# Add gear type column to each dataset
nin_ep_dat$Gear.Type <- "Eel Pot"
nin_mt_dat$Gear.Type <- "Minnow Trap"
nin_gn_dat$Gear.Type <- "Gillnet"


# Remove unnecessary columns
nin_ep_dat <- nin_ep_dat[,-c(1:6,10:14,17,19)]
nin_mt_dat <- nin_mt_dat[,-c(1:6,10:14,17,19)]
nin_gn_dat <- nin_gn_dat[,-c(1:6,10,12,15,17)]

# Add Stretch.in column to EP & MT so that columns are equal
nin_ep_dat$Stretch.in <- NA
nin_mt_dat$Stretch.in <- NA


dat <- rbind(nin_gn_dat, nin_ep_dat)
dat <- rbind(dat, nin_mt_dat)

# Read in CPS data
load('H:/Documents/CPS_FHE_dat/cps_fish_dat')

# Clean up CPS data for usability
nini_cps_fish_dat <- subset(cps_fish_dat, cps_fish_dat$Pond == "Ninigret Pond")
nini_2015_cps_fish_dat <- subset(nini_cps_fish_dat, nini_cps_fish_dat$Year == 2015)

colnames(nini_2015_cps_fish_dat)[8] <- 'Length.cm'
nini_2015_cps_fish_dat$Length.mm <- nini_2015_cps_fish_dat$Length.cm * 10

colnames(nini_cps_fish_dat)[8] <- 'Length.cm'
nini_cps_fish_dat$Length.mm <- nini_cps_fish_dat$Length.cm * 10

FHE_summ <- count(dat$Common.Name)
CPS_summ <- count(nini_cps_fish_dat$COMMON.NAME)
compare_surveys <- merge(FHE_summ, CPS_summ, by = 'x', all = T)
colnames(compare_surveys) <- c('Common.Name','FHE','CPS')


# Subset Black Sea Bass
CPS_BSB <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "SEA BASS BLACK")
FHE_BSB <- subset(dat, dat$Common.Name == "SEA BASS BLACK")

# Subset Mummichogs
CPS_mummi <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "MUMMICHOG")
FHE_mummi <- subset(dat, dat$Common.Name == "MUMMICHOG")

# Subset Rainwater Killifish
CPS_rain_killi <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "RAINWATER KILLIFISH")
FHE_rain_killi <- subset(dat, dat$Common.Name == "RAINWATER KILLIFISH")

# Subset Striped Searobin
CPS_striped_searobin <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "SEAROBIN STRIPED")
CPS_striped_searobin <- subset(CPS_striped_searobin, CPS_striped_searobin$Length.mm < 250)
FHE_striped_searobin <- subset(dat, dat$Common.Name == "SEAROBIN STRIPED")

# Subset Tautog
CPS_tautog <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "TAUTOG")
CPS_tautog <- subset(CPS_tautog, CPS_tautog$Length.mm <= 200)
FHE_tautog <- subset(dat, dat$Common.Name == "TAUTOG")

# Subset Bluefish
CPS_bluefish <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "BLUEFISH")
FHE_bluefish <- subset(dat, dat$Common.Name == "BLUEFISH")
                       
# Atlantic Menhaden
CPS_menhaden <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "MENHADEN ATLANTIC")
FHE_menhaden <- subset(dat, dat$Common.Name == "MENHADEN ATLANTIC")                       
                       
# White Mullet
CPS_white_mullet <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "MULLET WHITE")
FHE_white_mullet <- subset(dat, dat$Common.Name == "MULLET WHITE")

# Scup
CPS_scup <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "SCUP")
FHE_scup <- subset(dat, dat$Common.Name == "SCUP")

# Threespine Stickleback
CPS_3spine <- subset(nini_cps_fish_dat, nini_cps_fish_dat$COMMON.NAME == "STICKLEBACK THREESPINE")
FHE_3spine <- subset(dat, dat$Common.Name == "STICKLEBACK THREESPINE")
                       
####Overlapping histogram plots of MT vs. EP vs. GN####

# BSB
hist(subset(FHE_BSB,FHE_BSB$Gear.Type == "Eel Pot")$Length.mm, col=rgb(1,.84,0,0.5),
     xlim = c(50,170), breaks = seq(50,170,10), ylim = c(0,.07), prob = TRUE,
     main="Black Sea Bass", xlab="Length.mm")
hist(subset(FHE_BSB,FHE_BSB$Gear.Type == "Minnow Trap")$Length.mm, col=rgb(.11,.56,1,0.5),
     breaks = seq(50,170,10), add=T, prob = TRUE)
hist(subset(FHE_BSB,FHE_BSB$Gear.Type == "Gillnet")$Length.mm, col=rgb(0,1,0,0.5),
     breaks = seq(50,170,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('Eel Pot ','(n=',as.character(
                       length(subset(FHE_BSB,FHE_BSB$Gear.Type == "Eel Pot")$Length.mm)),')'),
                     paste('Minnow Trap ','(n=',as.character(
                       length(subset(FHE_BSB,FHE_BSB$Gear.Type == "Minnow Trap")$Length.mm)),')'),
                     paste('Gillnet','(n=',as.character(
                       length(subset(FHE_BSB,FHE_BSB$Gear.Type == "Gillnet")$Length.mm)),')')), 
       fill = c(rgb(1,.84,0,0.5),rgb(.11,.56,1,0.5),rgb(0,1,0,0.5)))

# Mummichogs
hist(subset(FHE_mummi,FHE_mummi$Gear.Type == "Eel Pot")$Length.mm, col=rgb(1,.84,0,0.5),
     xlim = c(0,120), breaks = seq(0,200,10), ylim = c(0,.06), prob = TRUE,
     main="Mummichogs", xlab="Length.mm")
hist(subset(FHE_mummi,FHE_mummi$Gear.Type == "Minnow Trap")$Length.mm, col=rgb(.11,.56,1,0.5),
     breaks = seq(0,200,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('Eel Pot ','(n=',as.character(
  length(subset(FHE_mummi,FHE_mummi$Gear.Type == "Eel Pot")$Length.mm)),')'),
  paste('Minnow Trap ','(n=',as.character(
    length(subset(FHE_mummi,FHE_mummi$Gear.Type == "Minnow Trap")$Length.mm)),')'),
  paste('Gillnet','(n=',as.character(
    length(subset(FHE_mummi,FHE_mummi$Gear.Type == "Gillnet")$Length.mm)),')')), 
  fill = c(rgb(1,.84,0,0.5),rgb(.11,.56,1,0.5),rgb(0,1,0,0.5)))

# # Rainwater Killifish
# hist(subset(FHE_rain_killi,FHE_rain_killi$Gear.Type == "Eel Pot")$Length.mm, col=rgb(1,0,0,0.5),
#      xlim = c(0,70), breaks = seq(0,70,10), ylim = c(0,.1), prob = TRUE,
#      main="Rainwater Killifish", xlab="Length.mm")
# hist(subset(FHE_rain_killi,FHE_rain_killi$Gear.Type == "Minnow Trap")$Length.mm, col=rgb(0,0,1,0.5),
#      breaks = seq(0,70,10), add=T, prob = TRUE)
# hist(subset(FHE_rain_killi,FHE_rain_killi$Gear.Type == "Gillnet")$Length.mm, col=rgb(0,1,0,0.5),
#      breaks = seq(0,70,10), add=T, prob = TRUE)
# box()
# legend("topright", c(paste('Eel Pot ','(n=',as.character(
#   length(subset(FHE_rain_killi,FHE_rain_killi$Gear.Type == "Eel Pot")$Length.mm)),')'),
#   paste('Minnow Trap ','(n=',as.character(
#     length(subset(FHE_rain_killi,FHE_rain_killi$Gear.Type == "Minnow Trap")$Length.mm)),')'),
#   paste('Gillnet','(n=',as.character(
#     length(subset(FHE_rain_killi,FHE_rain_killi$Gear.Type == "Gillnet")$Length.mm)),')')), 
#   fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5),rgb(0,1,0,0.5)))

# # Striped Searobin
# hist(subset(FHE_striped_searobin,FHE_striped_searobin$Gear.Type == "Eel Pot")$Length.mm, col=rgb(1,0,0,0.5),
#      xlim = c(0,230), breaks = seq(0,230,10), ylim = c(0,.07), prob = TRUE,
#      main="Striped Searobin", xlab="Length.mm")
# hist(subset(FHE_striped_searobin,FHE_striped_searobin$Gear.Type == "Minnow Trap")$Length.mm, col=rgb(0,0,1,0.5),
#      breaks = seq(0,230,10), add=T, prob = TRUE)
# hist(subset(FHE_striped_searobin,FHE_striped_searobin$Gear.Type == "Gillnet")$Length.mm, col=rgb(0,1,0,0.5),
#      breaks = seq(0,230,10), add=T, prob = TRUE)
# box()
# legend("topright", c(paste('Eel Pot ','(n=',as.character(
#   length(subset(FHE_striped_searobin,FHE_striped_searobin$Gear.Type == "Eel Pot")$Length.mm)),')'),
#   paste('Minnow Trap ','(n=',as.character(
#     length(subset(FHE_striped_searobin,FHE_striped_searobin$Gear.Type == "Minnow Trap")$Length.mm)),')'),
#   paste('Gillnet','(n=',as.character(
#     length(subset(FHE_striped_searobin,FHE_striped_searobin$Gear.Type == "Gillnet")$Length.mm)),')')), 
#   fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5),rgb(0,1,0,0.5)))


####Overlapping histogram plots of CPS (all years) vs. FHE####

# BSB <- CPS subset for only 2015 catch in Nini
hist(CPS_BSB$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,170), breaks = seq(0,200,10), ylim = c(0,.04), prob = TRUE, add = F,
     main="Black Sea Bass", xlab="Length.mm")
hist(FHE_BSB$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,200,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_BSB$Length.mm)),')')
                     ,paste('FHE ','(n=',as.character(length(FHE_BSB$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))

# Mummichog <- CPS subset for only 2015 catch in Nini
hist(CPS_mummi$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,120), breaks = seq(0,120,10), ylim = c(0,.04), prob = TRUE, add = F,
     main="Mummichog", xlab="Length.mm")
hist(FHE_mummi$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,120,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_mummi$Length.mm)),')'),
                     paste('FHE ','(n=',as.character(length(FHE_mummi$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))


# Rainwater Killifish
hist(CPS_rain_killi$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,150), breaks = seq(0,150,10), ylim = c(0,.08), prob = TRUE, add = F,
     main="Rainwater Killifish", xlab="Length.mm")
hist(FHE_rain_killi$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,150,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_rain_killi$Length.mm)),')'),
                     paste('FHE ','(n=',as.character(length(FHE_rain_killi$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))

# Striped Searobin
hist(CPS_striped_searobin$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,220), breaks = seq(0,220,10), ylim = c(0,.025), prob = TRUE, add = F,
     main="Striped Searobin", xlab="Length.mm")
hist(FHE_striped_searobin$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,220,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_striped_searobin$Length.mm)),')'),
                     paste('FHE ','(n=',as.character(length(FHE_striped_searobin$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))

# Tautog
hist(CPS_tautog$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,200), breaks = seq(0,200,10), ylim = c(0,.04), prob = TRUE, add = F,
     main="Tautog", xlab="Length.mm")
hist(FHE_tautog$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,200,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_tautog$Length.mm)),')'),
                     paste('FHE ','(n=',as.character(length(FHE_tautog$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))

# Bluefish
hist(CPS_bluefish$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,240), breaks = seq(0,240,10), ylim = c(0,.04), prob = TRUE, add = F,
     main="Bluefish", xlab="Length.mm")
hist(FHE_bluefish$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,240,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_bluefish$Length.mm)),')'),
                     paste('FHE ','(n=',as.character(length(FHE_bluefish$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))

# Atlantic Menhaden
hist(CPS_menhaden$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,370), breaks = seq(0,370,10), ylim = c(0,.03), prob = TRUE, add = F,
     main="Menhaden", xlab="Length.mm")
hist(FHE_menhaden$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,370,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_menhaden$Length.mm)),')'),
                     paste('FHE ','(n=',as.character(length(FHE_menhaden$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))

# White mullet
hist(CPS_white_mullet$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,210), breaks = seq(0,210,10), ylim = c(0,.06), prob = TRUE, add = F,
     main="White Mullet", xlab="Length.mm")
hist(FHE_white_mullet$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,210,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_white_mullet$Length.mm)),')'),
                     paste('FHE ','(n=',as.character(length(FHE_white_mullet$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))

# Scup
hist(CPS_scup$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,220), breaks = seq(0,220,10), ylim = c(0,.03), prob = TRUE, add = F,
     main="Scup", xlab="Length.mm")
hist(FHE_scup$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,220,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_scup$Length.mm)),')'),
                     paste('FHE ','(n=',as.character(length(FHE_scup$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))

# Threespine Stickleback
hist(CPS_3spine$Length.mm, col=rgb(1,0,0,0.5),
     xlim = c(0,200), breaks = seq(0,200,10), ylim = c(0,.06), prob = TRUE, add = F,
     main="Threespine Stickleback", xlab="Length.mm")
hist(FHE_3spine$Length.mm, col=rgb(0,0,1,0.5),breaks = seq(0,200,10), add=T, prob = TRUE)
box()
legend("topright", c(paste('CPS ','(n=',as.character(length(CPS_3spine$Length.mm)),')'),
                     paste('FHE ','(n=',as.character(length(FHE_3spine$Length.mm)),')')), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))


####Testplots####
# BSB_dat <- subset(dat, dat$Common.Name == "SEA BASS BLACK")
# BSB_hist <- ggplot(BSB_dat,aes(x = Length.mm, fill = Gear.Type)) + 
#   geom_histogram(data=subset(BSB_dat, Gear.Type == 'Eel Pot'),fill = "red", 
#                  binwidth = 10, alpha = .5) +
#   geom_histogram(data=subset(BSB_dat, Gear.Type == 'Minnow Trap'),fill = "blue", 
#                  binwidth = 10, alpha = .5) +
#   geom_histogram(data=subset(BSB_dat, Gear.Type == 'Gillnet'),fill = "green", 
#                  binwidth = 10, alpha = .5) + 
#   ggtitle("Length Dist. of Black Sea Bass by Gear Type") + 
#   ylab("Frequency") +
#   theme(legend.position = 'top') + 
#   scale_fill_discrete(name = "Legend", labels = "Gear Type")
# 
# 
# tautog_dat <- subset(dat, dat$Common.Name == "TAUTOG")
# ggplot(tautog_dat,aes(x = Length.mm)) + 
#   geom_histogram(ata=subset(tautog_dat, Gear.Type == 'Eel Pot'),fill = "red", 
#                  binwidth = 10, alpha = .5) +
#   geom_histogram(data=subset(tautog_dat, Gear.Type == 'Minnow Trap'),fill = "blue", 
#                  binwidth = 10, alpha = .5) +
#   geom_histogram(data=subset(tautog_dat, Gear.Type == 'Gillnet'),fill = "green", 
#                  binwidth = 10, alpha = .5)
# 
# hist(BSB_dat$Length.mm, breaks = 10, xlim = c(0,200))
# hist(dat$Length.mm, breaks = 70)
# hist(nin_gn_dat$Length.mm)
# 
# target_species <- subset(dat, dat$Target.Species == "Yes")
# unique_target_species <- as.vector(unique(target_species$Common.Name))
# count(target_species, target_species$Common.Name)