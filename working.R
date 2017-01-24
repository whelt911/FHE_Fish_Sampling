nin_gn_dat_2016 <- subset(nin_gn_dat, nin_gn_dat$Date > "2016-01-01")

uniq_site <- unique(nin_gn_dat_2016$Site.ID)

x <- vector(length = length(uniq_site))
for (i in 1:length(uniq_site))
{
  y <- subset(nin_gn_dat_2016, nin_gn_dat_2016$Site.ID == uniq_site[i])
  z <- count(unique(y$COMMON.NAME))
  
  x[i] <- sum(z$freq)
}

a <- matrix(,nrow = length(uniq_site),ncol = 2)
a <- as.data.frame(a)
a[,1] <- uniq_site
a[,2] <- x
colnames(a) <- c('Site.ID','Species.Number')
