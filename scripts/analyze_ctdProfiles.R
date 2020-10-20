# Thu Oct 15 07:11:15 2020 ------------------------------
#6/30 CTD plots
names <- read.table("data/environment/CTD/cleaned/Profiles/20200630_HK_001_filter_align_derive_bin.asc")
names <- as.vector(names[1,])
a <- read.table("data/environment/CTD/cleaned/Profiles/20200630_HK_001_filter_align_derive_bin.asc", skip = 1)
a$lat <- 