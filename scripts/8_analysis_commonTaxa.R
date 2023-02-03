#Script to model taxa density with zero inflated GLMs

# Tue Jan 17 13:41:33 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(glmmTMB)
library(reshape2)
library(tidyverse)
library(car)
library(emmeans)

load("data/biology/counts/plankton.rda")
#====

#SETUP
#====
us <- filter(plankton, n>20)#create a dataframe of "usual suspects" with only the most commonly observed species (50% of all samples collected)
tows <- dplyr::select(us, sample, date, site, location, depth) %>% distinct()#get essential metadata

y.us <- dcast(us, sample~spStage, value.var = "sampleCount", fun.aggregate = mean)#create wide df
y.us[is.na(y.us)] <- 0
y.us <- left_join(y.us, distinct(dplyr::select(us, sample, location, depth, date, volume_total)))

mefx <- data.frame("type" = rep(NA, 17),#create dataframe to store model main effects
                      "location_chi" = rep(NA, 17),
                      "location_p" = rep(NA, 17),
                      "location:depth_chi" = rep(NA, 17),
                      "location:depth_p" = rep(NA, 17))

pairs <- data.frame("type"= rep(NA, 4),#create dataframe to store model pairwise comparisons for location
                    "on_f_est"= rep(NA, 4),
                    "on_f_p"= rep(NA, 4),
                    "on_off_est"= rep(NA, 4),
                    "on_off_p"= rep(NA, 4),
                    "f_off_est"= rep(NA, 4),
                    "f_off_p"= rep(NA, 4))
#====

#TAXA-SPECIFIC HURDLE MODELS
#====
#Balanus crenatus cyprids
x <- select(y.us, date, location, depth, balanus.crenatus_cyprid, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = balanus.crenatus_cyprid, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(balanus.crenatus_cyprid))

#Hurdle models for testing
m0 <- glmmTMB(balanus.crenatus_cyprid~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[1,] <- c("Balanus crenatus cyprid", 1.231, 0.540, 9.108, 0.027)#location estimate, p value, location:depth estimate, p value



#Balanus crenatus nauplius
x <- dplyr::select(y.us, date, location, depth, balanus.crenatus_nauplius4.5, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = balanus.crenatus_nauplius4.5, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(balanus.crenatus_nauplius4.5))

#Hurdle models for testing
m0 <- glmmTMB(balanus.crenatus_nauplius4.5~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[2,] <- c("Balanus crenatus late nauplius", 1.580, 0.454, 1.034, 0.793)#location estimate, p value, location:depth estimate, p value



#Bivalve umbo
x <- dplyr::select(y.us, date, location, depth, bivalve_umbo, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = bivalve_umbo, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(bivalve_umbo))

#Hurdle models for testing
m0 <- glmmTMB(bivalve_umbo~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m2

summary(m2)#Simple effects and model diagnostics
Anova(m2)#main effects

mefx[3,] <- c("Bivalve veliger", 15.635, 0.0004, 8.306, 0.04)#location estimate, p value, location:depth estimate, p value

emmeans(m2, pairwise~location, at = list(location = c("onshore", "front", "offshore")))#get pairwise comparisons
pairs[1,] <- c("Bivalve veliger", -2.140, 0.0024, -1.578, 0.0346, 0.562, 0.6593)#pairs on-f, p value, on-off, p value, f-off, p value



#Chthamalus cyprid
x <- dplyr::select(y.us, date, location, depth, chthamalus.spp_cyprid, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = chthamalus.spp_cyprid, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(chthamalus.spp_cyprid))

#Hurdle models for testing
m0 <- glmmTMB(chthamalus.spp_cyprid~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[4,] <- c("Chthamalus cyprid", 29.424, 0.0000004, 6.198, 0.102)#location estimate, p value, location:depth estimate, p value

emmeans(m3, pairwise~location, at = list(location = c("onshore", "front", "offshore")))#get pairwise comparisons
pairs[2,] <- c("Chthamalus cyprid", -0.666, 0.1862, 1.431, 0.0052, 2.097, 0.0002)#pairs on-f, p value, on-off, p value, f-off, p value



#Gastropod veliger
x <- dplyr::select(y.us, date, location, depth, gastropod_veliger, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = gastropod_veliger, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(gastropod_veliger))

#Hurdle models for testing
m0 <- glmmTMB(gastropod_veliger~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[5,] <- c("Gastropod veliger", 2.386, 0.303, 14.146, 0.003)#location estimate, p value, location:depth estimate, p value



#Pinnotherid zoea
x <- dplyr::select(y.us, date, location, depth, pinnotherid_zoea1.3, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = pinnotherid_zoea1.3, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(pinnotherid_zoea1.3))

#Hurdle models for testing
m0 <- glmmTMB(pinnotherid_zoea1.3~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[6,] <- c("Pinnotherid early zoea", 50.040, 0.081, 2.246, 0.523)#location estimate, p value, location:depth estimate, p value



#Polychaete setiger
x <- dplyr::select(y.us, date, location, depth, polychaete_setiger, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = polychaete_setiger, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(polychaete_setiger))

#Hurdle models for testing
m0 <- glmmTMB(as.integer(polychaete_setiger)~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[7,] <- c("Polychaete larva", 2.487, 0.288, 18.412, 0.0004)#location estimate, p value, location:depth estimate, p value



#Calanoid adult
x <- dplyr::select(y.us, date, location, depth, calanoid_adult, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = calanoid_adult, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(calanoid_adult))

#Hurdle models for testing
m0 <- glmmTMB(calanoid_adult~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[8,] <- c("Calanoid adult", 8.843, 0.012, 6.932, 0.074)#location estimate, p value, location:depth estimate, p value



#Copepod metanauplius
x <- dplyr::select(y.us, date, location, depth, copepod_metanauplius, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = copepod_metanauplius, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(copepod_metanauplius))

#Hurdle models for testing
m0 <- glmmTMB(copepod_metanauplius~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m2

summary(m2)#Simple effects and model diagnostics
Anova(m2)#main effects

mefx[9,] <- c("Copepod metanauplius", 15.489, 0.0004, 6.597, 0.0859)#location estimate, p value, location:depth estimate, p value

emmeans(m2, pairwise~location, at = list(location = c("onshore", "front", "offshore")))#get pairwise comparisons
pairs[3,] <- c("Copepod metanauplius", 0.201, 0.9602, -2.773, 0.0042, -2.975, 0.0054)#pairs on-f, p value, on-off, p value, f-off, p value



#Copepod nauplius
x <- dplyr::select(y.us, date, location, depth, copepod_nauplius, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = copepod_nauplius, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(copepod_nauplius))

#Hurdle models for testing
m0 <- glmmTMB(copepod_nauplius~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m2

summary(m2)#Simple effects and model diagnostics
Anova(m2)#main effects

mefx[10,] <- c("Copepod nauplius", 1.022, 0.560, 0.290, 0.962)#location estimate, p value, location:depth estimate, p value



#Cyclopoid adult
x <- dplyr::select(y.us, date, location, depth, cyclopoid_adult, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = cyclopoid_adult, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(cyclopoid_adult))

#Hurdle models for testing
m0 <- glmmTMB(cyclopoid_adult~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[11,] <- c("Cyclopoid adult", 4.10, 0.129, 9.519, 0.023)#location estimate, p value, location:depth estimate, p value



#Evadne
x <- dplyr::select(y.us, date, location, depth, evadne_adult, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = evadne_adult, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(evadne_adult))

#Hurdle models for testing
m0 <- glmmTMB(evadne_adult~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[12,] <- c("Evadne adult", 11.051, 0.004, 0.331, 0.954)#location estimate, p value, location:depth estimate, p value



#Larvacea
x <- dplyr::select(y.us, date, location, depth, oikopleura_larva, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = oikopleura_larva, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(oikopleura_larva))

#Hurdle models for testing
m0 <- glmmTMB(oikopleura_larva~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[13,] <- c("Larvacean", 9.283, 0.010, .521, 0.914)#location estimate, p value, location:depth estimate, p value



#Podon
x <- dplyr::select(y.us, date, location, depth, podon_adult, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = podon_adult, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(podon_adult))

#Hurdle models for testing
m0 <- glmmTMB(podon_adult~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[14,] <- c("Podon adult", 12.313, 0.002, 2.470, 0.481)#location estimate, p value, location:depth estimate, p value

emmeans(m3, pairwise~location, at = list(location = c("onshore", "front", "offshore")))#get pairwise comparisons
pairs[4,] <- c("Podon adult", 0.172, 0.8651, -0.937, 0.0320, -1.109, 0.0054)#pairs on-f, p value, on-off, p value, f-off, p value




#Diatom
x <- dplyr::select(y.us, date, location, depth, diatom_adult, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = diatom_adult, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(diatom_adult))

#Hurdle models for testing
m0 <- glmmTMB(diatom_adult~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[15,] <- c("Diatom", 7.801, 0.020, 5.370, 0.147)#location estimate, p value, location:depth estimate, p value



#Large eggs
x <- dplyr::select(y.us, date, location, depth, egg.large_embryo, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = egg.large_embryo, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(egg.large_embryo))

#Hurdle models for testing
m0 <- glmmTMB(egg.large_embryo~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m3

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[16,] <- c("Large egg", 1.174, 0.557, 1.378, 0.711)#location estimate, p value, location:depth estimate, p value



#Small eggs
x <- dplyr::select(y.us, date, location, depth, egg.small_embryo, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = egg.small_embryo, color = depth))

ggplot(x) + #visual overdispersion test
  geom_histogram(aes(egg.small_embryo))

#Hurdle models for testing
m0 <- glmmTMB(egg.small_embryo~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)#fit with poisson family distribution
m1 <- update(m0, family = truncated_nbinom1)#negative binomial family 1
m2 <- update(m1, family = truncated_nbinom2)#negative binomial family 2
m3 <- update(m2, ~. + (1|date))#with random variable for date

AIC(m0, m1, m2, m3)#select model with lowest AIC value, m2

summary(m2)#Simple effects and model diagnostics
Anova(m2)#main effects

mefx[17,] <- c("Small egg", 10.445, 0.005, 0.381, 0.944)#location estimate, p value, location:depth estimate, p value
#====

#SAVE TABLES
#====
write_csv(mefx, file = "output/mainEfx.csv")
write_csv(pairs, file = "output/pairwise.csv")
#====

#END ANALYSIS, GO TO FIGURE FILES