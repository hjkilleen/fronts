#Script to model taxa density with zero inflated GLMs

# Tue Jan 17 13:41:33 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(pscl)
library(NBZIMM)
library(performance)
library(multcomp)

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

pairs <- data.frame("type",#create dataframe to store model pairwise comparisons for location
                    "on_f_est",
                    "on_f_p",
                    "on_off_est",
                    "on_off_p",
                    "f_off_est",
                    "f_off_p")
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

AIC(m0, m1, m2, m3)#select model with lowest AIC value

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[1,] <- c("Balanus crenatus cyprid", 1.231, 0.540, 9.108, 0.027)

emmeans(m3, pairwise~location:depth)


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

AIC(m0, m1, m2, m3)#select model with lowest AIC value

summary(m3)#Simple effects and model diagnostics
Anova(m3)#main effects

mefx[2,] <- c("Balanus crenatus late nauplius", 1.580, 0.454, 1.034, 0.793)

#Bivalve umbo
mods.bivalve_umbo <- list()#set up
x <- dplyr::select(y.us, date, location, depth, bivalve_umbo, volume_total)

ggplot(x) + #look for outliers
  geom_boxplot(aes(x = location, y = bivalve_umbo, color = depth))

mods.bivalve_umbo[[1]] <- MASS::glm.nb(bivalve_umbo~location+depth:location+offset(log(volume_total)), data = x)#Neg bin GLM for testing
odTest(mods.bivalve_umbo[[1]])#test for overdispersion (p<0.05)
check_zeroinflation(mods.bivalve_umbo[[1]])#test for zero inflation (ratio<1)

mods.bivalve_umbo[[2]] <- NULL#Neg bin GLMM, if not zero inflated

mods.bivalve_umbo[[3]] <- glmm.zinb(bivalve_umbo~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)##Zero inflated neg bin GLMM

summary(mods.bivalve_umbo[[3]])#Simple effects and model diagnostics
anova(mods.bivalve_umbo[[3]], test = "Chisq")#Main effects

x$fit <- exp(fitted(mods.bivalve_umbo[[3]]))#Get fitted values
ggplot(x) + #Plot fitted values over raw data
  geom_boxplot(aes(x = location, y = bivalve_umbo, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)



#Calanoid adult
mods.calanoid_adult <- list()#set up
x <- dplyr::select(y.us, date, location, depth, calanoid_adult, volume_total)

ggplot(x) + #look for outliers
  geom_boxplot(aes(x = location, y = calanoid_adult, color = depth)) + 
  labs(x = "Location", y = "Calanoid Density (n/5 min. tow)") +
  theme_classic() +
  theme(text = element_text(size = 20))

mods.calanoid_adult[[1]] <- MASS::glm.nb(calanoid_adult~location+depth:location+offset(log(volume_total)), data = x)#Neg bin GLM for testing
odTest(mods.calanoid_adult[[1]])#test for overdispersion (p<0.05)
check_zeroinflation(mods.calanoid_adult[[1]])#test for zero inflation (ratio<1)

mods.calanoid_adult[[2]] <- glmer.nb(calanoid_adult~offset(log(volume_total))+location+depth:location+(1|date), data = x)#Neg bin GLMM
ss <- getME(mods.calanoid_adult[[2]], c("theta", "fixef"))#restart model with more iterations
mods.calanoid_adult[[2]] <- update(mods.calanoid_adult[[2]], start = ss, control = glmerControl(optCtrl = list(maxfun = 2e4)))

mods.calanoid_adult[[3]] <- NULL##Zero inflated neg bin GLMM

summary(mods.calanoid_adult[[2]])#Simple effects and model diagnostics
car::Anova(mods.calanoid_adult[[2]])#Main effects

x$fit <- fitted(mods.calanoid_adult[[2]])#Get fitted values
ggplot(x) + #Plot fitted values over raw data
  geom_boxplot(aes(x = location, y = calanoid_adult, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.calanoid_adult[[2]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))



#Chthamalus cyprid
mods.chthamalus.spp_cyprid <- list()#set up
x <- dplyr::select(y.us, date, location, depth, chthamalus.spp_cyprid, volume_total)
x$chthamalus.spp_cyprid <- as.numeric(x$chthamalus.spp_cyprid)

ggplot(x) + #look for outliers
  geom_boxplot(aes(x = location, y = chthamalus.spp_cyprid, color = depth)) + 
  labs(x = "Location", y = "Chthamalus cyprids Density\n(n/5 min. tow)") +
  theme_classic() +
  theme(text = element_text(size = 20))

mods.chthamalus.spp_cyprid[[1]] <- MASS::glm.nb(chthamalus.spp_cyprid~location+depth:location+offset(log(volume_total)), data = x)#Neg bin GLM for testing
odTest(mods.chthamalus.spp_cyprid[[1]])#test for overdispersion (p<0.05)
check_zeroinflation(mods.chthamalus.spp_cyprid[[1]])#test for zero inflation (ratio<1)

mods.chthamalus.spp_cyprid[[2]] <- NULL#Neg bin GLMM, if not zero inflated

mods.chthamalus.spp_cyprid[[3]] <- glmm.zinb(chthamalus.spp_cyprid~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)##Zero inflated neg bin GLMM

summary(mods.chthamalus.spp_cyprid[[3]])#Simple effects and model diagnostics
anova(mods.chthamalus.spp_cyprid[[3]], test = "Chisq")#Main effects

x$fit <- exp(fitted(mods.chthamalus.spp_cyprid[[3]]))#Get fitted values
ggplot(x) + #Plot fitted values over raw data
  geom_boxplot(aes(x = location, y = chthamalus.spp_cyprid, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.chthamalus.spp_cyprid[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))



#Copepod metanauplius
mods.copepod_metanauplius <- list()#set up
x <- dplyr::select(y.us, date, location, depth, copepod_metanauplius, volume_total)

ggplot(x) + #look for outliers
  geom_boxplot(aes(x = location, y = copepod_metanauplius, color = depth)) + 
  labs(x = "Location", y = "Copepod Metanauplius Density\n(n/5 min. tow)") +
  theme_classic() +
  theme(text = element_text(size = 20))

mods.copepod_metanauplius[[1]] <- MASS::glm.nb(copepod_metanauplius~location+depth:location+offset(log(volume_total)), data = x)#Neg bin GLM for testing
odTest(mods.copepod_metanauplius[[1]])#test for overdispersion (p<0.05)
check_zeroinflation(mods.copepod_metanauplius[[1]])#test for zero inflation (ratio<1)

mods.copepod_metanauplius[[2]] <- NULL#Neg bin GLMM, if not zero inflated

mods.copepod_metanauplius[[3]] <- glmm.zinb(copepod_metanauplius~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)##Zero inflated neg bin GLMM

summary(mods.copepod_metanauplius[[3]])#Simple effects and model diagnostics
anova(mods.copepod_metanauplius[[3]], test = "Chisq")#Main effects

x$fit <- exp(fitted(mods.copepod_metanauplius[[3]]))#Get fitted values
ggplot(x) + #Plot fitted values over raw data
  geom_boxplot(aes(x = location, y = copepod_metanauplius, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.copepod_metanauplius[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))



#Copepod nauplius
mods.copepod_nauplius <- list()#set up
x <- dplyr::select(y.us, date, location, depth, copepod_nauplius, volume_total)
x$copepod_nauplius[is.na(x$copepod_nauplius)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = copepod_nauplius, color = depth))

mods.copepod_nauplius[[1]] <- MASS::glm.nb(copepod_nauplius~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.copepod_nauplius[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.copepod_nauplius[[1]])#test for zero inflation (ration<1)

mods.copepod_nauplius[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.copepod_nauplius[[3]] <- glmm.zinb(copepod_nauplius~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.copepod_nauplius[[3]])#Simple effects and model diagnostics
anova(mods.copepod_nauplius[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.copepod_nauplius[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = copepod_nauplius, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)


#Cyclopoid adult
mods.cyclopoid_adult <- list()#set up
x <- dplyr::select(y.us, date, location, depth, cyclopoid_adult, volume_total)
x$cyclopoid_adult[is.na(x$cyclopoid_adult)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = cyclopoid_adult, color = depth))

mods.cyclopoid_adult[[1]] <- MASS::glm.nb(cyclopoid_adult~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.cyclopoid_adult[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.cyclopoid_adult[[1]])#test for zero inflation (ration<1)

mods.cyclopoid_adult[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.cyclopoid_adult[[3]] <- glmm.zinb(cyclopoid_adult~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.cyclopoid_adult[[3]])#Simple effects and model diagnostics
anova(mods.cyclopoid_adult[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.cyclopoid_adult[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = cyclopoid_adult, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)


#Diatom
mods.diatom_adult <- list()#set up
x <- dplyr::select(y.us, date, location, depth, diatom_adult, volume_total)
x$diatom_adult[is.na(x$diatom_adult)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = diatom_adult, color = depth))

mods.diatom_adult[[1]] <- MASS::glm.nb(diatom_adult~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.diatom_adult[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.diatom_adult[[1]])#test for zero inflation (ration<1)

mods.diatom_adult[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.diatom_adult[[3]] <- glmm.zinb(diatom_adult~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.diatom_adult[[3]])#Simple effects and model diagnostics
anova(mods.diatom_adult[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.diatom_adult[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = diatom_adult, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.diatom_adult[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))



#Large eggs
mods.egg.large_embryo <- list()#set up
x <- dplyr::select(y.us, date, location, depth, egg.large_embryo, volume_total)
x$egg.large_embryo[is.na(x$egg.large_embryo)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = egg.large_embryo, color = depth))

mods.egg.large_embryo[[1]] <- MASS::glm.nb(egg.large_embryo~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.egg.large_embryo[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.egg.large_embryo[[1]])#test for zero inflation (ration<1)

mods.egg.large_embryo[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.egg.large_embryo[[3]] <- glmm.zinb(egg.large_embryo~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.egg.large_embryo[[3]])#Simple effects and model diagnostics
anova(mods.egg.large_embryo[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.egg.large_embryo[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = egg.large_embryo, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)


#egg.small_embryo
mods.egg.small_embryo <- list()#set up
x <- dplyr::select(y.us, date, location, depth, egg.small_embryo, volume_total)
x$egg.small_embryo[is.na(x$egg.small_embryo)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = egg.small_embryo, color = depth))

mods.egg.small_embryo[[1]] <- MASS::glm.nb(egg.small_embryo~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.egg.small_embryo[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.egg.small_embryo[[1]])#test for zero inflation (ration<1)

mods.egg.small_embryo[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.egg.small_embryo[[3]] <- glmm.zinb(egg.small_embryo~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.egg.small_embryo[[3]])#Simple effects and model diagnostics
anova(mods.egg.small_embryo[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.egg.small_embryo[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = egg.small_embryo, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.egg.small_embryo[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))



#Evadne
mods.evadne_adult <- list()#set up
x <- dplyr::select(y.us, date, location, depth, evadne_adult, volume_total)
x$evadne_adult[is.na(x$evadne_adult)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = evadne_adult, color = depth)) + 
  labs(x = "Location", y = "Evadne Density (n/5 min. tow)") +
  theme_classic() +
  theme(text = element_text(size = 20))

mods.evadne_adult[[1]] <- MASS::glm.nb(evadne_adult~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.evadne_adult[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.evadne_adult[[1]])#test for zero inflation (ration<1)

mods.evadne_adult[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.evadne_adult[[3]] <- glmm.zinb(evadne_adult~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.evadne_adult[[3]])#Simple effects and model diagnostics
anova(mods.evadne_adult[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.evadne_adult[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = evadne_adult, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.evadne_adult[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))


#Gastropod veliger
mods.gastropod_veliger <- list()#set up
x <- dplyr::select(y.us, date, location, depth, gastropod_veliger, volume_total)
x$gastropod_veliger[is.na(x$gastropod_veliger)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = gastropod_veliger, color = depth)) + 
  labs(x = "Location", y = "Gastropod Veliger Density\n(n/5 min. tow)") +
  theme_classic() +
  theme(text = element_text(size = 20))

mods.gastropod_veliger[[1]] <- MASS::glm.nb(gastropod_veliger~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.gastropod_veliger[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.gastropod_veliger[[1]])#test for zero inflation (ration<1)

mods.gastropod_veliger[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.gastropod_veliger[[3]] <- glmm.zinb(gastropod_veliger~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.gastropod_veliger[[3]])#Simple effects and model diagnostics
anova(mods.gastropod_veliger[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.gastropod_veliger[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = gastropod_veliger, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.gastropod_veliger[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))


#Oikipleura
mods.oikopleura_larva <- list()#set up
x <- dplyr::select(y.us, date, location, depth, oikopleura_larva, volume_total)
x$oikopleura_larva[is.na(x$oikopleura_larva)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = oikopleura_larva, color = depth))

mods.oikopleura_larva[[1]] <- MASS::glm.nb(oikopleura_larva~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.oikopleura_larva[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.oikopleura_larva[[1]])#test for zero inflation (ration<1)

mods.oikopleura_larva[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.oikopleura_larva[[3]] <- glmm.zinb(oikopleura_larva~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.oikopleura_larva[[3]])#Simple effects and model diagnostics
anova(mods.oikopleura_larva[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.oikopleura_larva[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = oikopleura_larva, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.oikopleura_larva[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))



#Pinnotherid zoea
mods.pinnotherid_zoea1.3 <- list()#set up
x <- dplyr::select(y.us, date, location, depth, pinnotherid_zoea1.3, volume_total)
x$pinnotherid_zoea1.3[is.na(x$pinnotherid_zoea1.3)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = pinnotherid_zoea1.3, color = depth))

mods.pinnotherid_zoea1.3[[1]] <- MASS::glm.nb(pinnotherid_zoea1.3~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.pinnotherid_zoea1.3[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.pinnotherid_zoea1.3[[1]])#test for zero inflation (ration<1)

mods.pinnotherid_zoea1.3[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.pinnotherid_zoea1.3[[3]] <- glmm.zinb(pinnotherid_zoea1.3~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.pinnotherid_zoea1.3[[3]])#Simple effects and model diagnostics
anova(mods.pinnotherid_zoea1.3[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.pinnotherid_zoea1.3[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = pinnotherid_zoea1.3, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.pinnotherid_zoea1.3[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))


#Podon
mods.podon_adult <- list()#set up
x <- dplyr::select(y.us, date, location, depth, podon_adult, volume_total)
x$podon_adult[is.na(x$podon_adult)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = podon_adult, color = depth))

mods.podon_adult[[1]] <- MASS::glm.nb(podon_adult~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.podon_adult[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.podon_adult[[1]])#test for zero inflation (ration<1)

mods.podon_adult[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.podon_adult[[3]] <- glmm.zinb(podon_adult~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.podon_adult[[3]])#Simple effects and model diagnostics
anova(mods.podon_adult[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.podon_adult[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = podon_adult, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.podon_adult[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))



#Polychaete setiger
mods.polychaete_setiger <- list()#set up
x <- dplyr::select(y.us, date, location, depth, polychaete_setiger, volume_total)
x$polychaete_setiger[is.na(x$polychaete_setiger)] <- 0

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = polychaete_setiger, color = depth))

mods.polychaete_setiger[[1]] <- MASS::glm.nb(polychaete_setiger~location+depth:location+offset(log(volume_total)), data = x)#Neg bin for testing
odTest(mods.polychaete_setiger[[1]])#test for overdispersaion (p<0.05)
check_zeroinflation(mods.polychaete_setiger[[1]])#test for zero inflation (ration<1)

mods.polychaete_setiger[[2]] <- NULL#Neg bin GLMM if not zero inflated

mods.polychaete_setiger[[3]] <- glmm.zinb(polychaete_setiger~location+location:depth+offset(log(volume_total)), random = ~ 1 | date, data = x, zi_fixed = ~1)#Zero inflated neg bin GLMM

summary(mods.polychaete_setiger[[3]])#Simple effects and model diagnostics
anova(mods.polychaete_setiger[[3]], test = "Chisq")#main effects

x$fit <- exp(fitted(mods.polychaete_setiger[[3]]))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = polychaete_setiger, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(mods.polychaete_setiger[[3]], pairwise~location, at = list(location = c("onshore", "front", "offshore")))
