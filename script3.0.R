##### JIMMY HOLDGRAFER ###########
##### SEED DISPERSAL PROJECT #####

# H01: as food diversity increases, bird and seed richness increase
# H02: as food diversity increases, bird and seed observations increase

####################
##### PACKAGES #####
####################
library(tidyverse)
library(lme4)
library(lubridate)
library(lmerTest)
library(emmeans)
library(car)
library(MuMIn)
library(agridat)
library(DHARMa)
library(glmmTMB)
library(fitdistrplus)

####################
###### DATA ########
####################
bird.dat.og <- read.csv(file = "bird_data2.0.csv",
                   header = TRUE)
head(bird.dat.og)

seed.dat.og <- read.csv(file = "seed_traps2.0.csv",
                   header = TRUE)
head(seed.dat.og)

####################
### ACCLIM PER #####
####################

# Birds
bird.dat <- select(bird.dat.og, -c("camera.card", "X", "notes", "photo.number"))
bird.dat$date <- as.Date(with(bird.dat, paste(year, month, day, sep = "-")), "%Y-%m-%d")

bird.week <- bird.dat %>% 
	group_by(week = week(date)) %>% 
	summarise(value = sum(presence))

view(bird.week)
# looks like a 2 week acclimitizaton period
# observatons really picked up in January (observations are
# essentially increasing with time throughout our sampling period)

bird.dat.drop <- bird.dat %>% 
	mutate(week = week(date)) %>% 
	filter(week != 48 & week != 47)

# remove NA's
bird.clean <- bird.dat.drop %>% 
		 drop_na(species)

bird.clean$treatment <- as.factor(bird.clean$treatment)

# Seeds
seed.dat <- pivot_longer(seed.dat.og, cols = 5:35, names_to = "SPECIES",
												 values_to = "SEEDS")

seed.dat$DATE <- as.Date(seed.dat$DATE)

seed.dat <- seed.dat %>% 
	mutate(week = week(DATE)) %>% 
	filter(week != 48 & week != 47)

####################
## SUMMARIZE DATA ##
####################

# Birds
# count of individuals by species at each treatment and site
bird.sps.count.by.treat <- bird.clean %>% 
  group_by(site, treatment, species) %>% 
  summarise(count = n())
 
# total count of individuals by treatment and site
bird.obs <- bird.clean %>% 
  group_by(site, treatment) %>% 
  summarise(count = n())

bird.obs <- bird.obs %>% 
	select(treatment,site,count)

missing <- data.frame(site = "six", 
											treatment = "4",
											count = 0)

bird.obs <- rbind(bird.obs,missing)

sites <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,
					 8,8,8,8,9,9,9,9,10,10,10,10)
treatment <- c(0,4,8,12,0,4,8,12,0,4,8,12,0,4,8,12,
							 0,4,8,12,0,4,8,12,0,4,8,12,0,4,8,12,
							 0,4,8,12,0,4,8,12)

bird.rich <- as.data.frame(cbind(sites,treatment))
bird.rich$richness <- c(2,2,3,3,2,2,2,3,3,3,4,4,3,3,3,3,4,4,5,5,
									 2,0,2,1,1,2,2,1,4,4,4,4,2,2,2,2,1,1,1,1)
bird.rich$treatment <- as.factor(bird.rich$treatment)
bird.rich <- bird.rich %>% 
	select(treatment,sites,richness)

# Seeds
seed.obs <- seed.dat %>% 
  dplyr::group_by(TREATMENT, BLOCK) %>% 
  dplyr::summarise(sum(SEEDS)) 
seed.obs <- dplyr::rename(seed.obs, SEEDS = "sum(SEEDS)")

# total count of species by treatment and site
seed.species.count <- seed.dat %>% 
  group_by(TREATMENT, BLOCK, SPECIES) %>% 
  summarise(sum(SEEDS)) 

seed.species.count <- dplyr::rename(seed.species.count, SEEDS = "sum(SEEDS)")

seed.species.count$TREATMENT <- factor(seed.species.count$TREATMENT, 
																		levels=c('Control', 'Low', 'Medium', 'High'))

seed.species.count <- seed.species.count %>% 
	filter(SEEDS > 0)

seed.rich <- as.data.frame(cbind(sites,treatment))
seed.rich$richness <- c(1,2,5,2,0,0,0,2,1,0,0,5,0,3,4,3,0,2,2,3,
												1,1,2,2,1,1,1,1,0,1,1,3,0,0,4,4,2,0,0,0)
seed.rich <- seed.rich %>% 
	select(treatment,sites,richness)

####################
### DISTRIBUTION ###
####################
plotdist(bird.obs$count, histo = TRUE, demp = TRUE)
descdist(bird.obs$count, discrete=TRUE, boot=500) # NB or poisson

plotdist(bird.rich$richness, histo = TRUE, demp = TRUE)
descdist(bird.rich$richness, discrete=TRUE, boot=500) # poisson

plotdist(seed.obs$SEEDS, histo = TRUE, demp = TRUE)
descdist(seed.obs$SEEDS, discrete=TRUE, boot=500) # Poisson

plotdist(seed.rich$richness, histo = TRUE, demp = TRUE)
descdist(seed.rich$richness, discrete=TRUE, boot=500) # Poisson

####################
## BIRD OBS MODELS #
####################
# Bird observations
bird.obs.nb <- glmer.nb(count~treatment+ (1|site), 
                 	data=bird.obs) 
sim.bird.obs.nb <- simulateResiduals(fittedModel = bird.obs.nb, n = 250)
plot(sim.bird.obs.nb)

bird.obs.nb.zi <- glmmTMB(count~treatment+ (1|site), 
                      family=nbinom1, 
                      zi=~1, 
                      data=bird.obs)
sim.bird.obs.nb.zi <- simulateResiduals(fittedModel = bird.obs.nb.zi, n = 250)
plot(sim.bird.obs.nb.zi)
Anova(bird.obs.nb.zi)

bird.obs.pois.zi <- glmmTMB(count~treatment+ (1|site), 
                      family=poisson, 
                      zi=~1, 
                      data=bird.obs)
sim.bird.obs.pois.zi <- simulateResiduals(fittedModel = bird.obs.pois.zi, n = 250)
plot(sim.bird.obs.pois.zi)
Anova(bird.obs.pois.zi)

####################
# BIRD RICH MODELS #
####################
# GLMM
glmm.bird.rich <- glmer(richness ~ treatment + (1|sites),
												data=bird.rich, family=poisson)
sim.glmm.bird.rich <- simulateResiduals(fittedModel = glmm.bird.rich, n = 250)
plot(sim.glmm.bird.rich) # fails dispersion

# OD GLMM
bird.rich$obs <- 1:length(bird.rich$sites)
glmm.ovd.bird.rich <- glmer(richness ~ treatment + (1|sites) + (1|obs),
												data=bird.rich, family=poisson)
sim.glmm.ovd.bird.rich <- simulateResiduals(fittedModel = glmm.ovd.bird.rich, n = 250)
plot(sim.glmm.ovd.bird.rich)
hist(sim.glmm.ovd.bird.rich)

Anova(glmm.ovd.bird.rich)
####################
# SEED OBS MODELS #
####################
# Poisson zero inflated
poisZI.seeds.obs <- glmmTMB(SEEDS~TREATMENT+
                        (1|BLOCK), 
                      family=poisson, 
                      zi=~1, 
                      data=seed.obs)
sim.poisZI.seeds.obs <- simulateResiduals(fittedModel = poisZI.seeds.obs, n = 250)
plot(sim.poisZI.seeds.obs)
Anova(poisZI.seeds.obs)
####################
# SEED RICH MODELS #
####################
poisZI.seed.rich <- glmmTMB(richness~treatment+
                        (1|sites), 
                      family=poisson, 
                      zi=~1, 
                      data=seed.rich)
sim.poisZI.seed.rich <- simulateResiduals(fittedModel = poisZI.seed.rich, n = 250)
plot(sim.poisZI.seed.rich)
Anova(poisZI.seed.rich)
