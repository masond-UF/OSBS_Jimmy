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

################
##### DATA #####
################
bird.dat.og <- read.csv(file = "bird_data2.0.csv",
                   header = TRUE)
head(bird.dat.og)

seed.dat.og <- read.csv(file = "seed_traps2.0.csv",
                   header = TRUE)
head(seed.dat.og)

################
## ACCLIM PER ##
################

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

##########################
##### SUMMARIZE DATA #####
##########################

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

##########################
######### MODEL ##########
##########################
# Bird richness ####
# These models are overdispersed but fail to converge
# GLM
glm.bird.rich <- glm(richness ~ treatment, data=bird.rich, family = poisson)
summary(glm.bird.rich)

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

# Negative binomial
gnb.bird.rich <- glmer.nb(richness~treatment+ (1|sites), 
                 	data=bird.rich) 
sim.gnb.bird.rich <- simulateResiduals(fittedModel = gnb.bird.rich, n = 250)
plot(sim.gnb.bird.rich)

# Poisson zero inflated
poisZI.bird.rich <- glmmTMB(richness~treatment+
                        (1|sites), 
                      family=poisson, 
                      zi=~treatment, 
                      data=bird.rich)
sim.poisZI.bird.rich <- simulateResiduals(fittedModel = poisZI.bird.rich, n = 250)
plot(sim_fec_poisZI)

# Overdispersed poisson zero-inflated
odpZI.bird.rich <- glmmTMB(richness~treatment+(1|obs)+
                        (1|sites), 
                      family=poisson, 
                      zi=~treatment, 
                      data=bird.rich)

# Compare models
AIC(glm.bird.rich,glmm.bird.rich,glmm.ovd.bird.rich,gnb.bird.rich,
		poisZI.bird.rich,odpZI.bird.rich)

# Test signifigance 
Anova(glmm.bird.rich)

# Bird obs ####
# GLM
glm.bird.obs <- glm(count ~ treatment, data=bird.obs, family = poisson)
summary(glm.bird.obs)

# GLMM
glmm.bird.obs <- glmer(count ~ treatment + (1|site),
												data=bird.obs, family=poisson)
sim.glmm.bird.obs <- simulateResiduals(fittedModel = glmm.bird.obs, n = 250)
plot(sim.glmm.bird.obs) # fails dispersion

# OD GLMM
bird.obs$obs <- 1:length(bird.obs$site)
glmm.ovd.bird.obs <- glmer(count ~ treatment + (1|site) + (1|obs),
												data=bird.obs, family=poisson)
sim.glmm.ovd.bird.obs <- simulateResiduals(fittedModel = glmm.ovd.bird.obs, n = 250)
plot(sim.glmm.ovd.bird.obs)

# Negative binomial
gnb.bird.obs <- glmer.nb(count~treatment+ (1|site), 
                 	data=bird.obs) 
sim.gnb.bird.obs <- simulateResiduals(fittedModel = gnb.bird.obs, n = 250)
plot(sim.gnb.bird.obs)

# Poisson zero inflated
poisZI.bird.obs <- glmmTMB(count~treatment+
                        (1|site), 
                      family=poisson, 
                      zi=~treatment, 
                      data=bird.obs)
sim.poisZI.bird.obs <- simulateResiduals(fittedModel = poisZI.bird.obs, n = 250)
plot(sim.poisZI.bird.obs)

# Overdispersed poisson zero-inflated
odpZI.bird.obs <- glmmTMB(obs~treatment+(1|obs)+
                        (1|site), 
                      family=poisson, 
                      zi=~treatment, 
                      data=bird.obs)

# Compare models
AIC(glm.bird.obs,glmm.bird.obs,glmm.ovd.bird.obs,gnb.bird.obs,
		poisZI.bird.obs,odpZI.bird.obs)

# Test signifigance 
Anova(glmm.ovd.bird.obs)

# Seed rich ####

seed.rich$treatment <- as.factor(seed.rich$treatment)
is.factor(seed.rich$treatment)
seed.rich$sites <- as.factor(seed.rich$sites)
is.factor(seed.rich$sites)

# GLM
glm.seed.rich <- glm(richness ~ treatment, data=seed.rich, family = poisson)
summary(glm.seed.rich)

# GLMM
glmm.seed.rich <- glmer(richness ~ treatment + (1|sites),
												data=seed.rich, family=poisson)
sim.glmm.seed.rich <- simulateResiduals(fittedModel = glmm.seed.rich, n = 250)
plot(sim.glmm.seed.rich) # fails dispersion

# OD GLMM
seed.rich$obs <- 1:length(seed.rich$sites)
glmm.ovd.seed.rich <- glmer(richness ~ treatment + (1|sites) + (1|obs),
												data=seed.rich, family=poisson)
sim.glmm.ovd.seed.rich <- simulateResiduals(fittedModel = glmm.ovd.seed.rich, n = 250)
plot(sim.glmm.ovd.seed.rich)

# Negative binomial
gnb.seed.rich <- glmer.nb(richness~treatment+ (1|sites), 
                 	data=seed.rich) 
sim.gnb.seed.rich <- simulateResiduals(fittedModel = gnb.seed.rich, n = 250)
plot(sim.gnb.seed.rich)

# Poisson zero inflated
poisZI.seed.rich <- glmmTMB(richness~treatment+
                        (1|sites), 
                      family=poisson, 
                      zi=~treatment, 
                      data=seed.rich)
sim.poisZI.seed.rich <- simulateResiduals(fittedModel = poisZI.seed.rich, n = 250)
plot(sim.poisZI.seed.rich)

# Overdispersed poisson zero-inflated
odpZI.seed.rich <- glmmTMB(richness~treatment+(1|obs)+
                        (1|sites), 
                      family=poisson, 
                      zi=~treatment, 
                      data=seed.rich)

# Compare models
AIC(glm.seed.rich,glmm.seed.rich,glmm.ovd.seed.rich,gnb.seed.rich,
		poisZI.seed.rich,odpZI.seed.rich)

# Test signifigance 
Anova(glm.seed.rich)

# Seed obs ####
# GLM
glm.seeds.obs <- glm(SEEDS ~ TREATMENT, data=seed.obs, family = poisson)
summary(glm.seeds.obs)

# GLMM
glmm.seeds.obs <- glmer(SEEDS ~ TREATMENT + (1|BLOCK),
												data=seed.obs, family=poisson)
sim.glmm.seeds.obs <- simulateResiduals(fittedModel = glmm.seeds.obs, n = 250)
plot(sim.glmm.seeds.obs) # fails dispersion

# OD GLMM
seed.obs$obs <- 1:length(seed.obs$BLOCK)
glmm.ovd.seeds.obs <- glmer(SEEDS ~ TREATMENT + (1|BLOCK) + (1|obs),
												data=seed.obs, family=poisson)
sim.glmm.ovd.seeds.obs <- simulateResiduals(fittedModel = glmm.ovd.seeds.obs, n = 250)
plot(sim.glmm.ovd.seeds.obs)

# Negative binomial
gnb.seeds.obs <- glmer.nb(SEEDS~TREATMENT+ (1|BLOCK), 
                 	data=seed.obs) 
sim.gnb.seeds.obs <- simulateResiduals(fittedModel = gnb.seeds.obs, n = 250)
plot(sim.gnb.seeds.obs)

# Poisson zero inflated
poisZI.seeds.obs <- glmmTMB(SEEDS~TREATMENT+
                        (1|BLOCK), 
                      family=poisson, 
                      zi=~TREATMENT, 
                      data=seed.obs)
sim.poisZI.seeds.obs <- simulateResiduals(fittedModel = poisZI.seeds.obs, n = 250)
plot(sim.poisZI.seeds.obs)

# Overdispersed poisson zero-inflated
odpZI.seeds.obs <- glmmTMB(SEEDS~TREATMENT+(1|obs)+
                        (1|BLOCK), 
                      family=poisson, 
                      zi=~TREATMENT, 
                      data=seed.obs)

# Compare models
AIC(glm.seeds.obs,glmm.seeds.obs,glmm.ovd.seeds.obs,gnb.seeds.obs,
		poisZI.seeds.obs,odpZI.seeds.obs)

# Test signifigance 
Anova(gnb.seeds.obs)

# Seed richness is significant, bird observatons are significant

##########################
######### MEANS ##########
##########################
# Bird richness
totSD <- VarCorr(glmm.bird.rich) %>% as.data.frame() %>% 
	summarize(totSD=sum(vcov)) %>% mutate(totSD=sqrt(totSD))
tmp <- emmeans(glmm.bird.rich, pairwise~treatment, type="response",
					  			 bias.adj=T, sigma=totSD$totSD)
bird.rich.means <- tmp$emmeans %>% confint() %>% as.data.frame()
bird.rich.means$analysis <- "Bird richness"
colnames(bird.rich.means)[colnames(bird.rich.means) == "rate"] <- "mean"

# Bird observations
totSD <- VarCorr(glmm.ovd.bird.obs) %>% as.data.frame() %>% 
	summarize(totSD=sum(vcov)) %>% mutate(totSD=sqrt(totSD))
tmp <- emmeans(glmm.ovd.bird.obs, pairwise~treatment, type="response",
					  			 bias.adj=T, sigma=totSD$totSD)
bird.obs.means <- tmp$emmeans %>% confint() %>% as.data.frame()
bird.obs.means$analysis <- "Bird observations"
colnames(bird.obs.means)[colnames(bird.obs.means) == "rate"] <- "mean"

# Seed richness
tmp <- emmeans(glm.seed.rich, pairwise~treatment, type="response")
seed.rich.means <- tmp$emmeans %>% confint() %>% as.data.frame()
seed.rich.means$analysis <- "Seed richness"
colnames(seed.rich.means)[colnames(seed.rich.means) == "rate"] <- "mean"

# Seed observations
totSD <- VarCorr(gnb.seeds.obs) %>% as.data.frame() %>% 
	summarize(totSD=sum(vcov)) %>% mutate(totSD=sqrt(totSD))
tmp <- emmeans(gnb.seeds.obs, pairwise~TREATMENT, type="response",
							 bias.adj=T, sigma=totSD$totSD)
seed.obs.means <- tmp$emmeans %>% confint() %>% as.data.frame()
seed.obs.means$analysis <- "Seed observations"
colnames(seed.obs.means)[colnames(seed.obs.means) == "TREATMENT"] <- "treatment"
colnames(seed.obs.means)[colnames(seed.obs.means) == "response"] <- "mean"

means <- rbind(bird.rich.means, bird.obs.means, seed.rich.means, seed.obs.means)

##########################
######## FIGURE ##########
##########################

is.factor(means$treatment)
levels(means$treatment)[levels(means$treatment)=="0"] <- "Control"
levels(means$treatment)[levels(means$treatment)=="4"] <- "Low"
levels(means$treatment)[levels(means$treatment)=="8"] <- "Medium"
levels(means$treatment)[levels(means$treatment)=="12"] <- "High"
means$treatment <- as.factor(means$treatment)

ggplot(means, aes(x = treatment, y = mean))+
	geom_linerange(aes(ymin=asymp.LCL, ymax=asymp.UCL))+
	geom_point(aes(col = treatment),size = 8)+
	geom_point(shape = 1,size = 8,colour = "black")+
	theme_bw()+
	theme(text = element_text(size = 22),
				legend.title = element_blank(),
				legend.position = "none",
				plot.title = element_text(hjust = 0.5),
				strip.background = element_blank(),
   			strip.text.y = element_blank(),
				axis.title.x = element_text(face='bold', vjust=-2.5),
				axis.title.y = element_text(face='bold', vjust=3),
				strip.text.x = element_text(size = 18,face ='bold'),
				legend.spacing.x = unit(0.5, 'cm'),
				plot.margin = unit(c(1,1.2,0.9,1.2),"cm"),
				legend.box.spacing = unit(1.2,'cm'),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_line(size=1.2))+
	ylab("Mean")+
	xlab("Treatment")+
	geom_smooth()+
	facet_wrap(~analysis, scales="free")

seed.obs$TREATMENT <- as.factor(seed.obs$TREATMENT)
levels(seed.obs$TREATMENT)[levels(seed.obs$TREATMENT)=="Control"] <- "0"
levels(seed.obs$TREATMENT)[levels(seed.obs$TREATMENT)=="Low"] <- "4"
levels(seed.obs$TREATMENT)[levels(seed.obs$TREATMENT)=="Medium"] <- "8"
levels(seed.obs$TREATMENT)[levels(seed.obs$TREATMENT)=="High"] <- "12"
seed.obs$TREATMENT <- as.numeric(as.character(seed.obs$TREATMENT))
seed.obs$BLOCK <- as.factor(seed.obs$BLOCK)
colnames(seed.obs) <- c("TREATMENT", "BLOCK", "VALUE")
seed.obs$ANALYSIS <- "Seed observations"

colnames(seed.rich) <- c("TREATMENT", "BLOCK", "VALUE")
seed.rich$ANALYSIS <- "Seed richness"
seed.rich$TREATMENT <- as.numeric(as.character(seed.rich$TREATMENT))
seed.rich$BLOCK <- as.factor(seed.rich$BLOCK)

colnames(bird.obs) <- c("TREATMENT", "BLOCK", "VALUE")
bird.obs$TREATMENT <- as.numeric(as.character(bird.obs$TREATMENT))
bird.obs$ANALYSIS <- "Bird observations"
bird.obs$BLOCK <- as.factor(bird.obs$BLOCK)

colnames(bird.rich) <- c("TREATMENT", "BLOCK", "VALUE")
bird.rich$ANALYSIS <- "Bird richness"
bird.rich$TREATMENT <- as.numeric(as.character(bird.rich$TREATMENT))
bird.rich$BLOCK <- as.factor(bird.rich$BLOCK)


comb <- rbind(seed.obs,seed.rich,bird.obs,bird.rich)

ggplot(comb, aes(x = TREATMENT, y = VALUE))+
	geom_smooth(method="lm")+
	facet_wrap(~ANALYSIS, scales = "free")

ggplot(comb, aes(x = TREATMENT, y = VALUE))+
	geom_point()+
	geom_smooth(method="lm")+
	facet_wrap(~ANALYSIS, scales = "free")

