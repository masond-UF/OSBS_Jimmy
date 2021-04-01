##### JIMMY HOLDGRAFER ###########
##### SEED DISPERSAL PROJECT #####

# H01: as food diversity increases, seed abundance increases

####################
##### PACKAGES #####
####################
library(dplyr)
library(tidyverse)
library(lme4)

################
##### DATA #####
################
dat <- read.csv(file = "seeds.csv",
                   header = TRUE)

# pivot the data
dat_long <- pivot_longer(dat, cols = 5:29, names_to = "SPECIES",
												 values_to = "SEEDS")

##########################
##### SUMMARIZE DATA #####
##########################

# total count of individuals by treatment and site
total_count_by_treat_site <- dat_long %>% 
  dplyr::group_by(TREATMENT, BLOCK) %>% 
  dplyr::summarise(sum(SEEDS)) 
total_count_by_treat_site <- dplyr::rename(total_count_by_treat_site, SEEDS = "sum(SEEDS)")

plot(total_count_by_treat_site$SEEDS ~ total_count_by_treat_site$TREATMENT)
hist(total_count_by_treat_site$SEEDS)

total_count_by_treat_site %>% group_by(TREATMENT) %>% 
	summarise(MEAN = mean(SEEDS), SD = sd(SEEDS)) %>% 
	mutate(SE = SD/(sqrt(10)))


# total count of species by treatment and site
total_count_by_treat_site_species <- dat_long %>% 
  group_by(TREATMENT, BLOCK, SPECIES) %>% 
  summarise(sum(SEEDS)) 

total_count_by_treat_site_species <- rename(total_count_by_treat_site_species, 
																						SEEDS = "sum(SEEDS)")

total_count_by_treat_site_species_wider <- pivot_wider(total_count_by_treat_site_species, 
																	 names_from = SPECIES, values_from = SEEDS)

richness_by_treat_site %>% group_by(TREATMENT) %>% 
	summarise(MEAN = mean(RICHNESS), SD = sd(RICHNESS)) %>% 
	mutate(SE = SD/(sqrt(10)))



rich <- vector()
for(i in 1:40){
rich[i] <- length(which(total_count_by_treat_site_species_wider[i,3:27] >= 1))
}

rich.df <- as.data.frame(rich)
total_count_by_treat_site_species_wider$RICHNESS <- rich.df$rich
richness_by_treat_site <- total_count_by_treat_site_species_wider %>% 
					select(TREATMENT, BLOCK, RICHNESS)
####################
##### ANALYSES #####
####################

### effect of food diversity (ie treatment) on bird abundance ###
summary(aov(data = total_count_by_treat_site, formula = SEEDS ~ TREATMENT))
TukeyHSD(aov(data = total_count_by_treat_site, formula = SEEDS ~ TREATMENT))

mod <- aov(data = total_count_by_treat_site, formula = SEEDS ~ TREATMENT)
hist(mod$residuals)
plot(mod)

ggplot(data = total_count_by_treat_site, 
       aes(x = TREATMENT, y = SEEDS)) +
  geom_point(alpha = 0.3) +
  ylab("Seed Abundance") + 
  xlab("Food Diversity") +
  theme_bw()

### effect of food diversity (ie treatment) on seed richness ###
summary(aov(data = richness_by_treat_site, formula = RICHNESS ~ TREATMENT))
TukeyHSD(aov(data = richness_by_treat_site, formula = RICHNESS ~ TREATMENT))

mod <- aov(data = richness_by_treat_site, formula = RICHNESS ~ TREATMENT)
hist(mod$residuals)
plot(mod)

ggplot(data = richness_by_treat_site, 
       aes(x = TREATMENT, y = RICHNESS))+
  geom_point(alpha = 0.3) +
  ylab("Seed Richness") + 
  xlab("Food Diversity") +
  theme_bw()

richness_by_treat_site$TREATMENT <- factor(richness_by_treat_site$TREATMENT, 
																		levels=c('Control', 'Low', 'Medium', 'High'))

seed_rich_labs <- c("0", "4", "8", "12")


seed_rich <- ggplot(data = richness_by_treat_site, 
       aes(x = TREATMENT, y = RICHNESS, color = TREATMENT)) +
  geom_jitter(alpha = 0.4, size = 4, 
              width = 0.3, height = 0.2) +
  ylab("Seed Richness") + 
  xlab("Food Richness (# of Resources)")+
	scale_x_discrete(labels = seed_rich_labs)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  theme(legend.position = "none")+
	theme(plot.margin = unit(c(1,1.5,1,1), "cm"))

seed_abund <- ggplot(data = total_count_by_treat_site, 
       aes(x = TREATMENT, y = SEEDS, color = TREATMENT)) +
  geom_jitter(alpha = 0.4, size = 4, 
              width = 0.3, height = 0.2) +
  ylab("Seed Observations") + 
  xlab("Food Richness (# of Resources)")+
	scale_x_discrete(labels = seed_rich_labs)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  theme(legend.position = "none")+
	scale_y_continuous(limits = c(-0.2,20))+
	theme(plot.margin = unit(c(1,1.5,1,1), "cm"))

total_count_by_treat_site$TREATMENT <- factor(total_count_by_treat_site$TREATMENT, 
																		levels=c('Control', 'Low', 'Medium', 'High'))

grid.arrange(seed_rich, seed_abund, nrow = 1)


### tylers way of doing bird richness
summary(aov(data = total_count_by_treat_site_species, formula = SEEDS ~ TREATMENT))
TukeyHSD(aov(data = total_count_by_treat_site_species, formula = SEEDS ~ TREATMENT))
