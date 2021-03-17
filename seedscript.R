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
dat_og <- read.csv(file = "seeds.csv",
                   header = TRUE)
head(dat_og)

# make date column 
dat_og$DATE <- as.Date(dat_og$DATE)

# pivot the data
dat_long <- pivot_longer(dat, cols = 5:29, names_to = "SPECIES",
												 values_to = "SEEDS")

##########################
##### SUMMARIZE DATA #####
##########################

# total count of individuals by treatment and site
total_count_by_treat_site <- dat_long %>% 
  group_by(TREATMENT, BLOCK) %>% 
  summarise(sum(SEEDS)) 
total_count_by_treat_site <- rename(total_count_by_treat_site, SEEDS = "sum(SEEDS)")

plot(total_count_by_treat_site$SEEDS ~ total_count_by_treat_site$TREATMENT)
hist(total_count_by_treat_site$SEEDS)

# total count of species by treatment and site
total_count_by_treat_site_species <- dat_long %>% 
  group_by(TREATMENT, BLOCK, SPECIES) %>% 
  summarise(sum(SEEDS)) 

total_count_by_treat_site_species <- rename(total_count_by_treat_site_species, 
																						SEEDS = "sum(SEEDS)")

total_count_by_treat_site_species_wider <- pivot_wider(total_count_by_treat_site_species, 
																	 names_from = SPECIES, values_from = SEEDS)

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

### effect of food diversity (ie treatment) on bird richness ###
summary(aov(data = richness_by_treat_site, formula = RICHNESS ~ TREATMENT))
TukeyHSD(aov(data = richness_by_treat_site, formula = RICHNESS ~ TREATMENT))

mod <- aov(data = total_count_by_treat_site, formula = SEEDS ~ TREATMENT)
hist(mod$residuals)
plot(mod)

ggplot(data = total_count_by_treat_site, 
       aes(x = TREATMENT, y = RICHNESS))+
  geom_point(alpha = 0.3) +
  ylab("Seed Abundance") + 
  xlab("Food Diversity") +
  theme_bw()
