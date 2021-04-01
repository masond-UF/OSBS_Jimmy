##### JIMMY HOLDGRAFER ###########
##### SEED DISPERSAL PROJECT #####

# H01: as food diversity increases, bird diversity increases
# H02: as food diversity increases, bird abundance increases



####################
##### PACKAGES #####
####################
library(dplyr)
library(tidyverse)
library(lme4)





################
##### DATA #####
################
dat_og <- read.csv(file = "bird_data.csv",
                   header = TRUE)
head(dat_og)

# make date column 
# dat_og$date <- as.Date(with(dat_og, paste(year, month, day, sep = "-")), "%Y-%m-%d")

# remove unwanted columns
dat <- select(dat_og, -c("camera.card", "X", "notes", "photo.number"))
head(dat)

# remove NA's
clean_dat <- dat %>% 
  drop_na(species)
head(clean_dat)

# look to see what we have
unique(clean_dat$time)
unique(clean_dat$site)
unique(clean_dat$treatment)
unique(clean_dat$camera)
unique(clean_dat$species)
summary(clean_dat)

# make treatment factor
clean_dat$treatment <- as.factor(clean_dat$treatment)





##########################
##### SUMMARIZE DATA #####
##########################

### tables ###
# H1
# count of species by treatment
sps_count_by_treat <- clean_dat %>% 
  group_by(treatment, species) %>% 
  summarise(count = n())
sps_count_by_treat 


# count of individual species by treatment and site
sps_count_by_treat_site <- clean_dat %>% 
  group_by(treatment, site, species) %>% 
  summarise(count = n())
sps_count_by_treat_site



# H2
# total count of individuals by treatment
total_count_by_treat <- clean_dat %>% 
  group_by(treatment) %>% 
  summarise(count = n())
total_count_by_treat

# total count of individuals by treatment and site
total_count_by_treat_site <- clean_dat %>% 
  group_by(treatment, site) %>% 
  summarise(count = n())
total_count_by_treat_site
sum(total_count_by_treat_site$count)

### figs ###
plot(sps_count_by_treat_site$count ~ sps_count_by_treat_site$treatment, type = "l")
plot(total_count_by_treat_site$count ~ total_count_by_treat_site$treatment, type = "l")

# check dist of data
hist(sps_count_by_treat_site$count)
hist(total_count_by_treat_site$count)

####################
##### ANALYSES #####
####################

richness <- sps_count_by_treat_site %>% 
        group_by(treatment,site) %>% 
        summarise(richness = n())


rich_mean <- richness %>% 
        group_by(treatment) %>% 
	      summarise(MEAN = mean(richness), SD = sd(richness)) %>% 
	      mutate(SE = SD/(sqrt(10)))

### effect of food diversity (ie treatment) on bird abundance ###
summary(aov(data = richness, formula = richness ~ treatment))
TukeyHSD(aov(data = richness, formula = richness ~ treatment))

summary(aov(data = total_count_by_treat_site, formula = count ~ treatment))
TukeyHSD(aov(data = total_count_by_treat_site, formula = count ~ treatment))

bird_abund <- ggplot(data = total_count_by_treat_site, 
       aes(x = treatment, y = count, color = treatment)) +
  geom_jitter(alpha = 0.4, size = 4, 
              width = 0.3, height = 0.2) +
  ylab("Bird Observations") + 
  xlab("Food Richness (# of Resources)")+
  theme_bw()+
  theme(text = element_text(size = 15))+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,300), breaks = c(0,100,200,300))+
  theme(plot.margin = unit(c(1,1.5,1,1), "cm"))

bird_rich <- ggplot(data = richness, 
       aes(x = treatment, y = richness, color = treatment)) +
  geom_jitter(alpha = 0.4, size = 4, 
              width = 0.3, height = 0.2) +
  ylab("Bird Richness") + 
  xlab("Food Richness (# of Resources)")+
  theme_bw()+
  theme(text = element_text(size = 15))+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0.8,3.2),
                     breaks = c(1,2,3))+
  theme(plot.margin = unit(c(1,1.5,1,1), "cm"))


library(ggpubr)
library(cowplot)
library(gridExtra)

labs <- c("a","b")
grid.arrange(bird_rich, bird_abund, nrow = 1)


fit <- glmer.nb(count ~ treatment + (1 | site), 
                data = sps_count_by_treat_site,
                family = "quasipoisson")
summary(fit)
fit <- glm(count ~ treatment, 
                data = sps_count_by_treat_site,
                family = "quasipoisson")
summary(fit)
fit <- glmer.nb(count ~ treatment + (1 | site) + (1 | species), 
                data = sps_count_by_treat_site)
summary(fit)
