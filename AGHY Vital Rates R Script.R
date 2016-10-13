## Title: AGHY Vital Rates Analysis
## Authors: Marion Donald and Tom Miller
## Date Started: October 13, 2016
## Purpose: Determine effects of endophyte status and water treatment on vital rates
## Date Updated: October 13, 2016




## (Marion) read in AGHY plot info 
AGHY.plots<-read.xlsx("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project/AGHY_SFAEF_life_history_expt.xlsx",
                      sheetName="Plot-level data")

AGHY.plants<-read.xlsx("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project/AGHY_SFAEF_life_history_expt.xlsx",
                      sheetName="Plant-level data original")



AGHY<- merge(AGHY.plots,AGHY.plants, by="plot")
AGHY$mass_inf<-NA
AGHY$mass_inf<- (AGHY$seed_mass_t/AGHY$inf_collected_t)
AGHY$mass_total<-NA
AGHY$mass_total<- (AGHY$mass_inf * AGHY$inf_number_t)


## Test for effects of parent endo status and water addition or control on seed mass per plant
#### not sure if I need to weight the seed mass and if I should take the log of it
seed.model.13.0 <-lmer(mass_total ~ (1|plot), data = AGHY, REML =F)
seed.model.13.1 <-lmer(mass_total ~ endo + (1|plot), data = AGHY, REML = F)
seed.model.13.2 <-lmer(mass_total ~ water + (1|plot), data = AGHY, REML = F)
seed.model.13.3 <-lmer(mass_total ~ endo + water + (1|plot), data = AGHY, REML=F)
seed.model.13.4 <-lmer(mass_total ~ endo * water + (1|plot), data = AGHY, REML = F)
AICtab(seed.model.13.0,seed.model.13.1,seed.model.13.2,seed.model.13.3,seed.model.13.4, weights=T)

seed.model.13.0 <-lmer(mass_total ~ (1|plot), weights = inf_number_t, data = AGHY, REML =F)
seed.model.13.1 <-lmer(mass_total ~ endo + (1|plot), weights = inf_number_t,data = AGHY, REML = F)
seed.model.13.2 <-lmer(mass_total ~ water + (1|plot), weights = inf_number_t,data = AGHY, REML = F)
seed.model.13.3 <-lmer(mass_total ~ endo + water + (1|plot), weights = inf_number_t,data = AGHY, REML=F)
seed.model.13.4 <-lmer(mass_total ~ endo * water + (1|plot), weights = inf_number_t,data = AGHY, REML = F)
AICtab(seed.model.13.0,seed.model.13.1,seed.model.13.2,seed.model.13.3,seed.model.13.4, weights=T)


seed.model.13.0 <-lmer(log(mass_total) ~ (1|plot), weights = inf_number_t, data = AGHY, REML =F)
seed.model.13.1 <-lmer(log(mass_total) ~ endo + (1|plot), weights = inf_number_t,data = AGHY, REML = F)
seed.model.13.2 <-lmer(log(mass_total) ~ water + (1|plot), weights = inf_number_t,data = AGHY, REML = F)
seed.model.13.3 <-lmer(log(mass_total) ~ endo + water + (1|plot), weights = inf_number_t,data = AGHY, REML=F)
seed.model.13.4 <-lmer(log(mass_total) ~ endo * water + (1|plot), weights = inf_number_t,data = AGHY, REML = F)
AICtab(seed.model.13.0,seed.model.13.1,seed.model.13.2,seed.model.13.3,seed.model.13.4, weights=T)


