#Appendix S3 Linear Mixed Models
#Oleksy et al. 2020, Ecology

#Packages and Data for Appendix S3 Linear Mixed Models (LMMs)

library(lme4)
library(MuMIn)
library(lmerTest)

Regional=read.csv("LACdataset180507_2015_2016.csv")
LongTerm=read.csv("LACdataset180507_GL.csv")
IntraSeasonal=read.csv("LACdataset180517_LS.csv")

Regional$logchl_a=log(Regional$chl_a)
LongTerm$logchl_a=log(LongTerm$chl_a)
IntraSeasonal$logchl_a=log(IntraSeasonal$chl_a)

#Regional Model: The regional LMM included the most important variables from the Regional Climate BRT model as fixed effects, 
#along with sampling event and lake identity as random effects.

RegionalMod = lmer(logchl_a ~ ppt_week + tmean_month + tmean_day + tmean_percent_normal + Ordinal_date + (1 | date_sampled) + (1 | name), data=Regional)
summary(RegionalMod)
r.squaredGLMM(RegionalMod)

#Long-Term Model: The long-term LMM included the most important variables from the Long-term BRT model as fixed effects, 
#along with sampling date as a random effect.

##Centering Variables to Account for Disparate Scales
LongTerm$NO3_N_center= (LongTerm$NO3_N-mean(LongTerm$NO3_N, na.rm=TRUE))
LongTerm$Max_SWE_center= (LongTerm$Max_SWE-mean(LongTerm$Max_SWE, na.rm=TRUE))
LongTerm$TEMP_center= (LongTerm$TEMP-mean(LongTerm$TEMP, na.rm=TRUE))
LongTerm$NtoP_center= (LongTerm$NtoP-mean(LongTerm$NtoP, na.rm=TRUE))
LongTerm$ppt_day_center= (LongTerm$ppt_day-mean(LongTerm$ppt_day, na.rm=TRUE))
LongTerm$year_center=(LongTerm$year-mean(LongTerm$year, na.rm=TRUE))

##LMM
LongTermMod_center = lmer(logchl_a ~ NO3_N_center + Max_SWE_center + TEMP_center + NtoP_center + ppt_day_center + year_center + year_center*NO3_N_center + Max_SWE_center*NO3_N_center + (1 | date_sampled), data=LongTerm)
summary(LongTermMod_center)
r.squaredGLMM(LongTermMod_center)

#Intra-Seasonal Model: The intra-seasonal LMM included the most important variables from the Intra-seasonal BRT model as fixed effects, 
#along with sampling date as a random effect.

Intra_SeasonalMod = lmer(logchl_a ~ Ordinal_date + ppt_month + ndepload  + TEMP + NtoP + name + (1 | date_sampled), data=IntraSeasonal)
summary(Intra_SeasonalMod)
r.squaredGLMM(Intra_SeasonalMod)
