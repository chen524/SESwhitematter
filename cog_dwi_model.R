library(readxl)
library(tidyverse)


# cog~tracts FA model -----------------------------------------------------

# episodic memory
tracts_em <- ses_wm_tracts_roi_data %>%
  dplyr::select(contains('_FA')) %>%
  map(~ aov(EPISODIC ~ .x + AGE + GENDER + 
              HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
            data = ses_wm_tracts_roi_data)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

tracts_em['Tracts'] <- tracts # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
# p_adj <- p.adjust(wm_tracts_models$`Pr(>F)`, method = 'fdr')
tracts_em['padj'] <- p.adjust(tracts_em$`Pr(>F)`, method = 'fdr')
tracts_em_results <- subset(tracts_em, padj < 0.05)

print(tracts_em_results$Tracts)


# em moderation -----------------------------------------------------------
 
# based on the significant tracts in tracts_em_results
library(jtools) # for summ()
library(interactions)


a <- lm(EPISODIC ~ SLFL_FA*SES + AGE + GENDER + 
          HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
        data = ses_wm_tracts_roi_data)
summ(a)

# summary(a)
sim_slopes(a, pred = SLFL_FA, modx = SES)
interact_plot(a, pred = SLFL_FA, modx = SES)

ses_wm_tracts_roi_data %>%
  ggplot(aes(x = SES,
             y = SLFL_FA)) +
  geom_point() +
  geom_smooth(method = "lm")

# wm moderation -----------------------------------------------------------
# working memory
tracts_wm <- ses_wm_tracts_roi_data %>%
  dplyr::select(contains('_FA')) %>%
  map(~ aov(WORKING ~ .x + AGE + GENDER + 
              HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
            data = ses_wm_tracts_roi_data)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

tracts_wm['Tracts'] <- tracts # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
# p_adj <- p.adjust(wm_tracts_models$`Pr(>F)`, method = 'fdr')
tracts_wm['padj'] <- p.adjust(tracts_wm$`Pr(>F)`, method = 'fdr')
tracts_wm_results <- subset(tracts_wm, padj < 0.05)

a <- lm(WORKING ~ UFL_FA*SES + AGE + GENDER + 
          HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
        data = ses_wm_tracts_roi_data)
summ(a)

# summary(a)
sim_slopes(a, pred = UFL_FA, modx = SES)
interact_plot(a, pred = UFL_FA, modx = SES)


# lan moderation ----------------------------------------------------------

# language
tracts_lan <- ses_wm_tracts_roi_data %>%
  dplyr::select(contains('_FA')) %>%
  map(~ aov(LANGUAGE ~ .x + AGE + GENDER + 
              HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
            data = ses_wm_tracts_roi_data)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

tracts_lan['Tracts'] <- tracts # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
# p_adj <- p.adjust(wm_tracts_models$`Pr(>F)`, method = 'fdr')
tracts_lan['padj'] <- p.adjust(tracts_lan$`Pr(>F)`, method = 'fdr')
tracts_lan_results <- subset(tracts_lan, padj < 0.05)

a <- lm(LANGUAGE ~ UFR_FA*SES + AGE + GENDER + 
          HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
        data = ses_wm_tracts_roi_data)
summ(a)

# summary(a)
sim_slopes(a, pred = FMA_FA, modx = SES)
interact_plot(a, pred = FMA_FA, modx = SES)

# att moderation ----------------------------------------------------------


# attention
tracts_att <- ses_wm_tracts_roi_data %>%
  dplyr::select(contains('_FA')) %>%
  map(~ aov(ATTENTION ~ .x + AGE + GENDER + 
              HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
            data = ses_wm_tracts_roi_data)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

tracts_att['Tracts'] <- tracts # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
# p_adj <- p.adjust(wm_tracts_models$`Pr(>F)`, method = 'fdr')
tracts_att['padj'] <- p.adjust(tracts_att$`Pr(>F)`, method = 'fdr')
tracts_att_results <- subset(tracts_att, padj < 0.05)


a <- lm(ATTENTION ~ UFR_FA*SES + AGE + GENDER + 
          HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
        data = ses_wm_tracts_roi_data)
summ(a)

# summary(a)
sim_slopes(a, pred = ILFR_FA, modx = SES)
interact_plot(a, pred = ILFR_FA, modx = SES)


# ef moderation -----------------------------------------------------------


# ef
tracts_ef <- ses_wm_tracts_roi_data %>%
  dplyr::select(contains('_FA')) %>%
  map(~ aov(EXECUTIVE ~ .x + AGE + GENDER + 
              HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
            data = ses_wm_tracts_roi_data)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

tracts_ef['Tracts'] <- tracts # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
# p_adj <- p.adjust(wm_tracts_models$`Pr(>F)`, method = 'fdr')
tracts_ef['padj'] <- p.adjust(tracts_ef$`Pr(>F)`, method = 'fdr')
tracts_ef_results <- subset(tracts_ef, padj < 0.05)

# moderation 

a <- lm(EXECUTIVE ~ UFL_FA*SES + AGE + GENDER + 
          HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol, 
        data = ses_wm_tracts_roi_data)
summ(a)

# summary(a)
sim_slopes(a, pred = SLFR_FA, modx = SES)
interact_plot(a, pred = SLFR_FA, modx = SES)

# latent moderation model -------------------------------------------------

install.packages('semTools')
library(lavaan)
library(semTools)
head(ses_wm_tracts_roi_data)

# Testing model fit (without interaction)

# Measurement model (step 1)

model0 <- '
# loadings
tracts =~ ATRL_FA + ATRR_FA + CCL_FA + CCR_FA + CHL_FA + CHR_FA + CSTL_FA + CSTR_FA + FMA_FA + FMI_FA + IFOFL_FA + IFOFR_FA + ILFL_FA + ILFR_FA + SLFL_FA + SLFR_FA + SLFTL_FA + SLFTR_FA + UFL_FA + UFR_FA
cognition =~ EPISODIC + WORKING + LANGUAGE + ATTENTION + EXECUTIVE
'

fit0 <- cfa(model0, data = ses_wm_tracts_roi_data, estimator="MLM")

summary(fit0, fit.measures = TRUE, standardized=TRUE)

mi <- modificationindices(test_fit)
mi[mi$mi>10,]

# Structural model (step 2; not necessary here because of saturated structural part)

model1 <- '
# loadings
tracts =~ ATRL_FA + ATRR_FA + CCL_FA + CCR_FA + CHL_FA + CHR_FA + CSTL_FA + CSTR_FA + FMA_FA + FMI_FA + IFOFL_FA + IFOFR_FA + ILFL_FA + ILFR_FA + SLFL_FA + SLFR_FA + SLFTL_FA + SLFTR_FA + UFL_FA + UFR_FA
cognition =~ EPISODIC + WORKING + LANGUAGE + ATTENTION + EXECUTIVE

# regression
cognition ~ tracts + SES + tracts:SES + AGE + GENDER + 
          HPT + DIABETES + HPL + EstimatedTotalIntraCranialVol
'

fit1 <- sem(model1, data = ses_wm_tracts_roi_data, estimator="MLM")

summary(fit1, fit.measures = TRUE, standardized=TRUE)

mi <- modificationindices(test_fit)
mi[mi$mi>10,]

# Double Mean Centering

mydata_dmc <- indProd (mydata , var1 = c("IV1", "IV2", "IV3"),
                       var2 = c("MOD1", "MOD2", "MOD3"),
                       match = FALSE , meanC = TRUE ,
                       residualC = FALSE , doubleMC = TRUE)

head(mydata_dmc)

# Model estimation with latent interaction

int_model <- '
# Loadings
iv =~ IV1 + IV2 + IV3
mod =~ MOD1 + MOD2 + MOD3
dv =~ DV1 + DV2 + DV3
int =~ IV1.MOD1 + IV1.MOD2 + IV1.MOD3 + IV2.MOD1 + IV2.MOD2 +
IV2.MOD3 + IV3.MOD1 + IV3.MOD2 + IV3.MOD3

# Regression
dv ~ iv + mod + int

# Error covariances
IV1.MOD1 ~~ IV1.MOD2 + IV1.MOD3 + IV2.MOD1 + IV3.MOD1
IV1.MOD2 ~~ IV1.MOD3 + IV2.MOD2 + IV3.MOD2
IV1.MOD3 ~~ IV2.MOD3 + IV3.MOD3
IV2.MOD1 ~~ IV2.MOD2 + IV2.MOD3 + IV3.MOD1
IV2.MOD2 ~~ IV2.MOD3 + IV3.MOD2
IV2.MOD3 ~~ IV3.MOD3
IV3.MOD1 ~~ IV3.MOD2 + IV3.MOD3
IV3.MOD2 ~~ IV3.MOD3
'

int_fit <- sem(int_model, data = mydata_dmc, estimator="MLM")

summary(int_fit, fit.measures = TRUE, standardized=TRUE)

# Simple Slopes

probe <- probe2WayMC(int_fit, c("iv", "mod", "int"), "dv", "mod",
                     c(-sqrt(1.462),0, sqrt(1.462)))
probe

plotProbe(probe, c(-3,3))

# Double mean centering done manually

# Step 1: 1st mean centering of indicators for IV and MOD

attach(mydata2)
mydata2$IV1c <- scale(IV1, scale = FALSE)
mydata2$IV2c <- scale(IV2, scale = FALSE)
mydata2$IV3c <- scale(IV3, scale = FALSE)
mydata2$MOD1c <- scale(MOD1, scale = FALSE)
mydata2$MOD2c <- scale(MOD2, scale = FALSE)
mydata2$MOD3c <- scale(MOD3, scale = FALSE)
detach(mydata2)

# Step 2: Product terms

attach(mydata2)
mydata2$I1M1 <- IV1c * MOD1c
mydata2$I1M2 <- IV1c * MOD2c
mydata2$I1M3 <- IV1c * MOD3c
mydata2$I2M1 <- IV2c * MOD1c
mydata2$I2M2 <- IV2c * MOD2c
mydata2$I2M3 <- IV2c * MOD3c
mydata2$I3M1 <- IV3c * MOD1c
mydata2$I3M2 <- IV3c * MOD2c
mydata2$I3M3 <- IV3c * MOD3c
detach(mydata2)

# Step 3: 2nd mean centering (product terms)

attach(mydata2)
mydata2$I1M1c <- scale(I1M1, scale = FALSE)
mydata2$I1M2c <- scale(I1M2, scale = FALSE)
mydata2$I1M3c <- scale(I1M3, scale = FALSE)
mydata2$I2M1c <- scale(I2M1, scale = FALSE)
mydata2$I2M2c <- scale(I2M2, scale = FALSE)
mydata2$I2M3c <- scale(I2M3, scale = FALSE)
mydata2$I3M1c <- scale(I3M1, scale = FALSE)
mydata2$I3M2c <- scale(I3M2, scale = FALSE)
mydata2$I3M3c <- scale(I3M3, scale = FALSE)
detach(mydata2)

head(mydata2)