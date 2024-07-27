
library(tidyverse)
library(MatchIt)

m.out <- matchit(as.factor(NEWSESGROUP) ~ GENDER, data = newg_ses_wm_roi_data,
                 method = 'optimal', exact = ~ AGE,
                 distance = 'gam')
subsample <- match.data(m.out)


t.test(AGE~NEWSESGROUP, data = subsample)

write.csv(subsample, file = './plots/subsample_agematched.csv', row.names = F)

newg_ses_wm_roi_data_60 <- newg_ses_wm_roi_data %>%
  filter(AGE >= 60)
subsample_60 <- subsample %>%
  filter(AGE >= 60)

# >= 60 -------------------------------------------------------------------

newg_wm_60_aov_models <- newg_ses_wm_roi_data_60 %>%
  select(contains('_')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + HPT + DIABETES + HPL, 
            data = newg_ses_wm_roi_data_60)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

newg_wm_60_aov_models['DV'] <- dvnames # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
# p_adj <- p.adjust(newg_wm_aov_models$`Pr(>F)`, method = 'fdr')
newg_wm_60_aov_models['padj'] <- p.adjust(newg_wm_60_aov_models$`Pr(>F)`, method = 'fdr')

newg_wm_60_aov_results <- subset(newg_wm_60_aov_models, padj < 0.05)

tmp <- subsample_60 %>%
  group_by(NEWSESGROUP) %>%
  summarise(
    across(all_of(newg_wm_60_aov_results$DV), 
           .fns = list(
             mean = mean),
           na.rm = T))


# >=60 age matched --------------------------------------------------------


# ancova

subsample_60_aov_models <- subsample_60 %>%
  select(contains('_')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + HPT + DIABETES + HPL, 
            data = subsample_60)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

subsample_60_aov_models['DV'] <- dvnames # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
# p_adj <- p.adjust(newg_wm_aov_models$`Pr(>F)`, method = 'fdr')
subsample_60_aov_models['padj'] <- p.adjust(subsample_60_aov_models$`Pr(>F)`, method = 'fdr')

subsample_60_aov_results <- subset(subsample_60_aov_models, padj < 0.05)

tmp <- subsample_60 %>%
  group_by(NEWSESGROUP) %>%
  summarise(
    across(all_of(newg_wm_60_aov_models$DV), 
           .fns = list(
             mean = mean),
           na.rm = T))


# fa
subsample_60_fa_aov_models <- subsample_60 %>%
  filter(AGE >= 60) %>%
  select(contains('_FA')) %>%
  map(~ aov(.x ~ NEWSESGROUP + GENDER + HPT + DIABETES + HPL, 
            data = subsample_60)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))  
roi <- gsub('_FA', '', subsample_60_aov_models$DV[1:20])
subsample_60_fa_aov_models['DV'] <- roi
subsample_60_fa_aov_models['padj'] <- p.adjust(subsample_60_fa_aov_models$`Pr(>F)`, 
                                            method = 'fdr')
subsample_60_fa_aov_results <- subset(subsample_60_fa_aov_models, padj < 0.05)

# md
subsample_60_md_aov_models <- subsample_60 %>%
  select(contains('_MD')) %>%
  map(~ aov(.x ~ NEWSESGROUP + GENDER + HPT +DIABETES + HPL, 
            data = subsample_60)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))  
subsample_60_md_aov_models['DV'] <- roi
subsample_60_md_aov_models['padj'] <- p.adjust(subsample_60_md_aov_models$`Pr(>F)`, 
                                            method = 'fdr')
subsample_60_md_aov_results <- subset(subsample_60_md_aov_models, padj < 0.05)

# rd
subsample_60_rd_aov_models <- subsample_60 %>%
  select(contains('_RD')) %>%
  map(~ aov(.x ~ NEWSESGROUP + GENDER + HPT +DIABETES + HPL, 
            data = subsample_60)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))  
subsample_60_rd_aov_models['DV'] <- roi
subsample_60_rd_aov_models['padj'] <- p.adjust(subsample_60_rd_aov_models$`Pr(>F)`, 
                                               method = 'fdr')
subsample_60_rd_aov_results <- subset(subsample_60_rd_aov_models, padj < 0.05)

# ad 
subsample_60_ad_aov_models <- subsample_60 %>%
  select(contains('_AD')) %>%
  map(~ aov(.x ~ NEWSESGROUP + GENDER + HPT +DIABETES + HPL, 
            data = subsample_60)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))  
subsample_60_ad_aov_models['DV'] <- roi
subsample_60_ad_aov_models['padj'] <- p.adjust(subsample_60_ad_aov_models$`Pr(>F)`, 
                                               method = 'fdr')
subsample_60_ad_aov_results <- subset(subsample_60_ad_aov_models, padj < 0.05)


# summarise

tmp <- subsample %>%
  group_by(NEWSESGROUP) %>%
  summarise(
    across(.cols = c(CCR_FA, ILFL_FA, ILFL_MD, SLFTL_MD,
                     ILFL_RD,FMA_AD), 
           .fns = list(
             mean = mean,
             sd = sd),
           na.rm = T)
  )
