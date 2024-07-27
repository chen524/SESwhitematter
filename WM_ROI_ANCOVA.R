## Read whithe matter ROI data (FA, MD, RD) and do MANCOVA to test sesgroup effect
## BY LIU Chen 2022.11.13

library(readxl)
library(tidyverse)


# read and merge data -----------------------------------------------------
setwd('D:\\Projects\\SES_DTI')

# read FA, MD, RD data and add prefix respectivly, but not with SUBID columns
fa_data <- read_csv('ROI_FA_jhuicbmlabels.csv',
                    col_names = T) %>%
  as_tibble() %>%
  rename_with(~ paste0(., '_FA'), -SUBID)

md_data <- read_csv('ROI_MD_jhuicbmlabels.csv',
                    col_names = T) %>%
  as_tibble() %>%
  rename_with(.fn = ~ paste0(.x, '_MD'), -SUBID)

rd_data <- read_csv('ROI_RD_jhuicbmlabels.csv',
                    col_names = T) %>%
  as_tibble() %>%
  rename_with(.fn = ~ paste0(.x, '_RD'), -SUBID)

ad_data <- read_csv('ROI_L1_jhuicbmlabels.csv',
                    col_names = T) %>%
  as_tibble() %>%
  rename_with(.fn = ~ paste0(.x, '_AD'), -SUBID)

wm_roi_data <- list(fa_data, md_data, rd_data, ad_data) %>%
  reduce(full_join, by = 'SUBID') %>%
  as_tibble() 
# mutate_at(vars(SUBID), as_factor()) # cant do that cause SUBID be like SUB-2ZHUJIUCAI, which is integer
wm_roi_data$SUBID <- gsub('[^0-9]', '', wm_roi_data$SUBID)

# read ses cognitive data
ses_cog_mri_data <- read_csv('SES_MRIID_Beh_BNU.csv',
                             col_names = T) %>%
  as.tibble() %>%
  select(!c(BirthYear, OCC, INN, EDU, MMSE, MRINum, MRIIDOld))
ses_cog_mri_data$MRIIDNew <- gsub('[^0-9]', '', ses_cog_mri_data$MRIIDNew) %>%
  as.character()

# read ses cognitive data with missing values mutated (n = 841)
ses_cog_mri_mutate_data <- read.csv('D:\\Projects\\SES_T1\\derivatives\\FS_SES_841_IMPUTE.csv') %>%
  select(1:32, 291) %>%
  as_tibble() 
ses_cog_mri_mutate_data$MRIID <- gsub('[^0-9]', '', 
                                      ses_cog_mri_mutate_data$MRIID) %>%
  as.character()

# DTI-81 atlas with 50 structural labels
ses_wm_labels_roi_data <- left_join(ses_cog_mri_mutate_data, wm_roi_data, 
                             by = c('MRIID' = 'SUBID')) %>%
  as_tibble() %>%
  # rename(HPT = Hypertention,
  #        HPL = Hyperlipemia) %>% # rename
  # names of columns are all lower spelled, 
  # I just want the first 31 columns of behavior variables to be upper
  rename_at(vars(1:31), ~toupper(.)) %>% 
  mutate(across(where(is.integer), as.numeric)) %>% # most of the variables are numbers
  mutate_at(vars(ID, GENDER, SESGROUP, HPT, DIABETES,
                 HPL, MCI, MARRIAGE, HOUSING, SMOKING, DRINKING), 
            as_factor) %>% # some are factors 
  filter(!is.na(Tapetum_L_FA))

ses_wm_labels_roi_data$SESGROUP <- factor(ses_wm_labels_roi_data$SESGROUP,
                                          levels = c('1', '2', '3', '4', '5'),
                                          labels = c('Lowest', 'Lower', 'Medium', 'Higher', 'Highest'))

# JHU-20 atlas with 20 tracts
ses_wm_tracts_roi_data <- left_join(ses_cog_mri_mutate_data, 
                                    select(ses_wm_roi_data, 
                                           MRIIDNEW, ATRL_FA:UFR_AD), 
                                    by = c('MRIID' = 'MRIIDNEW')) %>%
  as_tibble() %>%
  mutate(across(where(is.integer), as.numeric)) %>% # most of the variables are numbers
  mutate_at(vars(ID, GENDER, SESGROUP, HPT, DIABETES,
                 HPL, MCI, MARRIAGE, HOUSING, SMOKING, DRINKING), 
            as_factor) %>% # some are factors 
  filter(!is.na(ATRL_FA))

ses_wm_tracts_roi_data$SESGROUP <- factor(ses_wm_tracts_roi_data$SESGROUP,
                                          levels = c('1', '2', '3', '4', '5'),
                                          labels = c('Lowest', 'Lower', 'Medium', 'Higher', 'Highest'))

write.csv(ses_wm_labels_roi_data, file = './derivatives/ses_wm_labels.csv',
          row.names = F)

write.csv(ses_wm_tracts_roi_data, file = './derivatives/ses_wm_tracts.csv',
          row.names = F)

# relative White Matter Hypertension volume also positively corralates with SES (p = 0.06)
wmh <- read_excel('./derivatives/WMH总体积_to晨晨.xlsx') %>%
  mutate(rWMH = WMH/TIV) %>%
  mutate(BH = str_extract(BH, '\\d+')) %>%
  mutate(BH = as.numeric(BH)) %>%
  right_join(ses_cog_mri_mutate_data[,1:6], ., by = c('ID' = 'BH'))

wmh_model <- lm(rWMH ~ SES + AGE + GENDER + TIV,
                data = wmh)
summary(wmh_model)

ggplot(data = wmh) +
  geom_point(aes(x = SES, y = rWMH)) +
  geom_smooth(mapping = aes(x = SES, y = rWMH), method = lm)

# white matter tracts measures ANCOVA ----------------------------------------------
wm_tracts_models <- ses_wm_tracts_roi_data %>%
  dplyr::select(contains('_FA')) %>%
  map(~ aov(.x ~ SES + AGE + GENDER + 
              HPT + DIABETES + HPL, 
            data = ses_wm_tracts_roi_data)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

tractsmeanames <- dvnames
tracts <- roi
wm_tracts_models['Tracts'] <- tracts # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
# p_adj <- p.adjust(wm_tracts_models$`Pr(>F)`, method = 'fdr')
wm_tracts_models['padj'] <- p.adjust(wm_tracts_models$`Pr(>F)`, method = 'fdr')

wm_tracts_results <- subset(wm_tracts_models, padj < 0.05)

# FA
# FMI is significantly correlated with SES
# higher SES is correlated with lower level of FA, and damaged whith matter fiber
ggplot(data = ses_wm_tracts_roi_data) +
  geom_point(mapping = aes(x = SES, y = FMI_FA), alpha = 0.5) +
  geom_smooth(mapping = aes(x = SES, y = FMI_FA), method = lm) +
  xlab('SES Score') + ylab('FA Value of FMI') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'serif'))

# MD
ggplot(data = ses_wm_tracts_roi_data) +
  geom_point(mapping = aes(x = SES, y = ATRR_MD), alpha = 0.5) +
  geom_smooth(mapping = aes(x = SES, y = ATRR_MD), method = lm) +
  xlab('SES Score') + ylab('MD Value of ATRR') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'serif'))


# white matter labels measures ANCOVA ----------------------------------------------
wm_labels_models <- ses_wm_labels_roi_data %>%
  dplyr::select(ends_with('_FA')) %>%
  map(~ aov(.x ~ SES + AGE + GENDER + 
              HPT + DIABETES + HPL, 
            data = ses_wm_labels_roi_data)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

labels <- sub('_FA', '', names(wm_labels_models)) # get dv names before map_dfr to get a list of 96 variables
wm_labels_models['Labels'] <- labels # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
wm_labels_models['padj'] <- p.adjust(wm_labels_models$`Pr(>F)`, method = 'fdr')
wm_labels_results <- subset(wm_labels_models, padj < 0.05)
write.csv(wm_labels_results, file = './derivatives/SES-FA_neg/SES_FA_jhuicbmlabels.csv',
          row.names = F)

# FA
ggplot(data = ses_wm_labels_roi_data) +
  geom_point(mapping = aes(x = SES, 
                           y = `Posterior_thalamic_radiation_(include_optic_radiation)_R_FA`), alpha = 0.5) +
  geom_smooth(mapping = aes(x = SES, 
                            y = `Posterior_thalamic_radiation_(include_optic_radiation)_R_FA`), method = lm) +
  xlab('SES Score') + ylab('FA of Posterior_thalamic_radiation_(include_optic_radiation)_R') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'serif'))

# MD
ggplot(data = ses_wm_labels_roi_data) +
  geom_point(mapping = aes(x = SES, 
                           y = Superior_corona_radiata_L_MD), alpha = 0.5) +
  geom_smooth(mapping = aes(x = SES, 
                            y = Superior_corona_radiata_L_MD), method = lm) +
  xlab('SES Score') + ylab('MD Value') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'serif'))

# RD
ggplot(data = ses_wm_labels_roi_data) +
  geom_point(mapping = aes(x = SES, 
                           y = 	
                             Posterior_corona_radiata_L_RD), alpha = 0.5) +
  geom_smooth(mapping = aes(x = SES, 
                            y = 	
                              Posterior_corona_radiata_L_RD), method = lm) +
  xlab('SES Score') + ylab('RD Value') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'serif'))

# AD
ggplot(data = ses_wm_labels_roi_data) +
  geom_point(mapping = aes(x = SES, 
                           y = 	
                             Posterior_corona_radiata_R_AD), alpha = 0.5) +
  geom_smooth(mapping = aes(x = SES, 
                            y = 	
                              Posterior_corona_radiata_R_AD), method = lm) +
  xlab('SES Score') + ylab('AD Value') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'serif'))

# white matter measures correlation ------------------------------------------

cor_models <- ses_wm_tracts_data %>%
  dplyr::select(contains('_')) %>%
  map(~ cor.test(.x, ses_wm_roi_data$SES)) %>%
  map_dfr(~ as.data.frame(.[1:4]))
dvnames <- names(cor_models)
cor_models['DV'] <- dvnames
cor_models['padj'] <- p.adjust(cor_models$p.value, method = 'fdr')
wm_cor_results <- subset(cor_models, padj < 0.05)
write_csv(wm_cor_results, file = 'SignificantCorResults.csv')


# correlation scatter plot ------------------------------------------------

ggplot(data = ses_wm_roi_data) +
  geom_point(mapping = aes(x = SES, y = ILFL_FA, color = SESGROUP), alpha = 0.75) +
  geom_smooth(mapping = aes(x = SES, y = ILFL_FA), method = lm) +
  xlab('SES Score') + ylab('FA Value of ILFL') +
  labs(color = 'SES Group') +
  scale_color_brewer(palette = 'RdYlBu') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'serif'))

ggplot(data = ses_wm_roi_data) +
  geom_point(mapping = aes(x = SES, y = SLFL_RD, color = SESGROUP), alpha = 0.75) +
  geom_smooth(mapping = aes(x = SES, y = SLFL_RD), method = lm) +
  xlab('SES Score') + ylab('RD Value of SLFL') +
  labs(color = 'SES Group') +
  scale_color_brewer(palette = 'RdYlBu') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'serif'))



# labels CCA to find significant labels correlated with 5 domains ---------------------

wm_labels <- ses_wm_labels_roi_data %>%
  dplyr::select(ends_with('_FA')) %>%
  scale() %>%
  as_tibble()
cog_5domains <- ses_wm_labels_roi_data %>%
  dplyr::select(EPISODIC, WORKING, LANGUAGE, ATTENTION, EXECUTIVE) %>%
  scale() %>%
  as_tibble()

library(stats)

# Perform CCA on the original data
cca_labels <- cancor(cog_5domains, wm_labels)
print(cca_labels$cor)

# permutation tests for tracts choosing

# Create an empty vector to store the p-values for each tract
labels_p_values <- numeric(length(labels))

for (i in 1:length(labels)) {
  # Select the i-th white matter tract as the predictor variable
  single_labels_data <- wm_labels[, i, drop = FALSE]
  
  # Perform CCA for the single tract
  cca_results <- cancor(cog_5domains, single_labels_data)
  
  # Number of permutations for significance testing
  num_perm <- 5000  # You can adjust the number of permutations
  
  # Create an empty vector to store the permuted canonical correlations
  perm_corr <- numeric(num_perm)
  
  # Perform permutation testing
  for (j in 1:num_perm) {
    # Randomly permute the rows of cognitive_data
    perm_cog_5dimains <- cog_5domains[sample(nrow(cog_5domains)), ]
    
    # Perform CCA on permuted data
    perm_results <- cancor(perm_cog_5dimains, single_labels_data)
    
    # Store the canonical correlations
    perm_corr[j] <- max(perm_results$cor)
  }
  
  # Calculate the p-value for the observed canonical correlation
  obsv_corr <- max(cca_results$cor)
  p_value <- sum(perm_corr >= obsv_corr) / num_perm
  
  # Store the p-value for the i-th tract
  labels_p_values[i] <- p_value
}

labels_cca_perm_p <- tibble(name = labels, p = labels_p_values)
labels_cca_perm_p['padj'] <- p.adjust(labels_cca_perm_p$p, method = 'fdr')
labels_cca_perm_p_results <- labels_cca_perm_p %>%
  subset(padj < 0.05) # these 29 tracts has significant correlation with 5 domains



# tracts CCA to find significant tracts correlated with 5 domains ---------------------
library(tidyverse)

# Check missing data situation

missing_tracts <- sapply(wm_tracts, function(x) sum(is.na(x)))

# Create a data frame with missing value counts
missing_tracts_info <- data.frame(Variable = names(missing_tracts), Missing_Values = missing_tracts)

wm_tracts <- ses_wm_tracts_roi_data %>%
  dplyr::select(ends_with('_FA')) %>%
  filter(!is.na(SLFTL_FA)) %>%
  scale() %>%
  as_tibble()
cog_5domains <- ses_wm_tracts_roi_data %>%
  dplyr::select(EPISODIC, WORKING, LANGUAGE, ATTENTION, EXECUTIVE,
                ends_with('_FA')) %>%
  filter(!is.na(SLFTL_FA)) %>%
  dplyr::select(EPISODIC, WORKING, LANGUAGE, ATTENTION, EXECUTIVE) %>%
  scale() %>%
  as_tibble()

library(stats)

# Perform CCA on the original data
cca_tracts <- cancor(cog_5domains, wm_tracts)
print(cca_tracts$cor)

# permutation tests for tracts choosing

# Create an empty vector to store the p-values for each tract
tracts_p_values <- numeric(length(tracts))

for (i in 1:length(tracts)) {
  # Select the i-th white matter tract as the predictor variable
  single_tracts_data <- wm_tracts[, i, drop = FALSE]
  
  # Perform CCA for the single tract
  cca_results <- cancor(cog_5domains, single_tracts_data)
  
  # Number of permutations for significance testing
  num_perm <- 5000  # You can adjust the number of permutations
  
  # Create an empty vector to store the permuted canonical correlations
  perm_corr <- numeric(num_perm)
  
  # Perform permutation testing
  for (j in 1:num_perm) {
    # Randomly permute the rows of cognitive_data
    perm_cog_5dimains <- cog_5domains[sample(nrow(cog_5domains)), ]
    
    # Perform CCA on permuted data
    perm_results <- cancor(perm_cog_5dimains, single_tracts_data)
    
    # Store the canonical correlations
    perm_corr[j] <- max(perm_results$cor)
  }
  
  # Calculate the p-value for the observed canonical correlation
  obsv_corr <- max(cca_results$cor)
  p_value <- sum(perm_corr >= obsv_corr) / num_perm
  
  # Store the p-value for the i-th tract
  tracts_p_values[i] <- p_value
}

tracts_cca_perm_p <- tibble(name = tracts, p = tracts_p_values)
tracts_cca_perm_p['padj'] <- p.adjust(tracts_cca_perm_p$p, method = 'fdr')
tracts_cca_perm_p_results <- tracts_cca_perm_p %>%
  subset(padj < 0.05) # these 29 tracts has significant correlation with 5 domains





# new group ---------------------------------------------------------------

newg_ses_wm_roi_data <- right_join(ses_cog_mri_data, wm_roi_data, 
                             by = c('MRIIDNew' = 'SUBID')) %>%
  as_tibble() %>%
  rename(HPT = Hypertention,
         HPL = Hyperlipemia) %>% # rename
  rename_with(toupper) %>% # names of columns are all lower spelled
  mutate(across(where(is.integer), as.numeric)) %>% # most of the variables are numbers
  mutate_at(vars(ID, GENDER, SESGROUP, HPT, DIABETES,
                 HPL, MCI, MARRIAGE, HOUSING, SMOKING, DRINKING), 
            as_factor) %>% # some are factors
  mutate(NEWSESGROUP = ifelse(SESGROUP ==1 | SESGROUP == 2 | SESGROUP == 3, 
                              1, 2))

newg_ses_wm_roi_data$NEWSESGROUP <- factor(newg_ses_wm_roi_data$NEWSESGROUP,
                                           levels = c('1', '2'),
                                           labels = c('Low', 'High'))

write.csv(newg_ses_wm_roi_data, file = './plots/newg_wm_ses_JHU.csv', row.names = F)


# ANCOVA of cognition within new ses groups -------------------------------

library(multcomp)
newg_ancova <- aov(ATTENTION ~ NEWSESGROUP + AGE + GENDER + HPT + DIABETES + HPL,
                 data = newg_ses_wm_roi_data)
summary(newg_ancova)
TukeyHSD(newg_ancova)

postHocs <- glht(newg_ancova, linfct = mcp(NEWSESGROUP = c('High - Low = 0')))
summary(postHocs)

# WM ROI features comparison ----------------------------------------------

newg_wm_aov_models <- newg_ses_wm_roi_data %>%
  dplyr::select(contains('_')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + HPT +DIABETES + HPL + MCI, 
            data = newg_ses_wm_roi_data)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))

# dvnames <- names(newg_wm_aov_models) # get dv names before map_dfr to get a list of 96 variables
newg_wm_aov_models['DV'] <- dvnames # add variable 'dv names' to results data frame

# add p_adjust as a variable to results data frame
# p_adj <- p.adjust(newg_wm_aov_models$`Pr(>F)`, method = 'fdr')
newg_wm_aov_models['padj'] <- p.adjust(newg_wm_aov_models$`Pr(>F)`, method = 'fdr')

newg_wm_aov_results <- subset(newg_wm_aov_models, padj < 0.05)

newg_wm_results_tract <- newg_wm_aov_results$DV
tmp <- newg_ses_wm_roi_data %>%
  group_by(NEWSESGROUP) %>%
  summarise(
    across(all_of(newg_wm_results_tract), 
           .fns = list(
             mean = mean,
             sd = sd),
           na.rm = T))

tmp <- tmp*1000


# plot using ggseg --------------------------------------------------------

# Enable this universe
options(repos = c(
  ggseg = 'https://ggseg.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('ggsegJHU')
library(ggsegJHU)
library(ggseg)
library(ggseg3d)
library(ggplot2)
library(scico)

# try
ggplot(data = jhu$data$geometry) + 
  geom_sf(aes(fill = jhu$data$region)) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(ncol = 2))


ggseg3d(atlas = jhu_3d) %>% 
  add_glassbrain("left") %>% 
  pan_camera("right lateral")

# FA plot

fa_plot_data <- tibble(
  hemi = c('center', 'center', 'left', 'left'),
  region = rep(c('Forceps minor', 'Inferior longitudinal fasciculus'), each = 2),
  side = c('upper axial', 'lower axial', 'upper axial', 'coronal'), # no need for side
  f = rep(newg_wm_aov_results$`F value`[1:2], each = 2)
) 

# hemi and region are enough for plot
fa_plot_data <- tibble(
  hemi = c('center', 'left'),
  region = c('Forceps minor', 'Inferior longitudinal fasciculus'),
  f = newg_wm_aov_results$`F value`[1:2]
) 
# fa_plot_data <- merge(fa_plot_data, jhu_data, by = c('hemi', 'region', 'side'))

# install.packages('sf')
# library(sf)

# fa_plot_data$geometry <- st_sf(st_cast(fa_plot_data$geometry, "POLYGON"))

fa_plot_data %>%
  ggseg(atlas = jhu,
        mapping = aes(fill = f)) +
  scale_fill_gradient(low = 'goldenrod',
                      high = 'firebrick') +
  labs(fill = 'F value') +
  theme_classic()

install.packages('scico')
library(scico)  
# plot using viridis inferior palette
fa_plot_data %>%
  ggseg(atlas = jhu,
        mapping = aes(fill = f)) +
  scale_fill_viridis_c(alpha = 0.8,
                       begin = 0.4,
                       end = 0.8,
                       direction = -1,
                       option = 'B') + # _c is for continous and scale_fill_viridis_d is for discrete data
  labs(fill = 'F value') +
  theme_classic()

# plot using scico lajolla palette

fa_plot_data %>%
  ggseg(atlas = jhu,
        mapping = aes(fill = f)
        ) +
  scale_fill_scico(palette = 'oslo',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = -1) +
  labs(fill = 'F value') +
  labs(title = 'F value map of FA comparison between low SES group and high SES group',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant tracts after p-correction were shown.') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, family = 'serif'),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, family = 'serif', hjust = 0.5),
        plot.caption = element_text(size = 12, family = 'serif', hjust = 0),
        legend.text = element_text(size = 8, family = 'serif'),
        legend.title = element_text(size = 12, family = 'serif'))

# MD plot

subset(newg_wm_aov_results, grepl('MD', DV)) # show rows whose DV contains 'MD'

md_plot_data <- tibble(
  hemi = c('left', 'left', 'right', 'left', 'center', 'center', 'left', 'right',
           'left', 'right', 'left', 'left', 'right'),
  region = c('Cingulum (cingulate gyrus)', 'Cingulum (hippocampus)', 'Cingulum (hippocampus)',
             'Corticospinal tract', 'Forceps major', 'Forceps minor',
             'Inferior fronto-occipital fasciculus', 'Inferior fronto-occipital fasciculus',
             'Inferior longitudinal fasciculus', 'Inferior longitudinal fasciculus',
             'Superior longitudinal fasciculus', 'Superior longitudinal fasciculus (temporal part)',
             'Uncinate fasciculus'),
  f = subset(newg_wm_aov_results, grepl('MD', DV))$`F value`
) 

md_plot_data %>%
  ggseg(atlas = jhu,
        mapping = aes(fill = f)) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(fill = 'F value') +
  labs(title = 'F value map of MD comparison between low SES group and high SES group',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant tracts after p-correction were shown.') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, family = 'serif'),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, family = 'serif', hjust = 0.5),
        plot.caption = element_text(size = 12, family = 'serif', hjust = 0),
        legend.text = element_text(size = 8, family = 'serif'),
        legend.title = element_text(size = 12, family = 'serif'))

# AD plot

subset(newg_wm_aov_results, grepl('AD', DV)) # show rows whose DV contains 'MD'

ad_plot_data <- tibble(
  hemi = c('left', 'left', 'right', 'center', 'center', 'left', 'right',
           'left', 'left', 'right', 'left', 'left'),
  region = c('Anterior thalamic radiation', 'Cingulum (hippocampus)', 'Cingulum (hippocampus)',
             'Forceps major', 'Forceps minor',
             'Inferior fronto-occipital fasciculus', 'Inferior fronto-occipital fasciculus',
             'Inferior longitudinal fasciculus', 
             'Superior longitudinal fasciculus', 'Superior longitudinal fasciculus',
             'Superior longitudinal fasciculus (temporal part)',
             'Uncinate fasciculus'),
  f = subset(newg_wm_aov_results, grepl('AD', DV))$`F value`
) 

ad_plot_data %>%
  ggseg(atlas = jhu,
        mapping = aes(fill = f)) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(fill = 'F value') +
  labs(title = 'F value map of AD comparison between low SES group and high SES group',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant tracts after p-correction were shown.') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, family = 'serif'),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, family = 'serif', hjust = 0.5),
        plot.caption = element_text(size = 12, family = 'serif', hjust = 0),
        legend.text = element_text(size = 8, family = 'serif'),
        legend.title = element_text(size = 12, family = 'serif'))

# RD plot

subset(newg_wm_aov_results, grepl('RD', DV))

rd_plot_data <- tibble(
  hemi = c('left', 'center', 'left', 'right',
           'left', 'right', 'right'),
  region = c('Cingulum (cingulate gyrus)', 
             'Forceps minor',
             'Inferior fronto-occipital fasciculus', 'Inferior fronto-occipital fasciculus',
             'Inferior longitudinal fasciculus', 'Inferior longitudinal fasciculus',
             'Uncinate fasciculus'),
  f = subset(newg_wm_aov_results, grepl('RD', DV))$`F value`
) 

rd_plot_data %>%
  ggseg(atlas = jhu,
        mapping = aes(fill = f)) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(fill = 'F value') +
  labs(title = 'F value map of RD (L1) comparison between low SES group and high SES group',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant tracts after p-correction were shown.') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, family = 'serif'),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, family = 'serif', hjust = 0.5),
        plot.caption = element_text(size = 12, family = 'serif', hjust = 0),
        legend.text = element_text(size = 8, family = 'serif'),
        legend.title = element_text(size = 12, family = 'serif'))


# >=60 plot ---------------------------------------------------

newg_60_ad_plot_data <- tibble(
  hemi = c('right', 'center', 'left', 'right'),
  region = c('Cingulum (hippocampus)', 'Forceps minor',
             'Superior longitudinal fasciculus', 'Superior longitudinal fasciculus'),
  f = subset(newg_wm_60_aov_results, grepl('AD', DV))$`F value`
) 

newg_60_ad_plot_data %>%
  ggseg(atlas = jhu,
        mapping = aes(fill = f)
  ) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(fill = 'F value') +
  labs(title = 'F value map of AD comparison between low SES group and high SES group',
       caption  = 'Participants age ≥60.\nAll comparisions were adjusted by fdr.\nOnly significant tracts after p-correction were shown.') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, family = 'serif'),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, family = 'serif', hjust = 0.5),
        plot.caption = element_text(size = 12, family = 'serif', hjust = 0),
        legend.text = element_text(size = 8, family = 'serif'),
        legend.title = element_text(size = 12, family = 'serif'))
