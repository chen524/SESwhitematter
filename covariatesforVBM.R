
library(tidyverse)

ses.mri <- read.csv(file = 'D:/projects/SES_anat/leisure_med/data/FS_SES_841_IMPUTE.csv') %>%
  select(ID:GDS, EstimatedTotalIntraCranialVol) %>%
  mutate_at(vars('ID', 'MRIID'), funs(as.character)) %>%
  rename(TIV = EstimatedTotalIntraCranialVol)

# sourcedata.folder <- 'D:\\Projects\\SES_T1\\data\\sourcedata'
# subject.folders <- list.dirs(sourcedata.folder, recursive = F, full.names = F)
# sub <- gsub('.*sub-(\\d+).*', '\\1', subject.folders)
# sub <- as.data.frame(sub)


mat.file <- Sys.glob(file.path('./sourcedata/new', "sub-*.txt"))
sub <- gsub('.*sub-(\\d+).*', '\\1', mat.file)
sub <- as.data.frame(sub)

ses.func <- left_join(sub, ses.mri, by = c('sub' = 'MRIID')) %>%
  mutate(sub = as.numeric(sub)
         
         # GENDER = fct_recode(GENDER,
         #                     '0' = 'Male',
         #                     '1' = 'Female')
         ) %>%
  arrange(sub) %>%
  # select(sub, SES, AGE, GENDER) %>%
  write_csv(file = './derivatives/ses_func_vars.csv', col_names = F)

cov.ses <- ses.funcfc %>%
  select(SES) %>%
  write_csv(file = 'E:\\derivatives\\func_cov_ses.csv', col_names = F)

cov.group <- ses.funcfc %>%
  select(NEWSESGROUP) %>%
  write_csv(file = 'E:\\derivatives\\func_cov_group.csv', col_names = F)

cov.age <- ses.funcfc %>%
  select(AGE) %>%
  write_csv(file = 'E:\\derivatives\\func_cov_age.csv', col_names = F)

cov.gender <- ses.funcfc %>%
  select(GENDER) %>%
  write_csv(file = 'E:\\derivatives\\func_cov_gender.csv', col_names = F)


# covars for FA network ---------------------------------------------------

ses_fa_network <- ses_cog_mri_mutate_data %>%
  select(MRIID, GENDER, AGE, SES, EstimatedTotalIntraCranialVol) %>%
  mutate_at(vars('MRIID'), funs(as.numeric)) %>%
  mutate(GENDER = recode(GENDER, 'Male' = 0, Female = 1)) %>%
  rename(TIV = EstimatedTotalIntraCranialVol) %>%
  arrange(MRIID)

# List .txt files in the specified directory
file_list <- list.files(path = 'D:\\Projects\\SES_DTI\\network\\network_FA_precedingzeros', 
                        pattern = "\\.txt$")

# Extract and clean the subject IDs
subject_ids <- gsub("sub-(0*)", "", tools::file_path_sans_ext(file_list))

# Convert subject IDs to numeric
subject_ids <- as.numeric(subject_ids)

# Create a data frame with the subject IDs
sub <- data.frame(SubjectID = subject_ids)

# scale variables
columns_to_scale <- c("SES", "AGE", "TIV")

# Create a custom function to scale each column
custom_scale <- function(x) ifelse(!is.na(x), scale(x, center = TRUE, scale = FALSE), x)

# Apply the custom_scale function to the selected columns
ses_fa_network_covars <- left_join(sub, ses_fa_network, 
                                   by = c('SubjectID' = 'MRIID')) %>%
  select(SES, AGE, GENDER, TIV) %>%
  mutate_at(vars(columns_to_scale), funs(custom_scale)) %>%
  write.table(file = './covariates/NBS_DesignMatrix.txt', sep = '\t', col.names = F,
              row.names = F)


         
         # GENDER = fct_recode(GENDER,
         #                     '0' = 'Male',
         #                     '1' = 'Female')
  # ) %>%
  # arrange(sub) %>%
  # # select(sub, SES, AGE, GENDER) %>%
  # write_csv(file = './derivatives/ses_func_vars.csv', col_names = F)
