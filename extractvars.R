
# vars --------------------------------------------------------------------

library(tidyverse)

ses.mri <- read.csv(file = 'D:/projects/SES_anat/leisure_med/data/FS_SES_841_IMPUTE.csv') %>%
  select(ID:GDS, EstimatedTotalIntraCranialVol) %>%
  mutate_at(vars('ID', 'MRIID'), funs(as.character)) %>%
  rename(TIV = EstimatedTotalIntraCranialVol)

ses.med <- ses.mri 
ses.med$MRIID <- as.numeric(ses.med$MRIID)
ses.med$MRIID <- sprintf('sub-%04d', ses.med$MRIID)
ses.med <- ses.med %>%
  arrange(MRIID) %>%
  write_csv(file = 'D:/projects/SES_anat/leisure_med/derivatives/ses_med_beh.csv',
            col_names = T)
  

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
  arrange(sub) 

ses.func$sub <- sprintf('sub-%04d', ses.func$sub)
ses.func %>%
  write.csv(file = './derivatives/vars.csv')
  write_delim(file = './derivatives/graphvars.csv',
              delim = ';')


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


# atlas -------------------------------------------------------------------

bna <- read_csv('./atlas/subregion_func_network_Yeo_updated.csv') %>%
  select(Label:region)

coordinate <- read_csv('./atlas/BNA_subregions.csv') 
coordinate <- coordinate[,c(1:3 ,6:7)]
coordinate.l <- coordinate[,c(2,5)] %>%
  rename(ID = `Label ID.L`,
         MNI = `lh.MNI(X,Y,Z)`)

coordinate.r <- coordinate[,c(3,6)] %>%
  rename(ID = `Label ID.R`,
         MNI = `rh.MNI(X,Y,Z)`)
coordinate <- rbind(coordinate.l, coordinate.r) %>%
  left_join(bna, ., by = c('Label' = 'ID')) %>%
  separate(MNI, into = c('x', 'y', 'z'), sep = ',') 
coordinate$Label <- 1
brainregions <- coordinate %>%
  select(Label, region, region, x, y, z) %>%
  cbind(., coordinate$region) %>%
  rename(desc = `coordinate$region`) %>%
  select(Label, region, desc, x, y, z) %>%
  write_delim(file = './atlas/BrainRegion_BN_Atlas.csv',
              delim = ';',
              col_names = F)
