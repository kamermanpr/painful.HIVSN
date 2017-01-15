############################################################
#                                                          #
#         Subsetting data_all_2SignsOnly.csv data          #
#                 into data analysis units                 #
#                                                          #
############################################################

# Load packages
library(tidyverse)

# Load data
df_all <- read_csv('./data/2Signs_data_all.csv')

# Inspect
dim(df_all)
names(df_all)
head(df_all)
tail(df_all)
glimpse(df_all)
summary(df_all)

############################################################
#                                                          #
#              2SignsOnly_pain-intensity.csv               #
#                                                          #
############################################################
# Need to extract: Painful SN only
## pain_intensity
## pain_sites
## hscl_depression_score
## hscl_anxiety_score
## pcs
## tb
df_all %>%
    filter(painful_sn == 'Yes') %>%
    select(pain_intensity,
           pain_sites,
           hscl_depression,
           hscl_anxiety,
           pcs,
           tb) %>%
    rename(hscl_anxiety_score = hscl_anxiety,
           hscl_depression_score = hscl_depression) %>%
    write_csv('./data/2Signs_pain-intensity.csv')

############################################################
#                                                          #
#              2SignsOnly_pain-vs-no-pain.csv              #
#                                                          #
############################################################
# Need to extract: Painful and non-painful SN
## painful_sn
## height
## mass
## age
## time_since_diag
## vitB12
## other_pain_sites
## current_cd4
## sex
df_all %>%
    select(painful_sn,
           height,
           mass,
           age,
           `time_sinc_ diag`,
           vitb12,
           other_pain_sites,
           current_cd4,
           sex) %>%
    rename(time_since_diag = `time_sinc_ diag`,
           vitB12 = vitb12) %>%
    write_csv('./data/2Signs_pain-vs-no-pain.csv')

############################################################
#                                                          #
#            2SignsOnly_qol-pain-intensity.csv             #
#                                                          #
############################################################
# Need to extract: Painful SN only
## eq5d_vas
## age
## other_pain_sites
## pain_intensity
## sex
## education
## hscl_anxiety
## hscl_depression
df_all %>%
    filter(painful_sn == 'Yes') %>%
    select(`eq-5d_vas`,
           age,
           other_pain_sites,
           pain_intensity,
           sex,
           education,
           hscl_anxiety,
           hscl_depression) %>%
    rename(eq5d_vas = `eq-5d_vas`,
           hscl_depression_score = hscl_depression,
           hscl_anxiety_score = hscl_anxiety) %>%
    write_csv('./data/2Signs_qol-pain-intensity.csv')

############################################################
#                                                          #
#            2SignsOnly_qol-pain-vs-no-pain.csv            #
#                                                          #
############################################################
# Need to extract: Painful and non-painful SN
## eq5d_vas
## age
## painful_sn
## other_pain_sites
## sex
## education
## hscl_anxiety_score
## hscl_depression_score
## sex
df_all %>%
    select(`eq-5d_vas`,
           age,
           painful_sn,
           other_pain_sites,
           sex,
           education,
           hscl_anxiety,
           hscl_depression) %>%
    rename(eq5d_vas = `eq-5d_vas`,
           hscl_depression_score = hscl_depression,
           hscl_anxiety_score = hscl_anxiety) %>%
    write_csv('./data/2Signs_qol-pain-vs-no-pain.csv')

# Clear environment
rm(list = ls())
