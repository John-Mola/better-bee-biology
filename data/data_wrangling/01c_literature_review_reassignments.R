##%######################################################%##
#                                                          #
####        MID-POINT QUALITY CHECK REASSIGNMENT        ####
#                                                          #
##%######################################################%##

# This script takes what we have gathered info for as of 2020-05-09 and randomly assigns each person two papers to review

# probable steps:
# 1. load in the data to date, filter out rows that we have not yet reviewed
# 2. because I'm feeling lazy, for each person I will then just create a dataframe where we filter out their name from the dataset, and then randomly select 2 papers from each other person's name.
# 3. Combine all that back together, and render it as a csv 


# PACKAGES ----------------------------------------------------------------


library(tidyverse)


# DATA SOURCE -------------------------------------------------------------

df_sofar <- read_rds("./data/data_output/literature-review-combined-downloaded-2020-05-09.RDS") %>% 
  filter(!is.na(relevant)) 


# WRANGLING ---------------------------------------------------------------

levels(as.factor(df_sofar$gathered_by))

reassignment <- function(person_name){
  df_sofar %>% 
  filter(gathered_by != person_name) %>% 
  group_by(gathered_by) %>% 
  sample_n(size = 2) %>% 
  mutate(reassigned_to = person_name)
}

df_ana <- reassignment("ana")
df_byron <- reassignment("byron")
df_janean <- reassignment("janean")
df_johnm <- reassignment("john m")
df_jonk <- reassignment("jon k")
df_kiera <- reassignment("kiera n")
df_tien <- reassignment("tien l")

df_reassigned <- bind_rows(df_ana, df_byron, df_janean, df_johnm, df_jonk, df_kiera, df_tien) %>% 
  ungroup() %>% 
  dplyr::select(-(relevant:methods), -PP, -AR)


df_reassigned %>% 
  group_by(reassigned_to) %>%
  group_walk(~ write_csv(.x, paste0("./data/data_output/assignments/",.y$reassigned_to, "_halfway_reassignments2020_07_20.csv")))

