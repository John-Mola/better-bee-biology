##%######################################################%##
#                                                          #
####        BETTER BEE BIOLOGY RESULTS WRANGLING        ####
#                                                          #
##%######################################################%##

# This script pulls in the results from our literature review google sheet and wrangles them into a clean, useable format.


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(googlesheets4) #this script requires the dev version (for sheet names)

# DATA SOURCES ------------------------------------------------------------

#google sheet url
gurl <- "https://docs.google.com/spreadsheets/d/1EpHpmNvederjgG7VGHBtcsFqa7lUlha4NEatYPCt3kI/edit#gid=1607450225"

#get names of sheets for mutate below...I hate how this isn't automated
people <- sheet_names(gurl)

# reading in each sheet (I thought I could do this as a range but couldn't figure out a simple way...this works, whatever.)
# reading in the "NP"'s as NA so list-columns aren't created. However, this is not satisfying as we want those values. What would be better is to convert NPs to -1 or 999999999 or something then we can filter them out downstream, count how many, etc
df1 <- read_sheet(gurl, sheet = 5, na = c(" ", "NP", "NA", "np")) %>% mutate(gathered_by = people[5])
df2 <- read_sheet(gurl, sheet = 6, na = c(" ", "NP", "NA", "np")) %>% mutate(gathered_by = people[6])
df3 <- read_sheet(gurl, sheet = 7, na = c(" ", "NP", "NA", "np")) %>% mutate(gathered_by = people[7])
df4 <- read_sheet(gurl, sheet = 8, na = c(" ", "NP", "NA", "np")) %>% mutate(gathered_by = people[8])
df5 <- read_sheet(gurl, sheet = 9, na = c(" ", "NP", "NA", "np")) %>% mutate(gathered_by = people[9])
df6 <- read_sheet(gurl, sheet = 10, na = c(" ", "NP", "NA", "np")) %>% mutate(gathered_by = people[10])
df7 <- read_sheet(gurl, sheet = 11, na = c(" ", "NP", "NA", "np")) %>% mutate(gathered_by = people[11])

df_all <- bind_rows(df1, df2, df3, df4, df5, df6, df7)

# write to hardcopy as backup each time

saveRDS(object = df_all, file = paste0("./data/data_output/literature-review-combined-downloaded-",Sys.Date(), ".RDS"))


# PLAYING WITH THE DATA ---------------------------------------------------

df_so_far <- df_all %>% 
  # presumably any paper not yet reviewed is "NA" in the "relevant" column so this gives us only the ones we've looked at so far
  filter(!is.na(relevant))

df_so_far %>% 
  group_by(relevant) %>% 
  tally() %>% 
  #group_by(gathered_by) %>% 
  mutate(total_reviewed = sum(n), percent_category = n/total_reviewed) %>% 
  ggplot(., aes(x = relevant, y = percent_category)) +
  geom_bar(stat = "identity") +
  #facet_wrap(~gathered_by) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_relevant <- df_so_far %>% 
  filter(relevant == "yes")

df_relevant %>% 
  filter(!is.na(collection_method), confidence_n_bees != "voucher") %>% 
  mutate(study_duration = (last_year - first_year)+1, bees_per_year = n_bees_removed/study_duration) %>%  
  ggplot(., aes(x = collection_method, y = bees_per_year)) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.5, aes(color = confidence_n_bees)) +
  geom_boxplot(alpha = 0.5) + 
  theme_classic()

df_relevant %>% 
  group_by(deposition_info, PY) %>% 
  tally() %>% 
  group_by(PY) %>% 
  mutate(year_pubs = sum(n)) %>% 
  filter(deposition_info == "yes") %>% 
  mutate(percent_reporting = n/year_pubs) %>% 
  ggplot(., aes(x = PY, y = percent_reporting)) +
  geom_point()

df_relevant %>% 
  group_by(deposition_info) %>% 
  tally()


df_so_far %>% 
  group_by(PY) %>% 
  mutate(pubs_that_year = n()) %>% 
  filter(relevant == "yes") %>% 
  group_by(PY) %>% 
  summarise(n = n(), pubs_that_year = first(pubs_that_year), percent_relevant = n/pubs_that_year) %>% 
  ggplot(., aes(x = PY, y = percent_relevant)) +
  geom_point()

# number of bees per publication year (controlling for study duration) That massive outlier is hte Westphal study...which occurs across five countries, so makes sense there's so many bees
df_relevant %>% 
  filter(!is.na(collection_method), confidence_n_bees != "voucher") %>% 
  mutate(study_duration = (last_year - first_year)+1, bees_per_year = n_bees_removed/study_duration) %>% 
  group_by(PY) %>% 
  summarise(total_bees_py = sum(bees_per_year, na.rm = T)) %>% 
  ggplot(., aes(x = PY, y = total_bees_py)) +
  geom_point() 

# same as above, exclude westphal
df_relevant %>% 
  filter(!is.na(collection_method), confidence_n_bees != "voucher", DI != "10.1890/07-1292.1") %>% 
  mutate(study_duration = (last_year - first_year)+1, bees_per_year = n_bees_removed/study_duration) %>% 
  group_by(PY) %>% 
  summarise(total_bees_py = sum(bees_per_year, na.rm = T), n=n(), mean_bpy = mean(bees_per_year, na.rm = TRUE)) %>%  
  # added "n" since we're not done adding the numbers. So this says number of bees collected per year per study (which I guess is just a complex way of calculating mean?)
  ggplot(., aes(x = PY, y = total_bees_py/n)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)  

df_relevant %>% 
  group_by(PY, collection_method) %>% 
  tally() %>% 
  ggplot(., aes(x = PY, y = n, color = collection_method)) +
  geom_path() +
  geom_point()



df_so_far %>% 
  filter(str_detect(follow_up, 'PROBLEM|DISCUSSION')) %>% 
  group_by(gathered_by) %>% 
  tally()

df_so_far %>% 
  filter(str_detect(follow_up, 'DISCUSSION')) %>% 
  select(follow_up)



# number of studies per publication year by collection type
df_rel2 <- df_so_far %>% 
  filter(relevant %in% c("yes", "no, observational", "no, review or meta-analisis"))

df_rel2 %>% 
  group_by(PY, relevant, collection_method) %>% 
  tally() %>% 
  mutate(study_type = if_else(relevant == "yes", collection_method, relevant)) %>% 
  filter(PY < 2012) %>%
  ggplot(., aes(x = PY, y = n, color = study_type)) +
  geom_path()


# what proportion have we reviewed so far

df_all %>% 
  mutate(reviewed_yet = if_else(!is.na(relevant), "yes", "no")) %>% 
  group_by(PY, reviewed_yet) %>% 
  tally() %>% 
  pivot_wider(names_from = reviewed_yet, values_from = n) %>% 
  mutate(total = no+yes, proportion_reviewed = yes/total) %>% 
  ggplot(., aes(x = PY, y = proportion_reviewed)) +
  geom_point()


# df_relevant %>% 
#   mutate(lag_time = PY - last_year) %>% 
#   ggplot(., aes(x = n_bees_removed/(last_year-first_year), y = lag_time)) +
#   geom_point()


df_so_far %>% 
  group_by(gathered_by) %>% 
  tally()


df_so_far

df_relevant %>% 
  filter(!is.na(collection_method), confidence_n_bees != "voucher") %>% 
  mutate(study_duration = (last_year - first_year)+1, bees_per_year = n_bees_removed/study_duration) %>% 
  ggplot(., aes(x = PY, y = study_duration)) +
  geom_jitter() +
  geom_smooth()
  