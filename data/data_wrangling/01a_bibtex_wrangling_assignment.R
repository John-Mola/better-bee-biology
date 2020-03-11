##%######################################################%##
#                                                          #
####     WRANGLING BIBTEXT DOWNLOADS AND SUBSAMPLES     ####
#                                                          #
##%######################################################%##

# This script takes the raw downloaded metadata from Web of Knowledge, puts it into a dataframe, then allows us to take subsamples based on whatever rules we so desire. 


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(bibliometrix)


# PARAMETERS --------------------------------------------------------------

# define the percent of papers to sample
percent_to_sample <- 0.10

# the humans who will be reviewing papers
humans <-  c("John M", "Jon Koch", "Janean S", "Ana M")


# DATA SOURCES ------------------------------------------------------------

# downloaded results from the following search string at WOK:
# ((TS= (pollinat* AND (*bee OR *bees OR bee OR bees) NOT( beekeep* OR apicultur* OR colony collapse* OR apis))) OR (TS = (apoidea AND (*bee OR *bees OR bee OR bees) NOT( beekeep* OR apicultur* OR colony collapse* OR apis)))) AND DOCUMENT TYPES: (Article) 
# Timespan: 1990-2019. Indexes: SCI-EXPANDED.

bib_path <- list.files(pattern="*.bib", path = "./data/data_raw/bibtex_downloads/", full.names = T)
bib_files <- lapply(bib_path, readFiles)
df_bib_files <- lapply(bib_files, convert2df, dbsource = "wos", format = "bibtex") %>% bind_rows()


# GENERAL WRANGLING -------------------------------------------------------

# for whatever reason there are files that don't have a publication year or are PY2020 despite our filter. I filter these out. 
df_bib_wrangled <- df_bib_files %>% 
  # this removes about 40 papers that lack a publication year or were published in 2020
  filter(!is.na(PY), PY != 2020) %>% 
  # functionally this does nothing but I keep it here to indicate I checked if there were duplicate records. There were not. 
  distinct() %>% 
  # adding a column for first author
  separate(col = AU, sep = ";", remove = FALSE, into = c("first_author"), extra = "drop")

# reducing to only distinct first-author-year combinations. Maybe they use the same methods or same datasets, so this is one way of reducing redundancy there
df_bib_duplicate_author_removed <- df_bib_wrangled %>% 
  distinct(first_author, PY, .keep_all = TRUE)

# 10 PERCENT TOTAL --------------------------------------------------------

df_bib_10total <- df_bib_duplicate_author_removed %>% 
  sample_frac(size = percent_to_sample)



# 10 PERCENT PER YEAR -----------------------------------------------------

df_bib_10year <- df_bib_duplicate_author_removed %>% 
  group_by(PY) %>% 
  sample_frac(size = percent_to_sample) %>% 
  ungroup()


# ASSIGNMENTS -------------------------------------------------------------


assignments_10total <- df_bib_10total %>% 
  mutate(assigned_to = rep(sample(humans), length = nrow(.)))

assignments_10year <- df_bib_10year %>% 
  mutate(assigned_to = rep(sample(humans), length = nrow(.)))

# check that it worked
assignments_10total %>% group_by(assigned_to) %>% tally()
assignments_10year %>% group_by(assigned_to) %>% tally()


# SAVE OUTPUTS ------------------------------------------------------------

write_csv(df_bib_wrangled, "./data/data_output/full_WOK_papers_list_wrangled.csv")

write_csv(df_bib_duplicate_author_removed, "./data/data_output/no_dup_author_year_WOK_papers_list_wrangled.csv")

write_csv(assignments_10total, "./data/data_output/assigned_no_dup_10percent_total.csv")

write_csv(assignments_10year, "./data/data_output/assigned_no_dup_10percent_year.csv")




# REMOVE PLOTTING FROM HERE LATER, NOT TIDY! ------------------------------

df_combined <- df_bib_10total %>% 
  mutate(how_sampled = "total") %>% 
  bind_rows(., mutate(df_bib_10year, how_sampled = "by year")) %>% 
  group_by(PY, how_sampled) %>% 
  tally()

df_combined %>% 
  ggplot(., aes(x = PY, y = n, color = how_sampled)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_path() +
  theme_classic(base_size = 15) +
  labs(x = "Publication Year", y = "Number of Publications \n Passing Filter", color = "How randomly selected?") +
  scale_color_brewer(type = "qual", palette = 2, labels = c("10% Each Year", "10% Total")) +
  theme(legend.position = c(0.3, 0.8), legend.title = element_blank())
  
