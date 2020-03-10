##%######################################################%##
#                                                          #
####     WRANGLING BIBTEXT DOWNLOADS AND SUBSAMPLES     ####
#                                                          #
##%######################################################%##

# This script takes the raw downloaded metadata from Web of Knowledge, puts it into a dataframe, then allows us to take subsamples based on whatever rules we so desire. 


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(bibliometrix)

# DATA SOURCES ------------------------------------------------------------

# downloaded results from the following search string at WOK:
# ((TS= (pollinat* AND (*bee OR *bees OR bee OR bees) NOT( beekeep* OR apicultur* OR colony collapse* OR apis))) OR (TS = (apoidea AND (*bee OR *bees OR bee OR bees) NOT( beekeep* OR apicultur* OR colony collapse* OR apis)))) AND DOCUMENT TYPES: (Article) 
# Timespan: 1990-2019. Indexes: SCI-EXPANDED.

bib_path <- list.files(pattern="*.bib", path = "./data/data_raw/bibtex_downloads/", full.names = T)
bib_files <- lapply(bib_path, readFiles)
df_bib_files <- lapply(bib_files, convert2df, dbsource = "isi", format = "bibtex") %>% bind_rows()

# WRANGLING ---------------------------------------------------------------

# for whatever reason there are files that don't have a publication year or are PY2020 despite our filter. I filter these out. 
df_bib_wrangled <- df_bib_files %>% 
  # this removes about 40 papers that lack a publication year or were published in 2020
  filter(!is.na(PY), PY != 2020)


  

