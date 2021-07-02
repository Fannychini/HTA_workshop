#=========================#
# PBS scraper for PRIMCAT #
#=========================#

## Libraries

library (dplyr)         # Data wrangling and glimpse()
library (tidyverse)     # Load the full tidyverse
library (magrittr)      # Pipes %>%, %<>%, %T>%, %$%
library (readr)         # Efficient reading of CSV data
library (tibble)        # Convert row names into a column
library (rvest)         # Scraping
library (jsonlite)      # JSON file reader
library (RCurl)         # Scraping
library (httr)          # Reading html links
library (tictoc)        # Running time

#==========================================

### PBS Online contains the Pharmaceutical Benefits Scheme (PBS), a listing of the pharmaceuticals subsidised by the Australian Government.

## Gather all index links on PBS
url_pbs <- "https://www.pbs.gov.au/browse/medicine-listing?initial="
pages_pbs <- paste0(url_pbs, letters)

## Get all links from each index
all_pbs <- find_all_links(pages_pbs)

## Get all sub links within one drug link
sub_pbs <- find_all_sublinks(all_pbs) %>%
  paste('https://www.pbs.gov.au',.,sep='') 

#==========================================

## Build data frame from each sub link
res = vector(mode="list", length = length(sub_pbs))
# If scraping breaks, rerun from here to pursue scraping to its last successful scrape.
# using code for 7 nodes
trial <- build_df(sub_pbs, res) 
# save res for 7 nodes
stock <- res

idx = as.numeric(lapply(res, is.null))
result = do.call(rbind, res[!idx])


res = vector(mode="list", length = length(sub_pbs))
# If scraping breaks, rerun from here to pursue scraping to its last successful scrape.
# using code for 6 nodes
trial2 <- build_df(sub_pbs[idx == 1], res)

# save res for 6 nodes
stock2 <- res

idx2 = as.numeric(lapply(res, is.null))
result2 = do.call(rbind, res[!idx2])
write_csv(result2[,names(result2) != "to_drop"], "scrape2.csv")


## Join the two tables (result and results2)
pbs_df <- result2 %>%
  rename(MaxUnits = MaxAmount) %>% # match column names
  rename(DPMQ = DPMA) %>%
  select(DrugItem, DrugName, Source, BodyLevel1, BodyLevel2, BodyLevel3, ProductForm, MaxUnits, NRepeat, DPMQ, MaxSafeNet, PatientCharge) %>%
  full_join(result) %>%
  select(DrugItem, DrugName, Source, BodyLevel1, BodyLevel2, BodyLevel3, ProductForm, MaxQtyPacks, MaxUnits, NRepeat, DPMQ, MaxSafeNet, PatientCharge) 

## Saving files
# write.csv(pbs_df, file="../output/pbs.csv")
# saveRDS(result, file = "../output/result1_pbs.RDS")
# saveRDS(result2, file = "../output/result2_pbs.RDS")
# saveRDS(pbs_df, file = "../output/pbs.RDS")