# Load libraries
library(stringr)
library(dplyr)
library(readr)

# Sets working directory, reads file and creates a nickname
setwd("Z:/Cap List/")
wd <- getwd()
date <- format(Sys.Date(), "%B%Y")

# Reads in files in format below
#epic <- read.csv(paste(wd,"/CooperAM_180223", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
epic <- read_csv(paste0(wd,"/CooperAM_180223", ".csv"))

# `Patient Name` needs to be split into MEMB_FIRST_NAME and MEMB_LAST_NAME
# `Home Address` needs to be split into MEMB_ADDRESS_LINE_1, MEMB_CITY, MEMB_STATE
epic2 <- epic %>%
mutate(MEMB_LAST_NAME = str_split(`Patient Name`, ",") %>% sapply("[", 1),
         MEMB_FIRST_NAME = str_split(`Patient Name`, ",") %>% sapply("[", 2),
         MEMB_ADDRESS_LINE_1 = str_split(`Patient Address`, ",") %>% sapply("[", 1),
         MEMB_CITY = str_split(`Patient Address`, ",") %>% sapply("[", 2) %>% str_replace(" CAMDEN", "CAMDEN"),
         MEMB_STATE = str_split(`Patient Address`, ",") %>% sapply("[", 3), 
         MEMB_ZIP = str_split(MEMB_STATE, " ") %>% sapply("[", 3))

epic2$MEMB_STATE <- epic2$MEMB_STATE %>% str_trim() %>% str_replace_all('[[:space:]]{1}[[:digit:]]{5,}', "") 

# Dplyr: new_col = existing_col
epic3 <- epic2 %>% select(MEMB_FIRST_NAME, MEMB_LAST_NAME, MEMB_MRN = MRN, MEMB_DOB = DOB, MEMB_ADDRESS_LINE_1, MEMB_CITY, MEMB_STATE, MEMB_ZIP, 
            MEMB_INSURANCE = Insurance)

savedate <- (strftime(Sys.Date(),format="%Y-%m-%d"))

write_csv(epic3, path=paste0(savedate,"-CooperAM.csv"), na="")
