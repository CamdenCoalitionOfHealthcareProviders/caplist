# # Load libraries
# library(stringr)
# library(dplyr)
# library(readr)

# Sets working directory, reads file and creates a nickname
# setwd("Z:/Cap List/")
# wd <- getwd()
# date <- format(Sys.Date(), "%B%Y")
#
# # Reads in files in format below
# #epic <- read.csv(paste(wd,"/CooperAM_180223", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
# epic <- read_csv(paste0(wd,"/CooperAM_180223", ".csv"))
#
# # `Patient Name` needs to be split into MEMB_FIRST_NAME and MEMB_LAST_NAME
# # `Home Address` needs to be split into MEMB_ADDRESS_LINE_1, MEMB_CITY, MEMB_STATE
# epic2 <- epic %>%
#   mutate(`Patient Last Name` = str_split(`Patient Name`, ",") %>% sapply("[", 1),
#          `Patient First Name` = str_split(`Patient Name`, ",") %>% sapply("[", 2) %>% str_trim(),
#          `Patient Address` = str_split(`Patient's Address`, ",") %>% sapply("[", 1),
#          `Patient City` = str_split(`Patient's Address`, ",") %>% sapply("[", 2) %>% str_replace(" CAMDEN", "CAMDEN"),
#          `Patient State` = str_split(`Patient's Address`, ",") %>% sapply("[", 3),
#          `Patient Zip` = str_split(`Patient State`, " ") %>% sapply("[", 3))
#
# epic2$`Patient State` <- epic2$`Patient State` %>% str_trim() %>% str_replace_all('[[:space:]]{1}[[:digit:]]{5,}', "")
#
# # Dplyr: new_col = existing_col
# epic3 <- epic2 %>%
#   select(`Patient Last Name`,
#          `Patient First Name`,
#          `Patient MRN` = MRN,
#          `Patient DOB` = DOB,
#          `Patient Address`,
#          `Patient City`,
#          `Patient State`,
#          `Patient Zip`,
#          `Due Date` = `Due Date or DOB`)
#
# savedate <- (strftime(Sys.Date(),format="%Y-%m-%d"))
#
# write_csv(epic3, path=paste0(savedate,"-CooperAM.csv"), na="")
