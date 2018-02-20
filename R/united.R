#' United Function: clean United patient list
#'
#' @return Tibble
#' @export
#'
#' @examples
#' united(x)
#' y <- united(x) %>% write_csv(y, "y.csv", na = "")

united <- function(x) {
  # Concatenates Provider information
  x$CURR_PCP_FULL_NAME <- paste(x$PROV_LNAME, x$PROV_FNAME, sep=", ")

  # Removes unused fields
  x$AGE <- NULL
  x$PANEL_ID <- NULL
  x$PLAN_DESC <- NULL
  x$TERM_DATE <- NULL
  x$PLAN_CODE <- NULL
  x$PROV_FNAME <- NULL
  x$PROV_LNAME <- NULL
  x$PROV_LANG_1 <- NULL
  x$PROV_LANG_2 <- NULL
  x$PROV_LANG_3 <- NULL
  x$PROV_EFF_DATE <- NULL
  x$EFFECTIVE_DATE <- NULL
  x$PROV_TERM_DATE <- NULL
  x$COSMOS_CUST_SEG <- NULL
  x$LINE_OF_BUSINESS <- NULL
  x$COSMOS_CUST_SEG_DESC <- NULL

  # Renames fields in the United cap list
  x <- rename(x, DOB = DATE_OF_BIRTH)
  x <- rename(x, GENDER = MEMB_GENDER)
  x <- rename(x, CURR_PCP_ID = PROVIDER_ID)
  x <- rename(x, PHONE_NUMBER = PROV_PHONE)
  x <- rename(x, CURR_PCP_ADDRESS_LINE_1 = PROV_ADDRESS_LINE_1)
  x <- rename(x, CURR_PCP_ADDRESS_LINE_2 = PROV_ADDRESS_LINE_2)
  x <- rename(x, CURR_PCP_CITY = PROV_CITY)
  x <- rename(x, CURR_PCP_STATE = PROV_STATE)
  x <- rename(x, CURR_PCP_ZIP = PROV_ZIP)
  x <- rename(x, VEND_FULL_NAME = PAYEE_NAME)

  # Maps languages in the united file to the full name of the language
  x$MEMB_LANGUAGE <- as.character(x$MEMB_LANGUAGE)
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="ARA"] <- "Arabic"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="CHI"] <- "Chinese"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="ENG"] <- "English"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="FRE"] <- "French"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="HEB"] <- "Hebrew"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="ITA"] <- "Italian"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="KOR"] <- "Korean"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="N/A"] <- ""
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="PER"] <- "Persian"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="POR"] <- "Portuegese"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="SPA"] <- "Spanish"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="TUR"] <- "Turkish"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="UNK"] <- "Unknown"
  x$MEMB_LANGUAGE[x$MEMB_LANGUAGE=="VIE"] <- "Vietnamese"

  # Adds text identifiers to Subscriber IDs
  x$SUBSCRIBER_ID <- paste("U", x$SUBSCRIBER_ID, sep="")

  # Sets the MEDICAID_NO field as numeric to get rid of scientific notation
  options(scipen=999)
  x$MEDICAID_NO <- as.numeric(as.character(x$MEDICAID_NO))

  # Cleans the home phone number field of parentheses, spaces and dashes
  x$HOME_PHONE_NUMBER <- gsub("\\(|\\)|\\-|\\ ", "", x$HOME_PHONE_NUMBER)

  # Cleans zip codes
  x$MEMB_ZIP <- clean.zipcodes(x$MEMB_ZIP)
  x$CURR_PCP_ZIP <- clean.zipcodes(x$MEMB_ZIP)

  # Cleans birth dates
  x$DOB <- as.Date(x$DOB, "%m/%d/%Y")

  # Deletes entries with the wrong vendor names
  x <- subset(x, !(VEND_FULL_NAME=="CHILD REGIONAL/CAMDEN"))

  # Keeps only where PCP City is Camden or Pennsauken, and keeps all of CamCare
  x <- subset(x, CURR_PCP_CITY=="CAMDEN" | CURR_PCP_CITY=="CADMEN" |CURR_PCP_CITY=="CANDEM" |CURR_PCP_CITY=="PENNSAUKEN" | VEND_FULL_NAME=="CAMCARE HEALTH CORPORATION")

  # If the code to rename vendors gives you trouble, modify the below code to fix the errors#
  x <- data.frame(lapply(x, as.character), stringsAsFactors=FALSE)

  # Adds Identification fields
  x$PAYER <- "UNITED"
  x$Source <- "United"

  # Sorts columns in x A-Z
  x <- x[,order(names(x))]

  # Merges x data
  AllPayers <- rbind(x)

  # Converts Current PCP City to all capital letters
  AllPayers$CURR_PCP_CITY <- toupper(AllPayers$CURR_PCP_CITY)

  # Renames vendors to match Current PCP City
  AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "LOURDES MEDICAL ASSOCIATES_CAMDEN"
  AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES PA" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "LOURDES MEDICAL ASSOCIATES_CAMDEN"
  AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES" & AllPayers$CURR_PCP_CITY == "PENNSAUKEN"] <- "LOURDES MEDICAL ASSOCIATES_PENNSAUKEN"
  AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES PA" & AllPayers$CURR_PCP_CITY == "PENNSAUKEN"] <- "LOURDES MEDICAL ASSOCIATES_PENNSAUKEN"
  AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "OSBORN FAMILY PRACTICE" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "OSBORN FAMILY PRACTICE_CAMDEN"
  AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "THE OSBORN FAMILY HEALTH CENTER" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "OSBORN FAMILY PRACTICE_CAMDEN"
  AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "THE OSBORN FAMILY HEALTH CENTER INC" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "OSBORN FAMILY PRACTICE_CAMDEN"
  AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "BROADWAY FAMILY PRACTICE" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "RELIANCE BROADWAY_CAMDEN"
  AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "BROADWAY FAMILY PRACTICE" & AllPayers$CURR_PCP_CITY == "PENNSAUKEN"] <- "RELIANCE BROADWAY_PENNSAUKEN"

  # Maps to practices
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ACOSTA RAMON"] <- "Acosta"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "RELIANCE BROADWAY_CAMDEN"] <- "Reliance Broadway"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "NEW JERSEY MEDICAL AND HEALTH ASSOCIATES LLC"] <- "Reliance Broadway"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "RELIANCE MEDICAL GROUP"] <- "Reliance Broadway"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "RELIANCE MEDICAL GROUP LLC"] <- "Reliance Broadway"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "RELIANCE BROADWAY_PENNSAUKEN"] <- "Reliance Pennsauken"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "CAMCARE HEALTH CORPORATION"] <- "CAMcare"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER AMBULATORY PEDIATRICS"] <- "Cooper Pediatrics"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER FAMILY MEDICINE"] <- "Cooper Family"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER FAMILY MEDICINE PC"] <- "Cooper Family"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER PEDIATRICS"] <- "Cooper Pediatrics"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER HEALTH SYSTEM PEDIATRICS DEPARTMENT"] <- "Cooper Pediatrics"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER HEALTH SYSTEM - PEDIATRICS DEPARTMENT "] <- "Cooper Pediatrics"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER HEALTH SYSTEM  PEDIATRICS DEPARTMENT"] <- "Cooper Pediatrics"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER HEALTH SYSTEM - PEDIATRICS DEPARTMENT"] <- "Cooper Pediatrics"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER PHYSICIANS OFFICES"] <- "Cooper IM"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER PHYSICIAN OFFICES PA"] <- "Cooper IM"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "Cooper_UHI_Nic"] <- "Cooper IM"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "JEFFREY A KLEEMAN DO"] <- "Fairview"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES_CAMDEN"] <- "Osborn"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES_PENNSAUKEN"] <- "Lourdes Pediatrics"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "OSBORN FAMILY PRACTICE_CAMDEN"] <- "Osborn"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "PROJECT HOPE"] <- "Project Hope"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "PROJECT HOPE HOMELESS PROGRAM"] <- "Project Hope"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "RIVER PRIMARY CARE CENTER"] <- "Reliance River"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ST LUKE'S CATHOLIC MED SVCS"] <- "St. Lukes"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ST LUKES CATHOLIC MEDICAL SERVICES INC"] <- "St. Lukes"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ST LUKE'S CATHOLIC MED  SVCS"] <- "St. Lukes"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ST LUKE’S CATHOLIC MEDICAL SERVICES INC"] <- "St. Lukes"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ST LUKE'S CATHOLIC MEDICAL SERVICES INC"] <- "St. Lukes"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "VIRTUA FAMILY MEDICINE-COOPER RIVER"] <- "Virtua"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "VIRTUA MEDICAL GROUP"] <- "Virtua"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "VIRTUA MEDICAL GROUP PA"] <- "Virtua"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "NELSON HOMER L"] <- "Broadway Community"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "NELSON, HOMER"] <- "Broadway Community"
  AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ERVILUS PATRICK"] <- "Broadway Community"

  # Sets as dataframe
  AllPayers <- as.data.frame(AllPayers)

  # Removes fields that don't need to go to CareEvolution from the CareEvolution version of the file
  AllPayers$MEMB_LANGUAGE <- NULL
  AllPayers$MEMB_ETHNICITY <- NULL

  # Adds last capitation
  AllPayers$LastCapitationDate <- format(Sys.time(), "%m/01/%Y")

  # Remove "U" from string to match TrackVia Subscriber IDs
  AllPayers$SUBSCRIBER_ID <- gsub("U", "", AllPayers$SUBSCRIBER_ID)

  # Prints warning message if dataframe is missing practices

  # Creates data frame with records a practice
  # These need to be fixed before sending to Nick (!!!)

  print(AllPayers)
}


# Function to check for missing data in vector
missing_practice <- function(x) {
  if (is.na(x) == TRUE) stop ('Dataframe includes missing practices. Fix these before sending to CareEvolution')
}

# missing_practice(AllPayers$PRACTICE)
