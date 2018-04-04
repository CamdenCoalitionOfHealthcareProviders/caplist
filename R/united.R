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
  x <- rename(x, c(DATE_OF_BIRTH = "DOB"))
  x <- rename(x, c(MEMB_GENDER = "GENDER"))
  x <- rename(x, c(PROVIDER_ID = "CURR_PCP_ID"))
  x <- rename(x, c(PROV_PHONE = "PHONE_NUMBER"))
  x <- rename(x, c(PROV_ADDRESS_LINE_1 = "CURR_PCP_ADDRESS_LINE_1"))
  x <- rename(x, c(PROV_ADDRESS_LINE_2 = "CURR_PCP_ADDRESS_LINE_2"))
  x <- rename(x, c(PROV_CITY = "CURR_PCP_CITY"))
  x <- rename(x, c(PROV_STATE = "CURR_PCP_STATE"))
  x <- rename(x, c(PROV_ZIP = "CURR_PCP_ZIP"))
  x <- rename(x, c(PAYEE_NAME = "VEND_FULL_NAME"))

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

  # Subset Cooper Cherry Hill practices
  pattern_1050 <- c('1050', 'KING') # Cooper 1050 N Kings
  pattern_1103 <- c('1103', 'KING') # Cooper 1103 Kings
  pattern_1210 <- c('1210', 'BRACE') # Cooper Brace

  y <- subset(x, CURR_PCP_CITY=="CHERRY HILL")

  y2 <- subset(y, y$VEND_FULL_NAME == "COOPER FAMILY MEDICINE" |y$VEND_FULL_NAME == "COOPER PHYSICIANS OFFICES")

  y2$VEND_FULL_NAME[str_detect(y2$CURR_PCP_ADDRESS_LINE_1, pattern_1050)] <- "COOPER 1050 KINGS"
  y2$VEND_FULL_NAME[str_detect(y2$CURR_PCP_ADDRESS_LINE_1, pattern_1103)] <- "COOPER 1103 KINGS"
  y2$VEND_FULL_NAME[str_detect(y2$CURR_PCP_ADDRESS_LINE_1, pattern_1210)] <- "COOPER 1210 BRACE"

  y2$PRACTICE[y2$VEND_FULL_NAME == "COOPER 1050 KINGS"] <- "Cooper 1050 Kings"
  y2$PRACTICE[y2$VEND_FULL_NAME == "COOPER 1103 KINGS"] <- "Cooper 1103 Kings"
  y2$PRACTICE[y2$VEND_FULL_NAME == "COOPER 1210 BRACE"] <- "Cooper 1210 Brace"

  y2$PAYER <- "UNITED"
  y2$Source <- "United"

  y2 <- y2 %>% filter(is.na(PRACTICE) == F)

  # Keeps only where PCP City is Camden, Pennsauken, or Oaklyn, and keeps all of CamCare
  x <- subset(x, CURR_PCP_CITY=="CAMDEN" | CURR_PCP_CITY=="CADMEN" |CURR_PCP_CITY=="CANDEM" | CURR_PCP_CITY=="PENNSAUKEN"| CURR_PCP_CITY=="OAKLYN" | VEND_FULL_NAME=="CAMCARE HEALTH CORPORATION")

  # If the code to rename vendors gives you trouble, modify the below code to fix the errors
  x <- data.frame(lapply(x, as.character), stringsAsFactors=FALSE)

  # Adds Identification fields
  x$PAYER <- "UNITED"
  x$Source <- "United"

  # Sorts columns in x and y3 A-Z
  x <- x[,order(names(x))]
  y3 <- y2[,order(names(y2))]

  # Converts Current PCP City to all capital letters
  x$CURR_PCP_CITY <- toupper(x$CURR_PCP_CITY)

  # Renames vendors to match Current PCP City
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES" & x$CURR_PCP_CITY == "CAMDEN"] <- "LOURDES MEDICAL ASSOCIATES_CAMDEN"
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES PA" & x$CURR_PCP_CITY == "CAMDEN"] <- "LOURDES MEDICAL ASSOCIATES_CAMDEN"
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES" & x$CURR_PCP_CITY == "PENNSAUKEN"] <- "LOURDES MEDICAL ASSOCIATES_PENNSAUKEN"
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES PA" & x$CURR_PCP_CITY == "PENNSAUKEN"] <- "LOURDES MEDICAL ASSOCIATES_PENNSAUKEN"
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "OSBORN FAMILY PRACTICE" & x$CURR_PCP_CITY == "CAMDEN"] <- "OSBORN FAMILY PRACTICE_CAMDEN"
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "THE OSBORN FAMILY HEALTH CENTER" & x$CURR_PCP_CITY == "CAMDEN"] <- "OSBORN FAMILY PRACTICE_CAMDEN"
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "THE OSBORN FAMILY HEALTH CENTER INC" & x$CURR_PCP_CITY == "CAMDEN"] <- "OSBORN FAMILY PRACTICE_CAMDEN"
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "BROADWAY FAMILY PRACTICE" & x$CURR_PCP_CITY == "CAMDEN"] <- "RELIANCE BROADWAY_CAMDEN"
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "BROADWAY FAMILY PRACTICE" & x$CURR_PCP_CITY == "PENNSAUKEN"] <- "RELIANCE BROADWAY_PENNSAUKEN"
  x$VEND_FULL_NAME[x$VEND_FULL_NAME == "COOPER PHYSICIANS OFFICES" & x$CURR_PCP_CITY == "OAKLYN"] <- "COOPER WOODLYNNE"

  # Maps to practices
  x$PRACTICE[x$VEND_FULL_NAME == "ACOSTA RAMON"] <- "Acosta"
  x$PRACTICE[x$VEND_FULL_NAME == "RELIANCE BROADWAY_CAMDEN"] <- "Reliance Broadway"
  x$PRACTICE[x$VEND_FULL_NAME == "NEW JERSEY MEDICAL AND HEALTH ASSOCIATES LLC"] <- "Reliance Broadway"
  x$PRACTICE[x$VEND_FULL_NAME == "RELIANCE MEDICAL GROUP"] <- "Reliance Broadway"
  x$PRACTICE[x$VEND_FULL_NAME == "RELIANCE MEDICAL GROUP LLC"] <- "Reliance Broadway"
  x$PRACTICE[x$VEND_FULL_NAME == "RELIANCE BROADWAY_PENNSAUKEN"] <- "Reliance Pennsauken"
  x$PRACTICE[x$VEND_FULL_NAME == "CAMCARE HEALTH CORPORATION"] <- "CAMcare"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER AMBULATORY PEDIATRICS"] <- "Cooper Pediatrics"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER FAMILY MEDICINE"] <- "Cooper Family"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER FAMILY MEDICINE PC"] <- "Cooper Family"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER PEDIATRICS"] <- "Cooper Pediatrics"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER HEALTH SYSTEM PEDIATRICS DEPARTMENT"] <- "Cooper Pediatrics"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER HEALTH SYSTEM - PEDIATRICS DEPARTMENT "] <- "Cooper Pediatrics"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER HEALTH SYSTEM  PEDIATRICS DEPARTMENT"] <- "Cooper Pediatrics"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER HEALTH SYSTEM - PEDIATRICS DEPARTMENT"] <- "Cooper Pediatrics"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER PHYSICIANS OFFICES"] <- "Cooper IM"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER PHYSICIAN OFFICES PA"] <- "Cooper IM"
  x$PRACTICE[x$VEND_FULL_NAME == "Cooper_UHI_Nic"] <- "Cooper IM"
  x$PRACTICE[x$VEND_FULL_NAME == "JEFFREY A KLEEMAN DO"] <- "Fairview"
  x$PRACTICE[x$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES_CAMDEN"] <- "Osborn"
  x$PRACTICE[x$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES_PENNSAUKEN"] <- "Lourdes Pediatrics"
  x$PRACTICE[x$VEND_FULL_NAME == "OSBORN FAMILY PRACTICE_CAMDEN"] <- "Osborn"
  x$PRACTICE[x$VEND_FULL_NAME == "PROJECT HOPE"] <- "Project Hope"
  x$PRACTICE[x$VEND_FULL_NAME == "PROJECT HOPE HOMELESS PROGRAM"] <- "Project Hope"
  x$PRACTICE[x$VEND_FULL_NAME == "RIVER PRIMARY CARE CENTER"] <- "Reliance River"
  x$PRACTICE[x$VEND_FULL_NAME == "ST LUKE'S CATHOLIC MED SVCS"] <- "St. Lukes"
  x$PRACTICE[x$VEND_FULL_NAME == "ST LUKES CATHOLIC MEDICAL SERVICES INC"] <- "St. Lukes"
  x$PRACTICE[x$VEND_FULL_NAME == "ST LUKE'S CATHOLIC MED  SVCS"] <- "St. Lukes"
  x$PRACTICE[x$VEND_FULL_NAME == "ST LUKE’S CATHOLIC MEDICAL SERVICES INC"] <- "St. Lukes"
  x$PRACTICE[x$VEND_FULL_NAME == "ST LUKE'S CATHOLIC MEDICAL SERVICES INC"] <- "St. Lukes"
  x$PRACTICE[x$VEND_FULL_NAME == "VIRTUA FAMILY MEDICINE-COOPER RIVER"] <- "Virtua"
  x$PRACTICE[x$VEND_FULL_NAME == "VIRTUA MEDICAL GROUP"] <- "Virtua"
  x$PRACTICE[x$VEND_FULL_NAME == "VIRTUA MEDICAL GROUP PA"] <- "Virtua"
  x$PRACTICE[x$VEND_FULL_NAME == "NELSON HOMER L"] <- "Broadway Community"
  x$PRACTICE[x$VEND_FULL_NAME == "NELSON, HOMER"] <- "Broadway Community"
  x$PRACTICE[x$VEND_FULL_NAME == "EAST CAMDEN MEDICAL PRACTICE"] <- "East Camden"
  x$PRACTICE[x$VEND_FULL_NAME == "NGUYEN BAO D"] <- "Bao Nguyen"
  x$PRACTICE[x$VEND_FULL_NAME == "BROADWAY COMM HLTH CARE"] <- "Broadway Community"
  x$PRACTICE[x$VEND_FULL_NAME == "COOPER WOODLYNNE"] <- "Cooper Woodlynne"

  # Date of birth cleaning before merging with Cherry Hill patients
  x$DOB <- as.Date(x$DOB)

  # Merges x data
  AllPayers <- rbind(x, y3)

  # Sets as dataframe
  AllPayers <- as.data.frame(AllPayers)

  # Adds last capitation
  AllPayers$LastCapitationDate <- format(Sys.time(), "%m/01/%Y")

  # Remove "U" from string to match TrackVia Subscriber IDs
  AllPayers$SUBSCRIBER_ID <- gsub("U", "", AllPayers$SUBSCRIBER_ID)

  print(AllPayers)
}


# Function to check for missing data in vector
missing_practice <- function(x) {
  if (is.na(x) == TRUE) stop ('Dataframe includes missing practices. Fix these before sending to CareEvolution')
}

# missing_practice(AllPayers$PRACTICE)
