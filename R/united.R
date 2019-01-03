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
  x <- x %>% rename(DOB = DATE_OF_BIRTH,
                    GENDER = MEMB_GENDER,
                    CURR_PCP_ID = PROVIDER_ID,
                    PHONE_NUMBER = PROV_PHONE,
                    CURR_PCP_ADDRESS_LINE_1 = PROV_ADDRESS_LINE_1,
                    CURR_PCP_ADDRESS_LINE_2 = PROV_ADDRESS_LINE_2,
                    CURR_PCP_CITY = PROV_CITY,
                    CURR_PCP_STATE = PROV_STATE,
                    CURR_PCP_ZIP = PROV_ZIP,
                    VEND_FULL_NAME = PAYEE_NAME)

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


  # If the code to rename vendors gives you trouble, modify the below code to fix the errors
  x <- data.frame(lapply(x, as.character), stringsAsFactors=FALSE)

  # Adds Identification fields
  x$PAYER <- "UNITED"
  x$Source <- "United"

  # Sorts columns in x and y3 A-Z
  x <- x[,order(names(x))]

  # Converts Current PCP City to all capital letters
  x$CURR_PCP_CITY <- toupper(x$CURR_PCP_CITY)

  # Create PRACTICE column
  x$PRACTICE <- NA

  # Establish addresses to search for
  pattern_acosta <- c('2 LEONARD') # Acosta
  pattern_bao <- c('2706 WESTFIELD') # Bao
  pattern_bwy_comm <- c('2809 RIVER', '442 S BROADWAY') # Broadway Comm
  pattern_cooper_1050 <- c('1050 KING', '1050 N KING') # Cooper 1050 N Kings
  pattern_cooper_1103 <- c('1103 KING', '1103 N KING') # Cooper 1103 Kings
  pattern_cooper_1210 <- c('1210 BRACE') # Cooper Brace
  pattern_cooper_eip <- c('3 COOPER PLZ STE 513', '3 COOPER PLZ RM 513') # Cooper EIP
  pattern_cooper_fam <- c('1865 HARRISON', '3156 RIVER', '639 COOPER') # Cooper Family
  pattern_cooper_im_1 <- c('1 COOPER', '2 COOPER') # Cooper IM
  pattern_cooper_im_2 <- c('3 COOPER PLZ STE 215', '3 COOPER PLZ RM 215', '3 COOPER PLAZA STE 215', '3 COOPER PLAZA SUITE 215', '3 COOPER PL STE 215', '3 COOPER PLAZA STE 220', '3 COOPER PLZ RM 220', '3 COOPER PLZ STE 220', '3 COOPER PLAZA STE 411', '3 COOPER PLZ RM 504', '3 COOPER PLZ RM 513')
  pattern_cooper_multi <- c('3 COOPER PLAZA STE 104', '3 COOPER PLZ STE 104', '3 COOPER PLZ RM 104', '3 COPPER PLAZA ST 104 CAMDEN') # Cooper Multispeciality Suite
  pattern_cooper_peds <- c('3 COOPER PLAZA STE 200', '3 COOPER PLZ RM 200', '3 COOPER PLZ STE 200', 'THREE COOPER PLAZA STE 200') # Cooper Pediatrics
  pattern_cooper_wood <- c('1105 1115 LINDEN ST', '1115 LINDEN ST', '2301 WOODLYNNE', '5101 N PARK') # Cooper Woodlyne
  pattern_east_camden <- c('309 MARLTON') # East Camden
  pattern_fairview <- c('YORKSHIP') # Fairview
  pattern_lourdes_peds <- c('2475 MCCLELLAN') # Lourdes Pediatrics
  pattern_osborn <- c('1600 HADDON', '1601 HADDON') # Osborn
  pattern_proj_hope <- c('519 WEST', '439 CLINTON') # Project Hope
  pattern_rel_bwy <- c('602 BROADWAY') # Reliance Broadway
  pattern_rel_penn <- c('6650 BROWNING') # Reliance Pennsauken

  # Use the string patterns (pattern_* variables) to search addresses and fill PRACTICE field
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, pattern_acosta)] <- "Acosta"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, pattern_bao)] <- "Bao Nguyen"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_bwy_comm, collapse = "|"))] <- "Broadway Community"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_cooper_1050, collapse = "|"))] <- "Cooper 1050 Kings"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_cooper_1103, collapse = "|"))] <- "Cooper 1103 Kings"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, pattern_cooper_1210)] <- "Cooper 1210 Brace"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_cooper_eip, collapse = "|"))] <- "Cooper EIP"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_cooper_fam, collapse = "|"))] <- "Cooper Family"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_cooper_im_1, collapse = "|"))] <- "Cooper IM"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_cooper_im_2, collapse = "|"))] <- "Cooper IM"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_cooper_multi, collapse = "|"))] <- "Cooper Multispecialty"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_cooper_peds, collapse = "|"))] <- "Cooper Pediatrics"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_cooper_wood, collapse = "|"))] <- "Cooper Woodlynne"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, pattern_east_camden)] <- "East Camden"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, pattern_fairview)] <- "Fairview"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, pattern_lourdes_peds)] <- "Lourdes Pediatrics"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_osborn, collapse = "|"))] <- "Osborn"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, paste(pattern_proj_hope, collapse = "|"))] <- "Project Hope"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, pattern_rel_bwy)] <- "Reliance Broadway"
  x$PRACTICE[str_detect(x$CURR_PCP_ADDRESS_LINE_1, pattern_rel_penn)] <- "Reliance Pennsauken"

  # Filter lists with only PRACTICE field filled out
  x <- x %>% filter(is.na(PRACTICE) == F)

  # Date of birth cleaning before merging with Cherry Hill patients
  x$DOB <- as.Date(x$DOB)

  # Sets as dataframe
  x <- as.data.frame(x)

  # Adds last capitation
  x$LastCapitationDate <- format(Sys.time(), "%m/01/%Y")

  # Remove "U" from string to match TrackVia Subscriber IDs
  x$SUBSCRIBER_ID <- gsub("U", "", x$SUBSCRIBER_ID)

  print(x)
}


# Function to check for missing data in vector
missing_practice <- function(x) {
  if (is.na(x) == TRUE) stop ('Dataframe includes missing practices. Fix these before sending to CareEvolution')
}

# missing_practice(AllPayers$PRACTICE)
