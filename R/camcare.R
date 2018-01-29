#' CAMcare Function: clean CAMcare patient list
#'
#' @return Tibble
#' @export
#'
#' @examples
#' camcare(x)
#' y <- camcare(x) %>% write_csv(y, "y.csv", na = "")

camcare <- function(x) {
  # Concatenates Provider information
  x$CURR_PCP_FULL_NAME  <-  paste(x$PROV_LNAME, x$PROV_FNAME, sep=", ")

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

  # Renames fields in the camcare
  # Dplyr: new_col = existing_col
  x <- reshape::rename(x, c(DATE_OF_BIRTH="DOB"))
  x <- reshape::rename(x, c(MEMB_GENDER="GENDER"))
  x <- reshape::rename(x, c(PROVIDER_ID="CURR_PCP_ID"))
  x <- reshape::rename(x, c(PROV_PHONE="PHONE_NUMBER"))
  x <- reshape::rename(x, c(PROV_ADDRESS_LINE_1="CURR_PCP_ADDRESS_LINE_1"))
  x <- reshape::rename(x, c(PROV_ADDRESS_LINE_2="CURR_PCP_ADDRESS_LINE_2"))
  x <- reshape::rename(x, c(PROV_CITY="CURR_PCP_CITY"))
  x <- reshape::rename(x, c(PROV_STATE="CURR_PCP_STATE"))
  x <- reshape::rename(x, c(PROV_ZIP="CURR_PCP_ZIP"))
  x <- reshape::rename(x, c(PAYEE_NAME="VEND_FULL_NAME"))

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

  # Removes entries with the wrong vendor names
  x  <-  subset(x, !(VEND_FULL_NAME=="CHILD REGIONAL/CAMDEN"))

  # Keeps only where PCP City is Camden or Pennsauken, and keeps all of CamCare
  x <- subset(x, CURR_PCP_CITY=="CAMDEN" | CURR_PCP_CITY=="CADMEN" |CURR_PCP_CITY=="CANDEM" |CURR_PCP_CITY=="PENNSAUKEN" | VEND_FULL_NAME=="CAMCARE HEALTH CORPORATION")

  # If the code to rename vendors gives you trouble, modify the below code to fix the errors#
  x  <-  data.frame(lapply(x, as.character), stringsAsFactors=FALSE)

  # Adds Identification fields
  x$PAYER <- "CAMCare"
  x$Source <- "United"

  # Adds in fields to match United, Horizon, and Cooper UHI AllPayers file
  x$SOCIAL_SEC_NO <- ""
  x$PRACTICE <- ""
  x$HIEID <- ""

  x$CURR_PCP_CITY <- toupper(x$CURR_PCP_CITY)

  # Maps to practice
  x$PRACTICE[x$VEND_FULL_NAME =="CAMCARE HEALTH CORPORATION"] <- "CAMCare"

  # Removes fields that don't need to go to CareEvolution from the CareEvolution version of the file
  x$MEMB_LANGUAGE <- NULL
  x$MEMB_ETHNICITY <- NULL

  # Adds last capitation
  x$LastCapitationDate <-  format(Sys.time(), "%m/01/%Y")

  # Sorts columns A-Z
  x <- x[,order(names(x))]

  print(x)
}
