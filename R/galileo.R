# Clean Galileo capitation list

#' Title
#'
#' @return
#' @export
#'
#' @examples
# Need to read in the 'Capitation List' report in Galileo

    # Column names to convert the Galileo capitation list to:
    # BUS_PHONE_NUMBER = col_character(),
    # CURR_PCP_ADDRESS_LINE_1 = col_character(),
    # CURR_PCP_ADDRESS_LINE_2 = col_character(),
    # CURR_PCP_CITY = col_character(),
    # CURR_PCP_FULL_NAME = col_character(),
    # CURR_PCP_ID = col_double(),
    # CURR_PCP_STATE = col_character(),
    # CURR_PCP_ZIP = col_integer(),
    # DOB = col_character(),
    # GENDER = col_character(),
    # HIEID = col_character(),
    # HOME_PHONE_NUMBER = col_double(),
    # IRS_TAX_ID = col_integer(),
    # LastCapitationDate = col_character(),
    # MEDICAID_NO = col_double(),
    # MEDICARE_NO = col_double(),
    # MEMB_ADDRESS_LINE_1 = col_character(),
    # MEMB_ADDRESS_LINE_2 = col_character(),
    # MEMB_CITY = col_character(),
    # MEMB_FIRST_NAME = col_character(),
    # MEMB_LAST_NAME = col_character(),
    # MEMB_STATE = col_character(),
    # MEMB_ZIP = col_integer(),
    # PAYER = col_character(),
    # PHONE_NUMBER = col_double(),
    # PRACTICE = col_character(),
    # SOCIAL_SEC_NO = col_character(),
    # Source = col_character(),
    # SUBSCRIBER_ID = col_character(),
    # VEND_FULL_NAME = col_character(),
    # VENDOR_ID = col_character(),
    # MonthlyBulkImport = col_character()

    # Column names in Galileo capitation list table
    # `Camden ID` = col_character(),
    # MedicaidID = col_character(),
    # `Pt Name` = col_character(),
    # `Home Address` = col_character(),
    # `Zip Code` = col_character(),
    # Gender = col_character(),
    # `Date of Birth` = col_character(),
    # `Attribution Begin Date` = col_character(),
    # `Attribution End Date` = col_character(),
    # `Provider Name` = col_character(),
    # `Selected Office Name` = col_character(),
    # `Health Plan` = col_character(),
    # Payer = col_character(),
    # `Subscriber ID` = col_character(),
    # `Phone Number` = col_character()

galileo <-
  function(x) {
    # Clean Galileo capitation list


    # Changes to individual fields
    # `Pt Name` needs to be split into MEMB_FIRST_NAME and MEMB_LAST_NAME
    x <-  x %>%
      mutate(MEMB_LAST_NAME = str_split(`Pt Name`, ", ") %>% sapply("[", 1),
             MEMB_FIRST_NAME = str_split(x$`Pt Name`, ", ") %>% sapply("[", 2))

    # `Home Address` needs to be split into MEMB_ADDRESS_LINE_1, MEMB_CITY, MEMB_STATE
    x <- x %>%
      mutate(MEMB_ADDRESS_LINE_1 = str_split(`Home Address`, ",") %>% sapply("[", 1),
             MEMB_CITY = str_split(`Home Address`, ",") %>% sapply("[", 2) %>% str_replace(" CAMDEN", "CAMDEN"),
             MEMB_STATE = str_split(`Home Address`, ",") %>% sapply("[", 3))

    # `Provider Name`: Remove extra spaces and asterisks
    # Remove extra spaces and asterisks, and space + commas
    x <- x %>%
      mutate(CURR_PCP_FULL_NAME = str_replace_all(`Provider Name`, '[[:space:]]{2,16}\\*', "")
             %>% str_replace_all(' ,', ", "))

    # `Phone Number`: Remove hyphens, parentheses, and 'None' for HOME_PHONE_NUMBER
    x <- x %>%
      mutate(HOME_PHONE_NUMBER = str_replace_all(`Phone Number`, "-|\\(|\\)|None| ", "") )
    x$HOME_PHONE_NUMBER <- substring(x$HOME_PHONE_NUMBER, 1, 10)

    # `Health Plan`: Clean and merge with Payer, with existing Payer values getting preference

    x <- x %>% mutate(
      gal_health_plan = case_when(
        `Health Plan` == "D-SNP: HORIZON MEDICARE BLUE TOTALCARE" ~ "HORIZON DUAL",
        `Health Plan` == "D-SNP: UNITEDHEALTHCARE DUAL COMPLETE" ~ "UNITED DUAL",
        `Health Plan` == "HMO: AETNA" ~ "AETNA",
        `Health Plan` == "HMO: AMERIGROUP NEW JERSEY, INC." ~ "AMERIGROUP",
        `Health Plan` == "HMO: HORIZON NJ HEALTH" ~ "HORIZON",
        `Health Plan` == "HMO: UNITEDHEALTHCARE" ~ "UNITED",
        `Health Plan` == "HMO: WELLCARE NJ" ~ "WELLCARE",
        `Health Plan` == "Not Assigned" ~ "Not Assigned",
        `Health Plan` == "PACE: LIFE AT LOURDES" ~ "PACE LIFE AT LOURDES"
      ))


    # `Selected Office Name`: Rename `Galileo Attributed Practice``
    # First, make `Selected Office Name` lower case
    x$`Selected Office Name` <- tolower(x$`Selected Office Name`)

    # Galileo:
    x <- x %>% mutate(
      `Galileo Attributed Practice` = case_when(
        str_detect(x$`Selected Office Name`, "acosta") == TRUE ~ "Acosta",
        str_detect(x$`Selected Office Name`, "advocare") == TRUE ~ "Advocare",
        str_detect(x$`Selected Office Name`, "camcare") == TRUE ~ "CAMcare",
        str_detect(x$`Selected Office Name`, "broadway comm") == TRUE ~ "Broadway Community",
        str_detect(x$`Selected Office Name`, "cooper fam") == TRUE ~ "Cooper Family",
        str_detect(x$`Selected Office Name`, "cooper im") == TRUE ~ "Cooper IM",
        str_detect(x$`Selected Office Name`, "cooper phys") == TRUE ~ "Cooper IM",
        str_detect(x$`Selected Office Name`, "cooper ped") == TRUE ~ "Cooper Pediatrics",
        str_detect(x$`Selected Office Name`, "cooper woodly") == TRUE ~ "Cooper Woodlyne",
        str_detect(x$`Selected Office Name`, "fairview") == TRUE ~ "Fairview",
        str_detect(x$`Selected Office Name`, "kleeman") == TRUE ~ "Fairview",
        str_detect(x$`Selected Office Name`, "franklin") == TRUE ~ "Franklin Scarlett",
        str_detect(x$`Selected Office Name`, "lourdes ped") == TRUE ~ "Lourdes Pediatrics",
        str_detect(x$`Selected Office Name`, "lourdes med") == TRUE ~ "Osborn",
        str_detect(x$`Selected Office Name`, "osborn") == TRUE ~ "Osborn",
        str_detect(x$`Selected Office Name`, "project") == TRUE ~ "Project Hope",
        str_detect(x$`Selected Office Name`, "reliance") == TRUE ~ "Reliance Broadway",
        str_detect(x$`Selected Office Name`, "uhi") == TRUE ~ "Cooper UHI",
        str_detect(x$`Selected Office Name`, "unassigned") == TRUE ~ "Unassigned",
        str_detect(x$`Selected Office Name`, "virtua") == TRUE ~ "Virtua"
      ))

    # Source column: Source of the data
    # If PAYER is equal to "None", then the Source of Data should be "Medicaid"
    # Need this column to be available in Galileo first
    # ifelse(x$Payer == 'None', x$Source == "Medicaid",
    #        )


    # If PAYER is equal to "None" (that is, a patient is not on MCO capitation list)
    # the value of PAYER should be replaced with the value of gal_health_plan
    x$payer_test <- ifelse(x$Payer == 'None', x$gal_health_plan, x$Payer)

    # MonthlyBulkImport
    x$MonthlyBulkImport <- "Monthly Import"
    x$LastCapitationDate <- format(Sys.time(), "%m/01/%Y")

    # `Date of Birth`: remove ' 12:00:00 AM' from field
    x$`Date of Birth` <- x$`Date of Birth` %>% str_replace_all(" 12:00:00 AM", "")
    x$`Attribution Begin Date` <- x$`Attribution Begin Date` %>% str_replace_all(" 12:00:00 AM", "")
    x$`Attribution End Date` <- x$`Attribution End Date` %>% str_replace_all(" 12:00:00 AM", "")

    # Add columns until they're added into Galileo file
    x$SOCIAL_SEC_NO <- ""
    x$MEDICARE_NO <- ""
    x$Source <- ""


    # Select Galileo columns, and rename when necessary, to match TrackVia Import file
    # Dplyr: new_col = existing_col
    x_export <- select(x,
                         CURR_PCP_FULL_NAME,
                         DOB = `Date of Birth`,
                         `Galileo Attributed Practice`,
                         Gender,
                         HOME_PHONE_NUMBER,
                         LastCapitationDate,
                         MCO_Subscriber_ID = `Subscriber ID`,
                         MEDICAID_NO = MedicaidID,
                         MEDICARE_NO,
                         MEMB_ADDRESS_LINE_1,
                         MEMB_CITY,
                         MEMB_FIRST_NAME,
                         MEMB_LAST_NAME,
                         MEMB_STATE,
                         MEMB_ZIP = `Zip Code`,
                         MonthlyBulkImport,
                         `Patient ID HIE` = `Camden ID`,
                         Payer = payer_test,
                         SOCIAL_SEC_NO,
                         Source
                         #  = `Health Plan`,
                         #  = `Attribution Begin Date`,
                         #  = `Attribution End Date`,
                      )
    print(x_export)
  }
