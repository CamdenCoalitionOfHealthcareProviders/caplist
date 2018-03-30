#' Galileo Function: clean Galileo capitation list
#'
#' @return Tibble
#' @export
#'
#' @examples
#' galileo(x)
#' y <- galileo(x) %>% write_csv(y, "y.csv", na = "")

galileo <-
  function(x) {
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
      mutate(HOME_PHONE_NUMBER = str_replace_all(`Phone Number`, "-|\\(|\\)|None| ", ""))
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
    ## First, make `Selected Office Name` lower case
    x$`Selected Office Name` <- tolower(x$`Selected Office Name`)

    ## Second, standardize practice names
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
        str_detect(x$`Selected Office Name`, "east camden") == TRUE ~ "East Camden",
        str_detect(x$`Selected Office Name`, "fairview") == TRUE ~ "Fairview",
        str_detect(x$`Selected Office Name`, "kleeman") == TRUE ~ "Fairview",
        str_detect(x$`Selected Office Name`, "franklin") == TRUE ~ "Franklin Scarlett",
        str_detect(x$`Selected Office Name`, "lourdes ped") == TRUE ~ "Lourdes Pediatrics",
        str_detect(x$`Selected Office Name`, "lourdes med") == TRUE ~ "Osborn",
        str_detect(x$`Selected Office Name`, "nguyen") == TRUE ~ "Bao Nguyen",
        str_detect(x$`Selected Office Name`, "osborn") == TRUE ~ "Osborn",
        str_detect(x$`Selected Office Name`, "project") == TRUE ~ "Project Hope",
        str_detect(x$`Selected Office Name`, "reliance") == TRUE ~ "Reliance Broadway",
        str_detect(x$`Selected Office Name`, "uhi") == TRUE ~ "Cooper IM",
        str_detect(x$`Selected Office Name`, "unassigned") == TRUE ~ "Unassigned",
        str_detect(x$`Selected Office Name`, "virtua") == TRUE ~ "Virtua"
      ))

    # Source column: Source of the patient list for each patient
    # If PAYER is equal to "None", then the Source of Data should be "Medicaid"
    x <- x %>% mutate(
      Source = case_when(
        str_detect(x$Source, "United") == TRUE ~ "United",
        str_detect(x$Source, "None") == TRUE ~ "Medicaid",
        str_detect(x$Source, "NGII") == TRUE ~ "NGII",
        str_detect(x$Source, "CooperUHI") == TRUE ~ "UHI_Nic"
      )
    )

    # If PAYER is equal to "None" (that is, a patient is not on MCO capitation list)
    # the value of PAYER should be replaced with the value of gal_health_plan
    x$payer_clean <- ifelse(x$Payer == 'None', x$gal_health_plan, x$Payer)

    # MonthlyBulkImport
    x$MonthlyBulkImport <- "Monthly Import"
    x$LastCapitationDate <- format(Sys.time(), "%m/01/%Y")

    # `Date of Birth`: remove ' 12:00:00 AM' from field
    x$`Date of Birth` <- x$`Date of Birth` %>% str_replace_all(" 12:00:00 AM", "")
    x$`Attribution Begin Date` <- x$`Attribution Begin Date` %>% str_replace_all(" 12:00:00 AM", "")
    x$`Attribution End Date` <- x$`Attribution End Date` %>% str_replace_all(" 12:00:00 AM", "")

    # Add columns until they're added into Galileo report
    x$MEDICARE_NO <- ""
    x$SOCIAL_SEC_NO <- ""

    # Make Camden ID lower case
    x$`Patient ID HIE` <- tolower(x$`Camden ID`)

    x$MCO_Subscriber_ID <- ifelse(x$`Subscriber ID` == 'None', "", x$`Subscriber ID`)

    # Select Galileo columns, and rename when necessary, to match TrackVia Import file
    # Dplyr: new_col = existing_col
    x <- select(x,
                 CURR_PCP_FULL_NAME,
                 DOB = `Date of Birth`,
                 `Galileo Attributed Practice`,
                 Gender,
                 HOME_PHONE_NUMBER,
                 LastCapitationDate,
                 MCO_Subscriber_ID,
                 Medicaid_Claims_ID = MedicaidID,
                 MEDICARE_NO,
                 MEMB_ADDRESS_LINE_1,
                 MEMB_CITY,
                 MEMB_FIRST_NAME,
                 MEMB_LAST_NAME,
                 MEMB_STATE,
                 MEMB_ZIP = `Zip Code`,
                 MonthlyBulkImport,
                 `Patient ID HIE`,
                 EMPI = `Person ID`,
                 Payer = payer_clean,
                 SOCIAL_SEC_NO,
                 Source
                )

    print(x)
  }
