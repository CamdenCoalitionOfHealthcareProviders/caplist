



# Split into three functions:
# AllPayers
# Galileo
  # Needs File from Galileo + CurrentHIEIDs file
# Deduplicate
  # Deduplicate uses AllPayers and Galileo Functions
  # User will put in Allpayers and Galileo as parameters

# mco_allpayers:
# Cleans file from HIE vendor
# To be used to bind with result of galileo_allpayers function
mco_allpayers <-
  function(a) {
    # Rename and Lower AllPayers "HIEID" column
    a2 <- a %>% mutate(`Patient ID HIE`= tolower(a$HIEID))

    # Update a$PRACTICE == NA to "Broadway Community" (one-time only for February 2018)
    a2$PRACTICE[is.na(a2$PRACTICE)] <- "Broadway Community"

    # De-duplicate AllPayers list
    a2_hie_dupe <- a2 %>% group_by(`Patient ID HIE`) %>% mutate(hieid_count = n()) %>% filter(hieid_count > 1)
    twins <- a2_hie_dupe %>% left_join(hie, by = c("Patient ID HIE" = "Patient ID", "SUBSCRIBER_ID" = "Cap List Subscriber ID"))

    # Remove possible twins records from AllPayers
    a3 <- a2 %>% anti_join(twins, by = "Patient ID HIE")

    # Add EMPI to AllPayers and remove NIC patients from AllPayers
    a_hie_join <- a3 %>% left_join(hie, by = c("Patient ID HIE" = "Patient ID")) %>% filter(`Cap List Payer` != "CooperUHI")

    # Rename AllPayers Columns (Dplyr: new_col = existing_col)
    a_hie_join_update <- a_hie_join %>% mutate(
      MonthlyBulkImport = "Monthly Import",
      SSN_clean = case_when(
        SOCIAL_SEC_NO == "NULL" ~ "",
        SOCIAL_SEC_NO != "NULL" ~ SOCIAL_SEC_NO
      )
    ) %>%
      select(
        everything(),
        EMPI = `Person ID`,
        MCO_Subscriber_ID = SUBSCRIBER_ID,
        `Cap List Payer` = PAYER,
        -SOCIAL_SEC_NO,
        SOCIAL_SEC_NO = SSN_clean,
        -`Cap List Payer`,
        -HIEID,
        -`Cap List Subscriber ID`,
        -`Galileo Attributed Practice`
      )

    # Convert column from character to date: LastCapitationDate
    a_hie_join_update$LastCapitationDate <- as.Date(a_hie_join_update$LastCapitationDate, "%m/%d/%Y")

    # Order the columns for binding with Galileo data frame
    a_hie_join_bind <- a_hie_join_update[order(colnames(a_hie_join_update))]
  }

# galileo_allpayers:
# Cleans and updates the ACO capitation list
# Removes MCO contract records
# Adds missing HIE IDs
# To be used to bind with result of mco_allpayers function
galileo_allpayers <-
  function(g, h) {
    g_clean <- galileo(g)

    # Convert Date columns from character to date: DOB and LastCapitationDate
    g_clean$DOB <- as.Date(g_clean$DOB, "%m/%d/%Y")
    g_clean$LastCapitationDate <- as.Date(g_clean$LastCapitationDate, "%m/%d/%Y")

    # Galileo: Find HIE IDs where "Patient ID HIE" is "none"
    g_clean_hie_join <- g_clean %>% left_join(h, by = c("EMPI" = "Person ID"))

    # Remove United MCO patients from Galileo list
    # United MCO patients will be bound to Galileo list later
    g_clean_hie_join <- g_clean_hie_join %>% filter(is.na(`Cap List Payer`))


    # Update  for non-MCO patients (Source == "Medicaid") to "1/1/2018"
    # g_clean_hie_join$LastCapitationDate[g_clean_hie_join$LastCapitationDate == "2018-02-01"] <- as.Date("01/01/2018", "%m/%d/%Y")
    g_clean_hie_join$LastCapitationDate[g_clean_hie_join$LastCapitationDate == "2018-02-01"] <- "2018-01-01"

    # Update records where g_clean_hie_join$`Patient ID HIE` == "none" with HIE ID from
    # CurrentHIEID file. Then, it removes records that still do not have an HIE ID.
    g_clean_hie_join_hie_update <- g_clean_hie_join %>% mutate(
      HIEID_update = case_when(
        `Patient ID HIE` == "none" ~ `Patient ID`,
        `Patient ID HIE` != "none" ~ `Patient ID HIE`
      )
    ) %>% filter(!is.na(HIEID_update))


    # Add new columns to match AllPayers and TrackVia
    g_clean_bind <- g_clean_hie_join_hie_update %>% mutate(
      BUS_PHONE_NUMBER = "",
      `CURR_PCP_ADDRESS_LINE_1` = "",
      `CURR_PCP_ADDRESS_LINE_2` = "",
      `CURR_PCP_CITY` = "",
      `CURR_PCP_ID` = "",
      `CURR_PCP_STATE` = "",
      `CURR_PCP_ZIP` = "",
      GENDER = Gender,
      `IRS_TAX_ID` = "",
      MEMB_ADDRESS_LINE_2 = "",
      PHONE_NUMBER = "", # PCP Phone Number
      VEND_FULL_NAME = "",
      VENDOR_ID = ""
    ) %>%
      select(
        everything(),
        PRACTICE = `Galileo Attributed Practice.x`,
        -`Patient ID HIE`, # remove original `Patient ID HIE` column
        `Patient ID HIE` = HIEID_update,
        -`Cap List Subscriber ID`,
        -`Galileo Attributed Practice.y`,
        -Gender,
        -`Patient ID`,
        -Payer
      )


    # Sorts columns A-Z
    g_clean_bind <- g_clean_bind[order(colnames(g_clean_bind))]

    print(g_clean_bind)
  }

# deduplicate:
# Combines results of mco_allpayers and galileo_allpayers
# Keeps ACO records where HIE ID is NOT in AllPayers (MCO) data frame
deduplicate <-
  function(m, g) {
    g_clean_no_mco <- g %>% anti_join(m, by = "Patient ID HIE")
    g_clean_no_mco
  }

# Combine mco_allpayers and results of deduplicate
combine <-
  function(a, d) {
    combine <- rbind(a, d)
    combine
  }

# Twins
# Needs `combined` function result
# Fine duplicate records based on `Patient ID HIE` field
twins <-
  function(c) {
    twins <- c %>% group_by(`Patient ID HIE`) %>% mutate(hie_count = n()) %>% filter(hie_count > 1)
    twins
  }


# Read In Files And Packages --------------------------------------------

# Load packages
library(tidyverse)
library(caplist)

# Read in Files
# Galileo
g <- read_csv("y:/cap_list_data/022018/Capitation List.csv")

# Current HIE IDs
hie <- read_csv("y:/test_files/CurrentHIEIDs.csv")

# AllPayers
a <- read_csv("y:/cap_list_data/022018/AllPayerHIEIDs-2018-02-06.csv")

# Clean AllPayers ---------------------------------------------------------

# Rename and Lower AllPayers "HIEID" column
a2 <- a %>% mutate(`Patient ID HIE`= tolower(a$HIEID))

# Update a$PRACTICE == NA to "Broadway Community" (one-time only for February 2018)
a2$PRACTICE[is.na(a2$PRACTICE)] <- "Broadway Community"

# De-duplicate AllPayers list
a2_hie_dupe <- a2 %>% group_by(`Patient ID HIE`) %>% mutate(hieid_count = n()) %>% filter(hieid_count > 1)
twins <- a2_hie_dupe %>% left_join(hie, by = c("Patient ID HIE" = "Patient ID", "SUBSCRIBER_ID" = "Cap List Subscriber ID"))

# Remove possible twins records from AllPayers
a3 <- a2 %>% anti_join(twins, by = "Patient ID HIE")

# Add EMPI to AllPayers and remove NIC patients from AllPayers
a_hie_join <- a3 %>% left_join(hie, by = c("Patient ID HIE" = "Patient ID")) %>% filter(`Cap List Payer` != "CooperUHI")

# Rename AllPayers Columns (Dplyr: new_col = existing_col)
a_hie_join_update <- a_hie_join %>% mutate(
  MonthlyBulkImport = "Monthly Import",
  SSN_clean = case_when(
    SOCIAL_SEC_NO == "NULL" ~ "",
    SOCIAL_SEC_NO != "NULL" ~ SOCIAL_SEC_NO
  )
) %>%
  select(
  everything(),
 EMPI = `Person ID`,
 MCO_Subscriber_ID = SUBSCRIBER_ID,
 `Cap List Payer` = PAYER,
 -SOCIAL_SEC_NO,
 SOCIAL_SEC_NO = SSN_clean,
 -`Cap List Payer`,
 -HIEID,
 -`Cap List Subscriber ID`,
 -`Galileo Attributed Practice`
)

# Convert column from character to date: LastCapitationDate
a_hie_join_update$LastCapitationDate <- as.Date(a_hie_join_update$LastCapitationDate, "%m/%d/%Y")

# Order the columns for binding with Galileo data frame
a_hie_join_bind <- a_hie_join_update[order(colnames(a_hie_join_update))]


# Clean Galileo -----------------------------------------------------------
g_clean <- galileo(g)

# Convert Date columns from character to date: DOB and LastCapitationDate
g_clean$DOB <- as.Date(g_clean$DOB, "%m/%d/%Y")
g_clean$LastCapitationDate <- as.Date(g_clean$LastCapitationDate, "%m/%d/%Y")

# Galileo: Find HIE IDs where "Patient ID HIE" is "none"
g_clean_hie_join <- g_clean %>% left_join(hie, by = c("EMPI" = "Person ID"))

# Remove United MCO patients from Galileo list
# United MCO patients will be bound to Galileo list later
g_clean_hie_join <- g_clean_hie_join %>% filter(is.na(`Cap List Payer`))


# Update  for non-MCO patients (Source == "Medicaid") to "1/1/2018"
# g_clean_hie_join$LastCapitationDate[g_clean_hie_join$LastCapitationDate == "2018-02-01"] <- as.Date("01/01/2018", "%m/%d/%Y")
g_clean_hie_join$LastCapitationDate[g_clean_hie_join$LastCapitationDate == "2018-02-01"] <- "2018-01-01"

# Update records where g_clean_hie_join$`Patient ID HIE` == "none" with HIE ID from
# CurrentHIEID file. Then, it removes records that still do not have an HIE ID.
g_clean_hie_join_hie_update <- g_clean_hie_join %>% mutate(
  HIEID_update = case_when(
    `Patient ID HIE` == "none" ~ `Patient ID`,
    `Patient ID HIE` != "none" ~ `Patient ID HIE`
  )
) %>% filter(!is.na(HIEID_update))


# Add new columns to match AllPayers and TrackVia
g_clean_bind <- g_clean_hie_join_hie_update %>% mutate(
  BUS_PHONE_NUMBER = "",
  `CURR_PCP_ADDRESS_LINE_1` = "",
  `CURR_PCP_ADDRESS_LINE_2` = "",
  `CURR_PCP_CITY` = "",
  `CURR_PCP_ID` = "",
  `CURR_PCP_STATE` = "",
  `CURR_PCP_ZIP` = "",
  GENDER = Gender,
  `IRS_TAX_ID` = "",
  MEMB_ADDRESS_LINE_2 = "",
  PHONE_NUMBER = "", # PCP Phone Number
  VEND_FULL_NAME = "",
  VENDOR_ID = ""
) %>%
  select(
    everything(),
    PRACTICE = `Galileo Attributed Practice.x`,
    -`Patient ID HIE`, # remove original `Patient ID HIE` column
    `Patient ID HIE` = HIEID_update,
    -`Cap List Subscriber ID`,
    -`Galileo Attributed Practice.y`,
    -Gender,
    -`Patient ID`,
    -Payer
)


# Sorts columns A-Z
g_clean_bind <- g_clean_bind[order(colnames(g_clean_bind))]

# col <- tibble(
#   a_col = colnames(a_hie_join_bind),
#   g_col = colnames(g_clean_bind),
#   a_len = nchar(a_col),
#   g_len = nchar(g_col)
# )

# De-duplicate HIE IDs --------------------------------------------------
# If in United or Medicaid, chose United
# Keep ACO records where HIE ID is NOT in AllPayers data frame
g_clean_no_mco <- g_clean_bind %>% anti_join(a_hie_join_bind, by = "Patient ID HIE")



# Bind AllPayers and Galileo ----------------------------------------------
combine <- rbind(a_hie_join_bind, g_clean_no_mco)


# Write files -------------------------------------------------------------
write_csv(twins, "y:/cap_list_data/022018/twins.csv", na = "")
write_csv(combine, "y:/cap_list_data/022018/tv_import_feb_2018.csv", na = "")
write_csv(a_hie_join_bind, "y:/cap_list_data/022018/tv_import_feb_2018_allpayers_only.csv", na = "")
write_csv(g_clean_bind, "y:/cap_list_data/022018/tv_import_feb_2018_aco_only.csv", na = "")




