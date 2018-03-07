#' allpayers_mco: clean AllPayersHIEID file for binding with result of allpayers_galileo function
#'
#' @return Tibble
#'
#' a = AllPayersHIEID file
#' c = CurrentHIEIDs file
#' @examples
#' allpayers_mco(a, c)
#'

allpayers_mco <- function(a, c) {
    # Rename and Lower AllPayers "HIEID" column
    a2 <- a %>% mutate(`Patient ID HIE`= tolower(a$HIEID))

    # Change c$`Cap List Subscriber ID` column to integer
    c$`Cap List Subscriber ID` <- as.integer(c$`Cap List Subscriber ID`)

    # De-duplicate AllPayers list
    a2_hie_dupe <- a2 %>% group_by(`Patient ID HIE`) %>% mutate(hieid_count = n()) %>% filter(hieid_count > 1)
    twins <- a2_hie_dupe %>% left_join(c, by = c("Patient ID HIE" = "Patient ID", "SUBSCRIBER_ID" = "Cap List Subscriber ID"))

    # Remove possible twins records from AllPayers
    a3 <- a2 %>% anti_join(twins, by = "Patient ID HIE")

    # Add EMPI to AllPayers and remove NIC patients from AllPayers
    a_hie_join <- a3 %>% left_join(c, by = c("Patient ID HIE" = "Patient ID")) %>% filter(`Cap List Payer` != "CooperUHI")

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
        Medicaid_Claims_ID = `Medicaid Claims ID`,
        SOCIAL_SEC_NO = SSN_clean,
        -SOCIAL_SEC_NO,
        -`Cap List Payer`,
        -HIEID,
        -`Cap List Subscriber ID`,
        -`Galileo Attributed Practice`,
        - MEDICAID_NO
      )

    # Convert column from character to date: LastCapitationDate
    a_hie_join_update$LastCapitationDate <- as.Date(a_hie_join_update$LastCapitationDate, "%m/%d/%Y")

    # Order the columns for binding with Galileo data frame
    a_hie_join_bind <- a_hie_join_update[order(colnames(a_hie_join_update))]
  }
