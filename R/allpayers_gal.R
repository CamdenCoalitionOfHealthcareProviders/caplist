#' allpayers_gal: clean Galileo file for binding with result of allpayers_mco function
#'
#' @return Tibble
#'
#' g = Galileo 'Capitation List' file
#' c = CurrentHIEIDs file
#'
#' @examples
#' allpayers_gal(g, c)
#'

allpayers_gal <- function(g, c) {
    g_clean <- galileo(g)

    # Convert Date columns from character to date: DOB and LastCapitationDate
    g_clean$DOB <- as.Date(g_clean$DOB, "%m/%d/%Y")
    g_clean$LastCapitationDate <- as.Date(g_clean$LastCapitationDate, "%m/%d/%Y")

    # Galileo: Find HIE IDs where "Patient ID HIE" is "none"
    g_clean_hie_join <- g_clean %>% left_join(c, by = c("EMPI" = "Person ID"))

    # Remove United MCO patients from Galileo list
    # United MCO patients will be bound to Galileo list later
    g_clean_hie_join <- g_clean_hie_join %>% filter(is.na(`Cap List Payer`))


    # Update  for non-MCO patients (Source == "Medicaid") to "1/1/2018"
    # g_clean_hie_join$LastCapitationDate[g_clean_hie_join$LastCapitationDate == "2018-02-01"] <- as.Date("01/01/2018", "%m/%d/%Y")
    # g_clean_hie_join$LastCapitationDate[g_clean_hie_join$LastCapitationDate == "2018-02-01"] <- "2018-01-01"

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
        -Payer,
        -`Medicaid Claims ID`
      )

    # Sorts columns A-Z
    g_clean_bind <- g_clean_bind[order(colnames(g_clean_bind))]

    print(g_clean_bind)
  }
