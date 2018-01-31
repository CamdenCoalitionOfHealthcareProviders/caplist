#' Northgate II Function: clean Northgate II patient list
#'
#' @return Tibble
#' @export
#'
#' @examples
#' ng(x)
#' y <- ng(x) %>% write_csv(y, "y.csv", na = "")

northgate <- function(x) {
  # Removes rows and columns that are all NA
  x2 <- x %>% remove_empty_cols() %>% remove_empty_rows()

  # Remove 1st three rows to leave header row up top
  x2 <- x2[-c(1:3),]
  x2[1,] <- gsub("\n", "", x2[1,])
  colnames(x2)=x2[1,]
  x2 <- x2[-c(1),]

  # Deletes the row with the Super's Unit
  x3 <- x2[!(x2$`Household Member Name`=="Super'S, Unit"),]

  # Separates the Apt field
  x3$Floor = as.character(lapply(strsplit(as.character(x3$`Apt#`), split="-"), "[", 1))
  x3$AptNum = as.character(lapply(strsplit(as.character(x3$`Apt#`), split="-"), "[", 2))

  # Removes LR or HR from the AptNum field
  x3$AptNum2 <- gsub("LR", "", x3$AptNum)
  x3$AptNum3 <- gsub("HR","",  x3$AptNum2)

  x3$AptNum <- NULL
  x3$AptNum2 <- NULL

  # Determines if the apartment is a LR or HR
  x3$FN <- ifelse(x3$Floor==15, "HR", "LR")

  # Concatenate AptNum3 field and the FN field
  x3$Apt.No <- paste0(x3$AptNum3, x3$FN, "")

  x4 <- reshape::rename(x3, c(Apt. = "Apt_old"))
  x4$Floor <- NULL
  x4$AptNum3 <- NULL
  x4$FN <- NULL

  # Adds a new field and populates with the current date
  x4$DateLastResidentList <- format(Sys.time(), "%m-01-%Y")

  # Removes X. from column names
  names(x4) <- sub("X.", "", sub("\\(.*\\)", "", names(x4)))

  # Renames Fields
  x4$Apt. <- NULL
  x4 <- reshape::rename(x4, c(Apt.No="Apt.No."))
  x4 <- reshape::rename(x4, c(Household.Member.Name="Household Member Name"))
  x4 <- reshape::rename(x4, c(Phone.Number="Phone Number"))
  x4 <- reshape::rename(x4, c(Move.In.Date="Move In Date"))

  # Deletes the 3  totals rows in the bottom of the spreadsheet
  x5 <- head(x4,-3)

  # Renames Household Member Name
  x5 <- reshape::rename(x5, c(Household.Member.Name="Household Member Name"))

  # Removes duplicate rows (all blanks except for Apt. No., Date Last Resident List)
  x6 <- subset(x5, Community != "")

  # Remove empy columns (using Janitor package)
  x7 <- remove_empty_cols(x6)

  print(x7)
}
