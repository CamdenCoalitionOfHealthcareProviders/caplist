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
  x2 <- x %>% remove_empty(c("rows", "cols"))

  # Remove 1st three rows to leave header row up top
  x2 <- x2[-c(1:3),]
  x2[1,] <- gsub("\n", "", x2[1,])
  colnames(x2)=x2[1,]
  x2 <- x2[-c(1),]

  # Deletes the row with the Super's Unit
  x3 <- x2[!(x2$`Household Member Name`=="Super'S, Unit"),]

  x4 <- x3

  # Adds a new field and populates with the current date
  x4$DateLastResidentList <- format(Sys.time(), "%m-01-%Y")

  # Removes X. from column names
  names(x4) <- sub("X.", "", sub("\\(.*\\)", "", names(x4)))

  # Renames Fields
  x4$Apt. <- NULL
  x4 <- reshape::rename(x4, c(`Apt#`="Apt.No."))
  x4 <- reshape::rename(x4, c(Household.Member.Name="Household Member Name"))
  x4 <- reshape::rename(x4, c(Phone.Number="Phone Number"))
  x4 <- reshape::rename(x4, c(Move.In.Date="Move In Date"))

  # Deletes the 4 totals rows in the bottom of the spreadsheet
  x5 <- head(x4,-4)

  # Renames Household Member Name
  x5 <- reshape::rename(x5, c(Household.Member.Name="Household Member Name"))

  # Removes duplicate rows (all blanks except for Apt. No., Date Last Resident List)
  x6 <- subset(x5, Community != "")

  # Remove empy columns (using Janitor package)
  x7 <- remove_empty(x6, "cols")

  print(x7)
}
