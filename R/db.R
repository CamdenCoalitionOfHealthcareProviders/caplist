#' Capitation List Database Functions
#'
#' @return Tibble
#' @export
#'
#' @examples
#' db_united("05/01/2018", united_file)

# United MCO cap list
db_united <- function(date, u){
  u2 <- u %>%
    mutate(mco = "United",
           month = as.Date(date, "%m/%d/%Y"),
           dob = as.Date(u$DATE_OF_BIRTH, "%m/%d/%Y"))
  
  u2
}

# United MCO cap list if no SSN
db_united_no_ssn <- function(date, u){
  u2 <- u %>%
    mutate(mco = "United",
           month = as.Date(date, "%m/%d/%Y"),
           dob = as.Date(u$DATE_OF_BIRTH, "%m/%d/%Y"),
           SOCIAL_SEC_NO = "")
  
  u2
}

# Horizon MCO cap list
db_horizon <- function(date, h){
  h2 <- h %>% 
    mutate(month = as.Date(date, "%m/%d/%Y"),
           mco = "Horizon",
           dob = as.Date(h$DOB, "%m/%d/%Y")) %>% select(-`DOB`)
  
  h2
}


# Camcare MCO cap list
db_camcare <- function(date, c){
  c2 <- c %>%
    mutate(mco = "Camcare",
           month = as.Date(date, "%m/%d/%Y"),
           dob = as.Date(c$DATE_OF_BIRTH, "%m/%d/%Y"))
  
  c2
}

# Camcare MCO cap list for specific month
db_camcare_1710 <- function(date, c){
  c2 <- c %>%
    mutate(mco = "Camcare",
           month = as.Date(date, "%m/%d/%Y"),
           dob = as.Date(c$DOB, "%m/%d/%Y"))
  
  c2
}

# ACO cap list
db_aco <- function(date, aco){
  g <- caplist::galileo(aco)
  g2 <- g %>%
    mutate(
      dob = as.Date(g$DOB, "%m/%d/%Y"),
      month = as.Date(date, "%m/%d/%Y")
    ) %>% select(-`DOB`)
  
  g2
}
