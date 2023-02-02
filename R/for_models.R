# DESCRIPTION ──────────────────────────────────────────────────────────────── #

# Functions to start running models

# DEPENDENCIES ─────────────────────────────────────────────────────────────── #

box::use(magrittr[`%>%`])
box::use(dplyr)


# FUNCTIONS ────────────────────────────────────────────────────────────────── #


#' Returns subset of data with years you choose.
#'
#' @param data Wytham breeding data.
#' @return dataset with certain/all years.

choose_data <- function(data, years){
  ifelse(years == 'ALL', 
         data <- data, 
         data <- data %>%
           dplyr::filter(., breeding_year == years) %>%
           droplevels())
  return(data)
}


