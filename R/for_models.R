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



#' Gets output of variance componenets for each model neatly, ready to plots
#' 
#' @param model which model type do what variance componenets returned for 
#' @return table, with var comp est and SE, as well as relative effect and SE

get_var_comps <- function(model){
  
  if(substitute(model) == substitute(LD_basic)){var_comp_names = c('Vby', 'Va', 'Vpe', 'Vr')}
  if(substitute(model) == substitute(LD_NB)){var_comp_names = c('Vby', 'Vnb', 'Va', 'Vpe', 'Vr')}
  if(substitute(model) == substitute(LD_dist)){var_comp_names = c('Vby', 'Vspat', 'Va', 'Vpe', 'Vr')}
  if(substitute(model) == substitute(LD_env)){var_comp_names = c('Vby', 'Venv', 'Va', 'Vpe', 'Vr')}
  
  data <- summary(model)$varcomp %>%
    #give name to column with component neames
    tibble::rownames_to_column(., var = "name") %>%
    #replace them 
    replace(., 'name', var_comp_names) %>%
    #rename columns component and std.error
    dplyr::rename('Est' = 'component', 'SE' = 'std.error') %>%
    #make columns with rel estimate and st err
    dplyr::mutate(Rel_Est = Est / sum(Est), Rel_SE = (SE / Est) * Rel_Est) %>%
    #remove zratio and bound and %ch 
    dplyr::select( -c('z.ratio', 'bound', '%ch')) 
  
  return(data)
  
}
