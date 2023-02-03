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
         output <- data, 
         output <- data %>%
           dplyr::filter(breeding_year %in% years) %>%
           droplevels())
  return(output)
}



#' Gets output of variance components for each model neatly, ready to plots
#' 
#' @param model which model type do want variance components returned for 
#' @return table, with var comp est and SE, as well as relative effect and SE

get_var_comps <- function(model){
  
  if(substitute(model) == substitute(LD_basic)){var_comp_names = c('Vby', 'Va', 'Vpe', 'Vr')
                                                mod_name = substitute(LD_basic)}
  if(substitute(model) == substitute(LD_NB)){var_comp_names = c('Vby', 'Vnb', 'Va', 'Vpe', 'Vr')
                                                mod_name = substitute(LD_NB)}
  if(substitute(model) == substitute(LD_spat)){var_comp_names = c('Vby', 'Vspat', 'Va', 'Vpe', 'Vr')
                                                mod_name = substitute(LD_spat)}
  if(substitute(model) == substitute(LD_envir)){var_comp_names = c('Vby', 'Venv', 'Va', 'Vpe', 'Vr')
                                                mod_name = substitute(LD_envir)}
  
  data <- summary(model)$varcomp %>%
    #give name to column with component names
    tibble::rownames_to_column(., var = "name") %>%
    #replace them 
    base::replace(., 'name', var_comp_names) %>%
    #rename columns component and std.error
    dplyr::rename('Est' = 'component', 'SE' = 'std.error') %>%
    #make columns with rel estimate and st err
    dplyr::mutate(Rel_Est = Est / sum(Est), 
                  Rel_SE = (SE / Est) * Rel_Est) %>%
    #remove zratio and bound and %ch 
    dplyr::select(-c('z.ratio', 'bound', '%ch')) %>%
    #add column for name of model
    dplyr::mutate(model_name = paste(mod_name))
  
  return(data)
  
}


#' Get heritabiltiy output from models - and Vp 
#' 
#' @param model which model
#' @return table, with Vp, over years and between years, and heritabiltiy between and within 




