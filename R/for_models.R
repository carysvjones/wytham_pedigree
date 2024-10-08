# DESCRIPTION ──────────────────────────────────────────────────────────────── #

# Functions to start running models

# DEPENDENCIES ─────────────────────────────────────────────────────────────── #

box::use(magrittr[`%>%`])
box::use(dplyr)
box::use(rptR)
box::use(mods =  ../ R / for_models)


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


#' Get bounding box for calculating territory polygons
#'
#' @param breeding_data breeding data with spatial object
#' @return box

bbox_polygon_G <- function(breeding_data) {
  bb <- sf::st_bbox(breeding_data)

  p <- matrix(
    c(
      bb["xmin"], bb["ymin"],
      bb["xmin"], bb["ymax"],
      bb["xmax"], bb["ymax"],
      bb["xmax"], bb["ymin"],
      bb["xmin"], bb["ymin"]
    ),
    ncol = 2, byrow = T
  )

  sf::st_polygon(list(p))
}




#' Get territory polygons
#'
#' @param breeding_data
#' @param wood_outline
#' @param yr year of data
#' @return dataset with areas for each box occupied within that year

get_territory_polygons <- function(breeding_data, wood_outline, yr) {
  # create box
  box <- sf::st_sfc(mods$bbox_polygon_G(breeding_data))

  # find areas
  breeding_data_areas <- breeding_data %>%
    dplyr::filter(dplyr::if_any(
      tidyselect::any_of(c("year", "breeding_year")),
      ~ . == yr
    )) %>%
    # get voronoi polygons
    sf::st_union() %>%
    sf::st_voronoi(box) %>%
    sf::st_cast() %>%
    sf::st_intersection(sf::st_union(wood_outline)) %>%
    # joining the territory polygons back up with the individuals that bred in them
    sf::st_sf() %>%
    sf::st_join(dplyr::filter(breeding_data, if_any(any_of(c("year", "breeding_year")), ~ . == yr))) %>%
    # get area in new column
    dplyr::mutate(area_polygon = sf::st_area(geometry))

  return(breeding_data_areas)
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



#' get heritability and phenotypic variance estimates
#' 
#' @param data as output from get_var_comps function.
#' @param model which model to get estimates for.
#' @return small tibble with Vp, VP within year, h2, h2 within year
#' 

get_herit <- function(data, model){
  
  herits <- tibble::tibble(name = c('Vp', 'Vp_yr', 'h2', 'h2_yr'),
                 Est = c(#Vp - total phenotypic var
                   sum(subset(data, model_name == model)$Est, na.rm = T ),
                   #Vp_yr, within year phenotypic var
                   sum(subset(data, model_name == model & name != 'Vby')$Est,
                       na.rm = T),
                   #h2 - across years
                   subset(data, model_name == model & name == 'Va')$Est / 
                     sum(subset(data, model_name == model)$Est, na.rm = T),
                   #within year h2
                   subset(data, model_name == model & name == 'Va')$Est / 
                     sum(subset(data, model_name == model & name != 'Vby')$Est,
                         na.rm = T)),
                 SE = c(#get SE for these - first Vp
                   NA, NA,
                   #h2 - across years
                   subset(data, model_name == model & name == 'Va')$SE / 
                     sum(subset(data, model_name == model)$SE, na.rm = T),
                   #within year h2
                   subset(data, model_name == model & name == 'Va')$SE / 
                     sum(subset(data, model_name == model & name != 'Vby')$SE)))
  
  return(herits)
  
}



#' get repeatability value for environmental variables of Mothers
#' 
#' @param data dataset to use.
#' @param response response factor.
#' @param nboot number of bootstraps, automatic = 1000.
#' @param npermut numebr permutations, automatic = 0.
#' @return model output from rpt function.

rep_val <- function(response, data, nboot = 1000, npermut = 0){
  
  # mod_output <- lm(data = dat, as.formula(paste(response, '~ breeding_year')))
  mod_output <- rptR::rpt(stats::as.formula(paste(response, '~ (1 | Mother)')), 
                          grname = "Mother", 
                          data = data, 
                          datatype = "Gaussian", 
                          nboot = nboot, 
                          npermut = npermut)
  
  return(mod_output) 
}









