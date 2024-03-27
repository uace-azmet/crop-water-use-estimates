#' `fxnAZMetDataPreview.R` - Format downloaded and transformed AZMet daily data for HTML table preview
#' 
#' @param dCalculateETc - AZMet daily data from `fxnCalculateETc.R`
#' @param timeStep - AZMet data time step
#' @return `dAZMetDataPreview` - Data table formatted for HTML table


fxnAZMetDataPreview <- function(dCalculateETc, timeStep) {
  
  # HOURLY "date_datetime", "wind_2min_timestamp"
  if (timeStep == "Hourly") {
    dAZMetDataPreview <- dCalculateETc %>%
      dplyr::mutate(dplyr::across(
        c("date_doy", "date_year", "meta_needs_review", "meta_version", "relative_humidity", "wind_2min_vector_dir", "wind_vector_dir", "wind_vector_dir_stand_dev"), 
        \(x) format(x, nsmall = 0)
      )) %>%
      dplyr::mutate(dplyr::across(
        c("dwpt", "dwptF", "eto_azmet", "heatstress_cottonC", "heatstress_cottonF", "precip_total", "temp_airC", "temp_airF", "temp_soil_10cmC", "temp_soil_10cmF", "temp_soil_50cmC", "temp_soil_50cmF", "wind_2min_spd_max_mph", "wind_2min_spd_max_mps", "wind_2min_spd_mean_mph", "wind_2min_spd_mean_mps", "wind_spd_max_mph", "wind_spd_max_mps", "wind_spd_mph", "wind_spd_mps", "wind_vector_magnitude", "wind_vector_magnitude_mph"), 
        \(x) format(x, nsmall = 1)
      )) %>%
      dplyr::mutate(dplyr::across(
        c("eto_azmet_in", "meta_bat_volt", "precip_total_in", "sol_rad_total", "sol_rad_total_ly", "vp_actual", "vp_deficit"), 
        \(x) format(x, nsmall = 2)
      )) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  }
  
  # DAILY "datetime" "wind_2min_timestamp"
  if (timeStep == "Daily") {
    dAZMetDataPreview <- dCalculateETc %>%
      dplyr::mutate(dplyr::across(
        c("chill_hours_0C", "chill_hours_20C", "chill_hours_32F", "chill_hours_45F", "chill_hours_68F", "chill_hours_7C", "date_doy", "date_year", "meta_needs_review", "meta_version", "relative_humidity_max", "relative_humidity_mean", "relative_humidity_min", "wind_2min_vector_dir", "wind_vector_dir", "wind_vector_dir_stand_dev"), 
        \(x) format(x, nsmall = 0)
      )) %>%
      dplyr::mutate(dplyr::across(
        c("dwpt_mean", "dwpt_meanF", "eto_azmet", "eto_pen_mon", "heat_units_10C", "heat_units_13C", "heat_units_3413C", "heat_units_45F", "heat_units_50F", "heat_units_55F", "heat_units_7C", "heat_units_9455F", "heatstress_cotton_meanC", "heatstress_cotton_meanF", "precip_total_mm", "temp_air_maxC", "temp_air_maxF", "temp_air_meanC", "temp_air_meanF", "temp_air_minC", "temp_air_minF", "temp_soil_10cm_maxC", "temp_soil_10cm_maxF", "temp_soil_10cm_meanC",  "temp_soil_10cm_meanF", "temp_soil_10cm_minC", "temp_soil_10cm_minF", "temp_soil_50cm_maxC", "temp_soil_50cm_maxF", "temp_soil_50cm_meanC", "temp_soil_50cm_meanF", "temp_soil_50cm_minC", "temp_soil_50cm_minF", "wind_2min_spd_max_mph", "wind_2min_spd_max_mps", "wind_2min_spd_mean_mph", "wind_2min_spd_mean_mps", "wind_spd_max_mph", "wind_spd_max_mps", "wind_spd_mean_mph", "wind_spd_mean_mps", "wind_vector_magnitude", "wind_vector_magnitude_mph"), 
        \(x) format(x, nsmall = 1)
      )) %>%
      dplyr::mutate(dplyr::across(
        c("eto_azmet_in", "eto_pen_mon_in", "eto_pen_mon_in_cumsum", "meta_bat_volt_max", "meta_bat_volt_mean", "meta_bat_volt_min", "precip_total_in", "precip_total_in_cumsum", "sol_rad_total", "sol_rad_total_ly", "vp_actual_max", "vp_actual_mean", "vp_actual_min", "vp_deficit_mean", "water_use_in", "water_use_in_cumsum"), 
        \(x) format(x, nsmall = 2)
      )) %>%
      dplyr::mutate(kc = format(x = kc, nsmall = 3)) %>%
      
      # Specific to this Shiny app -->
      dplyr::mutate(days_since_planting = format(x = days_since_planting, nsmall = 0)) %>%
      dplyr::mutate(datetime = gsub(" 0", " ", format(datetime, "%B %d, %Y"))) %>%
      dplyr::select(meta_station_name, datetime, water_use_in_cumsum, water_use_in, kc, eto_pen_mon_in, eto_pen_mon_in_cumsum, precip_total_in, precip_total_in_cumsum, days_since_planting) %>%
      dplyr::rename(
        `AZMet Station` = meta_station_name,
        Date = datetime,
        `Cumulative Water Use (inches)` = water_use_in_cumsum,
        `Daily Water Use (inches)` = water_use_in,
        `Crop Coefficient` = kc,
        `Daily Evapotranspiration (inches)` = eto_pen_mon_in,
        `Cumulative Evapotranspiration (inches)` = eto_pen_mon_in_cumsum,
        `Daily Precipitation (inches)` = precip_total_in,
        `Cumulative Precipitation (inches)` = precip_total_in_cumsum,
        `Days since Planting` = days_since_planting
      ) %>%
      # <--
      
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  }
  
  return(dAZMetDataPreview)
}
