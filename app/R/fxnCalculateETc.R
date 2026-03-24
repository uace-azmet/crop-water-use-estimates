#' `fxnCalculateETc.R` - Estimate daily crop water use based on ETo values
#' 
#' @param dAZMetDataELT - AZMet daily data from `fxnAZMetDataELT.R`
#' @param annualCrop - Annual crop selected by user
#' @param growingSeasonLength - Growing season length based on annual crop selected by user, defined in `app.R`
#' @return `dCalculateETc` - AZMet data with daily individual and cumulative ETc values


fxnCalculateETc <- function(dAZMetDataELT, annualCrop, growingSeasonLength) {
  dCalculateETc <- dAZMetDataELT %>%
    dplyr::mutate(days_since_planting = seq(from = 0, to = nrow(dAZMetDataELT) - 1)) %>%
    
    # Assign daily value for crop coefficient based on user input
    dplyr::mutate(kc = purrr::map(.x = days_since_planting, .f = function(.x) {
      if (.x > growingSeasonLength | .x <= 0) {
        NA_real_
      } else {
        cropCoefficientCurves$cropCoefficient[which(
          cropCoefficientCurves$crop == annualCrop & cropCoefficientCurves$growingDay == .x
        )]
      }
    })) %>%
    
    dplyr::mutate(precip_total_in = round(precip_total_in, digits = 2)) %>%
    dplyr::mutate(eto_pen_mon_in = round(eto_pen_mon_in, digits = 2)) %>%
    dplyr::mutate(kc = as.numeric(kc)) %>%
    dplyr::mutate(water_use_in = round((kc * eto_pen_mon_in), digits = 2)) %>%
    # dplyr::arrange(dplyr::desc(datetime)) %>%
    dplyr::mutate(
      precip_total_in_cumsum = 
        dplyr::if_else(
          condition = days_since_planting == 0, 
          true = NA_real_, 
          false = round(cumsum(precip_total_in) - precip_total_in[1], digits = 2)
        ),
      eto_pen_mon_in_cumsum = 
        dplyr::if_else(
          condition = days_since_planting == 0, 
          true = NA_real_, 
          false = round(cumsum(eto_pen_mon_in) - eto_pen_mon_in[1], digits = 2)
        ),
      water_use_in_cumsum = 
        dplyr::if_else(
          condition = days_since_planting == 0, 
          true = NA_real_, 
          false = round(cumsum(tidyr::replace_na(water_use_in, 0)), digits = 2)
        )#,
      # water_use_in_cumsum = 
      #   dplyr::if_else(
      #     condition = is.na(water_use_in), 
      #     true = NA_real_, 
      #     false = water_use_in_cumsum
      #   )
      # water_use_in_cumsum = round(cumsum(tidyr::replace_na(.data$water_use_in, 0)), digits = 2),
      # water_use_in_cumsum = dplyr::if_else(is.na(water_use_in), NA_real_, water_use_in_cumsum)
    )
  
  return(dCalculateETc)
}
