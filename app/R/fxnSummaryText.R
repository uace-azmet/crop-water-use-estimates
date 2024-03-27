#' `fxnSummaryText.R` - Build summary text based on user input
#' 
#' @param azmetStation - AZMet station selected by user
#' @param annualCrop - Annual crop selected by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `summaryText` - Summary text based on user input


fxnSummaryText <- function(azmetStation, annualCrop, plantingDate, endDate, dCalculateETc) {
  summaryText <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "Estimated total water use at the AZMet",
          azmetStation,
          "station for",
          annualCrop,
          "since the planting date of",
          gsub(" 0", " ", format(plantingDate, "%B %d, %Y")),
          "and through the end date of",
          gsub(" 0", " ", format(endDate, "%B %d, %Y")),
          "is",
          strong(paste(
            format(x = max(dCalculateETc$water_use_in_cumsum, na.rm = TRUE), nsmall = 2),
            "inches.",
            
            sep = " "
          )),
          "Total precipitation over this period is",
          paste(
            format(x = max(dCalculateETc$precip_total_in_cumsum, na.rm = TRUE), nsmall = 2),
            "inches.",
            
            sep = " "
          ),
          "These cumulative totals are based on values in the following table.",
          
          sep = " "
        )
      ), 
      
      class = "summary-text"
    )
  
  return(summaryText)
}
