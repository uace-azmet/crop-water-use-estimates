#' `fxnTableFooter.R` - Build footer for HTML table based on user input
#' 
#' @param annualCrop - Annual crop selected by user
#' @param plantingDate - Planting date selected by user
#' @param endDate - End date selected by user
#' @param growingSeasonLength - Growing season length based on annual crop selected by user
#' @return `tableFooter` - Footer for HTML table based on user input


fxnTableFooter <- function(annualCrop, plantingDate, endDate, growingSeasonLength) {
  
  # Inputs to build table footer
  
  daysSincePlanting <- as.integer(
    difftime(time1 = endDate, time2 = plantingDate, units = "days")
  )
  
  cropGrowthStageText <- fxnCropGrowthStageText(
    annualCrop = annualCrop, 
    daysSincePlanting = daysSincePlanting
  )
  
  dayOrDays <- if (daysSincePlanting == 1) {
    dayOrDays <- "day"
  } else {
    dayOrDays <- "days"
  }
  
  todayDate <- gsub(" 0", " ", format(lubridate::today(), "%B %d, %Y"))
  
  todayYear <- lubridate::year(lubridate::today())
  
  urlAPI <- a(
    "api.azmet.arizona.edu", 
    href="https://api.azmet.arizona.edu/v1/observations/daily", # Daily data
    target="_blank"
  )
  
  urlAzmetr <- a(
    "azmetr", 
    href="https://uace-azmet.github.io/azmetr/",
    target="_blank"
  )
  
  urlReferenceAZMet <- a(
    "api.azmet.arizona.edu", 
    href="https://api.azmet.arizona.edu/v1/observations/daily", 
    target="_blank"
  )
  
  urlReferenceBulletin <- a(
    "Penman-Monteith equation",
    href="https://extension.arizona.edu/pubs/standardized-reference-evapotranspiration-new-procedure-estimating-reference-evapotranspiration",
    target="_blank"
  )
  
  urlReferenceFAO <- a(
    "www.fao.org", 
    href="https://www.fao.org/3/x0490e/x0490e0b.htm", 
    target="_blank"
  )
  
  webpageAZMet <- a(
    "AZMet website", 
    href="https://azmet.arizona.edu/", 
    target="_blank"
  )
  
  webpageCode <- a(
    "GitHub page", 
    href="https://github.com/uace-azmet/crop-water-use-estimates", 
    target="_blank"
  )
  
  webpageDataVariables <- a(
    "data variables", 
    href="https://azmet.arizona.edu/about/data-variables", 
    target="_blank"
  )
  
  webpageNetworkMap <- a(
    "station locations", 
    href="https://azmet.arizona.edu/about/network-map", 
    target="_blank"
  )
  
  webpageStationMetadata <- a(
    "station metadata", 
    href="https://azmet.arizona.edu/about/station-metadata", 
    target="_blank"
  )
  
  # Build table footer
  tableFooter <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Evapotranspiration is reference evapotranspiration (<em>ETo</em>) based on the ", urlReferenceBulletin, ". Estimates of water use (<em>ETc</em>) are based on crop growth stage, crop coefficients (<em>Kc</em>), and the crop coefficient curve model from ", urlReferenceFAO, ". Daily water use is estimated by the formula <em>ETc = Kc * ETo</em>. Water use and crop coefficient values of 'NA' denote accumulation periods with dates equal to or before the selected planting date or past the 'late-season' growth stage.",
          br(), br(),
          "Based on the selected planting date, ", annualCrop, " is estimated to be ", cropGrowthStageText, " at the selected end date. Lengths of crop growth stages are based on September, October, and November planting dates. The time since the planting date is ", daysSincePlanting, " ", dayOrDays, ". The modeled total growing season length for ", annualCrop, " is ", growingSeasonLength, " days.", 
          br(), br(),
          " Daily AZMet data are from ", urlAPI, " and accessed using the ", urlAzmetr, " R package. Table values from recent dates may be based on provisional data. More information about ", webpageDataVariables, ", ", webpageNetworkMap, ", and ", webpageStationMetadata, " is available on the ", webpageAZMet, ". Users of AZMet data and related information assume all risks of its use.",
          br(), br(),
          "To cite the above AZMet data, please use: 'Arizona Meteorological Network (", todayYear, ") Arizona Meteorological Network (AZMet) Data. https://azmet.arizona.edu. Accessed ", todayDate, "', along with 'Arizona Meteorological Network (", todayYear, ") Crop Water Use Estimates. https://viz.datascience.arizona.edu/azmet/azmet-crop-water-use-estimates. Accessed ", todayDate, "'.",
          br(), br(),
          "For information on how this webpage is put together, please visit the ", webpageCode, " for this tool."
        )
      )
    )
  
  return(tableFooter)
}
