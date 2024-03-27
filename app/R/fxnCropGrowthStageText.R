#' `fxnCropGrowthStageText.R` - Check if days since planting date is greater than crop growing season length
#' 
#' @param annualCrop - Annual crop selected by user
#' @param daysSincePlanting - Based on planting and end dates selected by user, defined in `fxnTableFooter.R`
#' @return `cropGrowthStageText` - Character string


fxnCropGrowthStageText <- function(annualCrop, daysSincePlanting) {
  x <- cropCoefficientCurves %>%
    dplyr::filter(crop == annualCrop)
  
  if (daysSincePlanting <= max(x$growingDay)) {
    cropGrowthStage <- 
      x$cropGrowthStage[which(x$growingDay == daysSincePlanting)]
    
    cropGrowthStageText <- 
      paste0("in the '", cropGrowthStage, "' growth stage")
  } else {
    cropGrowthStageText <- "past the 'late-season' growth stage"
  }
  
  return(cropGrowthStageText)
}
