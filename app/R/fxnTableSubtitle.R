#' `fxnTableSubtitle.R` - Build subtitle for HTML table based on current date
#' 
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `tableSubtitle` - Subtitle for HTML table based on current date


fxnTableSubtitle <- function(plantingDate, endDate) {
  tableSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "Since", gsub(" 0", " ", format(plantingDate, "%B %d, %Y")), "through", gsub(" 0", " ", format(endDate, "%B %d, %Y")), 
          sep = " "
        )
      ), 
      
      class = "table-subtitle"
    )
  
  return(tableSubtitle)
}
