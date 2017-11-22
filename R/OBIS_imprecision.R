#'Return the percentage of taxa occurrences unidentified to the finest taxa level (i.e. species)
#'
#' @param taxa A string.
#' @return the percentage of taxa occurrences on OBIS
#' @examples
#'\dontrun{
#'OBIS_imprecisionrate("Abra")
#'}
#'@export

OBIS_imprecisionrate <- function(taxa) {
  dd <- robis::occurrence(taxa)$species
  incomplete <- length(which(is.na(dd == TRUE)))
  imprecisionrate <- incomplete / length(dd)
  return(imprecisionrate)
}
