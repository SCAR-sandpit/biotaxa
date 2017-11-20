#'Return the frequency ranking of an interested lower taxon based on a higher taxon
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
