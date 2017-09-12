#'Report the percentage of unique taxonomic records of a given taxa identified to species level
#'
#' @param taxa A string.
#' @return the percentage of taxonomic records identified to species level
#' @examples
#' taxaaccum("Achnanthes")

taxaprecision <- function(taxa) {
  taxa <- subset(data_m, Kingdoms == taxa | Phyla == taxa | Classes == taxa | Orders == taxa | Families == taxa | Genera == taxa)
  species_complete <- which(taxa$AphiaIDs != "")
  all_species <- dim(taxa)[[1]][1]
  species_precision = length(species_complete) / all_species
  return(species_precision)
}


