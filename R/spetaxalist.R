#'Return all taxa of a specific rank in a dataset
#'
#' @param data_m the dataset
#' @param rank the rank; the rank should not be lower than Genus
#' @return all taxa of a rank in a dataset
#' @examples
#'\dontrun{
#'taxalist(data_m, rank)
#'}
#'@export

spetaxalist <- function(data_m, rank) {
  if(rank == "Kingdom") {
    taxa = levels(data_m$Kingdoms)
  } else if(rank == "Phylum") {
    taxa = levels(data_m$Phyla)
  } else if(rank == "Class") {
    taxa = levels(data_m$Classes)
  } else if(rank == "Order") {
    taxa = levels(data_m$Orders)
  } else if(rank == "Family") {
    data_m$Families = as.factor(data_m$Families)
    taxa = levels(data_m$Families)
  } else if(rank == "Genus") {
    taxa = levels(data_m$Genera)
  }
  return(taxa)
}
