#'Return all taxa in a dataset
#'
#' @param data_m the dataset
#' @return all taxa and ranks of a dataset
#' @examples
#'\dontrun{
#'taxalist(data_m)
#'}
#'@export

taxalist <- function(data_m) {
  Kingdoms <- levels(data_m$Kingdoms)
  Phyla <- levels(data_m$Phyla)
  Classes <- levels(data_m$Classes)
  Orders <- levels(data_m$Orders)
  Families <- levels(data_m$Families)
  Genera <- levels(data_m$Genera)
  ranks <- c(rep("Kingdom", length(Kingdoms)), rep("Phylum", length(Phyla)), rep("Class", length(Classes)), rep("Order", length(Orders)), rep("Family", length(Families)), rep("Genus", length(Genera)))
  taxa <- c(Kingdoms, Phyla, Classes, Orders, Families, Genera)
  ddd <- data.frame(taxa, ranks)
  colnames(ddd) <- c("taxa", "rank")
  return(ddd)
}

