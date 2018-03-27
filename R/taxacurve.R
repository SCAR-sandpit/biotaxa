#'Create the accumulative curve of a given taxa
#'
#' @param taxa A string.
#' @param rank A string.
#' @return plotting the accumulative curve of \code{rank} of any given \code{taxa}.
#' importFrom graphics lines plot title
#' @examples
#' \dontrun{
#' taxaacurve("Animalia", "Phylum")
#' }
#' @export


taxacurve <- function(taxa, rank) {
  df <- subset(data_m, Kingdoms == taxa | Phyla == taxa | Classes == taxa |                 Orders == taxa | Families == taxa | Genera == taxa)
  dt = as.data.table(unique(df))
  setkey(dt, "year")
  if (rank == "Phylum") {
    dt[, id := as.numeric(factor(Phyla, levels = unique(Phyla)))]
    ranklabel = "phyla"
  } else if (rank == "Class") {
    dt[, id := as.numeric(factor(Classes, levels = unique(Classes)))]
    ranklabel = "classes"
  } else if (rank == "Order") {
    dt[, id := as.numeric(factor(Orders, levels = unique(Orders)))]
    ranklabel = "orders"
  } else if (rank == "Family") {
    dt[, id := as.numeric(factor(Families, levels = unique(Families)))]
    ranklabel = "families"
  } else if (rank == "Genus") {
    dt[, id := as.numeric(factor(Genera, levels = unique(Genera)))]
    ranklabel = "genera"
  } else if (rank == "Species") {
    dt[, id := as.numeric(factor(AphiaIDs, levels = unique(AphiaIDs)))]
    ranklabel = "species"
  }
  setkey(dt, "year", "id")
  dt.out <- dt[J(unique(year)), mult = "last"]#[, Phylum := NULL]
  dt.out[, id := cummax(id)]
  numtaxa <- cummax(as.numeric(factor(dt$id)))
  taxa_dt <- aggregate(numtaxa, list(year = dt$year), max )
  colnames(taxa_dt) <- c("year", "taxa count")
  plot(taxa_dt$year, taxa_dt$`taxa count`, xlab = "Year", ylab = paste("Number of", ranklabel, sep = " "))
  title(taxa)
}
