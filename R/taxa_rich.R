#'Return the predicted value of taxa richness (of given rank) based on logistic regression model
#'
#' @param taxa A string.
#' @param rank A string.
#' @return the predicted value of taxa richness (of given rank) based on logistic regression model
#' @import data.table
#' @importFrom dplyr count
#' @importFrom stats getInitial
#' @import drc
#' @import ggplot2
#' @examples
#' \dontrun{
#' taxa_rich("Animalia", "Phylum")
#' }
#' @export
#'
taxa_rich <- function(taxa, rank) {
  tryCatch({
    df <- subset(data_m, Kingdoms == taxa | Phyla == taxa | Classes == taxa | Orders == taxa | Families == taxa | Genera == taxa)
    dt = as.data.table(unique(df))
    setkey(dt, "year")
    if(rank == "Phylum") {
      dt[, id := as.numeric(factor(Phyla, levels = unique(Phyla)))]
      ranklabel = "phyla"
    } else if(rank == "Class") {
      dt[, id := as.numeric(factor(Classes, levels = unique(Classes)))]
      ranklabel = "classes"
    } else if(rank == "Order") {
      dt[, id := as.numeric(factor(Orders, levels = unique(Orders)))]
      ranklabel = "orders"
    } else if(rank == "Family") {
      dt[, id := as.numeric(factor(Families, levels = unique(Families)))]
      ranklabel = "families"
    } else if(rank == "Genus") {
      dt[, id := as.numeric(factor(Genera, levels = unique(Genera)))]
      ranklabel = "genera"
    } else if(rank == "Species") {
      dt[, id := as.numeric(factor(AphiaIDs, levels = unique(AphiaIDs)))]
      ranklabel = "species"
    }
    setkey(dt, "year", "id")
    dt.out <- dt[J(unique(year)), mult = "last"]#[, Phylum := NULL]
    dt.out[, id := cummax(id)]
    numtaxa <- cummax(as.numeric(factor(dt$id)))
    taxa_dt <- aggregate(numtaxa, list(year = dt$year), max )
    colnames(taxa_dt) <- c("year", "taxacount")

    N_obs <- taxa_dt$'taxacount'
    times <- c(taxa_dt$year)

    model <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = L.4())

    maxi = round(coef(summary(model))[3], digits = 0)
    phase1 = "Based on the dataset, a logistic regression model predicts there exist"
    phase2 = "of"
    phase3 = "in this region."
    complete_phase = paste(phase1, maxi, ranklabel, phase2, taxa, phase3, sep = " ")
    return(complete_phase)
  }#, error = function(e) {list(taxa = taxa, rank = rankabel, method = method, corr_coef = cat("model fails to converge", "\n"))}
  )
}

