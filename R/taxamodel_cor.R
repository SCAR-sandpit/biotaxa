#'Report the correlation coefficient of selected model
#'
#' @param taxa A string.
#' @param rank A string.
#' @param method A string.
#' @return the correlation coefficient of the \code{taxa} ~ \code{rank} \code{method} model.
#' @import data.table
#' @importFrom dplyr count
#' @importFrom stats predict
#' @importFrom stats cor
#'@examples
#'\dontrun{
#'taxamodeo_cor("Animalia", "Phylum", "logistic")
#'}
#'@export

taxamodel_corr <- function(taxa, rank, method) {
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
    times <- as.numeric(taxa_dt$year)
    if(method == "logistic") {

      ryegrass.m1 <- suppressWarnings(drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = L.4()))
      corr_coef <- cor(N_obs, predict(ryegrass.m1))
      res <- list(taxa=taxa, rank=rank, method=method, corr_coef=corr_coef)
      return(res)

    } else if(method == "Michaelis-Menten") {
      model.drm <- suppressWarnings(drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = MM.2()))

      corr_coef <- cor(N_obs, predict(model.drm))
      res <- list(taxa=taxa, rank=rank, method=method, corr_coef=corr_coef)
      return(res)
    } else if(method == "Asymtopic_Regression_Model") {
      model.drm <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = AR.3())
      corr_coef <- cor(N_obs, predict(model.drm))
      res <- list(taxa=taxa, rank=rank, method=method, corr_coef=corr_coef)
      return(res)
    }
  }, error = function(e) {list(taxa = taxa, rank = rank, method = method, corr_coef = cat("model fails to converge", "\n"))})
}
