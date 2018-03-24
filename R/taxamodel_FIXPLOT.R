#'Report the accumulative numbers of a rank of a given taxa overtime
#'
#' @param taxa A string.
#' @param rank A string.
#' @param method A string.
#' @return modeling result of the accumulation of \code{rank} of a \code{taxa} by \code{method}
#' @import data.table
#' @import ggplot2
#' @importFrom drc drm
#'@examples
#'\dontrun{
#'taxamodel_FIXPlOT("Animalia", "Genus", "logistic")
#'}
#'@export


taxamodel_FIXPLOT <- function(taxa, rank, method) {
  tryCatch({
    df <- subset(data_m, Kingdoms == taxa | Phyla == taxa | Classes == taxa |
        Orders == taxa | Families == taxa | Genera == taxa)
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

    minx <- min(as.vector(taxa_dt$year))
    maxx <- max(as.vector(taxa_dt$year))
    ylab = paste("Number of", ranklabel, sep = " ")
    p <- ggplot(taxa_dt, aes(x = year, y = taxacount, colour = "#FF9999", group = 1
      )) + geom_point(colour = "cornflowerblue")
    p <- p + labs(x = "Year", y = ylab) + ggtitle(taxa) + scale_x_discrete(breaks = c(seq(minx, maxx, 25))) + theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1), axis.text.y = element_text(angle = 60, hjust = 1), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

    if(method == "Michaelis-Menten") {

      # refer to this page https://stackoverflow.com/questions/27547548/solving-error-message-step-halving-factor-reduced-below-minimum-in-nls-step-a

      N_obs <- taxa_dt$'taxacount'
      times <- c(taxa_dt$year)

      model.drm <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = MM.2())

      newtimes <- times
      #return(newtimes)
      preds <- suppressWarnings(predict(model.drm, times = newtimes, interval = "prediction", level = 0.95))
      #return(preds)

      LW = preds[,2]
      UP = preds[,3]
      corr_coef <- cor(N_obs, predict(model.drm))
      p <- p + geom_line(data = data.frame(preds, taxa_dt$year), aes(taxa_dt$year, Prediction), colour = "#FF9999")
      p <- p + geom_ribbon(aes(ymin = LW, ymax = UP), linetype = 2, alpha = 0.1)

      p
    } else if (method == "logistic") {

      N_obs <- taxa_dt$'taxacount'
      times <- c(taxa_dt$year)

      ryegrass.m1 <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = L.4())

      pred <- suppressWarnings(as.data.frame(predict(
        ryegrass.m1,
        newdata = data.frame(N_obs = N_obs, times = times),
        interval = "prediction", level = 0.95)));
      pred$times <- times;

      LW = pred[,2]
      UP = pred[,3]

      p <- p + geom_line(data = data.frame(pred, taxa_dt$year), aes(taxa_dt$year, Prediction), colour = "#FF9999")
      p <- p + geom_ribbon(aes(ymin = LW, ymax = UP), linetype = 2, alpha = 0.1)
      p
    }
  }#, error = function(e) {list(taxa = taxa, rank = rank, method = method, corr_coef = cat("model fails to converge", "\n"))}
  )
}
