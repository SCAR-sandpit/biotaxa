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
    p <- ggplot(taxa_dt, aes(x = year, y = taxacount, colour = "#FF9999", group = 1)) + geom_point()
    p <- p + labs(x = "Year", y = ylab) + ggtitle(taxa) + scale_x_discrete(breaks = c(seq(minx, maxx, 25))) + theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1), axis.text.y = element_text(angle = 60, hjust = 1), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


    if(method == "logistic") {
      N_obs <- taxa_dt$'taxacount'
      times <- as.numeric(taxa_dt$year)
      SS<-stats::getInitial(N_obs~SSlogis(times,alpha,xmid,scale),data=data.frame(N_obs=N_obs,times=times))
      K_start <- SS["alpha"]
      R_start <- 1/SS["scale"]
      N0_start <- SS["alpha"]/(exp(SS["xmid"]/SS["scale"])) + 1
      log_formula<-formula(N_obs ~ K * N0 * exp(R * times) / (K + N0 * (exp(R * times) - 1)))
      m<-nls(log_formula,start = list(K = K_start, R = R_start, N0 = N0_start), control = list(maxiter = 500), trace = FALSE)
      corr_coef <- cor(N_obs,predict(m))

      #preds <- predict(m, newdata = data.frame(times = times, error = 0.05))
      #return(preds)

      #consult this page for confidence intervals (http://sia.webpopix.org/nonlinearRegression.html)

      n = length(times)
      K = summary(m)$coefficient[1]
      R = summary(m)$coefficient[2]
      N0 = summary(m)$coefficient[3]

      ## add variances - first, find standard errors
      K_se = summary(m)$coefficients[4]
      R_se = summary(m)$coefficients[5]
      N0_se = summary(m)$coefficients[6]

      ## compute standard deviations
      K_sd = K_se * sqrt(n)
      R_sd = R_se * sqrt(n)
      N0_sd = N0_se * sqrt(n)

      # compute upper bounds of model prediction
      UP = (K + K_sd) * (N0 + N0_sd) * exp((R + R_sd)*times)/((K + K_sd)+(N0 + N0_sd)*(exp((R + R_sd)*times)-1))

      LW = (K - K_sd) * (N0 - N0_sd) * exp((R - R_sd)*times)/((K - K_sd)+(N0 - N0_sd)*(exp((R - R_sd)*times)-1))
      #return(data.frame(UP, LW))
      #lines(times, LW, col ='red', lty = 'dashed')
      #

      #p <- plotly::ggplotly(p)
      #p
      p <- p + geom_line()
      p <- p + geom_ribbon(aes(ymin = LW, ymax = UP), linetype = 2, alpha = 0.1)
      #p <- plotly::ggplotly(p)
      p

      #return('correlation coefficient' = corr_coef)
    } else if(method == "Michaelis-Menten") {

      # refer to this page https://stackoverflow.com/questions/27547548/solving-error-message-step-halving-factor-reduced-below-minimum-in-nls-step-a

      N_obs <- taxa_dt$'taxacount'
      times <- c(taxa_dt$year)

      model.drm <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = MM.2())
      newtimes <- times
      #return(newtimes)
      preds <- predict(model.drm, times = newtimes, interval = "prediction", level = 0.95)
      #return(class(preds))

     LW = preds[,2]
     UP = preds[,3]

      corr_coef <- cor(N_obs, predict(model.drm))

      p <- p + geom_line()
      p <- p + geom_ribbon(aes(ymin = LW, ymax = UP), linetype = 2, alpha = 0.1)

      p
    } else if (method == "MM-test") {
      N_obs <- taxa_dt$'taxacount'
      times <- c(taxa_dt$year)

      fit <- drm(
        formula = N_obs ~ times,
        data = data.frame(N_obs = N_obs, times = times),
        fct = MM.2())

      pred <- as.data.frame(predict(
        fit,
        newdata = data.frame(N_obs = N_obs, times = times),
        interval = "prediction", level = 0.95));
      pred$times <- times;

      data.frame(times = times, N_obs = N_obs) %>%
        ggplot(aes(times, N_obs)) +
        geom_point() +
        geom_line(data = pred, aes(x = times, y = Prediction)) +
        geom_ribbon(
          data = pred,
          aes(x = times, ymin = Lower, ymax = Upper),
          alpha = 0.4);
    } else if (method == "logis-test") {

      N_obs <- taxa_dt$'taxacount'
      times <- c(taxa_dt$year)

      ryegrass.m1 <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = L.4())

      summary(ryegrass.m1)

      pred <- as.data.frame(predict(
        ryegrass.m1,
        newdata = data.frame(N_obs = N_obs, times = times),
        interval = "prediction", level = 0.95));
      pred$times <- times;

      #return(pred)
      LW = pred[,2]
      UP = pred[,3]

      p <- p + geom_line()
      p <- p + geom_ribbon(aes(ymin = LW, ymax = UP), linetype = 2, alpha = 0.1)

      p

      #data.frame(times = times, N_obs = N_obs) %>%
      #  ggplot(aes(times, N_obs, colour = "#FF9999", group = 1)) +
      #  geom_point() + labs(x = "Year", y = ylab) + ggtitle(taxa) + scale_x_discrete(breaks = c(seq(minx, maxx, 25))) + theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1), axis.text.y = element_text(angle = 60, hjust = 1), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +

        #geom_line(data = pred, aes(x = times, y = Prediction)) +
        #geom_ribbon(
        #  data = pred,
        #  aes(x = times, ymin = Lower, ymax = Upper),
        #  linetype = 2,
        #  alpha = 0.4);
    }

  }#, error = function(e) {list(taxa = taxa, rank = rank, method = method, corr_coef = cat("model fails to converge", "\n"))}
  )
}
