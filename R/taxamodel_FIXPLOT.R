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
    #return(taxa_dt)
    #plot(taxa_dt$year, taxa_dt$`taxa count`, xlab = "Year", ylab = paste("Number of", ranklabel, sep = " "), ylim = c(0, max(taxa_dt$"taxa count")*1.35))
    #title(taxa)
    minx <- min(as.vector(taxa_dt$year))
    maxx <- max(as.vector(taxa_dt$year))
    ylab = paste("Number of", ranklabel, sep = " ")
    p <- ggplot(taxa_dt, aes(x = year, y = taxacount, colour = "#FF9999", group = 1)) + geom_point()
    p <- p + labs(x = "Year", y = ylab) + ggtitle(taxa) + scale_x_discrete(breaks = c(seq(minx, maxx, 25))) + theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1), axis.text.y = element_text(angle = 60, hjust = 1), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

    #p <- plotly::ggplotly(p)
    #p

    if(method == "logistic") {
      N_obs <- taxa_dt$'taxacount'
      #N_obs <- numtaxa
      times <- as.numeric(taxa_dt$year)
      #return(data.frame(N_obs, times))

      SS<-stats::getInitial(N_obs~SSlogis(times,alpha,xmid,scale),data=data.frame(N_obs=N_obs,times=times))
      K_start <- SS["alpha"]
      R_start <- 1/SS["scale"]
      N0_start <- SS["alpha"]/(exp(SS["xmid"]/SS["scale"])) + 1
      log_formula<-formula(N_obs ~ K * N0 * exp(R * times) / (K + N0 * (exp(R * times) - 1)))
      m<-nls(log_formula,start = list(K = K_start, R = R_start, N0 = N0_start), control = list(maxiter = 500), trace = TRUE)
      corr_coef <- cor(N_obs,predict(m))
      return(corr_coef)
      lines(times,predict(m),col="red",lty=2,lwd=2)
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
      #lines(times, UP, col = 'red', lty = "dashed")
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
      times <- as.numeric(taxa_dt$year)

      #ddd <- data.frame(N_obs, times)
      #write.csv(ddd, "/Users/hhsieh/Desktop/ddd.csv", row.names = FALSE)

      #MM <- getInitial(N_obs~SSmicmen(times, Vm, K),data=data.frame(N_obs=N_obs,times=times))

      #Vm_start <- MM["Vm"]
      #K_start <- MM["K"]
      #return(c(Vm_start, K_start))
      #return(MM)
      #
      #model <- nls(N_obs ~ SSmicmen(times, Vm, K), data = data.frame(N_obs = N_obs, times = times), algorithm = "plinear", control = nls.control(minFactor = 1/4096), trace = TRUE) ##This is a very bad model for all ranks
      #summary(model)

      #y <- 1/N_obs
      #x <- 1/times
      #mm <- lm(y ~x)
      #summary(mm)

      #Vm_start <- 1 / summary(mm)$coefficients[1]
      #K_start <- summary(mm)$coefficients[2] / summary(mm)$coefficients[1]
      #return(c(Vm_start, K_start))


      #model <- nlsLM(N_obs ~ Vm * times / (K + times), start = list(K = K_start, Vm = Vm_start, trace = TRUE))
      #summary(model)


      #model <- nls(N_obs ~ SSmicmen(times, Vmax, Km), data = data.frame(N_obs, times), start = list(Km = K_start, Vmax = Vm_start), algorithm = "plinear", nls.control(minFactor = 1/32768), trace = TRUE)
      #summary(model)
      #summary(model)




      #model <- try(nlsLM(N_obs ~ Vm * times / (K + times), start = list(Vm = Vm_start, K = K_start), control = list(maxiter = 500), trace = TRUE))

      #corr_coef <- cor(N_obs, predict(model))
      #return(corr_coef)
      #lines(times,predict(model),col="red",lty=2,lwd=2)
      #n = length(times)
      ## add model predictions
      #a = summary(model)$coefficient[1]
      #b = summary(model)$coefficient[2]
      #lines(times,predict(model),col="red",lty=2,lwd=2)
      ## add variances - first, find standard errors
      #a_se = summary(model)$coefficients[3]
      #b_se = summary(model)$coefficients[4]
      ## compute standard deviations
      #a_sd = a_se * sqrt(n)
      #b_sd = b_se * sqrt(n)
      # compute upper bounds of model prediction
      #UP = (a + a_sd) * times / (b - b_sd + times)
      #lines(times, UP, col = 'red', lty = "dashed")
      #LW = (a - a_sd) * times / (b + b_sd + times)
      #lines(times, LW, col ='red', lty = 'dashed')
      #return('correlation coefficient' = corr_coef)
      #
      #
      N_obs <- taxa_dt$'taxacount'
      times <- as.numeric(taxa_dt$year)

      model.drm <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = MM.2())
      summary(model.drm) # return d = Vm, e = K
      plot(model.drm)

      #model.nls <- nls(N_obs ~ Vm * times / (K + times), data = data.frame(N_obs, times = times), start = list(K = max(N_obs)/2, Vm = max(N_obs)))
      #summary(model.nls)

      #MM <- getInitial(N_obs~SSmicmen(times, Vm, K),data=data.frame(N_obs=N_obs,times=times))

      #Vm_start <- MM["Vm"]
      #K_start <- MM["K"]
      #return(c(Vm_start, K_start))
      #return(MM)

      corr_coef <- cor(N_obs, predict(model.drm))
      #return(corr_coef)
      #lines(times,predict(model.drm),col="red",lty=2,lwd=2)
      n = length(times)
      ## add model predictions
      a = summary(model.drm)$coefficient[1]
      b = summary(model.drm)$coefficient[2]
      #lines(times,predict(model),col="red",lty=2,lwd=2)
      ## add variances - first, find standard errors
      a_se = summary(model.drm)$coefficients[3]
      b_se = summary(model.drm)$coefficients[4]
      ## compute standard deviations
      a_sd = a_se * sqrt(n)
      b_sd = b_se * sqrt(n)
      # compute upper bounds of model prediction
      UP = (a + a_sd) * times / (b - b_sd + times)
      #lines(times, UP, col = 'red', lty = "dashed")
      LW = (a - a_sd) * times / (b + b_sd + times)
      #return(data.frame(UP, LW))
      #summary(model.drm)

      #p <- p + geom_line()

      #p <- p + geom_ribbon(aes(ymin = LW, ymax = UP), linetype = 2, alpha = 0.1)
      #p <- plotly::ggplotly(p)
      #p
      #
    }
  }#, error = function(e) {list(taxa = taxa, rank = rank, method = method, corr_coef = cat("model fails to converge", "\n"))}
  )
}
