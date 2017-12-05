library(dplyr)
library(ggplot2)
library(gridExtra)
library(streambugs)

benchmarks.csv.path <- "states_vs_time.csv"

# handler to suppress specific warnings from the streambugs library
.streambugs.suppress.warning.handler <- function(w) {
    if(any(grepl("(no (envcond|parameters) for .*|limitation by (\\w)+( \\w+)? not considered)", w))) {
        invokeRestart("muffleWarning")
    }
}

.count.n.var <- function(sys.def) length(sys.def$y.names$y.names)
.count.n.interact <- function(sys.def) {
    par.stoich = c(unlist(sys.def$par.stoich.taxon), unlist(sys.def$par.stoich.web))
    length(par.stoich)
}

#' Time `n.repeats` times `run.streambugs` (C version).
#'
#' @param n.repeats number of re-runs of `run.streambugs`
#' @param streambugs.model.fun model definition function in the list format as
#'    returned e.g. by \code{\link{streambugs.example.model.toy}
#' @param ... parameter of the \code{streambugs.model.fun} function
#'
#' @return Data frame with \code{n.repeats} rows and columns:\describe{
#'      \item{\code{"n.var"}}{number of ODE variables (constant over rows)}
#'      \item{\code{"n.interact"}}{number of interactions between ODE variables
#'          (constant over rows)}
#'      \item{\code{"time"}}{ellapsed simulation time}
#'    }
.time.model <- function(n.repeats, streambugs.model.fun, ...) {
    model <- do.call(streambugs.model.fun, list(...))

    # Note: warning handler does not significantly affect timing
    withCallingHandlers({
        time <- replicate(n.repeats, system.time({
            res.C <- run.streambugs(
                y.names  = model$y.names,
                times    = model$times,
                par      = model$par,
                inp      = model$inp,
                C        = TRUE)
        })["elapsed"])
        sys.def <- streambugs.get.sys.def(model$y.names, model$par, model$inp)
    }, warning = .streambugs.suppress.warning.handler)
    return(data.frame(
        n.var=.count.n.var(sys.def),
        n.interact=.count.n.interact(sys.def),
        time=time)
    )
}

.time.toy <- function(n.repeats, ...)
    .time.model(n.repeats, streambugs.example.model.toy, ...)

benchmark.states.vs.time <- function() {
    # burn-in (lib loading etc)
    .time.toy(3)

    n.Reach.vec <- seq(1,10,by=1)
    n.Hab.vec <- seq(1,10,by=1)
    grid <- expand.grid("n.Reach"=n.Reach.vec, "n.Hab"=n.Hab.vec)
    tdf.list <- apply(grid, 1,
        function(n) data.frame(t(n),
            .time.toy(10, n.Reaches=n["n.Reach"], n.Habitats = n["n.Hab"])
        )
    )
    tdf <- Reduce(rbind, tdf.list, NULL)
    return(tdf)
}

# Uncomment to re-run benchmarks and write to CSV
#tdf <- benchmark.states.vs.time()
#write.csv(tdf, benchmarks.csv.path, row.names=FALSE)




.plot.states.vs.time <- function(tdf, plot.title = NULL, color.factor=NULL) {
  ttbl <- tibble::as_tibble(tdf)
  #ttbl.gReachHab <- ttbl %>% group_by(n.Reach,n.Hab,n.var,n.interact) %>% summarise_at("time", funs(mean, sd))

  p0 <- ggplot(ttbl, aes(x=n.var, y=time))
  p <- p0 + theme_bw() + xlab("#ODE variables") + ylab("time (s)")
  if (is.null(color.factor)) {
      p <- p +
          geom_point() + # Points
          geom_smooth(method=lm, se=FALSE, fullrange=FALSE) # Add linear regression line
  } else if (color.factor == "n.Reach") {
      p <- p +
          geom_point(aes(color=factor(n.Reach))) + # Points
          scale_color_discrete(name="#Reaches") +  # Legend name (opt: labels)
          geom_smooth(aes(color=factor(n.Reach)), method=lm, se=FALSE, fullrange=FALSE) # Add linear regression line
  } else if (color.factor == "n.Hab") {
      p <- p +
          geom_point(aes(color=factor(n.Hab))) + # Points
          scale_color_discrete(name="#Habitats") +  # Legend name (opt: labels)
          geom_smooth(aes(color=factor(n.Hab)), method=lm, se=FALSE, fullrange=FALSE) # Add linear regression line
  } else {
    stop("Invalid value of the `color.factor` argument")
  }
  if (!is.null(plot.title)) p <- p + ggtitle(plot.title)
  return(p)
}

write.benchmarks.plot <- function(tdf, pdf.basename="state_vs_time", ...) {
    p0 <- .plot.states.vs.time(tdf, ...)
    p1 <- .plot.states.vs.time(tdf, color.factor = "n.Reach", ...)
    p2 <- .plot.states.vs.time(tdf, color.factor = "n.Hab", ...)
    pdf(paste(pdf.basename,"pdf", sep="."), width=8, height=3*6, useDingbats=FALSE)
    grid.arrange(p0, p1, p2, nrow=3, ncol=1)
    dev.off()
}

tdf <- read.csv(benchmarks.csv.path, header=TRUE)
write.benchmarks.plot(tdf, plot.title = "vary #Reaches and #Habitats")
