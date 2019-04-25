#' @title Label Bootstrapped Results
#' @description labels bootstrapped results. We use this to create colored histograms.
#' @param results a vector, data from bootstrapping
#' @param lBound lower bound of confidence interval
#' @param uBound upper bound of confidence interval
#'
#' @return vector of labels corresponding to result values
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' labelBootResults(x, -1, 1)
labelBootResults <- function(results, lBound, uBound){
  y <- rep(x = "More Extreme Event", length(results))
  y[results > lBound & results < uBound] <- "Less Extreme Event"
  return(y)
}


#' @title Boostrap
#' @description Boostrap using given data and statistic
#'
#' @param fun function to calculate on each sample. This can be a user-defined function that takes in data as a vector and returns a statistic.
#' @param data data to use for bootstrapping. Should be a respresentative sample
#' @param nreps number of times to bootstrap
#' @param verbose default is 1 which will create a graph. To turn this off use verbose = 0.
#' @param h0 null hypothesis value
#' @param conf.level confidence value
#'
#' @return results from boostrapping. A vector of length @param nreps containing each statistic calculated
#' @export
#'
#' @import ggplot2 stats ggthemes
#'
#' @examples
#' x <- rnorm(100)
#' bootstrap(mean, x, 0.5, 1000, verbose = 0)
#' bootstrap(mean, x, 0.5, 1000)
bootstrap <- function(fun, data, h0, nreps, conf.level = 0.95, verbose = 1){
  results <- NULL
  for(i in 1:nreps){
    currentSample <- sample(data, replace = TRUE)
    results[i] <- fun(currentSample)
  }

  meanStat <- mean(results)
  confLines <- quantile(results, c(conf.level, 1-conf.level))

  fakeData <- data.frame(Rep = results,
                         Mean = meanStat)
  fakeData$Event <- labelBootResults(results, confLines[2], confLines[1])





  if(verbose > 0){
    plt <- ggplot(data = fakeData,
                  mapping = aes_(x = ~ Rep)) +
      geom_histogram(aes_(fill= ~ Event),
                     color = "black") +
      scale_fill_manual(name = "Event",
                        values = c("Less Extreme Event" = "white",
                                   "More Extreme Event" = "#56B4E9")) +
      theme_bw() +
      geom_vline(aes_(xintercept = ~ h0,
                      color = "Null Hypothesis"),
                 size = 3) +
      geom_vline(aes_(xintercept = ~ meanStat,
                      color = "Sim. Mean"),
                 size = 3) +
      scale_color_manual(name = "Value",
                         values = c("Null Hypothesis" = "#E69F00",
                                    "Sim. Mean" = "#CC79A7")) +
      labs(x = "Statistic",
           y = "Count",
           title = paste("Results of Boostrapping", nreps, "Times"))
    print(plt)
  }
  return(results)
}

