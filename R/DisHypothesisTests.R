#' @title Label discrete PDF
#' @description labels a discrete pdf
#' @param x x value
#' @param obsVal observed event
#' @param expVal expected value
#'
#' @return vector of labels for x value in relation to observed event
#' @export
#'
#' @examples
#' labelPDFDis(0:10, 3, 5)
labelPDFDis <- function(x, obsVal, expVal){
  y <- rep(x = "More Extreme Event", length(x))
  y[abs(x - expVal) < abs(obsVal - expVal)] <- "Less Extreme Event"
  y[x == obsVal] <- "Observed Event"
  y[x == expVal -(obsVal - expVal)] <- "Equally Extreme Event"
  return(y)
}

#' Show Extreme Events from a Discrete Distribution
#'
#' @param testID name of test being performed. This is used to title the graph
#' @param obsVal observed x value
#' @param expVal expected x value
#' @param xVals domain of x (possible values)
#' @param probFun probability mass function for the given distribution
#' @param ... addition arguments passed to probFun
#'
#' @return graph coloring events by how extreme they are under the null hypothesis
#' @export
#'
#' @examples
#' showXtremeEventsDis("Prop. Test", 3, 5, 0:10, probFun = dbinom, size = 10, prob = 0.5)
showXtremeEventsDis <- function(testID, obsVal, expVal, xVals, probFun, ...){
  fakeData <- data.frame(x = xVals,
                         Probability = probFun(xVals, ...),
                         Event = labelPDFDis(xVals,
                                             obsVal,
                                             expVal))

  plt <- ggplot(fakeData, aes_(x = ~ x, y = ~ Probability)) +
    geom_bar(mapping = aes_(fill = ~ Event),
             stat = "identity",
             color = "black") +
    theme_classic() +
    scale_fill_manual(values = c("Observed Event" = "#E69F00",
                                 "Equally Extreme Event" = "#000000",
                                 "More Extreme Event" = "#56B4E9",
                                 "Less Extreme Event" = "#FFFFFF")) +
    labs(x = "X",
         y = "Probability",
         title = paste("Results of", testID))
  print(plt)
  return(plt)
}


#' Show results of proportion test using \link[stats]{binom.test}
#'
#' @param x x value
#' @param n number of repetitions
#' @param p probability of success in one Bernoulli trial
#'
#' @return output of call to \link[stats]{binom.test}
#' @export
#'
#' @examples
#' showProp.Test(3, 10)
showProp.Test <- function(x, n, p = 0.5){
  testResult <- binom.test(x, n, p)
  obsVal <- testResult$statistic
  showXtremeEventsDis(testID = "Proportion Test",
                      obsVal = obsVal,
                      expVal = n*p,
                      xVals = 0:n,
                      probFun = dbinom,
                      size = n,
                      prob = p)
  return(testResult)
}



