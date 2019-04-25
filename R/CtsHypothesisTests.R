#' @title dnorm but with more arguments
#' @description compute density of normal distribution while allowing for more arguments which are ignored
#' @param x x value
#' @param mean mean of normal distribution
#' @param sd std. dev. of noraml distribution
#' @param log logical; if TRUE probabilities are given as log(p). See stats::dnorm
#' @param ... extra parameters which are ignored
#' @export
#' @examples
#' mcDNorm(x = 0)
#'
#' @return density of normal distribution
mcDNorm <- function(x, mean = 0, sd = 1, log = FALSE, ...){
  return(dnorm(x, mean, sd, log))
}

#' Density of t-distribution
#'
#' @importFrom stats dt
#' @param x x value
#' @param degFree degrees of freedom
#' @param ... optional additional parameters which are ignored
#' @export
#' @return density of given t-dist. at x
#'
#' @examples
#' mcDT(0,1)
#'
mcDT <- function(x, degFree, ...){
  return(dt(x, df = degFree))
}

#' Density of F-distribution
#'
#' @importFrom stats df
#' @param x x value
#' @param degFree1 degrees of freedom 1
#' @param degFree2 degrees of freedom 2
#' @param ... optional additional parameters which are ignored
#' @export
#' @return density of given F-dist. at x
#' @examples
#' mcDF(1,5,5)
#'
mcDF <- function(x, degFree1, degFree2, ...){
  return(df(x, df1 = degFree1, df2 = degFree2))
}

#' Density of Chi-Square distribution
#'
#' @importFrom stats dchisq
#' @param x x value
#' @param degFree degrees of freedom
#' @param ... optional additional parameters which are ignored
#' @export
#' @return density of given Chi-Square dist. at x
#'
#' @examples
#' mcDChiSq(4, 2)
#'
mcDChiSq <- function(x, degFree, ...){
  return(dchisq(x, df = degFree))
}

#' @title Used to shade in a PDF
#' @description Returns density with extreme event region having NAs
#'
#' @param fun density function to use
#' @param testStat test statistic value
#' @param x x value
#' @param ... optional parameters passed to density function
#' @return density if outside of extreme event region
#' @export
#'
#' @examples
#' shadePDFCts(x = 1:5, dnorm, 1.96)
#'
shadePDFCts <- function(x, fun, testStat, ...){
  y <- fun(x,...)
  y[abs(x) < abs(testStat)] <- NA
  return(y)
}


#' @title Highlight extreme events
#' @description Make graph highlighting events more extreme than observed sample
#'
#' @param testStat test statistic
#' @param densFun function that computes appropriate density
#' @param xlims  x limits of the graph to be used. This is passed to ggplot
#' @param degFree degrees of freedom when only one is needed. This gets passed into densFun
#' @param verbose if verbose > 0 the resulting graph is printed
#' @param ... extra arguments passed to density function
#' @param degFree1 first degrees of freedom parameter when more than one is needed
#' @param degFree2 second degrees of freedom parameter when more than one is needed
#' @param testID name of hypothesis test
#'
#' @return results of call testFun
#'
#' @import ggplot2 ggthemes
#'
#' @examples
#' x <- rnorm(100)
#' showT.Test(x, verbose = 0)
#' showT.Test(x)
#'
#' @export
showXtremeEventsCts <- function(testID, testStat, densFun, degFree = NULL, degFree1 = NULL,
                                degFree2 = NULL, xlims, verbose = 1, ...){

  if(xlims[1] == 0){
    fakeData <- data.frame(x = c(0, testStat),
                           Statistic = c("Lower Bound", "Your Test Statistic"))
  } else {
    fakeData <- data.frame(x = c(-testStat, testStat),
                           Statistic = c("Equally Extreme Event", "Your Test Statistic"))
  }

  plt <- ggplot(fakeData, aes_(x = ~ x)) +
    stat_function(fun = densFun,
                  args = list(degFree = degFree,
                              degFree1 = degFree1,
                              degFree2 = degFree2)) +
    stat_function(data = data.frame(x = xlims),
                  mapping = aes_(x = ~ x),
                  fun = shadePDFCts,
                  geom = "area",
                  fill = "#56B4E9",
                  args = list(fun = densFun,
                              testStat = testStat,
                              degFree = degFree,
                              degFree1 = degFree1,
                              degFree2 = degFree2),
                  n = 500) +
    geom_vline(aes_(xintercept = ~ x,
                    color = ~ Statistic),
               size = 3) +
    xlim(xlims) +
    scale_color_colorblind() +
    theme_bw() +
    labs(x = "Test Statistic",
         y = "Density",
         title = paste("Result of", testID))
  if(verbose > 0){
    print(plt)
  }
  return(plt)
}

#' @title Conduct z-test
#' @description Runs z-test and outputs graph for interpretation using stats::t.test
#'
#' @param group1 continuous data to test
#' @param group2 optional: second group to include for two sample t-test
#' @param mu optional: mean to test against for one-sample t-test
#' @param paired boolean, if TRUE perform matched pairs t-test
#' @param verbose default is 1 which will create a graph. To turn this off use verbose = 0.
#'
#' @return results of call to t.test
#'
#' @import ggplot2 ggthemes
#' @examples
#' x <- rnorm(100)
#' showT.Test(x, verbose = 0)
#' showT.Test(x)
#'
#' @export
showT.Test <- function(group1, group2 = NULL, mu = 0, paired = FALSE, verbose = 1){
  testResult <- t.test(group1, group2, mu = mu)
  testStat <- testResult$statistic
  degFree <- testResult$parameter
  xlimVal <- max(abs(testStat) + 1, 3)

  if(verbose > 0){
    showXtremeEventsCts(testID = "T-Test",
                        testStat = testStat,
                        densFun = mcDT,
                        xlims = c(-xlimVal, xlimVal),
                        degFree = degFree)
  }
  return(testResult)
}

#' Mosaic Plot
#'
#' @param x must be a matrix with each row and column labelled
#' @import magrittr
#' @importFrom tidyr expand
#' @importFrom  dplyr right_join filter_
#' @return mosaic plot showing observed proportions, colored by residuals from chi-sq. test
#' @export
#' @examples
#' x <- matrix(runif(9,5,100), ncol = 3, dimnames = list(c("Yes1", "No1", "Maybe1"),
#' c("Yes2", "No2", "Maybe2")))
#' showMosaicPlot(x)
showMosaicPlot <- function(x){
  if(is.null(rownames(x)) | is.null(colnames(x))){
    warning("Row and Columns must be named.")
    return()
  }
  cond <- sweep(x,2,colSums(x),`/`)

  var1CDF <- rowSums(x)/sum(x)
  var2CDF <- colSums(x)/sum(x)

  df <- expand(data.frame(), rownames(x), colnames(x)) %>%
    as.data.frame()
  names(df)<-c("Var1", "Var2")


  xStart <- 0
  var2df <- data.frame()
  for(i in 1:dim(x)[2]){
    var2df <- rbind(var2df, data.frame("Var2" = colnames(x)[i],
                                       "xStart" = xStart,
                                       "xEnd" = xStart + var2CDF[i]))
    xStart <- xStart + var2CDF[i]
  }

  totalDF <- data.frame()
  for(v2 in colnames(x)){
    yTop <- 1
    for(v1 in rownames(x)){
      temp <- data.frame("Var1" = v1,
                         "Var2" = v2,
                         "yEnd" = yTop,
                         "yStart" = yTop - cond[v1, v2])
      yTop <- yTop - cond[v1, v2]
      totalDF <- rbind(totalDF, temp)
    }
  }

  df <- right_join(totalDF, var2df)

  a <- chisq.test(x)

  a$residuals


  resData <- a$residuals
  resData <- resData %>%
    as.table() %>%
    as.data.frame()

  names(resData)[3] <- "Residual"

  xdf <- x %>%
    as.table() %>%
    as.data.frame()

  resData <- right_join(df, resData)

  labelDF1 <- resData %>%
    filter_(~Var2 == colnames(x)[1])

  labelDF1$yMean <- (labelDF1$yStart + labelDF1$yEnd)/2

  labelDF2 <- resData %>%
    filter_(~Var1 == rownames(x)[1])

  labelDF2$xMean <- (labelDF2$xStart + labelDF2$xEnd)/2

  plt <- resData %>%
    ggplot() +
    geom_rect(aes_(xmin = ~xStart,
                  xmax = ~xEnd,
                  ymin = ~yStart,
                  ymax = ~yEnd,
                  fill = ~Residual),
              color = "white") +
    geom_text(labelDF1,
              mapping = aes_(y = ~yMean,
                            label = ~Var1),
              x = -.05,
              angle = 90) +
    geom_text(labelDF2,
              mapping = aes_(x = ~xMean,
                            label = ~Var2),
              y = 1.05) +
    theme_bw() +
    lims(x = c(-0.05, 1.05), y = c(-0.05, 1.05)) +
    labs(title = "Mosaic Plot: Residuals of Chi-Square Test",
         x = "Proportion",
         y = "Proportion")
  print(plt)
  return(plt)
}


#' Show Chi-Square Test
#' @description show results of a chi-square test visually using \link[stats]{chisq.test}
#'
#' @param x a numeric vector or matrix. x and can also be factors
#' @param y a numeric vector
#' @param p a vector of proabilities the same length as x. Used for goodness-of-fit tests. Must be a valid distribution
#' @param simulate.p.value boolean, if TRUE use simulation to estimate p-value
#' @param nreps if simulate.p.value = TRUE number of simulations to complete
#' @param verbose level of visual output, 0 = silent
#'
#' @return results of \link[stats]{chisq.test} call
#' @export
#'
#' @examples
#' showChiSq.Test(x = c(1,2,1), y= c(1,2,2))
showChiSq.Test <- function(x, y = NULL, p = rep(1/length(x), length(x)),
                           simulate.p.value = FALSE, nreps = 2000, verbose = 1){
  testResult <- chisq.test(x = x, y = y, p = p, simulate.p.value = simulate.p.value, B = nreps)
  testStat <- testResult$statistic
  degFree <- testResult$parameter
  xlims <- c(0, max(qchisq(0.99, degFree), testStat + 1))

  if(verbose > 0){
    showXtremeEventsCts(testID = "Chi-Sq. Test",
                        testStat = testStat,
                        densFun = mcDChiSq,
                        xlims = xlims,
                        degFree = degFree)
  }
  return(testResult)
}

#' @title Show results of ANOVA
#' @description Visualization of distributional results of ANOVA. Please see \link[stats]{aov} for more
#' information on parameters
#' @import gridExtra
#' @param formula formula specifying a model.
#' @param data data on which to perform ANOVA
#' @param verbose if verbose > 0 the resulting graph is printed
#' @param ... Arguments passed to lm. See \link[stats]{aov} for more detail
#'
#' @return output of call to \link[stats]{aov}
#' @export
#' @examples
#' showANOVA(yield ~  N + P + K, npk)
showANOVA <- function(formula, data = NULL, verbose = 1, ...){
  anovaResults <- aov(formula, data, ...)
  resultsTable <- summary(anovaResults)[[1]]
  degFree2 <- resultsTable$Df[dim(resultsTable)[1]]
  nInputs <- (dim(resultsTable)[1]-1)
  inputNames <- rownames(resultsTable)
  if(verbose > 0){
    pltList <- NULL
    for(i in 1:nInputs){
      degFree1 <- resultsTable$Df[i]
      testStat <- resultsTable$`F value`[i]
      xlims <- c(0, max(qf(0.99, degFree1, degFree2), testStat + 1))
      pltList[[i]] <- showXtremeEventsCts(testID = paste("ANOVA:", inputNames[i]),
                                          testStat = testStat,
                                          densFun = mcDF,
                                          xlims = xlims,
                                          degFree1 = degFree1,
                                          degFree2 = degFree2,
                                          verbose = 0)
    }
    do.call(grid.arrange, c(pltList, ncol = floor(sqrt(nInputs))))

  }
}

#' Show hypothesis tests from OLS
#'
#' @param formula forumula for regression. Passed to \link[stats]{lm}
#' @param data data for regression. Passed to \link[stats]{lm}
#' @param verbose if verbose > 0 the resulting graph is printed
#'
#' @return model object resulting from the regression
#' @export
#'
#' @examples
#' showOLS(mpg ~ cyl + disp, mtcars)
showOLS <- function(formula, data, verbose = 1){
  modelResults <- lm(formula = formula, data = data)
  resultsTable <- summary(modelResults)[[4]]
  nInputs <- dim(resultsTable)[1]
  inputNames <- rownames(resultsTable)
  degFree <- nrow(data) - length(inputNames)
  if(verbose > 0){
    pltList <- NULL
    for(i in 1:nInputs){
      testStat <- resultsTable[i,3]
      xlimVal <- max(abs(testStat) + 1, 3)
      xlims <- c(0, max(mcDT(0.99, degFree = degFree), testStat + 1))
      pltList[[i]] <- showXtremeEventsCts(testID = paste("OLS:", inputNames[i]),
                                          testStat = testStat,
                                          densFun = mcDT,
                                          xlims = c(-xlimVal, xlimVal),
                                          degFree = degFree,
                                          verbose = 0)
    }
    do.call(grid.arrange, c(pltList, ncol = floor(sqrt(nInputs))))

  }
}

#' @title Visualize results of McNemar's Test
#' @description relevant parameters are passed to \link[stats]{mcnemar.test}
#'
#' @param x two dimensional contingency table as a matrix or a factor object
#' @param y factor object, ignored if x is a matrix
#' @param correct logical indicating whether or not to perform continuity correction
#' @param verbose if verbose > 0 the resulting graph is printed
#'
#' @return results of call to \link[stats]{mcnemar.test}
#' @export
#'
#' @examples
#' Performance <-
#' matrix(c(794, 86, 150, 570),
#'     nrow = 2,
#'     dimnames = list("1st Survey" = c("Approve", "Disapprove"),
#'                      "2nd Survey" = c("Approve", "Disapprove")))
#' showMcNemarTest(Performance)
#'
showMcNemarTest <- function(x, y = NULL, correct = TRUE, verbose = 1){
  testResult <- mcnemar.test(x = x, y = y, correct = correct)
  testStat <- testResult$statistic
  degFree <- testResult$parameter
  xlims <- c(0, max(qchisq(0.99, degFree), testStat + 1))

  if(verbose > 0){
    showXtremeEventsCts(testID = "McNemar's Test",
                        testStat = testStat,
                        densFun = mcDChiSq,
                        xlims = xlims,
                        degFree = degFree)
  }
  return(testResult)
}
