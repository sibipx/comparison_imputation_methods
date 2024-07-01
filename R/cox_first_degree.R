# code shamelessly stolen from: https://github.com/easonfg/cali_tutorial/blob/master/cox.R
# paper: https://academic.oup.com/jamia/article/27/4/621/5762806
# modified to fit slope and intercept separately

logit <- function (p) log(p/(1 - p))

cox_first_degree <- function(y, prob){
  dat <- data.frame(e = prob, o = y)
  dat$e[dat$e == 0] = 0.0000000001
  dat$e[dat$e == 1] = 0.9999999999
  dat$logite <- logit(dat$e)

  mfit1 = glm(formula = o~I(logite),
             family = binomial(link = "logit"), dat)

  slope = mfit1$coefficients[2]

  mfit2 = glm(formula = o~offset(logite),
              family = binomial(link = "logit"), dat)

  intercept = mfit2$coefficients[1]

  return(list(slope = slope, intercept = intercept))
}
