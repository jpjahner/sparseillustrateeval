# Krippendorf's alpha ranges from -1 to 1, where 1 is perfect agreement, 
# 0 is no agreement, and negative numbers indicate inverse agreement
krippendorf <- function(actual, estimated){
  x1 <- factor(append(as.numeric(actual != 0), c(0,1)))[1:length(actual)]
  x2 <- factor(append(as.numeric(estimated != 0), c(0,1)))[1:length(estimated)]
  if(all(x1==0) & all(x2==0)){1}else{
  t1 <- table(x1, x2)
  n0 <- sum(t1[,1])
  n1 <- sum(t1[,2])
  alpha <- 1 - (n0 + n1 - 1)*(t1[1,2]/(n0*n1))
  return(alpha)
  }
  
}

# Cohen's kappa ranges from 0 to 1, where 0 indicates no agreement
# and 1 indicates perfect agreement; kappa takes into account the probability
# of agreement by chance
calc_kappa <- function(actual, estimated){
  x1 <- factor(append(as.numeric(actual != 0), c(0,1)))[1:length(actual)]
  x2 <- factor(append(as.numeric(estimated != 0), c(0,1)))[1:length(estimated)]
  if(all(x1==0) & all(x2==0)){1}else{
  t1 <- table(x1, x2)
  po <- (t1[1,1]+t1[2,2])/sum(t1)
  p1 <- (sum(t1[1,])/sum(t1))*(sum(t1[,1])/sum(t1))
  p0 <- (sum(t1[2,])/sum(t1))*(sum(t1[,2])/sum(t1))
  pe <- p1 + p0
  k <- (po - pe)/(1-pe)
  return(k)}
}

# True positive rate
tpr <- function(actual, estimated) {
  p <- as.numeric(actual != 0)
  pp <- as.numeric(estimated != 0)
  tp <- sum(p * pp)
  if(sum(p) > 0) {
    return(tp/sum(p))
  } else {
    return(NA)
  }
}

# False positive rate
fpr <- function(actual, estimated) {
  n <- as.numeric(actual == 0)
  pp <- as.numeric(estimated != 0)
  fp <- sum(n * pp)
  if(sum(n) > 0) {
    return(fp/sum(n))
  } else{
    return(NA)
  }
}

# False negative rate
fnr <- function(actual, estimated) {
  fn <- sum(as.numeric(actual==1 & estimated==0))
  tp <- sum(as.numeric(actual==1 & estimated==1))
if(sum(fn)>0){
fnr <- fn/(fn+tp)}else{
fnr <- NA
}

return(fnr)

}


# Precision
precision <- function(actual, estimated) {
  p <- as.numeric(actual != 0)
  pp <- as.numeric(estimated != 0)
  tp <- sum(p * pp)
  if(sum(pp) > 0) {
    return(tp/sum(pp))
  } else {
    return(NA)
  }
}

# F1 score
f1 <- function(actual, estimated) {
  precision <- precision(actual, estimated)
  recall <- tpr(actual, estimated)
  return(2 / (1/precision + 1/recall))
}


# Calculate true and false positive rates
error_rates <- function(actual, estimated) {

  # actual: vector of true parameter values
  # estimated: vector or matrix (N parameters x N samples) of parameter estimates

  if(is.vector(estimated)) {
    tpr1 <- tpr(actual, estimated)
    fpr1 <- fpr(actual, estimated)
    fnr1 <- fnr(actual, estimated)
  } else {
    tpr1 <- apply(estimated, 2, function(i) tpr(actual, i))
    fpr1 <- apply(estimated, 2, function(i) fpr(actual, i))
    fnr1 <- apply(estimated, 2, function(i) fnr(actual, i))
  }
  return(list(true_positive_rate = tpr1, false_positive_rate = fpr1, false_negative_rate=fnr1))
}


# Calculate precision, recall, and F1 score
F1 <- function(actual, estimated) {

  # actual: vector of true parameter values
  # estimated: vector or matrix (N parameters x N samples) of parameter estimates

  if(is.vector(estimated)) {
    precision <- precision(actual, estimated)
    recall <- tpr(actual, estimated)
    f1 <- f1(actual, estimated)
  } else {
    precision <- apply(estimated, 2, function(i) precision(actual, i))
    recall <- apply(estimated, 2, function(i) tpr(actual, i))
    f1 <- apply(estimated, 2, function(i) f1(actual, i))
  }
  return(list(F1 = f1, precision = precision, recall = recall))
}


# Calculate RMSE
rmse2 <- function(actual, estimated) {

  # actual: vector of true values
  # estimated: vector or matrix (N parameters x N samples) of predictions/estimates

  if(is.vector(estimated)) {
    return(Metrics::rmse(actual, estimated))
  } else {
    return(apply(estimated, 2, function(i) Metrics::rmse(actual, i)))
  }
}


# Calculate coverage of parameter estimates
# (proportion of parameters whose uncertainty intervals include the actual value)
coverage <- function(actual, estimated, interval = 0.95) {

  # actual: vector of true parameter values
  # estimated: matrix (N parameters x N samples) of parameter estimates

  ci <- apply(estimated, 1, function(i) quantile(i, c((1-interval)/2, 1-((1-interval)/2))))
  in_interval <- sapply(1:length(actual), function(i) actual[i] >= ci[1,i] & actual[i] <= ci[2,i])
  return(sum(in_interval)/length(actual))
}


# Calculate where (which percentile) actual parameter values fall within estimated uncertainty intervals
percentile <- function(actual, estimated) {

  # actual: vector of true parameter values
  # estimated: matrix (N parameters x N samples) of parameter estimates

  return(sapply(1:length(actual), function(i) mean(estimated[i,] < actual[i])))
}


# Calculate R squared
r2 <- function(y,y_pred) {
  if(var(y_pred)==0 & var(y>0)) {
    return(0) 
  } else {
    return(cor(y,y_pred)^2)
  }
}


rsq <- function(actual, estimated) {

  # actual: vector of actual values
  # estimated: vector or matrix of predictions/estimates

  if(is.vector(estimated)) {
    return(r2(actual, estimated))
  } else {
    return(apply(estimated, 2, function(i) r2(actual, i)))
  }
}


# Calculate performance metrics (true and false positive rates, RMSE, coverage) for parameter estimates
parameter_metrics <- function(actual, estimated, ...) {

  # actual: vector of true parameter values
  # estimated: vector or matrix (N parameters x N samples) of parameter estimates

  er <- error_rates(actual, estimated)
  rmserr <- rmse2(actual, estimated)

  tpr <- mean(er$true_positive_rate)
  tpr_sd <- if(is.matrix(estimated)) sd(er$true_positive_rate) else NA
  fpr <- mean(er$false_positive_rate)
  fpr_sd <- if(is.matrix(estimated)) sd(er$false_positive_rate) else NA
  fnr <- mean(er$false_negative_rate)
  fnr_sd <- if(is.matrix(estimated)) sd(er$false_negative_rate) else NA
rmse <- mean(rmserr)
  rmse_sd <- if(is.matrix(estimated)) sd(rmserr) else NA
  coverage <- if(is.matrix(estimated)) coverage(actual, estimated, ...) else NA
  kappa <- calc_kappa(actual, estimated)
  alpha <- krippendorf(actual, estimated)

  return(c(tpr = tpr,
           tpr_sd = tpr_sd,
           fpr = fpr,
           fpr_sd = fpr_sd,
	   fnr = fnr,
	   fnr_sd = fnr_sd,
           rmse = rmse,
           rmse_sd = rmse_sd,
           coverage = coverage,
	   kappa = kappa,
	   alpha = alpha))
}


# Calculate performance metrics (RMSE, R2) for predictions
prediction_metrics <- function(y, ypred) {

  # y: actual values of y
  # ypred: vector or matrix (N observations x N samples) of predictions

  r_sq <- rsq(y, ypred)
  rmserr <- rmse2(y, ypred)

  rmse <- mean(rmserr)
  rmse_sd <- if(is.matrix(ypred)) sd(rmserr) else NA
  r2 <- mean(r_sq)
  r2_sd <- if(is.matrix(ypred)) sd(r_sq) else NA


  return(c(rmse = rmse,
           rmse_sd = rmse_sd,
           r2 = r2,
           r2_sd = r2_sd))
}
