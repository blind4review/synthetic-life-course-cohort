function(dirs,aggOptions=NULL){
  aggOptions <- aggOptions
  f <- function(simResultsMx){

    results  <- simResultsMx %>%
      group_by(dag,
               measurement,
               combining,
               analysis) %>%
      dplyr::summarise(truth=mean(TrueBigPop),
                mean=mean(estimates,na.rm=TRUE),
                bias=mean-truth,
                var = var(estimates,na.rm=TRUE),
                mse = bias^2+var,
                mean_se = mean(mean_se,na.rm=TRUE),
                lb = quantile(estimates,prob=0.025,na.rm=TRUE),
                ub = quantile(estimates,prob=0.975,na.rm=TRUE),
                bias_lb = lb-truth,
                bias_ub = ub-truth,
                #Add other summaries here
                ci_coverage=mean(coverage),
                percBias=100*bias/truth,
                propNA = sum(is.na(estimates))/n(),
                propSimFail = mean(simFailures),
                runtime_mean = mean(runtime_mean,na.rm=TRUE),
                runtime_mean_successes = mean(runtime_mean_successes,na.rm=TRUE)
      ) %>%
      ungroup() %>% t()

    return(results)
  }
  return(f)
}