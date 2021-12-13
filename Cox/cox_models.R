library(survival)
source("../convert_list.R")
source("../f_beta_eval.R")
source("../SimulationFunctions.R")


# The cox models with the four combinations, as specified in Cox_Regression.pdf

model_interval_wclust_cox <- function(data) {
  data = convert_list(data)
  data$time0 = data$time - 1
  x = summary(coxph(Surv(time0,time,status) ~ trait, cluster=ids, data=data))
  return(ifelse(x$coefficients[2] >= 1 & x$coefficients[6] < 0.05, 1, 0))
}
model_interval_noclust_cox <- function(data) {
  data = convert_list(data)
  data$time0 = data$time - 1
  x = summary(coxph(Surv(time0,time,status) ~ trait, data=data))
  return(ifelse(x$coefficients[2] >= 1 & x$coefficients[5] < 0.05, 1, 0))
}
model_right_wclust_cox <- function(data) {
  data = convert_list(data)
  x = summary(coxph(Surv(time,status) ~ trait, cluster=ids, data=data))
  return(ifelse(x$coefficients[2] >= 1 & x$coefficients[6] < 0.05, 1, 0))
}
model_right_noclust_cox <- function(data) {
  data = convert_list(data)
  x = summary(coxph(Surv(time,status) ~ trait, data=data))
  return(ifelse(x$coefficients[2] >= 1 & x$coefficients[5] < 0.05, 1, 0))
}


# The following tests holling, sin with a linear relationship, 
# and sin with an exponential relationship with the four models,
# with datasets either containing 30, 50, 100, 500, or 1000 organisms

# NOTE: This will take a couple hours to run - the output is already uploaded as 
# results_{fvt}_{hazard_relationship}.csv


num_orgs = c(30,50,100,500,1000)
models = c(model_interval_wclust_cox, 
           model_interval_noclust_cox, 
           model_right_wclust_cox, 
           model_right_noclust_cox)


outs_sin_exp = sapply(models, function(model) {
  sapply(num_orgs, function (n){
    eval_out = f_beta_eval(model=model, sim_exp_sin, n, 50)
    return(list("n"=n, "acc"=eval_out$acc, "f"=eval_out$fb))
  })
})

write.csv(outs_sin_exp, "results_sin_exp.csv", row.names=F)

outs_sin_lin = sapply(models, function(model) {
  sapply(num_orgs, function (n){
    eval_out = f_beta_eval(model=model, sim_sin, n, 50)
    return(list("n"=n, "acc"=eval_out$acc, "f"=eval_out$fb))
  })
})

write.csv(outs_sin_lin, "results_sin_lin.csv", row.names=F)

outs_hol_lin = sapply(models, function(model) {
  sapply(num_orgs, function (n){
    eval_out = f_beta_eval(model=model, sim_hollings, n, 50)
    return(list("n"=n, "acc"=eval_out$acc, "f"=eval_out$fb))
  }) 
})

write.csv(outs_hol_lin, "results_hol_lin.csv", row.names=F)


# After running, the data can be plotted using cox_plot_models.R


