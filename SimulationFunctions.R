library(truncnorm)
library(ggplot2)
library(reshape2)
library(survival)

# S(t) - r
survival_func = function(t, r, hazard_func){
  exp(-integrate(Vectorize(hazard_func), 0, t, subdivisions = 10000)$value) - r
}

# Find roots, i.e. find t where S(t) - r = 0
find_root = function(hazard_func, r, max_tod){
  uniroot(survival_func, interval=c(0,max_tod), r=r, hazard_func=hazard_func)$root
}

# hazard_funcs is list of hazard functions for each organism
# max_tod is used for root finding - maximum expected lifespan
simulate_tod = function(hazard_funcs, max_tod=500) {
  n = length(hazard_funcs)
  rs = runif(n)
  # Find root for each hazard function and its corresponding r value
  time_of_deaths = mapply(find_root, hazard_funcs, rs, MoreArgs = list(max_tod=max_tod))
  return(time_of_deaths)
}
# Creates plots of fvt with times of death 
create_plot <- function(data, plot_title, ymin, ymax, xmax){
  df = as.data.frame(t(data))
  df$time = as.numeric(row.names(df))-1
  head(df)
  df = melt(df, id.vars = 'time', variable.name = 'series') 
  ggplot(data=df, aes(time,value)) + 
    geom_line(aes(colour = series), show.legend = FALSE) + 
    ylim(ymin,ymax) + 
    xlim(0,xmax) +
    ggtitle(plot_title) + 
    xlab("Time")+ 
    ylab("Function-Valued Trait") + 
    theme_bw() +
    theme(plot.title = element_text(size=18, hjust=0.5), axis.title=element_text(size=12))
}
# Create nxt matrix, where time starts at t=0
# fvts - list of function-valued traits of each organism
# hazards_nat - list of natural death rate of each organism
# hazards_fvt - list of hazards rates influenced by fvt for each organism
# max_tod - estimated longest lifespan 
# create_plots - return plots of fvts with times of death
# fvt_name - specify name of fvt (holling, sin, etc) if writing data
create_data = function(fvts, hazards_nat, hazards_fvt, 
                       max_tod=500,
                       create_plots=F, fvt_name=""){
  # number of organisms
  n = length(fvts)
  ts = 0:max_tod
  # nxt matrix of fvts
  data_nodeath = t(sapply(fvts, function(fvt) { sapply(ts, fvt)}))
  data_nat = data_nodeath
  data_fvt = data_nodeath
  tods_nat = simulate_tod(hazards_nat, max_tod)
  tods_fvt = simulate_tod(hazards_fvt, max_tod)
  # If time of death is 2.5, return fvts from [0,1), [1,2), [2,3)
  # Then return indices 1,2,3 -> ceiling(tod)
  for (i in 1:n) {
    data_nat[i, -(1:ceiling(tods_nat[i]))] = NA
    data_fvt[i, -(1:ceiling(tods_fvt[i]))] = NA
  }
  # Truncate matrix time to last known survivor
  data_nat = data_nat[,1:ceiling(max(tods_nat))]
  data_fvt = data_fvt[,1:ceiling(max(tods_fvt))]
  if (create_plots) {
    p1 = create_plot(data_nodeath, "Arbitrary Function-Valued Traits of a Simulated Population",
                     min(data_nodeath, na.rm=T),
                     max(data_nodeath, na.rm=T),
                     ncol(data_nodeath))
    p2 = create_plot(data_nat, "Trait Does Not Influence Survival",
                     min(c(data_nat, data_fvt), na.rm=T),
                     max(c(data_nat, data_fvt), na.rm=T),
                     max(c(ncol(data_nat), ncol(data_fvt))-1))
    p3 = create_plot(data_fvt, "Trait Influences Survival",
                     min(c(data_nat, data_fvt), na.rm=T),
                     max(c(data_nat, data_fvt), na.rm=T),
                     max(c(ncol(data_nat), ncol(data_fvt))-1))
    return(list(nodeath=data_nodeath, 
                nat=data_nat, 
                fvt=data_fvt, 
                tods_nat=tods_nat,
                tods_fvt=tods_fvt, 
                plot_nodeath=p1,
                plot_nat=p2,
                plot_fvt=p3))
  }
  if (fvt_name != "") {
    write.csv(data_nat, paste(c("Data/",fvt_name,"_con_",n,".csv")), collapse="")
    write.csv(data_fvt, paste(c("Data/",fvt_name,"_exp_",n,".csv")), collapse="")
  }
  return(list(nodeath=data_nodeath, 
              nat=data_nat, 
              fvt=data_fvt, 
              tods_nat=tods_nat,
              tods_fvt=tods_fvt))
}


# Simulates natural and fvt datasets for hollings, n=number of organisms
# holling has form a*t / (b+t)
# hazard has form lambda_fvt + m * holling
sim_hollings <- function(n, max_tod=500,
                         a_mean=70, a_sd=5, 
                         b_mean=10, b_sd=1, 
                         m=0.001, lambda_nat=0.05, lambda_fvt=0.05, 
                         create_plots=F, write_data=F) {
  as <- rtruncnorm(n, mean = a_mean, sd = a_sd, a=0)
  bs <- rtruncnorm(n, mean = b_mean, sd = b_sd, a=0)
  # List of function valued traits for each organism
  fvts_holling = mapply(function(a,b) {
    force(a)
    force(b)
    function(t) {
      a*t / (b+t)
    }
  }, as, bs)
  # List of hazard rates for each organism
  hazards_holling = sapply(fvts_holling, function(fvt) {
    function(t) {
      lambda_fvt + m * fvt(t)
    }
  })
  # List of natural death rates
  hazards_nat = replicate(n, function(t){lambda_nat})
  
  return(create_data(fvts_holling, hazards_nat, hazards_holling, 
                     max_tod=max_tod, create_plots=create_plots, 
                     ifelse(write_data, "holling", "")))
}

# Simulates natural and fvt datasets for sin, n=number of organisms
# sin has form c + b * sin(2*pi*t / a)
# hazard has form lambda_fvt + m * sin
sim_sin <- function(n, max_tod=1000,
                         a_mean=20, a_sd=0.2, 
                         b_mean=0, b_sd=0.5, 
                         c=10, 
                         m=0.001, lambda_nat=0.05, lambda_fvt=0.04, 
                         create_plots=F, write_data=F) {
  # Here, a is period length and b is height of wave
  as = rtruncnorm(n, mean = a_mean, sd = a_sd, a=0)
  bs = rtruncnorm(n, mean = b_mean, sd = b_sd, a=-c, b=c)
  # List of fvts for each organism
  fvts_sin = mapply(function(a, b) {
    force(a)
    force(b)
    function(t) {
      c + b * sin(2*pi*t / a)
    }
  }, as, bs)
  # List of hazard rates for each organism
  hazards_sin = sapply(fvts_sin, function(fvt) {
    function(t) {
      lambda_fvt + m * fvt(t)
    }
  })
  hazards_nat = replicate(n, function(t){lambda_nat})
  
  return(c(create_data(fvts_sin, hazards_nat, hazards_sin, 
                     max_tod=max_tod, create_plots=create_plots, 
                     ifelse(write_data, "sin", "")), 
           fvts=fvts_sin,
           hazards_fvt=hazards_sin,
           hazards_nat=hazards_nat))
}

sim_exp_sin <- function(n, max_tod=500,
                        a_mean=20, a_sd=0.2, 
                        b_mean=0, b_sd=0.5, 
                        c=10, 
                        m=0.01, lambda_nat=0.05, lambda_fvt=0.04, 
                        create_plots=F, write_data=F) {
  # Here, a is period length and b is height of wave
  as = rtruncnorm(n, mean = a_mean, sd = a_sd, a=0)
  bs = rtruncnorm(n, mean = b_mean, sd = b_sd, a=-c, b=c)
  # List of fvts for each organism
  fvts_sin = mapply(function(a, b) {
    force(a)
    force(b)
    function(t) {
      c + b * sin(2*pi*t / a)
    }
  }, as, bs)
  # List of hazard rates for each organism
  hazards_sin = sapply(fvts_sin, function(fvt) {
    function(t) {
      lambda_fvt * exp(m*fvt(t))
    }
  })
  hazards_nat = replicate(n, function(t){lambda_nat})
  
  return(c(create_data(fvts_sin, hazards_nat, hazards_sin, 
                     max_tod=max_tod, create_plots=create_plots, 
                     ifelse(write_data, "sin", "")), 
         fvts=fvts_sin,
         hazards_fvt=hazards_sin,
         hazards_nat=hazards_nat))
}
