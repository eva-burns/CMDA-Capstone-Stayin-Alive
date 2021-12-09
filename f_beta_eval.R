# Function that computes f-score of a model

library(ModelMetrics)
source("SimulationFunctions.R")

# model: model to test on.  Takes in dataset and returns classification
# sim_function: function to call that simulates for certain fvt
#                     must return datasets as $nat and $fvt
# num_org: number of organisms in each dataset
# num_ds: number of natural / fvt datasets
# ...:  args to be passed to the sim_function
#       NOTE: if using an arg "m", must specify model= in function call
f_beta_eval <- function(model, sim_function, num_org, num_ds, ...){
  model_both = function() {
    datas = sim_function(num_org, ...)
    return(list(nat=model(datas$nat), fvt=model(datas$fvt)))
  }
  pred = replicate(num_ds, model_both())
  pred = c(as.integer(pred["nat",]), as.integer(pred["fvt",]))
  actual = c(rep(0,num_ds), rep(1,num_ds))
  accuracy = sum(actual==pred) / (2*num_ds)
  return(list(acc=accuracy, 
              fb=fScore(actual, pred, beta=1),
              preds=pred,
              cm=confusionMatrix(actual,pred)))
}

