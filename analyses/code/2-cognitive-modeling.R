# ==========================================================================
# Analyses Experiment: 2. Fits cognitive models
# ==========================================================================

# ==========================================================================
study <- 1 # specify the study
# ==========================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, modelr, purrr, doRNG)
parallel <- TRUE # fit on a parallel machine (Unix) or single core
if (parallel == TRUE) pacman::p_load(doFuture)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- fread(sub("X", study, "../../data/processed/studyX.csv"))

# ==========================================================================
# Specifies to-be-fitted models
# ==========================================================================
gt_sim <- function(pars, d, predict = FALSE, train = FALSE) {
  c <- pars["c"]
  pred <- apply(d, 1, function(i) {
    n <- as.numeric(i[["n"]])
    if (n == 1) {w <- 1}
    if (n == 2) {
      if(as.numeric(i[["f2_l"]]) + as.numeric(i[["f2_r"]]) != 0) {w <- pars[c("w2_12", "w2_2")]}
      if(as.numeric(i[["f3_l"]]) + as.numeric(i[["f3_r"]]) != 0) {w <- pars[c("w2_13", "w2_3")]}
    }
    if (n == 3) {w <- pars[c("w3_1", "w3_2", "w3_3")]}
    exp(-c * sum(w * abs(c(as.numeric(i[["f1_l"]]) - as.numeric(i[["f1_r"]]), 
                           if(as.numeric(i[["f2_l"]]) + as.numeric(i[["f2_r"]]) != 0) {as.numeric(i[["f2_l"]]) - as.numeric(i[["f2_r"]])},
                           if(as.numeric(i[["f3_l"]]) + as.numeric(i[["f3_r"]]) != 0) {as.numeric(i[["f3_l"]]) - as.numeric(i[["f3_r"]])}))))
  })
  if(predict) return(pred)
  obs <- d$resp
  ll <- dnorm(x = obs, mean = pred, sd = pars["sigma"], log = TRUE)
  ll[is.na(ll)] <- 0
  if(train) return(-sum((d$train) * ll))
  return(-sum(ll))
}

source("setup_models.R")
model_list <- list(
  m1 = M                    # gcm (Minkowski similarity and softmax choicerule)
)

# ==========================================================================
# Makes cross-validation data sets cv_data for each participant
# ==========================================================================
# k <- 20 # number of folds
# set.seed(42)
# cv_data <- d[, .(cvfold = list(crossv_kfold(.SD, k))), by = subj]

# ==========================================================================
# Fits models
# ==========================================================================
if (parallel == TRUE) {
  # registerDoMC(cores = detectCores())
  registerDoFuture()
  plan(multisession, workers = 4L)  ## on MS Windows
  setkey(d, "subj")  
  fits <- foreach(x = unique(d$subj),
                  .combine = "rbind",
                  .inorder = FALSE, 
                  .packages = c("data.table", "modelr", "Rsolnp"),
                  .export = c("model_list", "model_options", "gt_sim")) %dorng% {
                    d[.(x), .(
                      model = names(model_list),
                      map(model_list, exec, d = .SD)), by = subj]
                  }   
  
  #  fits <- fits[cv_data, on = "subj"]
  
} else {
  fits <- d[, .(
    model = names(model_list),
    fit = map(model_list, exec, dt = .SD)),
    by = subj]
}

# makes Figure 3
theme_set(theme_bw())
resps <- d[, .(value = mean(resp), sd = sd(resp)), by = .(c = nc, d = nd, subj)][, .(value = mean(value), sd = mean(sd), variable = "a"), by = .(c, d)]
preds <- fits[, V2[[1]][[2]], by = .(subj, model)][, .(value = mean(preds), sd = sd(preds)), by = .(c = nc, d = nd, subj)][, .(value = mean(value), sd = NA, variable = "b"), by = .(c, d)]
ggplot(rbind(resps, preds), aes(c, d, fill = value)) +
  geom_raster() +
  geom_text(aes(label = paste0(ifelse(value < 1, substr(round(value, 2), 2, 4), 1), ifelse(!is.na(sd), paste0("\n(", round(sd, 2), ")"), "")))) +
  scale_fill_gradient(low = "white", high = "darkgrey", name = "Similarity", limits = c(0, 1)) +
  facet_wrap(~variable, ncol = 2, labeller = labeller(variable = c("a" = "Responses",
                                                                   "b" = "Predictions"))) +
  scale_x_continuous(name = "Number of common features", expand = c(0, 0), breaks = 0:5) +
  scale_y_continuous(name = "Number of differing features", expand = c(0, 0), breaks = 0:5) +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.text = element_text(size = 11))
ggsave("../figures/experiment_grid.png", width = 7*.9, height = 3*.9)


# computes mean parameter estimates, log likelihood, MAE, RMSE
fits[, V2[[1]][[1]], by = .(subj, model)][, sapply(.SD, function(x) round(mean(x), 2)), .SDcols = !c("subj", "model")]
fits[, V2[[1]][[1]], by = .(subj, model)][, sapply(.SD, function(x) round(sd(x), 2)), .SDcols = !c("subj", "model")]
fits[, V2[[1]][[2]], by = .(subj, model)][, mean(abs(preds - obs))]
fits[, V2[[1]][[2]], by = .(subj, model)][, sqrt(mean((preds - obs)^2)), by = subj][, mean(V1)]
