# ==========================================================================
# Setup: Cognitive models
# Author: Florian I. Seitz
# ==========================================================================


## Set up the models -------------------------------------------------------
M <- function(d) {
  m <- solnp(pars = c(w2_12 = .5, w2_2 = .5, w2_13 = .5, w2_3 = .5, w3_1 = 1/3, w3_2 = 1/3, w3_3 = 1/3, c = 1, sigma = .05),
             fun = gt_sim,
             eqfun = function(pars, d, predict = FALSE, train = FALSE) {c(sum(pars[1:2]), sum(pars[3:4]), sum(pars[5:7]))},
             eqB = c(1, 1, 1),
             LB = rep(0, 9),
             UB = c(rep(1, 7), 10, 1),
             d = d)
  return(list(list(w2_12 = m$pars[["w2_12"]],
                   w2_2 = m$pars[["w2_2"]],
                   w2_13 = m$pars[["w2_13"]],
                   w2_3 = m$pars[["w2_3"]],
                   w3_1 = m$pars[["w3_1"]],
                   w3_2 = m$pars[["w3_2"]],
                   w3_3 = m$pars[["w3_3"]],
                   c = m$pars[["c"]],
                   sigma = m$pars[["sigma"]],
                   ll = -1 * tail(m$values, 1),
                   ll_all = -1 * gt_sim(m$pars, d, predict = FALSE, train = FALSE)
                   ),
              list(id = d$id,
                   f1_l = d$f1_l,
                   f2_l = d$f2_l,
                   f3_l = d$f3_l,
                   f1_r = d$f1_r,
                   f2_r = d$f2_r,
                   f3_r = d$f3_r,
                   critical = d$critical,
                   train = d$train,
                   nc = d$nc,
                   nd = d$nd,
                   preds = gt_sim(m$pars, d, predict = TRUE),
                   obs = d$resp)))
}