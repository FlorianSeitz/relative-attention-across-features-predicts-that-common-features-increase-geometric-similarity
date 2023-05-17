# ==========================================================================
# Analyses Study 2: Runs a general linear model with identity link
# ==========================================================================

# ==========================================================================
# Prepares packages and data
# ==========================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, lme4, lmerTest, emmeans, stringr, pbkrtest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio

dt <- fread("../../data/processed/study1.csv")[f1_l != f1_r]
dt[, subj := as.factor(subj)]
dt[, nd := as.factor(nd)]
dt[, nc := as.factor(nc)]

# ==========================================================================
# Performs generalized linear modelling (see Table 6)
# ==========================================================================
full_model <- lmer(resp ~ nd * nc + (1|subj), data = dt) 
restricted_model <- lmer(resp ~ nd + nc + (1|subj), data = dt) 

anova(restricted_model, full_model, refit = FALSE)

round(summary(full_model)$coefficients, 2) # equals estimates in Table 6

# ==========================================================================
# Makes pairwise comparisons
# ==========================================================================
(emm1 <- emmeans(full_model, ~ nc | nd, pbkrtest.limit = 8206))
pairs(emm1, adjust = "holm") # all pairwise comparisons 

# ==========================================================================
# Makes exploratory standard deviation analyses
# ==========================================================================
dt_sd <- dt[, .(sd = sd(resp, na.rm = T)), by = .(subj, phase, type)]
dt_sd[, .(mean(sd), median(sd), sd(sd)), by = phase]
dt_sd <- dcast(dt_sd, formula = subj + type ~ phase, value.var = "sd")
dt_sd[, t.test(x = tp, y = no_tp, paired = TRUE)]
