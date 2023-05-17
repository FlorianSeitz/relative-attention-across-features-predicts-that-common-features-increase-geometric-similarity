# ==========================================================================
# Analyses Experiment: 1. Preprocesses data
# ==========================================================================

# ==========================================================================
rm(list = ls(all = TRUE))
study <- 1 # specify the study
seed <- 432
# ==========================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change wd to where this script lies (RStudio needed!)
files <- list.files(path = "../../data/raw/main/", pattern = ".csv", full.names = TRUE)
dt <- rbindlist(lapply(files, fread), fill = TRUE)
# demo <- fread(sub("X", study, "../../data/raw/demographics_studyX.csv"))[!1:2, 11:19]

stim <- data.table(id = paste(1:8),
                   f1 = c(1, 0, 1, 0, 1, 0, 1, 0),
                   f2 = c(0, 0, 0, 0, 1, 1, 1, 1),
                   f3 = c(0, 0, 1, 1, 1, 1, 0, 0))
# ==========================================================================
# Prepares randomization columns and deletes unnecessary rows
# ==========================================================================
dt <- dt[!is.na(trials.thisN)]
# dt <- dt[stim == ""] # if practice trials are to be included

# ==========================================================================
# Prepares columns, deletes unnecessary rows and columns
# ==========================================================================
dt[, subj := factor(participant, labels = paste0("s10", 1:length(unique(participant))))]
dt[, trial := 1:.N, by = subj] # counts through rows
dt[, id_l := str_extract(stim_l, "\\d+")] # extracs stimulus id
dt[, id_r := str_extract(stim_r, "\\d+")] # extracs stimulus id

dt <- merge(dt, stim, by.x = "id_l", by.y = "id")
colnames(dt)[colnames(dt) %in% paste0("f", 1:3)] <- paste0("f", 1:3, "_l")
dt <- merge(dt, stim, by.x = "id_r", by.y = "id")
colnames(dt)[colnames(dt) %in% paste0("f", 1:3)] <- paste0("f", 1:3, "_r")

# subj <- unique(dt[, .(subj, id = participant)])
dt <- dt[, list(subj, trial, id = paste0(id_l, id_r), f1_l, f2_l, f3_l, f1_r, f2_r, f3_r, resp = slider.response, rt = slider.rt)]
dt[, n := 3 - ((f2_l + f2_r == 0) + (f3_l + f3_r == 0))]
dt[, nd := (f1_l != f1_r) + (f2_l != f2_r) + (f3_l != f3_r)]
dt[, nc := n - nd]

dt[, critical := ((f1_l + f1_r == 1) & (f2_l + f2_r == 0) & (f3_l + f3_r == 0)) |
    ((f1_l + f1_r == 1) & (((f2_l + f2_r == 2) & (f3_l + f3_r == 0)) | ((f2_l + f2_r == 0) & (f3_l + f3_r == 2)))) |
    ((f1_l + f1_r == 1) & (f2_l + f2_r == 2) & (f3_l + f3_r == 2))]
dt[, train := nc > 0 & nd > 0]
# dt[, train := critical == FALSE & nc > 0 & nd > 0]
dt <- dt[order(subj, trial)]

# fwrite(subj, "../../misc/subj.csv")
fwrite(dt, sub("X", study, "../../data/processed/studyX.csv"))

dt[f1_l + f1_r == 1 & f2_l + f2_r == 0 & f3_l + f3_r == 0, .(mean(resp), sd(resp)), by = subj]
dt[f1_l + f1_r == 1 & f2_l + f2_r == 2 & f3_l + f3_r == 0, .(mean(resp), sd(resp)), by = subj]
dt[f1_l + f1_r == 1 & f2_l + f2_r == 2 & f3_l + f3_r == 2, .(mean(resp), sd(resp)), by = subj]
dt[f1_l + f1_r == 1 & f2_l + f2_r == 1 & f3_l + f3_r == 1, .(mean(resp), sd(resp)), by = subj]
dt[as.numeric(id) %% 11 == 0, mean(resp)]
dt[f1_l - f1_r != 0 & f2_l + f2_r == 1 & f3_l + f3_r == 1, mean(resp), by = .(subj, (f2_l + f3_l == 2) | (f2_r + f3_r == 2))]

# ==========================================================================
# Preprocesses demographics
# ==========================================================================
demo <- merge(subj, demo, by = "id")
demo[, ":=" (age = as.numeric(age), 
             gender = as.factor(gender), 
             edu = as.factor(edu), 
             impaired_vision = as.factor(impaired_vision), 
             vision_corrected = as.factor(vision_corrected),
             attention = as.factor(attention))]
fwrite(demo[, -c("id", "strategy")], sub("X", study, "../../data/processed/demo_studyX.csv"))
fwrite(demo[, c("subj", "strategy")], sub("X", study, "../../data/processed/strategy_studyX.csv"))
