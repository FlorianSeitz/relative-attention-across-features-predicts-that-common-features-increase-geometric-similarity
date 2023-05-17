rm(list = ls(all = TRUE))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, R.matlab, ggplot2, ggforce, Rsolnp)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dt <- as.data.table(expand.grid(c = 0:3, d = 0:3))
dt[, cb := exp(-d)]
dt[, cb_w := exp(-d/(c+d))]

theme_set(theme_bw())
ggplot(melt(dt[!(c == 0 & d == 0)], id.vars = c("c", "d")), aes(c, d, fill = value)) +
  geom_raster() +
  geom_text(aes(label = ifelse(value < 1, substr(round(value, 2), 2, 4), 1))) +
  scale_fill_gradient(low = "white", high = "darkgrey", name = "Similarity", limits = c(0, 1)) +
  facet_wrap(~variable, ncol = 2, labeller = labeller(variable = c("cb" = "Without relative attention",
                                                                   "cb_w" = "With relative attention"))) +
  scale_x_continuous(name = "Number of common features", expand = c(0, 0), breaks = 0:5) +
  scale_y_continuous(name = "Number of differing features", expand = c(0, 0), breaks = 0:5) +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.text = element_text(size = 11))
ggsave("../figures/similarity_grid.png", width = 7*.9, height = 3*.9)
