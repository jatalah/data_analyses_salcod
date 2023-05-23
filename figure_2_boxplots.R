library(tidyverse)
library(ggpubr)
rm(list = ls())


# Boxplot FA---------
d_long <- read_csv('data/glm_percentage_data_long.csv')

fa_boxplot <- 
d_long %>%
  ggplot(aes(y = value, name, fill = Treatment)) +
  geom_boxplot(size = .3, alpha = .5) +
  scale_y_continuous(trans = "sqrt") +
  theme_minimal(base_size = 12) +
  coord_flip() +
  labs(y = "%", x = NULL) +
  scale_fill_discrete(name = NULL) +
  theme(legend.position = "bottom") +
  # stat_compare_means(method = "anova", aes(label = after_stat(p.signif)), size = 3) +
  facet_wrap(~Type, scales = "free")

fa_boxplot

ggsave(fa_boxplot, 
       filename = "figures/boxplot_fa.png",
       width = 8,
       height = 4,
       # units = "mm",
       dpi = 900,
       bg = "white")


