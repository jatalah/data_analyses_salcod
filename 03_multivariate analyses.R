library(ggord)
library(tidyverse)
library(vegan)
library(ggpubr)
rm(list = ls())

# read data ----------
raw <- read_csv('data/raw_data_percentage.csv')

t_multivariate_data <-
  raw %>%
  select(Lauric:Docosahexaenoic) %>%
  mutate_all(
    .funs = function(x)
      sqrt(sqrt((x)))
  )

cap <-
  dbrda(
    t_multivariate_data ~ Treatment + Season + Sex + Maturity + Age + Fulton ,
    data = raw,
    dist = "bray"
  )

rds_biplot <-
  ggord(
    cap,
    repel = T,
    grp_in = raw$Treatment,
    grp_title = NULL,
    poly = F,
    alpha = 1,
    ellipse = T,
    lty = 1,
    arrow = F,
    vec_ext = .3,
    max.overlaps = 20,
    size = 3,
    veclsz = 0.5,
    txt = 3
  ) +
  theme_minimal(base_size = 10)

rds_biplot

ord <- prcomp(t_multivariate_data)

fa_pca <-
  ggord(
    ord,
    repel = T,
    grp_in = raw$Treatment,
    grp_title = NULL,
    poly = F,
    alpha = 1,
    ellipse = T,
    lty = 1,
    arrow = F,
    vec_ext = 2,
    max.overlaps = 20,
    size = 3,
    veclsz = 0.5,
    txt = 3
  ) +
  theme_minimal(base_size = 10)

fa_pca

biplots <-
  ggarrange(
    fa_pca,
    rds_biplot,
    ncol = 2,
    common.legend = T,
    align = 'hv',
    legend = "bottom",
    labels = "auto"
  )


biplots

## Figure biplots-------------
ggsave(
  biplots,
  filename = "figures/biplots.png",
  width = 7.3,
  height = 3.3,
  bg = "white",
  # units = "mm",
  dpi = 900
)


# DistLM----
# conditional test ----
permanova_fa <-
  adonis2(
    t_multivariate_data ~ Treatment + Season + Fjord + Sex + Fulton + Maturity + Age,
    na.action = "na.omit",
    data = raw,
    method = 'bray',
    by = 'terms',
    permutations = 999
  )

permanova_fa %>%
  as_tibble(rownames = 'Term') %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  rename(P = `Pr(>F)`, SS = SumOfSqs) %>%
  write_csv('tables/conditional_test.csv', na = "")

# marginal test. The marginal effect is the effect of a particular term when all other model terms are included in the model
distlm_fa <-
  adonis2(
    t_multivariate_data ~ Treatment + Season + Fjord + Sex + Fulton + Maturity + Age,
    na.action = "na.omit",
    data = raw,
    method = 'bray',
    by = 'margin',
    permutations = 999
  )

distlm_fa %>%
  as_tibble(rownames = 'Term') %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  rename(P = `Pr(>F)`, SS = SumOfSqs) %>%
  write_csv('tables/marginal_test.csv', na = "")


# overall test ----
adonis2(
  t_multivariate_data ~ Treatment + Season + Fjord + Sex + Fulton + Maturity + Age,
  na.action = "na.omit",
  data = raw,
  method = 'bray',
  by = NULL,
  permutations = 999
)

# indicators species
library(indicspecies)
ind_spp <-
  multipatt(
    t_multivariate_data,
    raw$Treatment,
    duleg = T,
    control = how(nperm = 999)
  )
summary(ind_spp)

# ind species table
ind_spp_table <-
  ind_spp$sign %>%
  rownames_to_column ("taxa") %>%
  dplyr::filter(p.value <= 0.1) %>%
  mutate(Treatment = fct_recode(factor(index), "Control" = "1", "Impacted" = "2")) %>%
  select(Treatment, Taxa = taxa, stat, p.value) %>%
  arrange(Treatment, p.value) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

ind_spp_table %>% dplyr::filter(Treatment == "Control") %>% select(Taxa) %>% deframe()
ind_spp_table %>% dplyr::filter(Treatment == "Impacted") %>% select(Taxa) %>% deframe()
write_csv(ind_spp_table, 'tables/ind_spp_table.csv')

## Calculate multivariate dispersions
mod <- betadisper(vegdist(t_multivariate_data), raw$Treatment)

## Perform test
anova(mod)

## Plot the groups and distances to centroids on the
plot(mod, ellipse = T, hull = F)

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)