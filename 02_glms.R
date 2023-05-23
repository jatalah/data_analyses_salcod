library(tidyverse)
library(broom)
library(car) # for levene test
library(broom.mixed)
library(glmmTMB)
library(mixedup) # for extract variance components
rm(list = ls())

# read data -------
# d <- read_csv('data/glm_percentage_data.csv')
d_long <- read_csv('data/glm_percentage_data_long.csv')


# Transformation for beta regression-----
betareg_scaler <-
  function(y) {
    n <- length(!is.na(y))
    (y / 100 * (n - 1)  + 0.5) / n
  }

# fit a beta glmmm to six FA and five indices. Location is nested in treatment. The diag term is to specify heterogeneity of variance between Treatment. The control argument is for convergences problems ----- ----------
glms <-
  d_long %>%
  group_by(name, Type) %>%
  mutate(value = betareg_scaler(value)) %>%
  nest() %>%
  mutate(
    glms = map(
      data,
      ~ glmmTMB(
        value  ~
          Season + Treatment + Sex + Maturity + Age + Fulton +
          (1 | Location) +
          # (1|Location: Treatment) +
          diag(Treatment + 0 | ID),
        beta_family(link = "logit"),
        control = glmmTMBControl(optimizer = optim,
                                 optArgs = list(method = "BFGS")),
        data = .x
      )
    ),
    levene = map(data, ~ leveneTest(value ~ Treatment, .x)),
    var_com = map(glms, extract_vc),
    glms_table = map(glms, ~ tidy(.x, exponentiate  = F))
  )

# summary Treatment
glms %>%
  select(glms_table) %>%
  unnest(cols = c(glms_table)) %>%
  dplyr::filter(term == "TreatmentImpact")


glms %>%
  select(glms_table) %>%
  unnest(cols = c(glms_table)) %>%
  ungroup() %>%
  dplyr::filter(effect == "fixed") %>%
  select(
    Variable = name,
    Predictor = term,
    estimate,
    std.error,
    p = p.value
  ) %>%
  mutate(across(is.numeric, ~ round(., 2))) %>%
  # mutate(across(where(is.numeric), round, 2))
  write_csv('tables/table_glmm.csv')


# levene tests----
glms %>%
  select(levene) %>%
  unnest(cols = c(levene)) %>%
  drop_na()

# variance components---
glms %>%
  select(var_com) %>%
  unnest(cols = c(var_com)) %>%
  dplyr::filter(effect != "Intercept") %>%
  ggplot(aes(effect, variance)) +
  geom_boxplot(aes(effect, sd)) +
  geom_point(aes(color = name))

# write_csv('tables/results_glmm.csv')


# variance random effects
glms %>%
  select(glms_table) %>%
  unnest(cols = c(glms_table)) %>%
  dplyr::filter(group == "ID" & str_detect(term, "sd"))  %>%
  ggplot(aes(term, estimate)) +
  geom_boxplot() +
  geom_point(aes(color = name))

# Figure coefficient plots GLMMs --------------
glmm_results <-
  glms %>%
  select(glms_table) %>%
  unnest(cols = c(glms_table)) %>%
  write_csv('tables/results_glmm.csv')

coeff_plot_fa_glms <-
  glmm_results %>%
  dplyr::filter(!str_detect(term, "(Intercept)|sd_|cor_")) %>%
  ggplot(aes(term, color = name)) +
  geom_pointrange(
    aes(
      y = estimate,
      ymin = estimate + std.error,
      ymax = estimate - std.error
    ),
    position = position_dodge2(width = .4),
    alpha = .6,
    size = .5
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  geom_hline(yintercept = 0,
             lty = 2,
             linewidth = .1) +
  # scale_color_viridis_d(name = "Fatty acid", option = "D") +
  labs(x = NULL, y = "Estimate") +
  facet_grid( ~ Type)

coeff_plot_fa_glms


ggsave(
  coeff_plot_fa_glms,
  filename = "figures/coeff_plot_fa_glms.png",
  width = 7,
  height = 3.5,
  dpi = 900,
  bg = "white"
)
