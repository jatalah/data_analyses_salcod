library(tidyverse)
library(skimr)
library(lubridate)
conflicted::conflicts_prefer(stats::filter)
rm(list = ls())

raw <-
  read_csv('data/salcod_liver_fa_percentage.csv') %>% # datos como porcentaje
  mutate(
    w3 = Linoleic + Eicosatrienoic_3n3 + Eicosapentaenoic + Docosapentaenoic +
      Docosahexaenoic,
    w6 = Linoleic + g_Linolenic_Acid + Eicosadienoic + Eicosatrienoic_3n6 +
      Arachidonic + Docosadienoic + Docosatetraenoic,
    SFA = Lauric + Tridecanoic + Myristic + Pentadecanoic + Palmitic + Heptadecanoic +
      Stearic + Arachidic + Behenic + Lignoceric,
    MFA = Myristoleic + Palmitoleic + cis_10_Heptadecenoic + Oleic + cis_11_Eicosenoic +
      Nervonic + Elaidic + Vaccenic + Erucic,
    PUFA = Linoleic + g_Linolenic_Acid + Linolenic + Eicosadienoic + Eicosatrienoic_3n6 +
      Eicosatrienoic_3n3 + Arachidonic + Docosadienoic + Eicosapentaenoic + Docosatetraenoic +
      Docosapentaenoic + Docosahexaenoic
  ) %>%
  mutate(Fulton = 100 * Weight_g / (Length_cm ^ 3), .after = "Weight") %>% 
  mutate(Treatment = if_else(Distance_farm>4e3, "Control", "Impact"),
         Location = if_else(Fjord == "Bergsfjord" & Latitude >70.3, "Outer Bergsfjord", Fjord),
         Age = replace_na(Age, 3), .after = "Treatment",
         Date = dmy(Date)) %>% # NA imputed using L vs Age relationship
  select(-Treatment_2) %>% 
  mutate(Treatment = if_else(ID %in% c("F_248", "F_239"), "Impact", Treatment)) %>% 
  dplyr::filter(CC > 0.7) # filter coastal cod

write_csv(raw, 'data/raw_data_percentage.csv')

raw %>% group_by(Treatment) %>% count()

# Age length relationship
ggplot(raw, aes(Age, Length_cm)) +
  geom_point() +
  geom_smooth(method = "gam")


# data summaries---------
skim(raw)
names(raw)
dim(raw)
table(raw$Treatment)
table(raw$Location)
summary(raw$Distance_farm)
raw %>% 
  group_by(Season) %>% 
  count()

summary(raw$Date)
raw %>% distinct(Season)


# data for glms--------
d <- 
  read_csv('data/raw_data_percentage.csv') %>% 
  select(
    ID,
    Season,
    Location,
    Fjord,
    Treatment,
    Sex,
    Maturity,
    Age,
    Fulton,
    w3:PUFA,# FA indices
    Oleic,
    Linoleic,
    Linolenic,
    Docosahexaenoic,
    Eicosapentaenoic,
    Arachidonic
  ) %>%
  write_csv('data/glm_percentage_data.csv')


d_long <-
  d %>%
  pivot_longer(
    cols = c(
      w3:PUFA,
      Oleic,
      Linoleic,
      Linolenic,
      Docosahexaenoic,
      Eicosapentaenoic,
      Arachidonic
    )
  ) %>% 
  mutate(Type = if_else(
    name %in% c(
      "Oleic",
      "Linoleic",
      "Linolenic",
      "Docosahexaenoic",
      "Eicosapentaenoic",
      "Arachidonic"
    ),
    "FA",
    "FA index"
  )) %>% 
  write_csv('data/glm_percentage_data_long.csv')


d_long %>% distinct(name)
