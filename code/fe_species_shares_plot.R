library(tidyverse)
library(here)
# library(dplyr)
# library(ggplot2)

spec_bi <- read.csv("data/reference/bi/bi_basal-area_by_species-roi.csv", stringsAsFactors=TRUE)

spec_fe <- read.csv("data/reference/forsteinrichtung/fe_species_composition_by_roi.csv", stringsAsFactors=TRUE)


#caclculate distribution per group and site
dat <- spec_fe %>%
  filter(gruppe %in% c("BU", "DGL", "FI", "KI", "LAE", "EI")) %>%
  droplevels() %>%
  mutate(gruppe = fct_relevel(gruppe, c("BU", "DGL", "FI", "KI", "LAE", "EI"))) %>%
  group_by(site) %>%
  mutate(perc_ShapeHa = perc_ShapeHa/ sum(perc_ShapeHa, na.rm = TRUE)*100,
         perc_MischHa = perc_MischHa/ sum(perc_MischHa, na.rm = TRUE)*100)%>%
  group_by(site, gruppe) %>%
  summarise(perc_MischHa = sum(perc_MischHa),
            perc_ShapeHa = sum(perc_ShapeHa))


write.csv(dat, file = here("data/reference/forsteinrichtung", "fe_species_composition_by_roi_aggregated.csv"), row.names = FALSE)


# palette  in the order of reference data levels
pal.class <- c("#bf7013","#a5027c","#4066aa","#94a6b0","#e20030", "#fecc00")

# plot perc_MischHa
dat %>%
  ggplot(aes(x=as.factor(gruppe), y= perc_MischHa, fill = gruppe)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values= pal.class) +
  facet_wrap(site ~ .) +
  theme_classic()


# plot perc_ShapeHa
dat %>%
  ggplot(aes(x=as.factor(gruppe), y= perc_ShapeHa, fill = gruppe)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values= pal.class) +
  facet_wrap(site ~ .) +
  theme_classic()
