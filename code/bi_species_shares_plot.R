spec_bi <- read.csv("data/reference/bi/bi_basal-area_by_species-roi.csv", stringsAsFactors=TRUE)

spec_fe <- read.csv("data/reference/forsteinrichtung/ba_area_hauptbestand.csv", stringsAsFactors=TRUE)
names(spec_fe) <- names(spec_bi)
spec_fe <- spec_fe %>%
  mutate(sol = sol/ sum(sol, na.rm = TRUE)*100,
         har = har/ sum(har, na.rm = TRUE)*100,
         hei = hei/ sum(hei, na.rm = TRUE)*100)


spec_code <- read.csv2("data/reference/ba_code_ni_bdat.csv", stringsAsFactors=TRUE)



fe.long <- spec_fe %>% gather(aoi, fe, -ba)

bi.long <- spec_bi %>% gather(aoi, bi, -ba)

dat <- fe.long %>%
  full_join(bi.long, by = c("ba", "aoi")) %>%
  left_join(spec_code %>% select(id_ni, gruppe), by = c("ba" = "id_ni"))

dat <- dat %>%
  group_by(aoi, gruppe) %>%
  summarise(bi = sum(bi, na.rm = T),
            fe = sum(fe, na.rm = T)) %>%
  gather(data, anteil, c(bi, fe))


# plot bars
dat %>%
  ggplot(aes(x=as.factor(gruppe), y= anteil, fill = aoi)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(data ~ .) +
  theme_classic()

