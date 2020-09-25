
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sp)
library(raster)

#read bi data
bi <- read.csv("data/reference/bi/bi_ready.csv", stringsAsFactors=TRUE)


# calculate relative species proportions
# function
calc_species_shares <- function(data){
  data %>%
    mutate(vfm_total = sum(vfm),
           vfm_ha_total = sum(vfm_ha),
           ba = pi*(0.5*dbh)^2,
           ba_total = sum(ba)) %>%
    group_by(species) %>%
    summarise(percentage_vfm = sum(vfm)/min(vfm_total)*100,
              percentage_vfm_ha = sum(vfm_ha)/min(vfm_ha_total)*100,
              percentage_ba = sum(ba)/min(ba_total)*100)
}
# apply
total <- calc_species_shares(data.frame(bi))


# plot bars
total.long <- total %>% gather(metric, value, -species)

total.long %>% ggplot(aes(x=as.factor(species), y= value, fill=metric)) +
  geom_bar(stat="identity", position=position_dodge())


#### statistics per region ####
#----------------------------------------------------------------------------------------------------

# Create Spatial Points Dataframe from inventory plot data
epsg <- "31467"
data_crs <- CRS(paste("+init=EPSG:",epsg, sep = ""))

coordinates(bi) <- c("datph2_rw","datph2_hw")
proj4string(bi) <- data_crs

# reproject
epsg <- "25832"
user_crs <- CRS(paste("+init=EPSG:",epsg, sep = ""))
bi <- spTransform(bi, user_crs)


# roi extends
ext.sol <- extent(526000,563000,5716000,5747000)
ext.har <- extent(581000,609780,5715000,5758000)
ext.hei <- extent(580000,594000,5844000,5857000)

# crop to rois
bi_sol = crop(bi, ext.sol)
bi_har = crop(bi, ext.har)
bi_hei = crop(bi, ext.hei)

# calculate relative species proportions
# function
calc_species_shares_ba <- function(data){
  data %>%
    mutate(ba = pi*(0.5*dbh)^2,
           ba_total = sum(ba)) %>%
    group_by(species) %>%
    summarise(percentage_ba = sum(ba)/min(ba_total)*100)
}

#apply
ba.sol <- calc_species_shares_ba(data.frame(bi_sol))
ba.har <- calc_species_shares_ba(data.frame(bi_har))
ba.hei <- calc_species_shares_ba(data.frame(bi_hei))

ba.all <- ba.sol %>%
  full_join(ba.har, suffix = c(".sol", ""), by = "species") %>%
  full_join(ba.hei, suffix = c(".har", ".hei"), by = "species")

names(ba.all) <- c("ba", "sol", "har", "hei")

write.csv(ba.all, file = here("data/reference/bi", "bi_basal-area_by_species-roi.csv"), row.names = FALSE)





