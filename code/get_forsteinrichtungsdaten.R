
library(here)
library(RODBC)
library(sf)
library(raster)
library(tidyr)

#### access data ####
#------------------------------------------------------------------------------------------------------------------------------------
# connect to access database (Forsteinrichtung)
db <- "K:/aktiver_datenbestand/ni/forst/bestandesinventur/nlf/stand_2017_0615/daten/sachdaten/Landesdatensatz NLF_01.03.2017_BE_DA.accdb"
con <- odbcConnectAccess2007(db, readOnly=TRUE)

# load table and filter
fe_raw <- sqlFetch(con, "tblDatZu")
fe_dat <- fe_raw %>%
  select(DatWab_Key, DatZu_SE, DatZu_HauHilFl, DatZu_Bestschicht, DatZu_BA, DatZu_Hoehe, DatZu_BestGrad, DatZu_MischHa, DatZu_MischProz)


# group data to get one dataset per polygon
# one time for SE min and one time for SE max

se_min <- fe_dat %>%
  # polygons have an ID (WEFLKAZ) which is composed of DatWab_Key and Struktureinheit (SE)
  group_by(DatWab_Key, DatZu_HauHilFl) %>%
  # only trees in the main stand are considered
  filter(DatZu_Bestschicht == 1) %>%
  # assign mininmal number of SE among group to all members
  summarise(SE = min(DatZu_SE),
            DatZu_BA,
            DatZu_MischHa,
            DatZu_MischProz) %>%
  # group additionally by species and sum fraction (sometimes multiple instances per species exist)
  group_by(DatWab_Key, DatZu_HauHilFl, SE, DatZu_BA) %>%
  summarise(DatZu_BA,
            MischHa = sum(DatZu_MischHa),
            MischProz = sum(DatZu_MischProz)) %>%
  distinct()

se_max <- fe_dat %>%
  # polygons have an ID (WEFLKAZ) which is composed of DatWab_Key and Struktureinheit (SE)
  group_by(DatWab_Key, DatZu_HauHilFl) %>%
  # only trees in the main stand are considered
  filter(DatZu_Bestschicht == 1) %>%
  # assign maximal number of SE among group to all members
  summarise(SE = max(DatZu_SE),
            DatZu_BA,
            DatZu_MischHa,
            DatZu_MischProz) %>%
  # group additionally by species and sum fraction (sometimes multiple instances per species exist)
  group_by(DatWab_Key, DatZu_HauHilFl, SE, DatZu_BA) %>%
  summarise(DatZu_BA,
            MischHa = sum(DatZu_MischHa),
            MischProz = sum(DatZu_MischProz)) %>%
  distinct()

# get dataset with most abundant species (min 50% fraction) per area
fe_pure <- bind_rows(list(se_min, se_max)) %>%
  distinct() %>%
  group_by(DatWab_Key, DatZu_HauHilFl, SE) %>%
  # filter to most abundant species ignoring stands where two species have same fraction
  slice_max(n = 1, order_by = MischProz, with_ties = F) %>%
  # filter to stands with species composition above 50%
  filter(MischProz >= 50) %>%
  # merge DatWab_Key and SE, DatWab_Key has tailing 1 in part of access data while always 0 in shape, SE has to be in two digits
  mutate(DatWab_Key_SE = paste(substr(DatWab_Key, 1, 21), "0", "-", sprintf("%02d", SE), sep="") )

# get dataset with most abundant species (multiple if same fraction) per area
fe_dominant <- bind_rows(list(se_min, se_max)) %>%
  distinct() %>%
  group_by(DatWab_Key, DatZu_HauHilFl, SE) %>%
  # filter to most abundant species including stands twice if two species share a fraction
  slice_max(n = 1, order_by = MischProz)

# get fraction per species for each area in one row
fe_all <- bind_rows(list(se_min, se_max)) %>%
  distinct() %>%
  # merge DatWab_Key and SE, DatWab_Key has tailing 1 in part of access data while always 0 in shape, SE has to be in two digits
  mutate(DatWab_Key_SE = paste(substr(DatWab_Key, 1, 21), "0", "-", sprintf("%02d", SE), sep="") )


# fe_wide <- fe_all %>%
#   pivot_wider(names_from = DatZu_BA, values_from = MischProz)


#### geo data ####
#------------------------------------------------------------------------------------------------------------------------------------

# The input file geodatabase
fgdb <- "K:/aktiver_datenbestand/ni/forst/bestandesinventur/nlf/stand_2017_0615/daten/geometriedaten/Geometriedaten.gdb"

# read layer
fc <- sf::st_read(fgdb, layer = "Waldeinteilung")
# cast to multipolygon to avoid problems with multisurface type
fc<- st_cast(fc,  "MULTIPOLYGON")

# filter no forest areas
fc_forest <- fc %>% filter(Uabt_neu != "X")
# buffer with 0 to overcome topology errors
fc_forest <- sf::st_buffer(fc_forest, dist = 0)

# roi extends
ext.sol <- extent(526000,563000,5716000,5747000)
ext.har <- extent(581000,609780,5715000,5758000)
ext.hei <- extent(580000,594000,5844000,5857000)

# crop to rois
fc_forest_sol = st_crop(fc_forest, ext.sol)
fc_forest_har = st_crop(fc_forest, ext.har)
fc_forest_hei = st_crop(fc_forest, ext.hei)

#### statistics ####
#------------------------------------------------------------------------------------------------------------------------------------

# calculate area per species based (only areas with single dominant species >=50% fraction; total shape area is considered to belong to respective species)
fc_pure <- fe_pure %>% inner_join(data.frame(fc_forest),  by = c("DatWab_Key_SE" = "SE_neu"))

spec_area <- fc_pure %>% group_by(DatZu_BA) %>% summarise(spec_area = round(sum(SHAPE_Area)/10000, 2))


spec_area_acc <- fe_all %>% group_by(DatZu_BA) %>% summarise(spec_area = round(sum(MischHa), 2))



fc_sol <- fe_all %>% inner_join(data.frame(fc_forest_sol),  by = c("DatWab_Key_SE" = "SE_neu"))
area_sol <- fc_sol %>% group_by(DatZu_BA) %>% summarise(spec_area = round(sum(MischHa), 1))

fc_har <- fe_all %>% inner_join(data.frame(fc_forest_har),  by = c("DatWab_Key_SE" = "SE_neu"))
area_har <- fc_har %>% group_by(DatZu_BA) %>% summarise(spec_area = round(sum(MischHa), 1))

fc_hei <- fe_all %>% inner_join(data.frame(fc_forest_hei),  by = c("DatWab_Key_SE" = "SE_neu"))
area_hei <- fc_hei %>% group_by(DatZu_BA) %>% summarise(spec_area = round(sum(MischHa), 1))

area_all <- area_sol %>% full_join(area_har, by = "DatZu_BA") %>% full_join(area_hei, by = "DatZu_BA")

names(area_all) <- c("BA", "sol", "har", "hei")

write.csv(area_all, file = here("data/reference/forsteinrichtung", "ba_area_hauptbestand.csv"), row.names = FALSE)

