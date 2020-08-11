library(RODBC)
library(dplyr)

# get data from data base
con <- odbcConnectAccess("C:/Users/jwiesehahn/Arbeit/data/inventory/bwi2012/bwi_all.mdb")

query <- "
SELECT b3_wzp.Tnr, b3_wzp.Enr, b3_wzp.Ba, baanteile.AnteilBa, b0_ecke.Bl
FROM ((b3_wzp LEFT JOIN (SELECT
  Tnr, Enr, Ba, Schicht, AnteilBa
  FROM b3_bestock_baanteile
  WHERE Schicht = 1)  AS baanteile ON (b3_wzp.Ba = baanteile.Ba) AND (b3_wzp.Enr = baanteile.Enr) AND (b3_wzp.Tnr = baanteile.Tnr))
  LEFT JOIN b0_ecke ON (b3_wzp.Enr = b0_ecke.Enr) AND (b3_wzp.Tnr = b0_ecke.Tnr))
  LEFT JOIN b3_ecke_w ON (b3_wzp.Enr = b3_ecke_w.Enr) AND (b3_wzp.Tnr = b3_ecke_w.Tnr)
WHERE (((baanteile.AnteilBa) Is Not Null) AND ((b3_wzp.Pk) In (0,1)) AND ((b3_wzp.Bs) In (0,1)) AND ((b0_ecke.InvE3)=1) AND ((b3_ecke_w.Wa) In (3,4,5)));
"

data <- sqlQuery( con , query)


# get data from meta data base
con_meta <- odbcConnectAccess("C:/Users/jwiesehahn/Arbeit/data/inventory/bwi20150320_alle_metadaten/bwi20150320_alle_metadaten.mdb")

query_meta <- "
SELECT ICode AS Ba, Zu_BaBWI AS Ba_Gr, BaGr
FROM x_ba"

data_meta <- sqlQuery( con_meta , query_meta)


# get coordinate data
bwi_koordinaten_ni <- read.csv2("C:/Users/jwiesehahn/Arbeit/data/inventory/bwi_koordinaten_ni.csv")
bwi_koordinaten_ni <- bwi_koordinaten_ni %>% select(tnr,enr, GK3_Rechts, GK3_Hoch) %>% rename(Tnr=tnr, Enr=enr)


# merge data frames
dat_bwi <- merge(data, data_meta, by="Ba")
dat_bwi <- merge(dat_bwi, bwi_koordinaten_ni, by= c("Tnr", "Enr"))

#remove spaces and convert to factor
dat_bwi$BaGr <- gsub(" ", "", dat_bwi$BaGr, fixed = TRUE)
dat_bwi$BaGr <- as.factor(dat_bwi$BaGr)


#group data and get values per tree group for pure stands in Niedersachsen
bwi <- dat_bwi %>%
  group_by(Tnr, Enr, Ba_Gr, BaGr, Bl, GK3_Rechts, GK3_Hoch) %>%
  summarise(ba_anteil = mean(AnteilBa)) %>%
  filter(BaGr %in% c("FI", "KI", "LAE", "DGL", "EI", "BU")) %>%
  filter(ba_anteil > 0.75) %>%
  filter(Bl == 3)

# save file
dir.create("./data/reference/bwi")
write.csv(bwi, "./data/reference/bwi/ref_ni.csv")

odbcCloseAll()
