library(tidyverse)


# Vall-Prat data

CPS_ESMRES_Dataset_Panel <- rio::import("vallprat2022/CPS_ESMRES_Dataset-Panel.dta")
vall_shp <- sf::read_sf("vallprat2022/DistCorts1899-1918.shp")

# Beltran Tapia data

data_EEH_gis <- readRDS("btmg2018/data_EEH_gis.rds")%>%
  filter(province%in%c("Barcelona", "Lérida", "Girona", "Tarragona"))%>%
  sf::st_as_sf()%>%
  sf::st_transform(sf::st_crs(vall_shp))

joint_shp <- vall_shp%>%
  sf::st_join(data_EEH_gis)

joint_intersection <- vall_shp%>%
  sf::st_intersection(data_EEH_gis)%>%
  select(CortsId, id)

### TRY 

datalist<- list()

for (i in 1:nrow(vall_shp)) {
  # ... make some data
  dat <- sf::st_intersection(vall_shp, data_EEH_gis[1:nrow(data_EEH_gis), ])%>%
    mutate(area1=as.numeric(sf::st_area(geometry)))%>%
    group_by(id)%>%
    mutate(area2=as.numeric(sum(area1)))%>%
    ungroup()%>%
    mutate(area_pct=as.numeric(area1/area2))
  datalist[[i]] <- dat # add it to your list
}

big_data <- do.call(rbind, datalist)

big_data1 <- big_data%>%
  unique()%>%
  mutate(area_pct=as.numeric(area_pct))%>%
  mutate(area_pct_95=case_when(area_pct>=0.95~1,
                               area_pct<=0.05~0,
                               TRUE~area_pct),
         area_pct_90=case_when(area_pct>=0.9~1,
                               area_pct<=0.1~0,
                               TRUE~area_pct))

big_data_95 <- big_data1%>%
  filter(area_pct_95>0)

big_data%>%ggplot()+geom_sf(aes(fill=ALT_MEDIAN))


# Apply weights
big_data_weighted <- big_data_95%>%
  select(-area1, -area2, -area_pct, -area_pct_90,-geometry)%>%
  rename(weight=area_pct_95)%>%
  sf::st_drop_geometry()%>%
  select(OBJECTID, DistCorts, commons:weight)%>%
  group_by(OBJECTID, DistCorts)%>%
  summarise_at(vars(commons:mf_esc_pub_ele_inc_rel),
               funs(sum(. * weight)/sum(weight)))%>%
  rename(Constituency=DistCorts)

write_rds(big_data_weighted, "btmg_weighted.rds")

# Join both datasets

joint <- big_data_weighted%>%
  left_join(vall_shp)%>%
  sf::st_as_sf()%>%
  right_join(CPS_ESMRES_Dataset_Panel%>%filter(ElectionType==1))

write_rds(joint, "joint_data.rds")
