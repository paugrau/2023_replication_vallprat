rm(list = ls())

#Define WD
setwd("~")

#Libraries
library("readxl")
library("tidyverse")
library("sf")
library("ggplot2")
library("viridis")
library("ggspatial")
library("readstata13")
library("ggthemes")
library("sf")
library("extrafont")
library("sysfonts")
library("showtext")

#Define Font Type
font_add(family = "Times", regular = "~/Fonts/times.ttf") # Define folder
showtext_auto()

# Define Theme
theme_set(theme_get()+ theme(legend.position = "bottom", text=element_text(family="Times"),
                             panel.grid = element_blank(),
                             axis.title = element_blank(),
                             axis.text = element_blank(),
                             axis.ticks = element_blank(),
                             panel.background = element_blank()))

# Define Maps
maplegend <- annotation_north_arrow(location = "br", which_north = "true", 
                                    pad_y = unit(0.25, "in"),
                                    style = north_arrow_fancy_orienteering)
maplot <- annotation_scale(location = "br", bar_cols = c("grey60", "white"),line_width = 1.5)

#### Load data ####

#Import maps
map_Muni <- st_read("~/muni.shp")
map_DC <- st_read("~/DistCorts1899-1918.shp")
map_DP <- st_read("~/DistProv1897-1918.shp")

#Import data
df_CST <- read.dta13("~/CPS_ESMRES_Dataset-Cross-Section.dta")
  
  # Two df for each type of elections
  df_DC <- df_CST %>% filter(ElectionType=="Corts")
  df_DP <- df_CST %>% filter(ElectionType=="Diputats Provincials")

#Join maps and data
map_DC <- map_DC %>% rename(Constituency=DistCorts) %>% 
  left_join(.,df_DC, by="Constituency")

map_DP <- map_DP %>% rename(Constituency=DistProv) %>% 
  left_join(.,df_DP, by="Constituency")

# Modify Muni Map
map_Muni <- map_Muni %>% 
  rename(CortsRandom = CrtsRnd,
         ColShock = ColShck,
         Manresa_Dum = X_MnrsDm)
  
################ MAIN TEXT ##########################  
latex <- "~/"
#### Figure 2: Lliga Candidacies ####
# Lower Chamber Elections (F.2.a)
ggplot(map_DC) +
  geom_sf(aes(fill = Lliga), size = 0.015) +
  scale_fill_gradient(low="white", high = "black", na.value = "white",
                      name="Lliga Candidacies \n(1901-1923)")+
  maplegend + maplot
ggsave(filename = paste0(latex,"LligaLCD.pdf"),
       width=3, height=3, units="in", scale=2)

# Provincial Elections (F.2.b)
ggplot(map_DP) +
  geom_sf(aes(fill = Lliga), size = 0.015) +
  scale_fill_gradient(low="white", high = "black", na.value = "white",
                      name="Lliga Candidacies \n(1901-1923)")+
  maplegend + maplot
ggsave(filename = paste0(latex,"LligaProvD.pdf"),
       width=3, height=3, units="in", scale=2)

#### Figure 3: Constituency Colonial Shock ####
# Lower Chamber Elections (F.3.a)
ggplot(map_DC) +
  geom_sf(aes(fill = ColShock), size = 0.015) +
  scale_fill_gradient(low="white", high = "black", na.value = "white",
                      name="Exposure \nColonial Shock")+
  maplegend + maplot
ggsave(filename = paste0(latex,"CS_DCorts.pdf"),
       width=3, height=3, units="in", scale=2)

# Provincial Elections (F.3.b)
ggplot(map_DP) +
  geom_sf(aes(fill = ColShock), size = 0.015) +
  scale_fill_gradient(low="white", high = "black", na.value = "white",
                      name="Exposure \nColonial Shock")+
  maplegend + maplot
ggsave(filename = paste0(latex,"CS_DProv.pdf"),
       width=3, height=3, units="in", scale=2)

############## APPENDIX #################
#### Figure A.1 Constituencies ####
ggplot() + 
  geom_sf(data = map_Muni, mapping = aes(fill = CortsRandom), size = 0.015, show.legend = FALSE)+
  geom_sf(data = map_DP, fill= NA, colour = "black", size = .4) +
  coord_sf(crs = st_crs("EPSG:3043"), xlim = c(240000, 530000), ylim = c(4470000, 4770000), expand = FALSE)+
  maplegend + maplot
ggsave(filename = paste0(latex,"MapaCatalunya_ECS.pdf"),
       width=3, height=3, units="in", scale=2)

#### Figure A.2 Municipality Colonial Shock ####
ggplot(map_Muni) +
  geom_sf(aes(fill = ColShock), size = 0.015) +
  scale_fill_viridis(direction = -1, option = "magma", na.value = "grey85",name="Exposure \nColonial Shock") +
  geom_sf(data = map_DC, fill= NA, colour = "black", size = .4) +
  maplegend + maplot
ggsave(filename = paste0(latex,"CS_Muni.pdf"),
       width=3, height=3, units="in", scale=2)


#### Manresa Assembly Delegates ####

### Figure A.3
# Lower Chamber Elections (Figure A.3.a)
map_DC <- map_DC %>% mutate(logIdMob = log(IdMob))
ggplot(map_DC) +
  geom_sf(aes(fill = logIdMob), size = 0.015) +
  scale_fill_gradient(low="white", high = "black", na.value = "white",
                      name="(log) Manresa \nDelegates")+
  maplegend + maplot
ggsave(filename = paste0(latex,"Manresa_DCorts.pdf"),
       width=3, height=3, units="in", scale=2)

# Provincial Elections (Figure A.3.b)
map_DP <- map_DP %>% mutate(logIdMob = log(IdMob))
ggplot(map_DP) +
  geom_sf(aes(fill = logIdMob), size = 0.015) +
  scale_fill_gradient(low="white", high = "black", na.value = "white",
                      name="(log) Manresa \nDelegates")+
  maplegend + maplot
ggsave(filename = paste0(latex,"Manresa_DProv.pdf"),
       width=3, height=3, units="in", scale=2)

### Figure A.4
# Select Municipality of Manresa
map_Manresa <- map_Muni %>% filter(mun_rst=="Manresa")

ggplot(map_Muni) +
  geom_sf(aes(fill = as.factor(Manresa_Dum)), size = 0.015) +
  scale_fill_manual(values = c("white", "darkgray"),
                    labels = c("No", "Yes"),
                    name = "Manresa Assembly \nDelegates (1892)")+
  stat_sf_coordinates(data = map_Manresa , 
                      aes(shape=mun_rst),
                      size=4,
                      color="black")+
  scale_shape_manual(values=c(7))+
  guides(shape=guide_legend(title="",nrow = 2)) +
  maplot + maplegend
ggsave(filename = paste0(latex,"Manresa_Muni.pdf"),
       width=3, height=3, units="in", scale=2)

#### Lliga Winning MPs ####

#Load data and join to map
df_WinLliga <- read.csv("~/LligaWin.csv")

  #Lower Chamber
  df_WinLliga_DC <- df_WinLliga %>% filter(ElectionType=="Corts")

  map_DC <- map_DC %>%
    left_join(.,df_WinLliga_DC, by="Constituency")

  #Provincial
  df_WinLliga_DP <- df_WinLliga %>% filter(ElectionType=="Prov")
  
  map_DP <- map_DP %>%
    left_join(.,df_WinLliga_DP, by="Constituency")

map_DC %>% filter(Constituency!="Barcelona") %>% # Barcelona has a very large number of Lliga MPs
  ggplot() +
  geom_sf(aes(fill = LligaWin), size = 0.015) +
  scale_fill_gradient(low="white", high = "black", na.value = "white",
                      name="Lliga MPs \n(1901-1923)",
                      breaks=c(0,2,4,6,8,10))+
  maplegend + maplot
ggsave(filename = paste0(latex,"LligaMPs_DCorts.pdf"),
       width=3, height=3, units="in", scale=2)

ggplot(map_DP) +
  geom_sf(aes(fill = LligaWin), size = 0.015) +
  scale_fill_gradient(low="white", high = "black", na.value = "white",
                      name="Lliga MPs \n(1901-1923)",
                      breaks=c(0,2,4,6,8,10,12))+
  maplegend + maplot
ggsave(filename = paste0(latex,"LligaMPs_DProv.pdf"),
       width=3, height=3, units="in", scale=2)
