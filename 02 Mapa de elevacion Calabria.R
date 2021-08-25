library(tidyverse)
library(raster)
library(openxlsx)
library(sf)
library(ggspatial)
library(gridExtra)
library(ggrepel)
library(ggplot2)
library(colorspace)
library(cowplot)
Italia      <- getData('GADM', country='ITALY', level=2) %>% st_as_sf()
Calabria    <- subset(Italia, NAME_1  == "Calabria")
ITALY_Alt   <- getData("alt", country='ITALY', mask=TRUE)
Calabria_c  <- cbind(Calabria, st_coordinates(st_centroid(Calabria$geometry)))

Calabria_ch   <- crop(ITALY_Alt, Calabria)
Calabria_ch     <- Calabria_ch  <- mask(Calabria_ch , Calabria)
plot(Calabria_ch )
dem.p          <-  rasterToPoints(Calabria_ch )
df             <-  data.frame(dem.p)
colnames(df) = c("lon", "lat", "alt")
summary(df$alt)
cortes <- c(0,500, 1000,1500,1800,  2101)

A<- ggplot()+
  geom_sf(data= Italia, fill="white")+
  geom_raster(data = df, aes(x=lon, y=lat, fill = alt) )+
  scale_fill_distiller(palette   = "RdYlGn",
                       na.value = 'white',breaks = cortes ,
                       labels = c("[0 - 499] ","[500 - 999]","[1000-1499]", "[1500-1799]", "[1800-1999]", "[2000-2101]"))+
  theme_bw()+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  guides(fill = guide_legend(title.position = "right",direction = "vertical",
                             title.theme = element_text(angle = 90, size = 9, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Elevation \n(m.s.n.m)'))+
  geom_sf(data= Calabria, fill=NA)+
  geom_point(data = Calabria_c, aes(x=X, y=Y),color = "red") +
  geom_text_repel(data = Calabria_c, aes(x=X, y=Y, label = NAME_2),
                       size = 3.5,box.padding = 9, segment.angle = 30, fontface = "bold")+
  theme(legend.position = c(0.85,0.20),
       panel.grid.major = element_line(color = gray(.5),
                                       linetype = "dashed", size = 0.5),
       legend.background = element_blank(),
       legend.text = element_text(size=7,face=2),
       legend.title = element_text(size=7,face=2),
       panel.background = element_rect(fill = "aliceblue"))+
  coord_sf(xlim = c(15.2,17.9), ylim = c(37.8,40.3),expand = FALSE)+
  labs(title = "A)",# añadir titulo
       caption = "Muestra elaborada por @Gorky ")


B <- ggplot()+
  geom_sf(data= Italia, fill="white")+
  geom_sf(data= Calabria, fill="red")+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= 1))

W <-ggdraw() + draw_plot(A) + draw_plot(B, x = 0.77, y = 0.67, width = .25, height = .25)
#------------------------------------------------------------------------

Bio_pre     <-getData("worldclim", var = "prec",res=0.5,lon=15, lat=40) 
Bio_tem     <-getData("worldclim", var = "tmean",res=0.5,lon=15, lat=40)
plot(Bio_tem[[2]] )                                                  

Precipitación_anual   <- crop(Bio_pre[[2]], Calabria)
Precipitación_anual   <- Precipitación_anual  <- mask(Precipitación_anual , Calabria)
plot(Precipitación_anual)
dem.pre          <-  rasterToPoints(Precipitación_anual )
df_pre             <-  data.frame(dem.pre)
colnames(df_pre) = c("lon", "lat", "prec")
summary(df_pre$prec)

Temperatura_anual   <- crop(Bio_tem[[2]] , Calabria)
Temperatura_anual  <- Temperatura_anual  <- mask(Temperatura_anual , Calabria)
plot(Temperatura_anual)
dem.tem          <-  rasterToPoints(Temperatura_anual )
df_tem             <-  data.frame(dem.tem)
colnames(df_tem) = c("lon", "lat", "tem")
summary(df_tem$tem)


C <-ggplot()+
    geom_sf(data= Italia, fill="white")+
    geom_raster(data = df_pre, aes(x=lon, y=lat, fill = prec) )+
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "GnBu"), 
                         na.value = 'white', breaks = c(60,80,100,114),
                         labels = c("[51 - 59] ","[60 - 79]","[80-99]", "[100-114]"))+
    guides(fill = guide_legend(title.position = "top",direction = "vertical",
           title.theme = element_text(angle = 0, size = 9, colour = "black"),
           barheight = .5, barwidth = .95,
           title.hjust = 0.5, raster = FALSE,
           title = 'Precipitation\n(mm)'))+
    geom_sf(data= Calabria, fill=NA)+
    geom_point(data = Calabria_c, aes(x=X, y=Y),color = "red") +
    geom_text_repel(data = Calabria_c, aes(x=X, y=Y, label = NAME_2),
    size = 3.5,box.padding = 9, segment.angle = 30, fontface = "bold")+
    theme_bw()+
    scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
   scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
    annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
    ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
    coord_sf(xlim = c(15.2,17.9), ylim = c(37.8,40.3),expand = FALSE)+
    theme(legend.position = c(0.85,0.20),
    panel.grid.major = element_line(color = gray(.5),
          linetype = "dashed", size = 0.5),
     legend.background = element_blank(),
     legend.text = element_text(size=7,face=2),
     legend.title = element_text(size=7,face=2),
     panel.background = element_rect(fill = "aliceblue"))+
     labs(title = "B)",# añadir titulo
          caption = "Muestra elaborada por @Gorky ")
                                                                             
D <-ggplot()+
  geom_sf(data= Italia, fill="white")+
  geom_raster(data = df_tem, aes(x=lon, y=lat, fill = tem) )+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "Spectral"), 
                       na.value = 'white', breaks = c(-30, 0, 30,60,90,129),
                       labels = c("[-10 - -29] ","[-30 - 0]","[0 - 29]", "[30 - 59]",
                                  "[60 - 89]", "[90 - 129]"))+
  guides(fill = guide_legend(title.position = "top",direction = "vertical",
                             title.theme = element_text(angle = 0, size = 9, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Temperature \n(°C)'))+
  geom_sf(data= Calabria, fill=NA)+
  geom_point(data = Calabria_c, aes(x=X, y=Y),color = "red") +
  geom_text_repel(data = Calabria_c, aes(x=X, y=Y, label = NAME_2),
                  size = 3.5,box.padding = 9, segment.angle = 30, fontface = "bold")+
  theme_bw()+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  coord_sf(xlim = c(15.2,17.9), ylim = c(37.8,40.3),expand = FALSE)+
  theme(legend.position = c(0.85,0.20),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        legend.background = element_blank(),
        legend.text = element_text(size=7,face=2),
        legend.title = element_text(size=7,face=2),
        panel.background = element_rect(fill = "aliceblue"))+
  labs(title = "C)",# añadir titulo
       caption = "Muestra elaborada por @Gorky ")

Ww <-ggdraw() + draw_plot(C) + draw_plot(B, x = 0.77, y = 0.67, width = .25, height = .25)
Www <-ggdraw() + draw_plot(D) + draw_plot(B, x = 0.77, y = 0.67, width = .25, height = .25)


ggsave(plot = W ,"Mpas/Italy_elevacion.png", units = "cm", width = 21,height = 29, dpi = 900) 
ggsave(plot = Ww ,"Mpas/Italy_presipitacion.png", units = "cm", width = 21,height = 29, dpi = 900) 
ggsave(plot = Www ,"Mpas/Italy_Temperatura.png", units = "cm", width = 21,height = 29, dpi = 900) 








