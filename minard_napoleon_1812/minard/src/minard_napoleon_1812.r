# Image sources:


#https://www.datavis.ca/gallery/re-minard.php
#https://www.datavis.ca/gallery/minard/1812-2.jpg
#https://www.datavis.ca/gallery/minard/napon.gif
#https://www.datavis.ca/gallery/minard/minard-odt.jpg

library(plotly)
library(tidyverse)
troops <- read.table("../data/troops.txt", header=T)
cities <- read.table("../data/cities.txt", header=T)
temps <- read.table("../data/temps.txt", header=T)
temps$date <- as.Date(strptime(temps$date,"%d%b%Y"))

library(maps)
borders <- data.frame(map("world", xlim=c(10,50), ylim_temp=c(40, 80), plot=F)[c("x","y")])

xlim <- scale_x_continuous(limits = c(24, 39))
ylim <- scale_y_continuous(limits = c(54, 56))
ylim_temp <- scale_y_continuous(limits = c(-40, 0))

# data prep ----
troops$groupdirection <- paste(troops$direction, troops$group, sep="_")

troops_text <- as.data.frame(do.call(cbind, approx(troops$long, troops$lat, n = 200)))
troops_text$survivors <- ""
troops_text$direction <- ""
troops_text$group <- ""
troops_text$groupdirection <- ""
colnames(troops_text) <- colnames(troops)
troops_text <- rbind(troops, troops_text)
print(head(troops))
print(head(troops_text))

cities_text <- as.data.frame(do.call(cbind, approx(cities$long, cities$lat, n = 200)))
cities_text$city <- ""
colnames(cities_text) <- colnames(cities)
cities_text <- rbind(cities, cities_text)
print(head(cities))
print(head(cities_text))

# main figure ----

library(ggrepel)
options(repr.plot.width = 2, repr.plot.height = 2)
options(scipen=10000) # instead of scientific notation
p1 <- 
  ggplot(data = troops, aes(x = long, y = lat, group=1)) +
  geom_line(data = subset(troops, direction == "R"), 
            aes(group=groupdirection,  
                size= survivors, 
                colour = direction)) + 
  geom_line(data = subset(troops, direction == "A"), 
            aes(group=groupdirection,  
                size= survivors, 
                colour = direction)) + 
  scale_color_manual(values=c("#e7d4a7", "grey")) +
  geom_point(data = cities, aes(size = 1e2), color = "blue", show.legend = F ) +
  geom_text_repel(data =  troops_text, aes(label = survivors), size = 2, force = 10,
                  direction = "y",
                  angle = 35,
                  box.padding = unit(0.5, "lines"))+
  geom_label_repel(data = cities_text, aes(label = city),
                   fill = "white", size = 3, force =15, direction = "y",
                   box.padding = unit(0.5, "lines"),
                   point.padding = unit(0.5, "lines"))+ 
  guides(#color = "none", 
    #size = "none"
  )



p1




# data prep ----

temps$tempC <- "°C"
temps$tempC <- paste(temps$temp, temps$tempC, sep="")
temps

temps_splineint <- as.data.frame(spline(temps$long, temps$temp))

p2 <- ggplot(data=temps, aes(x=long, y=temp))+
  geom_line(aes(color = temp ), size = 2) +
  scale_color_gradientn(
    colours = c("red", "blue"),
    limits = c(-40, 0))+
  geom_point(aes(x = long, y = temp, colour = temp), size = 3) +
  stat_smooth(aes(y=temp, x=long), formula = y ~ s(x, k = 24), method = "gam", se = FALSE)+
  geom_text_repel(data = temps, aes(label = paste(month, day, "\n",tempC)),
                  size = 4,  direction = "y",
                  box.padding = unit(0.5, "lines"),
                  point.padding = unit(0.5, "lines"),
                  nudge_y = -40)+ 
  xlim + 
  ylim_temp +
  guides(#color = "none"
  )
p2


library(cowplot)
theme_set(theme_minimal())
plot_grid(
  p1 +
    theme(legend.justification = "left")
  , 
  p2 + 
    theme(#legend.text = element_blank(), legend.title = element_blank()
    )
  , align = "hv", ncol = 1
)















# plotly ----
# boarder segments ----
df_A1 <- borders
x2 <- c(df_A1$x[1], rep(df_A1$x[2:(NROW(df_A1)-1)], each=2),  df_A1$x[NROW(df_A1)])
y2 <- c(df_A1$y[1], rep(df_A1$y[2:(NROW(df_A1)-1)], each=2),  df_A1$y[NROW(df_A1)])
df_A2.3 <- data.frame(x2,y2)

df_A2.3$segnum <- rep(1:(NROW(df_A1)-1), each=2)
df_A2.3$segw <- rep(df_A1$survivors[1:(NROW(df_A1)-1)], each=2)
borders <- df_A2.3

# group 1 retreat ----
df_R1 <- subset(troops, direction == "R") %>% filter(group == 1)
long2 <- c(df_R1$long[1], rep(df_R1$long[2:(NROW(df_R1)-1)], each=2),  df_R1$long[NROW(df_R1)])
lat2 <- c(df_R1$lat[1], rep(df_R1$lat[2:(NROW(df_R1)-1)], each=2),  df_R1$lat[NROW(df_R1)])
df_R2 <- data.frame(long2,lat2)

df_R2$segnum <- rep(1:(NROW(df_R1)-1), each=2)
df_R2$segw <- rep(df_R1$survivors[1:(NROW(df_R1)-1)], each=2)

# group 2 retreat ----
df_R1 <- subset(troops, direction == "R") %>% filter(group == 2)
long2 <- c(df_R1$long[1], rep(df_R1$long[2:(NROW(df_R1)-1)], each=2),  df_R1$long[NROW(df_R1)])
lat2 <- c(df_R1$lat[1], rep(df_R1$lat[2:(NROW(df_R1)-1)], each=2),  df_R1$lat[NROW(df_R1)])
df_R2.2 <- data.frame(long2,lat2)

df_R2.2$segnum <- rep(1:(NROW(df_R1)-1), each=2)
df_R2.2$segw <- rep(df_R1$survivors[1:(NROW(df_R1)-1)], each=2)

# group 3 retreat ----
df_R1 <- subset(troops, direction == "R") %>% filter(group == 2)
long2 <- c(df_R1$long[1], rep(df_R1$long[2:(NROW(df_R1)-1)], each=2),  df_R1$long[NROW(df_R1)])
lat2 <- c(df_R1$lat[1], rep(df_R1$lat[2:(NROW(df_R1)-1)], each=2),  df_R1$lat[NROW(df_R1)])
df_R2.3 <- data.frame(long2,lat2)

df_R2.3$segnum <- rep(1:(NROW(df_R1)-1), each=2)
df_R2.3$segw <- rep(df_R1$survivors[1:(NROW(df_R1)-1)], each=2)


# group 1 advance ----
df_A1 <- subset(troops, direction == "A") %>% filter(group == 1)
long2 <- c(df_A1$long[1], rep(df_A1$long[2:(NROW(df_A1)-1)], each=2),  df_A1$long[NROW(df_A1)])
lat2 <- c(df_A1$lat[1], rep(df_A1$lat[2:(NROW(df_A1)-1)], each=2),  df_A1$lat[NROW(df_A1)])
df_A2 <- data.frame(long2,lat2)

df_A2$segnum <- rep(1:(NROW(df_A1)-1), each=2)
df_A2$segw <- rep(df_A1$survivors[1:(NROW(df_A1)-1)], each=2)

# group 2 advance ----
df_A1 <- subset(troops, direction == "A") %>% filter(group == 2)
long2 <- c(df_A1$long[1], rep(df_A1$long[2:(NROW(df_A1)-1)], each=2),  df_A1$long[NROW(df_A1)])
lat2 <- c(df_A1$lat[1], rep(df_A1$lat[2:(NROW(df_A1)-1)], each=2),  df_A1$lat[NROW(df_A1)])
df_A2.2 <- data.frame(long2,lat2)

df_A2.2$segnum <- rep(1:(NROW(df_A1)-1), each=2)
df_A2.2$segw <- rep(df_A1$survivors[1:(NROW(df_A1)-1)], each=2)

# group 3 advance ----
df_A1 <- subset(troops, direction == "A") %>% filter(group == 3)
long2 <- c(df_A1$long[1], rep(df_A1$long[2:(NROW(df_A1)-1)], each=2),  df_A1$long[NROW(df_A1)])
lat2 <- c(df_A1$lat[1], rep(df_A1$lat[2:(NROW(df_A1)-1)], each=2),  df_A1$lat[NROW(df_A1)])
df_A2.3 <- data.frame(long2,lat2)

df_A2.3$segnum <- rep(1:(NROW(df_A1)-1), each=2)
df_A2.3$segw <- rep(df_A1$survivors[1:(NROW(df_A1)-1)], each=2)

# one of the groups that turns from advance to retreat is difficult to plot, and better to wrangle by hand:
start <- df_A2.3[4,]
mid <- df_R2[33,]
seg <- rbind(start, mid)
seg$segnum <- 1
seg$segw <- mid$segw
end <- rbind(seg[2,], df_R2[36,])
end$segnum <- 2
seg <- rbind(seg, end)

# plot retreat ----
p <- 
  ggplot(data=troops, aes(x=long2,y=lat2)) + 
  geom_line(data=df_R2, aes(group = segnum, size=segw), colour="darkgrey", alpha = .9)+ 
  geom_line(data=df_R2.2, aes(group = segnum, size=segw), colour="darkgrey", alpha = .9)+ 
  geom_line(data=df_R2.3, aes(group = segnum, size=segw), colour="darkgrey", alpha = .9)+ 
  geom_line(data=seg, aes(group = segnum, size=segw), color="darkgrey", alpha = .9)+
  geom_line(data=df_A2, aes(group = segnum, size=segw), color="#e7d4a7", alpha = .9)+ 
  geom_line(data=df_A2.2, aes(group = segnum, size=segw), color="#e7d4a7", alpha = .9)+ 
  geom_line(data=df_A2.3, aes(group = segnum, size=segw), color="#e7d4a7", alpha = .9)+
  geom_text(data = cities, aes( x=long,y=lat, label = city),
            nudge_x = 0, nudge_y = .0, angle=0)+
  geom_point(data = cities, aes(x=long,y=lat, group= city),
             size = 1)+
  geom_line(data=borders, aes(x=x2, y=y2, group = segnum), color="#e7d4a7", alpha = .9)+
  geom_point(data=borders, aes(x=x2, y=y2, group = 1), size = .2)+
  xlim + ylim

ggplotly(p)

# map ----
library(ggmap)
library(OpenStreetMap)
require(maps)
require(ggplot2)
xlim
ylim


#  “osm”, “osm-bw”, “maptoolkit-topo”, “waze”, “bing”, “stamen-toner”, “stamen-terrain”, “stamen-watercolor”, “osm-german”, “osm-wanderreitkarte”, “mapbox”, “esri”, “esri-topo”, “nps”, “apple-iphoto”, “skobbler”, “hillshade”, “opencyclemap”, “osm-transport”, “osm-public-transport”, “osm-bbike”, “osm-bbike-german”
# https://i1.wp.com/blog.fellstat.com/wp-content/uploads/2013/04/Rplot001.png

# zoom depends on number of tiles available from server
# main window size: c(53.5,23.5), c(57.5,38)
map <- openmap(
  c(54,23.5), c(56,38),
  #c(30,1), c(65,50), # large
               # minNumTiles=4
               #zoom = 4, # ! run time. nps max 8.
              minNumTiles = 9,
               type = "stamen-toner", # stamen-toner 
              #mergeTiles = TRUE
               )

  
map_longlat <- openproj(map, projection = "+proj=longlat")

autoplot.OpenStreetMap(map_longlat)



pmap <- 
  autoplot.OpenStreetMap(map_longlat) + 
  #scale_y_continuous(expand = c(0,0), limits = c(54, 56))+
  geom_line(data=df_R2, aes(x=long2,y=lat2, group = segnum, size=segw), colour="#4c4c4c", alpha = 1)+ 
  geom_line(data=df_R2.2, aes(x=long2,y=lat2, group = segnum, size=segw), colour="#4c4c4c", alpha = 1)+ 
  geom_line(data=df_R2.3, aes(x=long2,y=lat2, group = segnum, size=segw), colour="#4c4c4c", alpha = 1)+ 
  geom_line(data=seg, aes(x=long2,y=lat2, group = segnum, size=segw), color="#4c4c4c", alpha = 1)+
  geom_line(data=df_A2, aes(x=long2,y=lat2, group = segnum, size=segw), color="#e2ca93", alpha = 1)+ 
  geom_line(data=df_A2.2, aes(x=long2,y=lat2, group = segnum, size=segw), color="#e2ca93", alpha = 1)+ 
  geom_line(data=df_A2.3, aes(x=long2,y=lat2, group = segnum, size=segw), color="#e2ca93", alpha = 1)+
  geom_text(data = cities, aes( x=long,y=lat, label = city),
            nudge_x = 0, nudge_y = .2, angle = -20)+
  geom_point(data = cities, aes(x=long,y=lat, group= city),
             size = 1, color = "darkred")+
  
  geom_line(data=borders, aes(x=x2, y=y2, group = segnum), color="black", alpha = .6)+
  # geom_point(data=borders, aes(x=x2, y=y2, group = 1), size = .2)+ 
  xlim + ylim

# ggplotly(pmap)

# set map to EU and define initial zoom window

# temp plotly ----

long <- c(temps$long[1], rep(temps$long[2:(NROW(temps)-1)], each=2),  temps$long[NROW(temps)])
temp <- c(temps$temp[1], rep(temps$temp[2:(NROW(temps)-1)], each=2),  temps$temp[NROW(temps)])
temp2 <- data.frame(long,temp)

temp2$segnum_temp <- rep(1:(NROW(temps)-1), each=2)
temp2 <- merge(temps, temp2 )

temp2 <- temp2[order(temp2$segnum_temp, temp2$date), ]
tempmean <- (temp2 %>% group_by(segnum_temp) %>% summarise(median = median(temp)))
tempmean <- as.data.frame(tempmean)
temp2 <- merge(temp2, tempmean) 

px <- ggplot(data=temp2, aes(x=long, y=temp)) + 
  geom_line(data=temp2, aes(group = segnum_temp, color=median), show.legend = FALSE)+
  scale_color_gradientn(colours = c("red", "blue"),limits = c(-40, 0))+
  geom_point(aes(x = long, y = temp, colour = temp), size = 2, show.legend = FALSE) +
  geom_text(data = temp2, aes(label = paste(month, day, "\n",tempC)), size = 3, nudge_y = -8 ) + xlim + ylim_temp

#ggplotly(px)


# multiplot range slider ----

#subplot(p, px, nrows = 2, margin = 0.04, heights = c(0.5, 0.5)) %>%  rangeslider(start = 24.5, end = 37) 

subplot(pmap, px, nrows = 2, margin = 0.02, heights = c(0.4, 0.3), shareX = TRUE) 
# %>%
  #layout(xaxis=list(autorange=F, range=c(23,39)), 
  #       yaxis=list(autorange=F, range=c(54,56.5)))

