library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(tidyr)

# import ----
df <- 
  read.table(file="../data/killing_hope_dates.tsv", 
             header = TRUE, 
             sep = "\t", 
             stringsAsFactors = TRUE,
             quote = "")


world <- map_data("world")

world$region <- str_replace(world$region, "Democratic Republic of the Congo", "DRC")

df$region <- str_replace(df$region, "The Philippines", "Philippines")
df$region <- str_replace(df$region, "Korea", "South_Korea/North_Korea")
df$region <- str_replace(df$region, "East Timor", "Timor-Leste")
df$region <- str_replace(df$region, "The Congo", "Republic of Congo")
df$region <- str_replace(df$region, "Zaire", "DRC")
df$region <- str_replace(df$region, "British Guiana", "Guyana")
df$region <- str_replace(df$region, "Western Europe", "Austria/Belgium/France/Germany/Liechtenstein/Luxembourg/Monaco/Netherlands/Switzerland")
df$region <- str_replace(df$region, "The Middle East", "Egypt/Turkey/Iran/Iraq/Saudi Arabia/Yemen/Syria/Jordan/United Arab Emirates/Israel/Lebanon/Oman/Palestine/Kuwait/Qatar/Bahrain")
df$region <- str_replace(df$region, "Soviet Union late", "Armenia/Moldova/Estonia/Latvia/Lithuania/Georgia/Azerbaijan/Tajikistan/Kyrgyzstan/Belarus/Uzbekistan/Turkmenistan/Ukraine/Kazakhstan/Russia")

df$region <- str_replace(df$region, "Eastern Europe", "Belarus/Bulgaria/Czech Republic/Hungary/Poland/Moldova/Romania/Russia/Slovakia/Ukraine")



DRC

df <- separate_rows(df,region,sep = "/")
df$region <- str_replace(df$region, "_", " ")

df$iscountry <- match(df$region, world$region, nomatch = NA_integer_, incomparables = NULL)
  
# Once all names match to "map_data" we can continue
df[ is.na(df$iscountry), ] %>% select(region)
 

# merge 
df_world <- left_join(world, df, by = c("region"))

# extract only the regions
df_world <- df_world[ !is.na(df_world$Description), ] 

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  )

world %>%
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_polygon(data = world, fill="lightgray",
               color = "black",  size = 0.1) +
  geom_polygon( data = df_world,
                color = "gray90",  size = 0.1,
                aes(fill=Start, x=long,y=lat,group=group)) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank()) 
 # transition_reveal(Start)
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +


# Expand events to cover the range of start-end year
# set the years as dates
df_world$Start <- lubridate::ymd(df_world$Start, truncated = 2L)
df_world$End <- lubridate::ymd(df_world$End, truncated = 2L)

event_year <- df_world %>%
  select(Event, Start, End) %>%
  group_by(Event, Start, End) %>%
  unique()

library(tidyr)
event_year_long <- event_year %>%
  group_by(Event, Start, End) %>%
  mutate(date = list(seq.Date(Start, End, by = "year"))) %>%
  tidyr::unnest()

# merge
df_world_date <- left_join(df_world, event_year_long, by = c("Event", "Start", "End"))

# Print simply as "Year" for plotting
df_world_date$Year <- year(df_world_date$date)

df_point <- df_world_date %>%
  select("region", "Year", "Event") %>%
  unique() 

library(wesanderson)
library(gganimate)
pal <- wes_palette("Zissou1", 10, type = "continuous")

p1 <- world %>%
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_polygon(data = world, fill="lightgray",
               color = "gray90",  size = 0.1) +
  geom_polygon( data = df_world_date,
                color = "gray90",  size = 0.1,
                aes(fill=Event, frame = Year, x=long,y=lat,group=group)) +
  scale_fill_gradientn(colours = pal) + 
  theme(legend.position="none",
           axis.line=element_blank(),
           axis.text=element_blank(),
           axis.ticks=element_blank(),
           axis.title=element_blank(),
           panel.background=element_blank(),
           panel.border=element_blank(),
           panel.grid=element_blank())

# Create plotly graph
# ggplotly(p1, height = 400, width = 500) %>% animation_opts(frame = 10, #easing = "linear", redraw = TRUE
 # ) 

# animate does not work for geom_polygon



library(gganimate)
p2 <- world %>%
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_point( data = (df_world_date  %>%
                        filter(Year >= 1970)),
                 size = 10,
                aes(fill=Event, frame = Year, 
                    x=long,y=lat,
                    group=group)) +
  scale_fill_gradientn(colours = pal) + 
  theme(legend.position="none")

# p2 + transition_time(Year, transition_length = 3,  range = c(1970,1975))





library(tidyr)
event_year_long <- event_year %>%
  group_by(Event, Start, End) %>%
  mutate(date = list(seq.Date(Start, End, by = "year"))) %>%
  tidyr::unnest()

event_year_long$Year <- year(event_year_long$date)

event_year_long <- event_year_long %>% group_by(Year)

world %>%
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_map( aes(map_id = region), 
            map = world, data = world)+
  geom_map( aes(fill = Event, map_id = region), 
            map = df_world_date, data = df_world_date) +
  annotate(
    "text", label = event_year_long$Year,
    x = 2, y = 15, size = 8, colour = "red"
  )

# use map instead of polygon ----
# land color = #ffeddf
p  <- world %>%
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_map( aes(map_id = region), 
            map = world, data = world)+
    geom_map( aes(fill = Event, map_id = region), 
      map = df_world_date, data = df_world_date) +
  scale_fill_gradientn(colours = pal)

# p


# animate gif ----
library(gganimate)
library(gifski)

# add text and year ----
# get the first lat,long for a region/event
df_slice <- df_world_date %>%
  ungroup() %>%
  select(Year, lat, long, Event, region, Description) %>%
  group_by(Year, Event, region, Description) %>% 
  slice(1)

# spread the lat value between min/max such that labels will be correct but not overlapping
library(BBmisc)
df_slice$lat_scaled <- df_slice$lat %>% BBmisc::normalize(method = "range", range = c(-80,80))


# repel text ----
#ocean <- "#5391c9"

library(ggrepel)
animate_plot <- p + transition_manual(frames = Year) +
  labs(title = paste("{current_frame}")) +# give the year 
  geom_label_repel(data = df_slice, 
                   aes(label=region,
                       y = lat, 
                       x = long,
                       group=Event),
                   xlim=c(-200,-100),
                   show.legend = FALSE, 
                   direction="y")  +
  theme(legend.position="none",
        panel.background = element_rect(fill = "#5391c9"))
#+ 
  #xlim(c(-210, 210))

library(gifski)
animate(animate_plot, nframes = 50, fps = 5, end_pause = 10,renderer=gifski_renderer("test.gif"),
        height = 6, width = 9, units = "in", res = 150 # 450x300px
        )

