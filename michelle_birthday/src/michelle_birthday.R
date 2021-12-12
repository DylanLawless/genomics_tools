library(geosphere)
library(leaflet)
library(RColorBrewer)

# wide format ----
df <- 
  read.table(file="../data/travel_plans_wide.csv", 
             header = TRUE, 
             sep = ",", 
             stringsAsFactors = TRUE,
             quote = "")

df <- as.tibble(df)
names(df)

# flight paths ----
flows <- gcIntermediate(df[,4:5], df[,6:7], sp = TRUE, addStartEnd = TRUE)

# annotations ----
flows$Person <- df$Person
flows$counts <- df$Number
flows$origins <- df$origins
flows$destinations <- df$destinations

hover <- paste0(flows$Person, " - ", 
                flows$origins, " to ", 
                flows$destinations, ': ', 
                as.character(flows$counts))

pal <- colorFactor(brewer.pal(9, 'Set1'), flows$origins)

# plot ----
leaflet() %>%
  addTiles() %>% # default osm
  #addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows, label = hover, #weight = ~counts, 
               group = ~origins, color = ~pal(origins),
               opacity = 1) %>%
#  addLayersControl(overlayGroups = unique(flows$origins), options = layersControlOptions(collapsed = FALSE), position = c("topright")) %>%
  addCircleMarkers(~longitude.x, ~latitude.x, popup = ~as.character(Person),
             fillColor = ~pal(origins), radius = 5, stroke = TRUE, weight = 1, opacity = 1.0, fill = TRUE, data=df) %>%
addMarkers(~longitude.x, ~latitude.x, label = ~hover, data=df)
           

x












# import ----
df <- 
  read.table(file="../data/travel_plans.csv", 
             header = TRUE, 
             sep = ",", 
             stringsAsFactors = TRUE,
             quote = "")

names(df)


library(leaflet)
library(magrittr)


leaflet()%>%
  addTiles() %>%
  addPolylines(data = df, lng = ~lon, lat = ~lat, group = ~Person) %>%
  addMarkers(lng=-78.8757119, lat=35.8824723, popup="Here's where we are right now")




pts <- df %>% filter(Event== "Departure") %>% select(lon, lat)
dest <- (df %>% filter(Number == 1) %>% filter(location=="Lucerne" )) %>% select(lon, lat)


inter <- gcIntermediate( pts , dest , n=50 , addStartEnd=TRUE , sp = TRUE) 



inter %>%
  leaflet() %>% 
  addTiles() %>% 
  addPolylines( color = "red")


draw_line <- data.frame(inter)












pts <- df %>% filter(Event== "Departure") 
lon.y <- (df %>% filter(Number == 1) %>% filter(location=="Lucerne" )) %>% select(lon)
 (df %>% filter(location=="Lucerne" )) %>% select(lat)

pts$lon.y <- 8.309307
pts$lat.y <- 47.05017








# loop
for (i in 1:10) {
  # use aes_string with names of the data.frame
  p <- p + geom_line(aes_string(y = names(dfp)[i]))
}
# print the result
print(p)


leaflet()%>%
  addTiles() %>%
  addPolylines(data = n1, lng = ~long, lat = ~lat, group = ~Person) %>%
  addPolylines(data = n2, lng = ~long, lat = ~lat, color  = "red") 














gcIntermediate(c(n2$lon, n2$lat), 
               c(dest$lon, dest$lat), 
               n=200,
               addStartEnd=TRUE,
               sp=TRUE)  %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines()




for (i in (1:dim(df)[1])) { 
  
  inter <- gcIntermediate(c(df$lon[1], df$lat[1]), 
                          c(dest$lon[i], dest$lat[i]), 
                          n=200)

  leaflet()%>%
    addTiles() %>%
    addPolylines(data = inter, lng = ~lon, lat = ~lat, group = ~Person)
  
#  lines(inter, lwd=0.1, col="turquoise2")    
}



# lines ----
mydf <- data.frame(Observation = c("A", "B"),
                   InitialLat = c(62.469722,48.0975),
                   InitialLong = c(6.187194, 16.3108),
                   NewLat = c(51.4749, 51.4882),
                   NewLong = c(-0.221619, -0.302621),
                   stringsAsFactors = FALSE)


mydf2 <- data.frame(group = c("A", "B"),
                    lat = c(mydf$InitialLat, mydf$NewLat),
                    long = c(mydf$InitialLong, mydf$NewLong))

#  group      lat      long
#1     A 62.46972  6.187194
#2     B 48.09750 16.310800
#3     A 51.47490 -0.221619
#4     B 51.48820 -0.302621

library(leaflet)
library(magrittr)

leaflet()%>%
  addTiles() %>%
  addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group) %>%
  addMarkers(lng=-78.8757119, lat=35.8824723, popup="Here's where we are right now")




library(leaflet)
library(geosphere)
gcIntermediate(c(5,52), c(-120,37),
               n=30, 
               addStartEnd=TRUE,
               sp=TRUE) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines()





# Other ---
m <- leaflet() %>%
addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-78.8757119, lat=35.8824723, popup="Here's where we are right now")
m  # Print the map


addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group)




library(tidyverse)
library(leaflet)
library(tidygeocoder)
library(geosphere)
library(osrm)
# Getting Geocoded Locations- Flying from Toronto to Tel Aviv
addresses <- tibble(singlelineaddress = c("YYZ","TLV")) %>% 
  geocode(address=singlelineaddress,method = 'arcgis') %>% 
  transmute(id = singlelineaddress,
            lon=long,
            lat=lat)
# Geocoded Locations
addresses

trip <-gcIntermediate(addresses[1,2:3],
                      addresses[2,2:3],
                      # Choose the number of points you want. Determines Smoothness (less is more)
                      n=30,
                      addStartEnd = T) 


trip

# I find that using a smaller n makes for a nicer map and is pretty subjective. So for the plotting function I let the user be in control of the n with the nCurves argument. It is set default at 30.


plot_flight<-function(to, 
                      from,
                      colour="black",
                      opacity=1,
                      weight=1,
                      radius=2,
                      label_text=c(to,from),
                      label_position="bottom",
                      font = "Lucida Console",
                      font_weight="bold",
                      font_size= "14px",
                      text_indent="15px",
                      mapBoxTemplate= "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                      nCurves=30){
  
  address_single <- tibble(singlelineaddress = c(to,from)) %>% 
    geocode(address=singlelineaddress,method = 'arcgis') %>% 
    transmute(id = singlelineaddress,
              lon=long,
              lat=lat)
  
  
  
  trip <-gcIntermediate(address_single[1,2:3],
                        address_single[2,2:3],
                        n=nCurves,
                        addStartEnd = T) 
  m<-leaflet(trip,
             options = leafletOptions(zoomControl = FALSE,
                                      attributionControl=FALSE)) %>%
    fitBounds(lng1 = max(address_single$lon)+0.1,
              lat1 = max(address_single$lat)+0.1,
              lng2 = min(address_single$lon)-0.1,
              lat2 = min(address_single$lat)-0.1) %>%
    addTiles(urlTemplate = mapBoxTemplate) %>%
    addCircleMarkers(lat = address_single$lat,
                     lng = address_single$lon,
                     color = colour,
                     stroke = FALSE,
                     radius = radius,
                     fillOpacity = opacity) %>%
    addPolylines(color = colour,
                 opacity=opacity,
                 weight=weight,
                 smoothFactor = 0) %>%
    addLabelOnlyMarkers(address_single$lon,
                        address_single$lat,
                        label =  label_text,
                        labelOptions = labelOptions(noHide = T,
                                                    direction = label_position,
                                                    textOnly = T,
                                                    style=list("font-family" = font,
                                                               "font-weight"= font_weight,
                                                               "font-size"=font_size,
                                                               "text-indent"=text_indent)))
  
  m
}



plot_flight("YYZ",
            "TLV",
            weight = 1,
            opacity = 1,
            nCurves = 30,
            mapBoxTemplate = "https://api.mapbox.com/styles/v1/benyamindsmith/ckvufe72g2qvu15pewrh6m20f/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYmVueWFtaW5kc21pdGgiLCJhIjoiY2tiMDR3NXpzMDU3bjJ1cXVqNmx3ODhudyJ9.KffbwvHgcIn1GL1DV4uUBQ")


# The code for 2 or more flights is very similar to the code for an individual flight except instead of having to and from arguments, there is an addresses argument which will take a vector of locations which should be ordered in the flight path you want.


plot_flights<-function(addresses,
                       colour="black",
                       opacity=1,
                       weight=1,
                       radius=2,
                       label_text=addresses,
                       label_position="bottom",
                       font = "Lucida Console",
                       font_weight="bold",
                       font_size= "14px",
                       text_indent="15px",
                       mapBoxTemplate= "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                       nCurves=100){
  
  address_single <- tibble(singlelineaddress = addresses) %>% 
    geocode(address=singlelineaddress,method = 'arcgis') %>% 
    transmute(id = singlelineaddress,
              lon=long,
              lat=lat)
  
  
  trip <- matrix(nrow=1,ncol=2)
  
  for(i in 2:nrow(address_single)){
    trip<-rbind(trip,
                gcIntermediate(address_single[i-1,2:3],
                               address_single[i,2:3],
                               n=nCurves,
                               addStartEnd = T) )
  }
  m<-leaflet(trip,
             options = leafletOptions(zoomControl = FALSE,
                                      attributionControl=FALSE)) %>%
    fitBounds(lng1 = max(address_single$lon)+0.1,
              lat1 = max(address_single$lat)+0.1,
              lng2 = min(address_single$lon)-0.1,
              lat2 = min(address_single$lat)-0.1) %>%
    addTiles(urlTemplate = mapBoxTemplate) %>%
    addCircleMarkers(lat = address_single$lat,
                     lng = address_single$lon,
                     color = colour,
                     stroke = FALSE,
                     radius = radius,
                     fillOpacity = opacity) %>%
    addPolylines(color = colour,
                 opacity=opacity,
                 weight=weight,
                 smoothFactor = 0) %>%
    addLabelOnlyMarkers(address_single$lon,
                        address_single$lat,
                        label =  label_text,
                        labelOptions = labelOptions(noHide = T,
                                                    direction = label_position,
                                                    textOnly = T,
                                                    style=list("font-family" = font,
                                                               "font-weight"= font_weight,
                                                               "font-size"=font_size,
                                                               "text-indent"=text_indent)))
  
  m
}



plot_flights(c("DUB",
               "GIG",
               "IE",
               "GIG",
               "CPT/FACT",
               "BOM",
               "TLV"),
             weight = 1,
             opacity = 1,
             nCurves = 10,
             label_position="right",
             mapBoxTemplate = "https://api.mapbox.com/styles/v1/benyamindsmith/ckvufe72g2qvu15pewrh6m20f/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYmVueWFtaW5kc21pdGgiLCJhIjoiY2tiMDR3NXpzMDU3bjJ1cXVqNmx3ODhudyJ9.KffbwvHgcIn1GL1DV4uUBQ")








plot_flights(c("YYZ",
               "GIG",
               "CPT/FACT",
               "BOM",
               "TLV"),
             weight = 1,
             opacity = 1,
             nCurves = 10,
             label_position="right",
             mapBoxTemplate = "https://api.mapbox.com/styles/v1/benyamindsmith/ckvufe72g2qvu15pewrh6m20f/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYmVueWFtaW5kc21pdGgiLCJhIjoiY2tiMDR3NXpzMDU3bjJ1cXVqNmx3ODhudyJ9.KffbwvHgcIn1GL1DV4uUBQ")









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

# save a copy 
df_original <- df
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



library(tidyr)
event_year_long <- event_year %>%
  group_by(Event, Start, End) %>%
  mutate(date = list(seq.Date(Start, End, by = "year"))) %>%
  tidyr::unnest()

event_year_long$Year <- year(event_year_long$date)

event_year_long <- event_year_long %>% group_by(Year)


# get the first lat,long for a region/event
df_slice <- df_world_date %>%
  ungroup() %>%
  select(Year, lat, long, Event, region, Description) %>%
  group_by(Year, Event, region, Description) %>% 
  slice(1)

world %>%
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_map( aes(map_id = region), 
            size = 0.1,
            map = world, data = world)+
  geom_map( aes(fill = Event, map_id = region), 
            map = df_world_date, data = df_world_date)

# use map instead of polygon ----
# land color = #ffeddf
p  <- world %>%
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_map( aes(map_id = region, size = 1), 
            color = "black", fill = "#f8f0ea", size = 0.1,
            map = world, data = world)+
    geom_map( aes(fill = Event, map_id = region), 
              color = "black",  size = 0.1,
      map = df_world_date, data = df_world_date) +
  scale_fill_gradientn(colours = pal) +
  theme(legend.position="none",
        panel.background = element_rect(fill = "#d5edff"),
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())


p

# animate gif ----
library(gganimate)
library(gifski)

# add repel text and year ----
# This is not required, but a nice thing for reference
# spread the lat value between min/max such that labels will be correct but not overlapping
#library(BBmisc)
#df_slice$lat_scaled <- df_slice$lat %>% BBmisc::normalize(method = "range", range = c(-80,80))

# Get the description / year
df_slice_description <- df_slice %>% ungroup() %>% group_by(Year, Event, Description) %>% slice(1) %>% unique()

library(ggrepel)
animate_plot <- p + transition_manual(frames = Year) +
  labs(title = paste("{current_frame}")) +# give the year
  geom_label_repel(data = df_slice,
                   aes(label=region, y = lat, x = long, group=Event),
                   xlim=c(-200,-100), show.legend = FALSE,direction="y")

  
# check number of frames required
event_year_long %>% 
  ungroup() %>% 
  select(Year) %>% 
  unique() %>% 
  tally()

library(gifski)
animate(animate_plot, 
        nframes = 55, 
        fps = 5, 
        end_pause = 1,
        renderer=gifski_renderer("test.gif"),
        height = 6, width = 10, units = "in", res = 150
        )



# reactable ----

# The collapsed version of table with modern country names
df_summarised <-
  df %>%
  select(-iscountry)  %>%
  group_by(Event, Start, End, Description) %>%
  summarize(Region = toString(region)) %>%
  ungroup

library(reactable)
options(reactable.theme = reactableTheme(
  borderColor = "#dfe2e5",
  stripedColor = "#fcf0e6",
  highlightColor = "#f9e2cf",
  cellPadding = "8px 12px",
  style = list(fontFamily = "-apple-system, Arial, BlinkMacSystemFont, Segoe UI, Helvetica,  sans-serif",
               fontSize = "1.0rem"),
  searchInputStyle = list(width = "50%")
))

df_t_original <- 
  reactable(df_original,
            compact = TRUE,
            searchable = TRUE,
            #elementId = "download-table",
            defaultPageSize = 10,
            defaultColDef = colDef(minWidth = 90 ),
            # columns list
            filterable = TRUE,
            showSortable = TRUE,
            showPageSizeOptions = TRUE,
            striped = TRUE,
            highlight = TRUE
  )

df_t_modern <- 
  reactable(df_summarised,
            compact = TRUE,
            searchable = TRUE,
            #elementId = "download-table",
            defaultPageSize = 10,
            defaultColDef = colDef(minWidth = 90 ),
            # columns list
            filterable = TRUE,
            showSortable = TRUE,
            showPageSizeOptions = TRUE,
            striped = TRUE,
            highlight = TRUE
  )

df_t_original
df_t_modern



columns = list(
  "Disease label" = colDef(minWidth = 200),  # overrides the default
  "GCEP" = colDef(minWidth = 200), 
  "SOP" = colDef(minWidth = 70), 
  "Online report" = colDef(cell = function(value, index) {
    # Render as a link
    url <- sprintf(df[index, "Online report"], value)
    htmltools::tags$a(href = url, target = "_blank", "link")
  }),
  
  Classification = colDef( minWidth = 130,
                           style = function(value) {
                             if (value == "Disputed") {color <- "#962fbf"
                             } else if (value == "Limited") {color <- "#e5ab00"
                             } else if (value == "Moderate") {color <- "#fa7e1e"
                             } else if (value == "No Known") {color <- "#d62976"
                             } else if (value == "Definitive") {color <- "#339900"
                             } else if (value == "Strong") {color <- "#99cc33"
                             } else if (value == "Refuted") {color <- "#4f5bd5"
                             } else { color <- "black"}
                             list(color = color) })
  
),