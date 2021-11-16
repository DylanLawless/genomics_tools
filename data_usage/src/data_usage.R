library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

# Data usage tracking
# caption: Long-term projects use data from many sources. Data may be reused multiple times, and some planned analysis is not required. Data tracking systems can show these analytics and improve multipurpose use.


# import ----
df <- 
  read.table(file="../data/data_usage.tsv", 
             header = TRUE, 
             sep = "\t", 
             stringsAsFactors = FALSE)

df$Date <- ymd(df$date)

df$fileuse <- as.factor(df$fileuse)
df$fileuse <- factor(df$fileuse, levels = c("Yes","No"))

# critical for highlighting
d <- highlight_key(df, ~datatype)


p <- d %>%
  ggplot( aes(x=Date, 
              y=log10(size_GB), 
              fill=datatype,
              shape=fileuse,
              label=data_purpose,
              label2=size_GB
              )) + 
  geom_point(aes( size=log10(size_GB) ),
             position = position_jitter(width = 10, height = 0.5)) +
  facet_grid(fileuse ~.)


p

gg <- ggplotly(p, tooltip = c("label2", "label", "fill") ) 
highlight(gg, on = "plotly_hover",)

# supply custom colors to the brush 
cols <- (RColorBrewer::brewer.pal(8, "Dark2"))

# Use attrs_selected() for complete control over the selection appearance
# note any relevant colors you specify here should override the color argument
s <- attrs_selected(
  showlegend = TRUE,
  mode = "lines+markers",
  marker = list(symbol = "x"),
  color = cols, 
  dynamic = TRUE
)

highlight(layout(gg, showlegend = TRUE), selected = s,
         # on = "plotly_hover",
          )





