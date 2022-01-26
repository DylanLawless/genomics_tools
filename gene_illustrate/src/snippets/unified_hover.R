library(tidyverse)
library(plotly)

# data 
df <- data.frame(
  x = c(1, 2, 3, 4),
  y = runif(8, 5, 10),
  cat = c("a", "a", "a", "a", "b", "b", "b", "b")
)

# annotation
df2 <- data.frame(
  x = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5),
  y = c(1,1,1,1,1,1,1,1)
)


p <- ggplot(df, groups = cat) +
  geom_point(aes(x = x, y = y, color = cat, 
                 text = paste('</br> X is ', x, '</br> Y is ', y, '</br> Cat is ', cat)), 
             position = position_dodge(width = 0.3)) +
  geom_point(data = df2, aes(x = x, y = y,
             text = paste('</br> Pos : ', x))) +
  facet_grid(cat~.)

ggplotly(p, tooltip = "text")  %>% layout(hovermode = "x unified")



