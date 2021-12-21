---
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(plotly)
library(nflfastR)
library(reactable)
library(crosstalk)
library(htmltools)
pbp <- read_csv("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.csv.gz")

```

```{r, echo = TRUE, eval = FALSE}
library(tidyverse)
library(plotly)
library(nflfastR)
library(reactable)
```

Packages above.

Code at: [GitHub](https://gist.github.com/jthomasmock/06a26d2cb0bc3053b804dc45f8379b63)

```{r}
qbs <- pbp %>%
  filter(
    play_type %in% c("pass", "run"),
    penalty == 0,
    !is.na(epa)
  ) %>% 
  rename(team = posteam) %>% 
  group_by(name, team) %>%
  summarize(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    n_plays = n(),
    epa_per_play = round(sum(epa) / n_plays, digits = 2),
    success_per_play = round(sum(success) / n_plays, digits = 2),
    .groups = "drop"
  ) %>%
  filter(n_dropbacks >= 100) %>% 
  ungroup() %>% 
  left_join(nflfastR::teams_colors_logos %>% select(team = team_abbr, team_color), by = "team") %>% 
  arrange(desc(epa_per_play)) %>% 
  select(name:success_per_play, team_color)

qb_epa_per_play <- qbs %>%
  summarize(mean = mean(epa_per_play)) %>%
  pull(mean)

qb_success_per_play <- qbs %>%
  summarize(mean = mean(success_per_play)) %>%
  pull(mean)

qb_data <- SharedData$new(qbs)

qb_plot <- qb_data %>%
  ggplot(
    aes(
      x = success_per_play, y = epa_per_play,
      text = paste(
        "QB:", name,
        "\nTeam:", team,
        "\nEPA/Play:", epa_per_play,
        "\nSuccess Rate:", success_per_play
        )
      )
    ) +
  # Notice that color/size inside aes()
  geom_point(aes(color = team_color, size = n_plays / 60), alpha = 0.50) +
  # we need this to assign red/black to the actual color
  scale_color_identity() +
  
  # add labels for all players
  geom_hline(yintercept = qb_epa_per_play, color = "red", linetype = "dashed") +
  geom_vline(xintercept = qb_success_per_play, color = "red", linetype = "dashed") +
  labs(
    x = "Success rate",
    y = "EPA per play",
    caption = "Data from nflscrapR",
    title = "QB success rate and EPA/play",
    subtitle = "2019, min 100 pass attempts, includes all QB's rush and pass plays"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 12)
  ) +
  theme(legend.position = "none")



rwb_pal <- function(x) rgb(colorRamp(c("#F76B4F", "white", "#3DA4EC"))(x), maxColorValue = 255)



react_table <- qb_data %>% 
  
  reactable(
    searchable = TRUE,
    striped = TRUE,
    highlight = TRUE,
    selection = "multiple",
    onClick = "select",
    rowStyle = list(cursor = "pointer"),
    theme = reactableTheme(
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(
        fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%")
  ),
    width = 850,
    defaultPageSize = 15,
    columns = list(
      name = colDef(
        name = "QB Name",
        width = 150
      ),
      team = colDef(
        "Team"
      ),
      n_dropbacks = colDef(
        "Dropbacks"
      ),
      n_rush = colDef(
        "Rushes"
      ),
      n_plays= colDef(
        "Plays"
      ),
      epa_per_play = colDef(
        name = "EPA/Play",
        format = colFormat(digits = 2),
        style = function(value){
          normalized <- (value - min(qbs$epa_per_play)) / (max(qbs$epa_per_play) - min(qbs$epa_per_play))
          color <- rwb_pal(normalized)
          list(background = color)
        }
        ),
      success_per_play = colDef(
        name = "SR",
        format = colFormat(digits = 0, percent = TRUE),
        style = function(value){
          normalized <- (value - min(qbs$success_per_play)) / (max(qbs$success_per_play) - min(qbs$success_per_play))
          color <- rwb_pal(normalized)
          list(background = color)
        }
      ),
      team_color = colDef(show = FALSE)
    )
  )


```

### `crosstalk`

Crosstalk lets HTMLWidgets like `reactable` or `plotly` to talk to each other.

```{r}

nfl_plot <-ggplotly(qb_plot, tooltip = c("text"), width=700, height = 500)

div(
  h3("Compare and filter QBs by EPA or Success rate per play"),
  h4("Filter sliders for specific ranges affect plot & table"),
  bscols(
  widths = c(2, 10),
  list(
    filter_slider("epa_per_play", "EPA/Play", qb_data, ~epa_per_play),
    filter_slider("success_per_play", "Success/Play", qb_data, ~success_per_play)
  ),
  nfl_plot
  ),
  react_table
)
```

