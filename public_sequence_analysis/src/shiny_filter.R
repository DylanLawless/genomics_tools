# Load packages
library(shiny)
library(ggplot2)

source("public_sequence_analysis.R")

tmp_per_year %>% 
  filter(position >= 664) %>%
  filter(position <= 668) %>%
  group_by(var, position) %>%
  ggplot(aes(x=Year, y=Freq_per_year, color=var))+
  geom_point()+
  geom_line(aes(group=interaction(var, position)))

df <- tmp_per_year %>% 
  select(Year, var, position, Freq_per_year ) %>%
  group_by(var, position) %>%
  filter(position >= 664) %>%
  filter(position <= 668) 




library(ggplot2)
library(plotly)
library(DT)
m<-highlight_key(mpg)
p<-ggplot(m,aes(displ,hwy))+geom_point(aes(color = cyl)) + geom_smooth(se = TRUE)
gg<-highlight(ggplotly(p),"plotly_selected")
m<-highlight_key(mpg)


p<-ggplot(m,aes(displ,hwy))+geom_point(aes(color = cyl)) + geom_smooth(se = TRUE)

gg<-highlight(ggplotly(p),"plotly_selected",
              opacityDim = getOption("opacityDim", 0.01),)
crosstalk::bscols(gg,DT::datatable(m))


gg2 <- gg%>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick") %>% 
  layout(dragmode = "zoom")

crosstalk::bscols(gg2,DT::datatable(m))

# data






# filter crosstalk ----
# select ----
library(crosstalk)


# generally speaking, use a "unique" key for filter, 
# especially when you have multiple filters!

t1 <- tmp_per_year %>% 
  select(Year, var, position, Freq_per_year ) %>%
  group_by(var, position) %>%
  filter(position >= 664) %>%
  filter(position <= 668) 

tx <- highlight_key(t1)
gg <- tx %>%
  ggplot(aes(x=Year, y=Freq_per_year, color=var))+
  geom_point()+
  geom_line(aes(group=interaction(var, position)))


filter <- bscols(
  filter_select("id", "Select a position", tx, ~position),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)


bscols(filter)










tx <- highlight_key(txhousing)
gg <- ggplot(tx) + geom_line(aes(date, median, group = city))
filter <- bscols(
  filter_select("id", "Select a city", tx, ~city),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)

tx2 <- highlight_key(txhousing, ~city, "Select a city")
gg <- ggplot(tx2) + geom_line(aes(date, median, group = city))
select <- highlight(
  ggplotly(gg, tooltip = "city"), 
  selectize = TRUE, persistent = TRUE
)

bscols(filter, select)



# Single variant plot ----
position <-df$position
Year <- df$Year
Freq_per_year <- df$Freq_per_year
var <- df$var
ExampleDF<- data.frame(position, Year, Freq_per_year, var, stringsAsFactors=FALSE)


library(shiny)
library(ggplot2)
library(dplyr)

ui <-(fluidPage(
  titlePanel("positions"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create position graphs"),
      
      selectInput("Selectposition", "Choose a position to display",
                  choices = df$position)
      
    ),
    
    # Show a plot of the Years
    mainPanel(
      plotOutput("SubcompPlot")
    )
  )
))

Server <- function(input, output) {
  
  output$SubcompPlot <- renderPlot({
    
    Filtered <- df %>% filter(position == input$Selectposition)
    
    ggplot(data=Filtered, aes(x = Filtered$Year, y = Filtered$Freq_per_year,
                              color=Filtered$var)) + 
      geom_point( ) +
      geom_line( )
    })
}

shinyApp(ui, Server)



tmp_per_year %>% 
  filter(position >= 664) %>%
  filter(position <= 668) %>%
  group_by(var, position) %>%
  ggplot(aes(x=Year, y=Freq_per_year ))+
  geom_point(aes(color=var))+
  geom_line(aes(group=interaction(var, position))) +
  facet_wrap(.~position)









# filter range ----
location = c("100 ail","16th and Whitmore","40AB01 - ANTWERPEN","100 ail","16th and Whitmore","40AB01 - ANTWERPEN") 
date = c("2015-09-01 00:00:00","2016-03-06 19:00:00","2016-11-22 15:00:00","2018-02-01 09:30:00", "2018-02-01 03:00:00", "2017-03-07 10:00:00") 
pm25=c("FALSE","FALSE","FALSE","FALSE","FALSE","FALSE")
pm10=c("TRUE","FALSE","FALSE","TRUE","FALSE","FALSE")
no2=c("TRUE","FALSE","FALSE")
latitude=c(47.932907,41.322470,36.809700,47.932907,41.322470,36.809700)
longitude=c(106.92139000,-95.93799000
            ,-107.65170000,106.92139000,-95.93799000
            ,-107.65170000)

df = data.frame(location, date = as.Date(date),
                latitude,longitude,pm25,pm10,no2)
library(dplyr)
library(shiny)

ui = fluidPage(
  
  dateRangeInput("dateRange", 
                 "Select date range:",
                 start = '01/01/2013'),
  submitButton(text = "Submit", icon = NULL, width = NULL),
  dataTableOutput("merged") 
)

#server.r
server = function(input, output, session) {
 
  output$merged <- renderDataTable({
    (  filtered <-    df %>%
         filter(date >= input$dateRange[1] & 
                  date <= input$dateRange[2]))
     filtered
  })
}

shinyApp(ui = ui, server = server)
