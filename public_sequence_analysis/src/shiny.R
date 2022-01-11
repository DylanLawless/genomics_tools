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
  filter(position >= 664) %>%
  filter(position <= 668) 

ui <- fluidPage(
  plotOutput("plot", click = "plot_click"),
  tableOutput("data")
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) + geom_point()
  }, res = 96)
  
  output$data <- renderTable({
    req(input$plot_click)
    nearPoints(mtcars, input$plot_click)
  })
}

shinyApp(ui, server)





yearplot <- df %>% 
  ggplot(aes(x=Year, y=Freq_per_year, color=var))+
  geom_point()+
  geom_line(aes(group=interaction(var, position)))


# data plot
ui <- fluidPage(
  plotOutput("plot", brush = "plot_brush"),
  tableOutput("data")
)



library(purrr)
library(shiny)
library(dplyr, warn.conflicts = FALSE)


# dymanic filter
make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      make_ui(iris$Sepal.Length, "Sepal.Length"),
      make_ui(iris$Sepal.Width, "Sepal.Width"),
      make_ui(iris$Species, "Species")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  selected <- reactive({
    filter_var(iris$Sepal.Length, input$Sepal.Length) &
      filter_var(iris$Sepal.Width, input$Sepal.Width) &
      filter_var(iris$Species, input$Species)
  })
  
 # output$data <- renderTable(head(iris[selected(), ], 12))

    output$plot <- renderPlot({
      head(iris[selected(), ], 12) %>%
      ggplot(aes(Sepal.Length, Sepal.Width)) + geom_point()
  }, res = 96)
}
shinyApp(ui, server)




output$plot <- renderPlot({
  ggplot(mtcars, aes(wt, mpg)) + geom_point()
}, res = 96)

output$data <- renderTable({
  req(input$plot_click)
  nearPoints(mtcars, input$plot_click)
})














prod <- read.table(text=
                     "week_end    product nts
2021-10-22  A   17
2021-10-15  B   12
2021-10-08  C   18
2021-10-01  A   37
2021-09-24  B   46
2021-09-17  C   27
2021-09-10  A   31
2021-09-03  A   45
2021-08-27  B   23
2021-08-20  B   12", header=T)

ui <- fluidPage(
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectizeInput("productid", "Select product","Names"),
                  dateRangeInput("date_range", "Period you want to see:",
                                 start = min(prod$week_end),
                                 end   = max(prod$week_end),
                                 min   = min(prod$week_end),
                                 max   = max(prod$week_end)
                  )#,
                  # sliderInput("dateid",
                  #             "Slide your Date:",
                  #             min = as.Date(date_range$start_dt,"%Y-%m-%d"),
                  #             max = as.Date(date_range$end_dt,"%Y-%m-%d"),
                  #             value=as.Date(date_range$asofdate,"%Y-%m-%d"),
                  #             timeFormat="%Y-%m-%d")
                ),
                
                mainPanel(
                  fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("ntplot"), DTOutput("t1"))
                  )
                )
  )
)

server <- function(input, output,session) {
  nt_data <- reactive({
    chart_nts <- prod %>%
      dplyr::filter(product %in% input$productid & (week_end >= input$date_range[1] & week_end <= input$date_range[2])) %>%
      group_by(week_end,product) %>%
      dplyr::summarise(c_nts = sum(nts))
    data.frame(chart_nts)
  })
  
  output$t1 <- renderDT({nt_data()})
  
  observe({
    updateSelectizeInput(session,"productid",choices = prod$product)
  })    
  
  output$ntplot <- renderPlot({
    #dateid<-input$dateid
    data <- nt_data() # %>% dplyr::filter(week_end >= input$date_range[1] & week_end <= input$date_range[2]) 
    g <- ggplot(data,aes(y= c_nts, x = week_end)) +
      geom_bar(stat = "identity")
    g
  })
  
  
} 

shinyApp(ui, server)










df


ui <- fluidPage(
  selectInput("x", "X variable", choices = names(iris)),
  selectInput("y", "Y variable", choices = names(iris)),
  plotOutput("plot")
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(iris, aes(.data[[input$x]], .data[[input$y]])) +
      geom_point(position = ggforce::position_auto())
  }, res = 96)
}
shinyApp(ui, server)





ui <- fluidPage(
  selectInput("x", "X variable", choices = names(df)),
  selectInput("y", "Y variable", choices = names(df)),
  selectInput("y", "Y variable", choices = names(df)),
  plotOutput("plot")
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(df, aes(.data[[input$x]], .data[[input$y]])) +
      geom_point(position = ggforce::position_auto())
  }, res = 96)
}
shinyApp(ui, server)




df %>%
  filter(position == .data[[input$z]])
  ggplot(aes(x=Year, y=Freq_per_year, color=var))+
  geom_point()+
#  geom_line(aes(group=interaction(var, position)))
  geom_line(aes(group= position))
  
  
  
  
  
  
  
  ui <- fluidPage(
    selectInput("var", "Select variable", choices = names(mtcars)),
    sliderInput("min", "Minimum value", 0, min = 0, max = 100),
    selectInput("sort", "Sort by", choices = names(mtcars)),
    tableOutput("data")
  )
  server <- function(input, output, session) {
    observeEvent(input$var, {
      rng <- range(mtcars[[input$var]])
      updateSliderInput(
        session, "min", 
        value = rng[[1]], 
        min = rng[[1]], 
        max = rng[[2]]
      )
    })
    
    output$data <- renderTable({
      mtcars %>% 
        filter(.data[[input$var]] > input$min) %>% 
        arrange(.data[[input$sort]])
    })
  }  
  shinyApp(ui, server)

d



library(shiny)
library(dplyr)
library(ggplot2)

ui <- 
  fluidPage(
    sliderInput(inputId = "range", 
                label = "Pick a range for mtcars$wt", 
                min = 1.5,
                max = 5.5,
                value = c(1.5, 5.5)),
    selectInput("x", "X variable", choices = (iris$Species%>% unique()) ),
    
    plotOutput(outputId = "myPlot")
  )

server <-
  function(input, output) {
    output$myPlot <- 
      renderPlot({
        dat <- 
          mtcars %>%
          filter(wt >= input$range[1], 
                 wt <= input$range[2]) %>%
          filter(wt == input$x[1])
        
        
        ggplot(dat, aes(wt, mpg)) +
          geom_point()
      })
    
  }

shinyApp(ui = ui, server = server)

# basic example
shinyApp(
  ui = fluidPage(
    selectInput("variable", "Variable:",
                c("Cylinders" = "cyl",
                  "Transmission" = "am",
                  "Gears" = "gear")),
    tableOutput("data")
  ),
  server = function(input, output) {
    output$data <- renderTable({
      mtcars[, c("mpg", input$variable), drop = FALSE]
    }, rownames = TRUE)
    
    output$plot <- renderPlot({
      mtcars %>%
        ggplot(aes(mpg, input$variable)) + geom_point()
    }, res = 96)
    
    
  }
)


















library(ggplot2)
library(magrittr)
library(tidyverse)
a<-LETTERS[seq(from = 1, to = 26)]
b<-seq(as.Date("2016/1/1"), by="month",length.out=26)
c<-c((rep("L1",13)),rep("L4",13))
d<-c((rep("LA",8)),rep("SQ",6),rep("VI",6),rep("RI",6))
e<-c((rep("SRC",13)),rep("REC",13))
f<-seq(20,200,length.out=26)
g<-seq(60,180,length.out=26)
h<-seq(65,85,length.out=26)
DF<- data.frame("Lot"=a, "Date"=b, "Line"=c, "Flavor"=d, "Plasma"=e, "Yield1"=f, "Yield2"=g, "Yield3"=h)
selectInput("input1", label = "Step",
            choices = c("Yield1", "Yield2", "Yield3"), selected = "Yield3")

selectInput("input2", label = "Facility",
            choices = c("LA", "VI", "RI", "SQ"), selected = "LA")

selectInput("input3", label = "Type",
            choices = c("SRC", "REC"), selected = "SRC")
selectedData <- reactive({
  DF %>% filter(Flavor == input$input2, Plasma == input$input3)
})
ggplot(DF,aes(Date, Yield3)) + geom_line(aes(color=Line)) + facet_wrap(~ Line)
renderPlot({
  ggplot(selectedData(),aes_string("Date", input$input1)) + geom_line(aes_string(color="Line")) + facet_wrap(~ "Line")
})



shinyApp(renderPlot({
  ggplot(selectedData(),aes_string("Date", input$input1)) + geom_line(aes_string(color="Line")) + facet_wrap(~ "Line")
})
)
