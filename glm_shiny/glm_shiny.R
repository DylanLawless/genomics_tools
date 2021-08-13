
library(dplyr)
library(ggplot2)
library(tidyr)


# pheno
n_case = 20 # slider
n_control = 10 # slider
total = n_case+n_control
freq_pheno <- n_case/total
p1 <- total * freq_pheno
pheno <- as.data.frame( rep(1:0, each = p1, len = total) )%>% setNames(.,c("pheno"))

# geno case
freq_case_1 <- 0.5 # slider
freq_case_2 <- 0.6 # slider
n1 <- n_case * freq_case_1
n2 <- n_case * freq_case_2

case_1 <- as.data.frame( rep(1,n1) ) %>% setNames(.,c("SNP1"))
case_0 <- as.data.frame( rep(0,n_case-n1) ) %>% setNames(.,c("SNP1"))
SNP1 <- rbind(case_1, case_0)

case_1 <- as.data.frame( rep(1,n2) ) %>% setNames(.,c("SNP2"))
case_0 <- as.data.frame( rep(0,n_case-n2) ) %>% setNames(.,c("SNP2"))
SNP2 <- rbind(case_1, case_0)

case_geno <- cbind(SNP1, SNP2)

# geno control
freq_control_1 <- 0.5 # slider
freq_control_2 <- 0.3 # slider
n1 <- n_control * freq_control_1
n2 <- n_control * freq_control_2

control_1 <- as.data.frame( rep(1,n1) ) %>% setNames(.,c("SNP1"))
control_0 <- as.data.frame( rep(0,n_control-n1) ) %>% setNames(.,c("SNP1"))
SNP1 <- rbind(control_1, control_0)

control_1 <- as.data.frame( rep(1,n2) ) %>% setNames(.,c("SNP2"))
control_0 <- as.data.frame( rep(0,n_control-n2) ) %>% setNames(.,c("SNP2"))
SNP2 <- rbind(control_1, control_0)

control_geno <- cbind(SNP1, SNP2)

df <- cbind(pheno, rbind(case_geno, control_geno))

q <- gather(df, SNP1:SNP2, key = "var_pos", value = "binary")

fit <- 
  lapply(split(q,
               q$var_pos, drop = TRUE),
         function(x) 
           coef(summary(glm(
             pheno ~ 
               binary,
             family="binomial",
             data=x)))[,'Pr(>|z|)']
  )

fit <- as.data.frame(fit) 

# Wide to long transpose
library(data.table)
fit_long <-transpose(fit)

# get row and colnames in order
colnames(fit_long) <- rownames(fit)
rownames(fit_long) <- colnames(fit)

# rownames to column
fit_long$var_pos <- row.names(fit_long)
library(stringr)
fit_long$Position <- as.numeric( str_replace_all(fit_long$var_pos, "SNP", "") )

fit_long %>%
  ggplot(aes(x=Position, y=(-log(binary)) ))+
  geom_point()+
  theme(axis.title.x=element_blank(), 
        panel.background = element_rect("#F7F7F7"))+ labs(x = "Position", y = "-log10 (Pvalue) ")+
  geom_hline(linetype="dotted", 
             yintercept=-log10(.05/2))





orr <- 
  lapply(split(q,
               q$var_pos, drop = TRUE),
         function(x) 
exp(cbind(coef(glm(pheno ~ binary,family="binomial",data=x)), 
confint(       glm(pheno ~ binary,family="binomial",data=x)))) %>% 
  as.data.frame()
  )

class(orr)
orr_long <- do.call ( rbind , orr) %>% filter(str_detect(row.names(orr_long), 'binary'))

orr_long <- orr_long %>% rename(OR = V1, lower = '2.5 %', higher = '97.5 %')

fit_or <- cbind(fit_long, orr_long)

#arm::coefplot(glm, trans=arm::invlogit, title="x")
#require(MASS)
#or <- exp(cbind(coef(glm), confint(glm)))  %>% as.data.frame()
#or$Position <- 1
#or_ci <- or[2, c(1:3)]
#fit_or <- cbind(fit_long,or_ci )

fit_or$sd <-sqrt(total*(fit_or$higher - fit_or$lower)/3.92)

fit_or %>%
  ggplot(aes(x=Position, y=(-log(binary)) ))+
  geom_point(aes(size=OR, alpha=rev(sd) ))+
  theme(axis.title.x=element_blank(), 
        panel.background = element_rect("#F7F7F7"))+ labs(x = "Position", y = "-log10 (Pvalue) ")+
  geom_hline(linetype="dotted", 
             yintercept=-log10(.05/2))
# Manhat plot with OR size and SD from CI

x








library(shiny)
ui<-navbarPage("Model Developement by Subhasish",
               tabPanel("Data Import",
                        sidebarLayout(sidebarPanel( fileInput("file","Upload your CSV",multiple = FALSE),
                                                    tags$hr(),
                                                    h5(helpText("Select the read.table parameters below")),
                                                    checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                                    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                                    radioButtons(inputId = 'sep', label = 'Separator', 
                                                                 choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                        ),
                        mainPanel(uiOutput("tb1"))
                        ) ),
               tabPanel("Model_dev",
                        sidebarLayout(sidebarPanel(
                          uiOutput("model_select"),
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select")),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("other_val_show"))))
)
server<-function(input,output) { data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  
})  
output$table <- renderTable({
  if(is.null(data())){return ()}
  data()
})
output$tb1 <- renderUI({
  tableOutput("table")
})
output$model_select<-renderUI({
  selectInput("modelselect","Select Algo",choices = c("Logistic_reg"="logreg","SVM"="svm"))
})
output$var1_select<-renderUI({
  selectInput("ind_var_select","Select Independent Var", choices =as.list(names(data())),multiple = FALSE)
})
output$rest_var_select<-renderUI({
  checkboxGroupInput("other_var_select","Select other Var",choices =as.list(names(data())))
})
output$other_val_show<-renderPrint({
  input$other_var_select
  input$ind_var_select
  f<-data()
  
  library(caret)
  form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  print(form)
  

  
  logreg <-glm(as.formula(form),family=binomial(),data=f)
  print(summary(logreg))
  
  
  print(coef(summary(glm(
    as.formula(form),family=binomial(),data=f)))[,'Pr(>|z|)'])
  
  
})

}
shinyApp(ui=ui,server=server)




df <- read.table(file="./data/geno.csv", sep=",", header=TRUE, check.names=FALSE)





























library(shiny)
library(shinydashboard)
library(ggplot2)

#data <-  data.frame(x=c(1,2,3,4),y=c(10,11,12,13))

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sliderInput("sliderA","A", min=1, max=3, step=0.5, value=1),
                   sliderInput("sliderK","K", min=1, max=10, step=1, value=1)),
  dashboardBody(
    fluidRow(column(6,plotOutput('waveplot')))
  ))

server <- function(input, output, session) { 
  output$waveplot <- renderPlot({
    x <- seq(0,10,0.1)
    yfxn <- function(x) { input$sliderA*sin(input$sliderK*x) }
    y <- yfxn(x)
    df <- data.frame(x,y)
    ggplot(df,aes_string(x=x,y=y))+geom_point(size=2)+geom_line()+ 
      scale_x_continuous()
  })
}

shinyApp(ui, server)












#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

Note_text <- 
 ( "Notes to be added" )


library(Rcpp)

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
options(scipen = 999)
# Define UI for slider demo app ----
ui <- fluidPage(
  # App title ----
  titlePanel(""),
  h4(tags$a(href = "https://www.antoinesoetewey.com/", "")),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      # Input: Simple integer interval ----
      numericInput("principal", "Principal (loan amount)", 504000, min = 0, step = 1000),
      hr(),
      numericInput("interest", "Annual interest rate (in %)", 1, min = 0, max = 100, step = 0.01),
      hr(),
      sliderInput("length", "Duration of the loan (in years)",
                  min = 0,
                  max = 70,
                  value = 50,
                  step = 1
      ),
      hr(),
      sliderInput("period", "Initial period",
                  min = 0,
                  max = 40,
                  value = 10,
                  step = 1
      ),
      hr(),
      checkboxInput("plot", "Display plot?", TRUE),
      hr()# ,
    #  HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/mortgage-calculator/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/mortgage-calculator">code</a>. See more information about this app in this <a href="https://www.statsandr.com/blog/mortgage-calculator-r-shiny">article</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a> or <a href="https://www.statsandr.com/">www.statsandr.com</a>.</p>')
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Table summarizing the values entered ----
      uiOutput("text"),
      br(),
      plotOutput("distPlot"),
      br(),
      DT::dataTableOutput("tbl"),
      br(),
      p(em("This R Shiny app is based on code from https://www.antoinesoetewey.com and partially based on the R code of Prof. Thomas Girke.")),
      br(),
      br()
    )
  )
)
# Define server logic for slider examples ----
server <- function(input, output) {
  mortgage <- function(P = 504000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
    J <- I / (12 * 100)
    N <- 12 * L
    M <- P * J / (1 - (1 + J)^(-N))
    monthPay <<- M
    # Calculate Amortization for each Month
    if (amort == TRUE) {
      Pt <- P # current principal or amount of the loan
      currP <- NULL
      while (Pt >= 0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
      aDFmonth <<- data.frame(
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((monthPay - monthP)[1:(length(monthP))])
      )
      aDFmonth <<- subset(aDFmonth, Year <= L * 12)
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
        Year = as.factor(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
    if (plotData == TRUE) {
      aDFyear2 <- aDFyear %>%
        rename(
          Interest = Annual_Interest,
          Payment = Annual_Payment,
          Principal = Annual_Principal
        )
      aDFyear2$Year <- as.factor(aDFyear2$Year)
      aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
      ggplot(aDFyear2, aes(x = Year, 
                           y = value, 
                           fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_vline(#linetype="dotted", 
                   xintercept= input$period)+
        labs(y = "Payment") +
        scale_y_continuous(labels = percent) +
        theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "top")
    }
  }
  output$text <- renderUI({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    HTML(paste0(
      "<h3>", "Notes", "</h3>",
      "", Note_text,
      "<br>",
      "<b>", "Amortization: ", format(round(input$principal, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<h3>", "Summary", "</h3>",
      "Principal (loan amount): ", format(round(input$principal, 2), big.mark = ","),
      "<br>",
      "Annual interest rate: ", input$interest, "%",
      "<br>",
      "Term: ", input$length, " years (", input$length * 12, " months)",
      "<br>",
      "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "Total cost: ", "</b>", format(round(input$principal, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })
  output$distPlot <- renderPlot({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = input$plot)
  })
  # Data output
  output$tbl <- DT::renderDataTable({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    df_month <- DT::datatable(data.frame(round(aDFmonth, 2)),
                              extensions = "Buttons",
                              options = list(
                                lengthChange = TRUE,
                                dom = "Blrtip",
                                buttons = c("copy", "csv", "excel", "pdf", "print"),
                                lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), c("All", "10", "12", "15", "25", "50", "100"))
                              ),
                              rownames = FALSE
    ) %>%
      formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "", interval = 3, mark = ",")
  })
}
# Run the application
shinyApp(ui = ui, server = server)


Principal_year <- aDFmonth %>% filter(Year == 1) %>%
  dplyr::group_by(Year) %>%
  summarise(Principal_year = sum(Principal, na.rm = TRUE))

