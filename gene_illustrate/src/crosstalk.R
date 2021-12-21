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
